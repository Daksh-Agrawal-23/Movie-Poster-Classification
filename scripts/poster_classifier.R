# poster_classifier.R
# Requires: imager, dplyr, tibble, caret, glmnet, pROC
# Install missing packages if needed:
# install.packages(c("imager","dplyr","tibble","caret","glmnet","pROC"))

library(imager)   # used exclusively for image reading and pixel ops (per your requirement)
library(dplyr)
library(tibble)
library(caret)
library(glmnet)
library(pROC)

set.seed(42)

# ---- PARAMETERS ----
base_folder <- "posters"
comedy_folder <- file.path(base_folder, "movies_comedy_firstgenre_1996-2025_cap500")
thriller_folder <- file.path(base_folder, "movies_thriller_firstgenre_1996-2025_cap500")
hue_bins_n <- 8           # number of hue histogram bins
edge_thresh_factor <- 1.0 # will use mean + edge_thresh_factor * sd for edge threshold
center_radius_frac <- 0.25 # fraction of min(width,height) to be center for vignette

# ---- Helper: safe load and preprocess image to numeric arrays ----
read_image_array <- function(path) {
  img <- tryCatch(load.image(path), error = function(e) NULL)
  if (is.null(img)) return(NULL)
  # ensure we have 3 channels (R,G,B). If grayscale, replicate channels.
  arr <- as.array(img) # dims: x, y, cc, frame
  # if no channel dimension (rare), treat as grayscale
  dims <- dim(arr)
  if (length(dims) == 2) {
    # single channel  (x,y)
    arr <- array(arr, dim = c(dims[1], dims[2], 1))
    dims <- dim(arr)
  }
  # if alpha channel present (4 channels), drop alpha (assume last channel)
  if (dims[3] >= 4) {
    arr <- arr[,,1:3,drop=FALSE]
    dims <- dim(arr)
  }
  # if single channel, replicate to R,G,B to simplify later code
  if (dims[3] == 1) {
    arr <- array(rep(arr,3), dim = c(dims[1], dims[2], 3))
  }
  # ensure values are numeric between 0 and 1
  if (max(arr, na.rm=TRUE) > 1) arr <- arr / 255
  return(arr)
}

# ---- Helper: compute HSV from RGB arrays (values range 0..1) ----
rgb_to_hsv_arrays <- function(R, G, B) {
  maxc <- pmax(R, G, B)
  minc <- pmin(R, G, B)
  V <- maxc
  delta <- maxc - minc
  # Saturation
  S <- ifelse(maxc == 0, 0, delta / maxc)
  # Hue (0..1)
  H <- array(0, dim = dim(R))
  # mask where delta != 0
  nz <- delta > 0
  # avoid division by zero
  # where R is max
  maskR <- nz & (R == maxc)
  H[maskR] <- ( (G[maskR] - B[maskR]) / delta[maskR] ) %% 6
  # where G is max
  maskG <- nz & (G == maxc)
  H[maskG] <- ((B[maskG] - R[maskG]) / delta[maskG]) + 2
  # where B is max
  maskB <- nz & (B == maxc)
  H[maskB] <- ((R[maskB] - G[maskB]) / delta[maskB]) + 4
  H <- H / 6  # normalize to 0..1
  H[!nz] <- 0
  # ensure within 0..1
  H <- (H + 1) %% 1
  list(H = H, S = S, V = V)
}

# ---- Helper: compute entropy of a numeric vector (values 0..1) ----
entropy_from_vals <- function(vals, nbins = 256) {
  h <- hist(vals, breaks = seq(0,1,length.out = nbins+1), plot = FALSE)$counts
  p <- h / sum(h)
  p_nozero <- p[p > 0]
  -sum(p_nozero * log2(p_nozero))
}

# ---- Feature extractor for a single image path ----
extract_features_from_image <- function(path, verbose = FALSE) {
  arr <- read_image_array(path)
  if (is.null(arr)) {
    if (verbose) message("Failed to read: ", path)
    return(NULL)
  }
  # arr dims: width x height x 3
  dims <- dim(arr)
  w <- dims[1]; h <- dims[2]
  R <- arr[,,1]
  G <- arr[,,2]
  B <- arr[,,3]
  
  # Grayscale (brightness) — standard luminance formula
  gray <- 0.2989 * R + 0.5870 * G + 0.1140 * B
  
  # Basic brightness and contrast
  mean_brightness <- mean(gray, na.rm = TRUE)
  contrast <- sd(as.vector(gray), na.rm = TRUE)
  
  # Colorfulness — Hasler & Süsstrunk metric
  rg <- R - G
  yb <- 0.5 * (R + G) - B
  std_rg <- sd(as.vector(rg), na.rm = TRUE)
  std_yb <- sd(as.vector(yb), na.rm = TRUE)
  mean_rg <- mean(as.vector(rg), na.rm = TRUE)
  mean_yb <- mean(as.vector(yb), na.rm = TRUE)
  colorfulness <- sqrt(std_rg^2 + std_yb^2) + 0.3 * sqrt(mean_rg^2 + mean_yb^2)
  
  # HSV
  hsv <- rgb_to_hsv_arrays(R, G, B)
  H <- hsv$H  # 0..1
  S <- hsv$S
  V <- hsv$V
  
  # Hue mean and sd (circular mean)
  angles <- 2 * pi * as.vector(H)
  mean_sin <- mean(sin(angles), na.rm = TRUE)
  mean_cos <- mean(cos(angles), na.rm = TRUE)
  circular_mean_angle <- atan2(mean_sin, mean_cos)
  if (circular_mean_angle < 0) circular_mean_angle <- circular_mean_angle + 2*pi
  hue_mean <- circular_mean_angle / (2*pi)
  # circular resultant length r to compute angular sd
  r_len <- sqrt(mean_sin^2 + mean_cos^2)
  # angular standard deviation (approx)
  hue_sd <- sqrt(-2 * log(max(min(r_len, 1-1e-12), 1e-12)))
  
  # Hue histogram (bins normalized)
  hue_vals <- as.vector(H)
  hb <- hist(hue_vals, breaks = seq(0,1,length.out = hue_bins_n+1), plot = FALSE)$counts
  hue_bins <- hb / sum(hb)
  
  # saturation mean
  saturation_mean <- mean(as.vector(S), na.rm = TRUE)
  
  # edge density (approx via simple discrete gradients)
  # compute forward differences
  dx <- matrix(0, nrow = w, ncol = h)
  dy <- matrix(0, nrow = w, ncol = h)
  if (w > 1) dx[1:(w-1), ] <- abs(gray[2:w, , drop = FALSE] - gray[1:(w-1), , drop = FALSE])
  if (h > 1) dy[, 1:(h-1)] <- abs(gray[, 2:h, drop = FALSE] - gray[, 1:(h-1), drop = FALSE])
  grad_mag <- sqrt(dx^2 + dy^2)
  thr <- mean(grad_mag, na.rm = TRUE) + edge_thresh_factor * sd(as.vector(grad_mag), na.rm = TRUE)
  edge_density <- mean(grad_mag > thr, na.rm = TRUE)
  
  # vignette ratio = center mean brightness / border mean brightness
  cx <- (w + 1) / 2
  cy <- (h + 1) / 2
  radius <- min(w, h) * center_radius_frac
  xs <- rep(1:w, times = h)
  ys <- rep(1:h, each = w)
  dists <- sqrt((xs - cx)^2 + (ys - cy)^2)
  mask_center <- matrix(dists <= radius, nrow = w, ncol = h)
  center_mean <- mean(gray[mask_center], na.rm = TRUE)
  border_mean <- mean(gray[!mask_center], na.rm = TRUE)
  vignette_ratio <- ifelse(is.na(border_mean) || border_mean == 0, NA, center_mean / border_mean)
  
  # channel entropies
  ent_R <- entropy_from_vals(as.vector(R), nbins = 256)
  ent_G <- entropy_from_vals(as.vector(G), nbins = 256)
  ent_B <- entropy_from_vals(as.vector(B), nbins = 256)
  ent_mean <- mean(c(ent_R, ent_G, ent_B), na.rm = TRUE)
  
  # Build a named vector (data.frame row)
  feat <- tibble(
    filepath = path,
    mean_brightness = mean_brightness,
    contrast = contrast,
    colorfulness = colorfulness,
    hue_mean = hue_mean,
    hue_sd = hue_sd,
    saturation_mean = saturation_mean,
    edge_density = edge_density,
    vignette_ratio = vignette_ratio,
    entropy_R = ent_R,
    entropy_G = ent_G,
    entropy_B = ent_B,
    entropy_mean = ent_mean
  )
  # append hue bins as separate columns
  for (i in seq_len(length(hue_bins))) {
    feat[[paste0("hue_bin_", i)]] <- hue_bins[i]
  }
  
  return(feat)
}

# ---- Collect image file paths ----
get_image_files_safe <- function(folder) {
  if (!dir.exists(folder)) return(character(0))
  exts <- c("png","jpg","jpeg","tiff","bmp","gif")
  files <- list.files(folder, pattern = paste0("\\.(", paste(exts, collapse="|"), ")$"), full.names = TRUE, ignore.case = TRUE)
  return(files)
}

comedy_files <- get_image_files_safe(comedy_folder)
thriller_files <- get_image_files_safe(thriller_folder)

if (length(comedy_files) == 0 && length(thriller_files) == 0) stop("No image files found in the two folders. Check paths.")

all_files <- c(comedy_files, thriller_files)
labels <- c(rep("comedy", length(comedy_files)), rep("thriller", length(thriller_files)))

# ---- Extract features with progress bar ----
n_files <- length(all_files)
cat(sprintf("Found %d images (%d comedy, %d thriller). Extracting features...\n", n_files, length(comedy_files), length(thriller_files)))
pb <- txtProgressBar(min = 0, max = n_files, style = 3)
feat_list <- vector("list", n_files)
for (i in seq_along(all_files)) {
  feat_list[[i]] <- extract_features_from_image(all_files[i])
  setTxtProgressBar(pb, i)
}
close(pb)

# bind into data.frame and attach labels
df_feats <- bind_rows(feat_list) %>%
  mutate(label = factor(labels, levels = c("comedy", "thriller")),
         label_num = ifelse(label == "comedy", 0L, 1L)) # numeric 0/1 for some models

# remove rows with NA features (if any)
na_rows <- which(!complete.cases(df_feats))
if (length(na_rows) > 0) {
  message("Dropping ", length(na_rows), " images due to NA features.")
  df_feats <- df_feats[complete.cases(df_feats), ]
}

# ---- Prepare dataset for modeling ----
# drop filepath
model_df <- df_feats[, !(names(df_feats) %in% "filepath"), drop = FALSE]

# Split into train/test (stratified)
train_index <- createDataPartition(model_df$label, p = 0.8, list = FALSE)
train <- model_df[train_index, ]
test  <- model_df[-train_index, ]

# features names
feature_names <- setdiff(names(train), c("label","label_num"))

# ---- Fit models ----


lm_mod <- lm(label_num ~ ., data = train[, c("label_num", feature_names), drop = FALSE])
lm_preds_raw <- predict(lm_mod, newdata = test)
# clamp to 0..1 then classify threshold 0.5
lm_preds_prob <- pmin(pmax(lm_preds_raw, 0), 1)
lm_preds_label <- factor(ifelse(lm_preds_prob > 0.5, "thriller", "comedy"), levels = levels(test$label))

# 2) Logistic regression (glm binomial)
glm_mod <- glm(label_num ~ ., data = train[, c("label_num", feature_names), drop = FALSE],
               family = binomial(link = "logit"))
glm_preds_prob <- predict(glm_mod, newdata = test, type = "response")
glm_preds_label <- factor(ifelse(glm_preds_prob > 0.5, "thriller", "comedy"), levels = levels(test$label))

# 3) Penalized logistic (glmnet) with cross-validated lambda
x_train <- as.matrix(train[, feature_names, drop = FALSE])
y_train <- train$label_num
x_test  <- as.matrix(test[, feature_names, drop = FALSE])
cv_glmnet <- cv.glmnet(x_train, y_train, family = "binomial", alpha = 1, nfolds = 5) # LASSO (alpha=1)
glmnet_mod <- cv_glmnet
glmnet_preds_prob <- predict(glmnet_mod, newx = x_test, s = "lambda.min", type = "response")[,1]
glmnet_preds_label <- factor(ifelse(glmnet_preds_prob > 0.5, "thriller", "comedy"), levels = levels(test$label))

# ---- Evaluation helper ----
eval_classification <- function(true_labels, pred_labels, pred_probs = NULL) {
  cm <- confusionMatrix(pred_labels, true_labels, positive = "thriller")
  acc <- cm$overall["Accuracy"]
  sens <- cm$byClass["Sensitivity"]
  spec <- cm$byClass["Specificity"]
  prec <- cm$byClass["Precision"]
  recall <- sens
  f1 <- 2 * (prec * recall) / (prec + recall)
  auc <- NA
  if (!is.null(pred_probs)) {
    rocobj <- tryCatch(roc(response = as.numeric(true_labels == "thriller"), predictor = as.numeric(pred_probs)), error = function(e) NULL)
    if (!is.null(rocobj)) auc <- auc(rocobj)
  }
  list(confusion = cm$table, accuracy = unname(acc), sensitivity = unname(sens), specificity = unname(spec),
       precision = unname(prec), recall = unname(recall), F1 = unname(f1), AUC = as.numeric(auc))
}

cat("\n--- Evaluation on Test Set ---\n")
lm_eval <- eval_classification(test$label, lm_preds_label, lm_preds_prob)
cat("\nLM (linear regression on 0/1) results:\n")
print(lm_eval)

glm_eval <- eval_classification(test$label, glm_preds_label, glm_preds_prob)
cat("\nGLM (logistic) results:\n")
print(glm_eval)

glmnet_eval <- eval_classification(test$label, glmnet_preds_label, glmnet_preds_prob)
cat("\nGLMNET (penalized logistic) results:\n")
print(glmnet_eval)

# ---- Report important stats / variable importance ----
cat("\n--- Model Summaries & Important Variables ---\n")

cat("\nLM coefficients (top by absolute value):\n")
lm_coefs <- summary(lm_mod)$coefficients
lm_coefs_df <- as.data.frame(lm_coefs) %>%
  rownames_to_column("feature") %>%
  filter(feature != "(Intercept)") %>%
  mutate(abs_est = abs(Estimate)) %>%
  arrange(desc(abs_est))
print(head(lm_coefs_df, 10))

cat("\nGLM (logistic) coefficients (top by absolute value):\n")
glm_coefs <- summary(glm_mod)$coefficients
glm_coefs_df <- as.data.frame(glm_coefs) %>%
  rownames_to_column("feature") %>%
  filter(feature != "(Intercept)") %>%
  mutate(abs_est = abs(Estimate)) %>%
  arrange(desc(abs_est))
print(head(glm_coefs_df, 10))

cat("\nGLMNET coefficients at lambda.min (non-zero):\n")
glmnet_coef_mat <- coef(glmnet_mod, s = "lambda.min")
# convert to tidy
coefs_sparse <- as.matrix(glmnet_coef_mat)
coefs_df <- data.frame(
  feature = rownames(coefs_sparse),
  coefficient = as.numeric(coefs_sparse)
)
coefs_df <- coefs_df %>% filter(feature != "(Intercept)") %>% arrange(desc(abs(coefficient)))
print(head(coefs_df[coefs_df$coefficient != 0, ], 20))

# ---- Save results ----
results <- list(
  feature_dataframe = model_df,
  train = train,
  test = test,
  models = list(lm = lm_mod, glm = glm_mod, glmnet = glmnet_mod),
  predictions = list(lm = list(prob = lm_preds_prob, label = lm_preds_label),
                     glm = list(prob = glm_preds_prob, label = glm_preds_label),
                     glmnet = list(prob = glmnet_preds_prob, label = glmnet_preds_label)),
  evaluations = list(lm = lm_eval, glm = glm_eval, glmnet = glmnet_eval)
)

# optionally save to RDS
saveRDS(results, file = "poster_classification_results.rds")
cat("\nSaved results to poster_classification_results.rds\n")
