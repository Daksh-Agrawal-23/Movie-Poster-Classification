# Robust TMDb poster downloader with progress indicator
# Requirements: httr, jsonlite, data.table, stringi
# install.packages(c("httr","jsonlite","data.table","stringi"))

library(httr)
library(jsonlite)
library(data.table)
library(stringi)

# --------- USER SETTINGS ----------
TMDB_API_KEY <- "1e893b6d64d6509d92d2b622cd40437c"   # <- set your key
files <- c("movies_comedy_firstgenre_1996-2025_cap500.tsv",
           "movies_thriller_firstgenre_1996-2025_cap500.tsv")
out_dir_base <- "posters"
per_request_delay <- 0.45   # politeness base delay (seconds)
max_retries_tmdb <- 6
tmdb_retry_pause <- 2
download_timeout <- 60      # seconds
status_print_every <- 50    # print status every N items
# ----------------------------------

if (TMDB_API_KEY == "" || TMDB_API_KEY == "YOUR_TMDB_API_KEY_HERE") stop("Set TMDB_API_KEY before running.")

dir.create(out_dir_base, showWarnings = FALSE, recursive = TRUE)

safe_name <- function(x) {
  x <- iconv(x, to = "ASCII//TRANSLIT")
  x <- gsub("[^A-Za-z0-9 _-]", "", x)
  x <- gsub("\\s+", "_", trimws(x))
  x <- substr(x, 1, 120)
  tolower(ifelse(nchar(x) == 0, "unknown", x))
}

# Safe TMDb search wrapper: returns parsed list or NULL
tmdb_search_movie_safe <- function(title, year = NULL, api_key = TMDB_API_KEY) {
  params <- list(api_key = api_key, query = title, include_adult = "false", page = 1)
  if (!is.null(year) && !is.na(year)) params$year <- as.integer(year)
  url <- modify_url("https://api.themoviedb.org/3/search/movie", query = params)
  
  attempt <- 1L
  repeat {
    Sys.sleep(per_request_delay * runif(1, 0.9, 1.1))
    resp <- tryCatch(
      RETRY("GET", url, times = 1, pause_base = tmdb_retry_pause, pause_cap = 30, terminate_on = c(401)),
      error = function(e) e
    )
    if (inherits(resp, "error")) {
      if (attempt >= max_retries_tmdb) {
        warning("TMDb search failed (network) for: ", title, " (year=", year, ") — ", resp$message)
        return(NULL)
      } else {
        wait <- tmdb_retry_pause * attempt
        message(sprintf("Network error on TMDb search (attempt %d/%d). Waiting %ds then retrying...", attempt, max_retries_tmdb, wait))
        Sys.sleep(wait)
        attempt <- attempt + 1
        next
      }
    }
    status <- status_code(resp)
    if (status != 200) {
      if (status == 429 && attempt < max_retries_tmdb) {
        wait <- tmdb_retry_pause * attempt
        message(sprintf("TMDb 429 (rate limit). Attempt %d/%d. Pausing %ds before retry...", attempt, max_retries_tmdb, wait))
        Sys.sleep(wait)
        attempt <- attempt + 1
        next
      } else {
        warning("TMDb search returned status ", status, " for: ", title)
        return(NULL)
      }
    }
    txt <- tryCatch(content(resp, as = "text", encoding = "UTF-8"), error = function(e) NULL)
    if (is.null(txt) || nchar(txt) == 0) return(NULL)
    parsed <- tryCatch(fromJSON(txt, flatten=TRUE), error = function(e) { warning("Failed to parse TMDb JSON for: ", title, " (", e$message, ")"); return(NULL) })
    return(parsed)
  }
}

tmdb_poster_url <- function(poster_path, size = "w500") {
  if (is.null(poster_path) || poster_path == "") return(NULL)
  paste0("https://image.tmdb.org/t/p/", size, poster_path)
}

# iterate files with progress bar per file
for (f in files) {
  message("Processing file: ", f)
  dt <- tryCatch(fread(f, sep = "\t", na.strings = c("", "NA", "\\N")), error = function(e) stop("Cannot read file: ", f, " : ", e$message))
  has_tconst <- "tconst" %in% names(dt)
  title_col <- if ("primaryTitle" %in% names(dt)) "primaryTitle" else if ("title" %in% names(dt)) "title" else stop("No title column found in ", f)
  year_col <- if ("startYear" %in% names(dt)) "startYear" else if ("year" %in% names(dt)) "year" else NA_character_
  genre_tag <- tools::file_path_sans_ext(basename(f))
  out_dir <- file.path(out_dir_base, genre_tag)
  dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
  
  total_items <- nrow(dt)
  pb <- txtProgressBar(min = 0, max = total_items, style = 3)
  # counters for status
  cnt_downloaded <- 0L
  cnt_exists <- 0L
  cnt_no_match <- 0L
  cnt_no_poster <- 0L
  cnt_download_failed <- 0L
  cnt_other_fail <- 0L
  
  # consistent log rows
  log_rows <- vector("list", 0L)
  add_log <- function(idx, title, year, poster, status, tmdb_id = NA_integer_) {
    log_rows[[length(log_rows)+1L]] <<- list(idx = as.integer(idx),
                                             title = as.character(title),
                                             year = ifelse(is.na(year), NA_integer_, as.integer(year)),
                                             poster = ifelse(is.null(poster), NA_character_, as.character(poster)),
                                             status = as.character(status),
                                             tmdb_id = ifelse(is.null(tmdb_id) || is.na(tmdb_id), NA_integer_, as.integer(tmdb_id)))
  }
  
  for (i in seq_len(total_items)) {
    row <- dt[i]
    id_for_file <- if (has_tconst) as.character(row$tconst) else NULL
    title <- as.character(row[[title_col]])
    year <- if (!is.na(year_col)) as.integer(row[[year_col]]) else NA_integer_
    file_base <- if (!is.null(id_for_file) && nzchar(id_for_file)) id_for_file else paste0(safe_name(title), if (!is.na(year)) paste0("_", year) else "")
    poster_filename <- file.path(out_dir, paste0(file_base, ".jpg"))
    
    # skip if exists
    if (file.exists(poster_filename) && file.info(poster_filename)$size > 2000) {
      cnt_exists <- cnt_exists + 1L
      add_log(i, title, year, poster_filename, "exists", NA_integer_)
      setTxtProgressBar(pb, i)
      if (i %% status_print_every == 0 || i == total_items) {
        message(sprintf("[ %s ] %d/%d — downloaded:%d exists:%d no_match:%d no_poster:%d dl_fail:%d other_fail:%d",
                        genre_tag, i, total_items, cnt_downloaded, cnt_exists, cnt_no_match, cnt_no_poster, cnt_download_failed, cnt_other_fail))
      }
      next
    }
    
    # search TMDb: try year first, then without year
    parsed <- NULL
    if (!is.na(year)) parsed <- tmdb_search_movie_safe(title, year)
    if (is.null(parsed) || is.null(parsed$results) || length(parsed$results) == 0) parsed <- tmdb_search_movie_safe(title, NULL)
    
    if (is.null(parsed) || is.null(parsed$results) || length(parsed$results) == 0) {
      cnt_no_match <- cnt_no_match + 1L
      add_log(i, title, year, NA_character_, "no_match", NA_integer_)
      setTxtProgressBar(pb, i)
      if (i %% status_print_every == 0 || i == total_items) {
        message(sprintf("[ %s ] %d/%d — downloaded:%d exists:%d no_match:%d no_poster:%d dl_fail:%d other_fail:%d",
                        genre_tag, i, total_items, cnt_downloaded, cnt_exists, cnt_no_match, cnt_no_poster, cnt_download_failed, cnt_other_fail))
      }
      next
    }
    
    # coerce results to data.frame safely
    results_df <- tryCatch({
      if (is.data.frame(parsed$results)) parsed$results else as.data.frame(parsed$results, stringsAsFactors = FALSE)
    }, error = function(e) parsed$results)
    
    # pick best candidate (prefer year match when available)
    best <- NULL
    if (!is.na(year) && "release_date" %in% names(results_df)) {
      results_df$year <- suppressWarnings(as.integer(substr(results_df$release_date, 1, 4)))
      exact_year_idx <- which(!is.na(results_df$year) & results_df$year == year)
      if (length(exact_year_idx) > 0) {
        best <- results_df[ exact_year_idx[which.max(results_df$popularity[exact_year_idx])], , drop = FALSE ]
      }
    }
    if (is.null(best)) best <- results_df[ which.max(results_df$popularity), , drop = FALSE ]
    
    poster_path <- if ("poster_path" %in% names(best)) best$poster_path else NA_character_
    tmdb_id <- if ("id" %in% names(best)) best$id else NA_integer_
    
    if (is.null(poster_path) || is.na(poster_path) || poster_path == "") {
      cnt_no_poster <- cnt_no_poster + 1L
      add_log(i, title, year, NA_character_, "no_poster", tmdb_id)
      setTxtProgressBar(pb, i)
      if (i %% status_print_every == 0 || i == total_items) {
        message(sprintf("[ %s ] %d/%d — downloaded:%d exists:%d no_match:%d no_poster:%d dl_fail:%d other_fail:%d",
                        genre_tag, i, total_items, cnt_downloaded, cnt_exists, cnt_no_match, cnt_no_poster, cnt_download_failed, cnt_other_fail))
      }
      next
    }
    
    poster_url <- tmdb_poster_url(poster_path, size = "w500")
    # download with retries/backoff
    dl_ok <- FALSE
    for (dtry in seq_len(5)) {
      Sys.sleep(0.2 * dtry)
      resp_dl <- tryCatch(
        GET(poster_url, write_disk(poster_filename, overwrite = TRUE), timeout(download_timeout)),
        error = function(e) e
      )
      if (inherits(resp_dl, "error")) {
        # network error, continue to retry
        next
      }
      if (status_code(resp_dl) == 200 && file.exists(poster_filename) && file.info(poster_filename)$size > 2000) {
        dl_ok <- TRUE
        break
      } else {
        if (file.exists(poster_filename)) try(unlink(poster_filename), silent = TRUE)
      }
    }
    
    if (dl_ok) {
      cnt_downloaded <- cnt_downloaded + 1L
      add_log(i, title, year, poster_filename, "downloaded", tmdb_id)
    } else {
      cnt_download_failed <- cnt_download_failed + 1L
      add_log(i, title, year, NA_character_, "download_failed", tmdb_id)
    }
    
    # update progress bar and periodic status
    setTxtProgressBar(pb, i)
    if (i %% status_print_every == 0 || i == total_items) {
      message(sprintf("[ %s ] %d/%d — downloaded:%d exists:%d no_match:%d no_poster:%d dl_fail:%d other_fail:%d",
                      genre_tag, i, total_items, cnt_downloaded, cnt_exists, cnt_no_match, cnt_no_poster, cnt_download_failed, cnt_other_fail))
    }
  } # end file rows
  
  close(pb)
  # safe assemble log
  if (length(log_rows) == 0) {
    log_dt <- data.table(idx = integer(), title = character(), year = integer(), poster = character(), status = character(), tmdb_id = integer())
  } else {
    log_dt <- rbindlist(lapply(log_rows, as.data.table), fill = TRUE)
    for (cn in c("idx","title","year","poster","status","tmdb_id")) if (!(cn %in% names(log_dt))) log_dt[[cn]] <- NA
  }
  log_file <- file.path(out_dir, paste0("poster_log_", genre_tag, ".csv"))
  fwrite(log_dt, log_file)
  message("Finished file: ", f, " ; log -> ", log_file,
          sprintf(" (downloaded:%d exists:%d no_match:%d no_poster:%d dl_fail:%d)", cnt_downloaded, cnt_exists, cnt_no_match, cnt_no_poster, cnt_download_failed))
} # end files

message("All done.")
