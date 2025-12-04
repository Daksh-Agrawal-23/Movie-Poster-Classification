# 🎬 Movie Poster Classification Pipeline (R)

This repository contains a complete workflow for building a **movie poster classifier** using R.  
It includes robust poster scraping from TMDb, feature extraction from images, and training classical machine-learning models to distinguish between **Comedy** and **Thriller** movie posters.

---

## 🚀 Features

### ✔ Poster Scraping & Dataset Creation
- Fetch movie metadata from TMDb (genre, year, popularity)
- Download poster images with:
  - retry handling  
  - rate-limit safe delays  
  - SHA-safe filenames  
  - periodic progress logs  
- Automatically organizes posters into genre-labeled folders
- Produces a full log for every file processed

### ✔ Feature Extraction (Image-Based)
Using the `imager` package, each poster is converted into a rich numeric feature set:
- mean brightness & contrast  
- colorfulness (Hasler–Süsstrunk)  
- hue circular mean / circular SD  
- 8-bin hue histogram  
- saturation statistics  
- edge density from gradient magnitude  
- vignette ratio (center vs border brightness)  
- RGB channel entropies  

### ✔ Machine Learning Models
Trained models include:
- Linear regression (baseline)
- Logistic regression (GLM)
- LASSO-regularized logistic regression (glmnet)

All models are evaluated using:
- accuracy
- precision, recall, F1
- sensitivity, specificity
- ROC–AUC

### ✔ Reproducible Outputs
The full pipeline saves:
- extracted feature dataframe  
- train/test splits  
- trained models  
- predictions  
- evaluation metrics  
- compiled results (`poster_classification_results.rds`)

---

## 📂 Project Structure

```
/posters/
    /movies_comedy_firstgenre_1996-2025_cap500/
    /movies_thriller_firstgenre_1996-2025_cap500/

/scripts/
    poster_downloader.R
    poster_classifier.R

/output/
    poster_classification_results.rds
    genre-wise download logs
```

---

## 🛠 Requirements

Install dependencies:

```r
install.packages(c(
  "httr", "jsonlite", "data.table", "stringi",
  "imager", "dplyr", "tibble", "caret",
  "glmnet", "pROC"
))
```

---

## ▶️ Usage

### 1. Download Posters  
Edit `TMDB_API_KEY` and run:

```r
source("scripts/poster_downloader.R")
```

### 2. Train Models

```r
source("scripts/poster_classifier.R")
```

### 3. View Results

Outputs are saved in `/output` as RDS files and printed in-console.

---

## 📌 Notes
- TMDb API key is required to run the scraper.
- Training data can be resized, refiltered, or expanded by modifying the input TSV files.
- The feature extraction pipeline is fully image-based and reproducible.

---

## 💡 Purpose

This project demonstrates:
- practical web scraping  
- robust TMDb API handling  
- classical machine-learning with image-derived features  
- interpretable poster-style classification  

A clean, reproducible R project showcasing the power of structured feature engineering.

---
