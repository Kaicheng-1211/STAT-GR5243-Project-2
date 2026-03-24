# STAT-GR5243 Project 2: DataScope — Interactive Data Analysis Web Application

**DataScope** is a comprehensive web application built with **R Shiny** that enables users to upload datasets, clean and preprocess data, engineer new features, conduct exploratory data analysis, fit probability distributions, and run regression diagnostics — all through an interactive, code-free interface.

## 🚀 Live Demo

> **Deployed App**: [https://statgr5243team18.shinyapps.io/stat-gr5243-project-2/](https://statgr5243team18.shinyapps.io/stat-gr5243-project-2/)

## ✨ Features

### 1. Dataset Loading
- Upload files in **CSV**, **Excel (.xlsx/.xls)**, **JSON**, **TSV**, and **RDS** formats
- Three built-in datasets: `iris`, `mtcars`, `diamonds`
- Automatic **data profiling** with column types, missing %, unique counts, and actionable alerts
- **Missingness heatmap** and row-level imputation diagnostics

### 2. Data Cleaning & Preprocessing
- Missing values: remove rows, remove columns (by threshold %), or impute (mean/median/mode)
- Duplicate detection and removal
- Scaling: Z-Score, Min-Max, Robust (Median/IQR)
- Categorical encoding: Label Encoding, One-Hot Encoding
- Outlier detection (IQR, Z-Score) with treatment options (remove, cap/winsorize, flag)
- Before/after comparison view and cleaning log

### 3. Feature Engineering
- Transformations: Log, Sqrt, Square, Box-Cox, Reciprocal, Cube Root
- Interaction term builder (multiply, add, divide)
- Binning: Equal-width and Quantile-based
- Custom R formula engine
- **Live before/after distribution previews**
- **Skewness report** with auto-suggestions for transformations
- 10-level undo stack

### 4. Exploratory Data Analysis (EDA)
- 7 interactive plot types: Histogram, Density, Scatter, Boxplot, Violin, Bar, QQ-Plot
- All plots rendered via **plotly** (hover, zoom, download)
- Dynamic data filtering (numeric range sliders, categorical checkboxes)
- Correlation heatmap (Pearson, Spearman, Kendall)
- **Pairs plots** via GGally
- **Spectral analysis** with periodogram for frequency detection
- Summary statistics via `skimr`
- KPI dashboard (rows, columns, missing %, type breakdown)

### 5. Distribution Fitting Lab
- Fit multiple distributions simultaneously: Normal, Log-Normal, Exponential, Gamma, Weibull
- **Density overlay** plots (empirical vs. fitted)
- **AIC/BIC/KS comparison table**
- **Cullen-Frey** (skewness-kurtosis) diagram for distribution selection
- Goodness-of-fit diagnostic plots

### 6. Regression Diagnostics
- Specify response and predictors for an OLS linear model
- **Cook's Distance** bar chart with threshold line
- **Leverage plot** (studentized residuals vs. hat values)
- **DFFITS** influence measures
- Color-coded influence table (🔴 high influence, 🟡 high leverage, 🟢 normal)
- One-click removal of influential points with automatic model re-fitting
- Standard residual diagnostic plots (Residuals vs Fitted, Normal Q-Q, Scale-Location, Cook's D)

### 7. User Experience
- Modern **Bootstrap 5** UI via `bslib`
- Responsive design for desktop and mobile
- Premium styling with gradient accents, card animations, and smooth transitions
- Comprehensive **User Guide** with step-by-step walkthrough and FAQ accordion

## 📦 Requirements

- **R** (version 4.2 or higher)
- **RStudio** (recommended)

### Install Dependencies

```r
install.packages(c(
  "shiny", "bslib", "shinyjs", "shinyWidgets", "shinyFeedback", "waiter",
  "readr", "readxl", "jsonlite",
  "dplyr", "tidyr", "recipes",
  "ggplot2", "plotly", "corrplot", "GGally", "thematic",
  "moments", "fitdistrplus", "naniar",
  "skimr", "DT", "MASS", "purrr"
))
```

## 🏗️ Project Structure

```
STAT-GR5243-Project-2/
├── app.R                        # Entry point
├── global.R                     # Packages, themes, helper functions
├── R/
│   ├── mod_guide.R              # User Guide module
│   ├── mod_upload.R             # Dataset loading & profiling
│   ├── mod_cleaning.R           # Data cleaning & preprocessing
│   ├── mod_feature.R            # Feature engineering
│   ├── mod_eda.R                # Exploratory data analysis
│   ├── mod_distributions.R      # Distribution fitting lab
│   └── mod_diagnostics.R        # Regression diagnostics
├── www/
│   └── custom.css               # Custom styling
├── README.md
├── requirements.txt
└── STAT-GR5243-Project-2.Rproj
```

## ▶️ How to Run

1. Clone this repository:
   ```bash
   git clone https://github.com/YOUR-USERNAME/STAT-GR5243-Project-2.git
   cd STAT-GR5243-Project-2
   ```
2. Open `STAT-GR5243-Project-2.Rproj` in RStudio
3. Install dependencies (see above)
4. Click **"Run App"** in RStudio, or run:
   ```r
   shiny::runApp()
   ```

## 👥 Contributors

- Ayaz Khan (aak2259)
- Kaicheng Li (kl3728)
- Yueyou Tao (yt2995)
- Tiange Wang (tw3106)
