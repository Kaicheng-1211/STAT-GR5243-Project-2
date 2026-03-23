# =============================================================================
# global.R — Package Loading, Helper Functions & Built-in Data
# STAT-GR5243 Project 2: Interactive Data Analysis App
# =============================================================================

# ── Core Shiny ──
library(shiny)
library(bslib)
library(shinyjs)
library(shinyWidgets)
library(shinyFeedback)
library(waiter)

# ── Data I/O ──
library(readr)
library(readxl)
library(jsonlite)

# ── Data Manipulation ──
library(dplyr)
library(tidyr)
library(recipes)

# ── Visualization ──
library(ggplot2)
library(plotly)
library(corrplot)
library(GGally)
library(thematic)

# ── Statistical Analysis ──
library(moments) # skewness, kurtosis
library(fitdistrplus) # distribution fitting
library(naniar) # missing value visualization

# ── Data Summaries ──
library(skimr)
library(DT)

# ── Auto-theme ggplot2 to match bslib theme ──
thematic_shiny()

# =============================================================================
# App Theme (Bootstrap 5 via bslib)
# =============================================================================
app_theme <- bs_theme(
    version = 5,
    bootswatch = "flatly",
    primary = "#2c3e50",
    secondary = "#18bc9c",
    success = "#18bc9c",
    info = "#3498db",
    warning = "#f39c12",
    danger = "#e74c3c",
    base_font = font_google("Inter"),
    heading_font = font_google("Inter"),
    code_font = font_google("Fira Code"),
    "navbar-bg" = "#1a252f",
    "body-bg" = "#f8f9fa"
)

# Dark mode variant
app_theme_dark <- bs_theme(
    version = 5,
    bootswatch = "darkly",
    primary = "#375a7f",
    secondary = "#00bc8c",
    success = "#00bc8c",
    info = "#3498db",
    warning = "#f39c12",
    danger = "#e74c3c",
    base_font = font_google("Inter"),
    heading_font = font_google("Inter"),
    code_font = font_google("Fira Code")
)

# =============================================================================
# Built-in Datasets
# =============================================================================
BUILTIN_DATASETS <- list(
    "iris"     = iris,
    "mtcars"   = mtcars,
    "diamonds" = ggplot2::diamonds[sample(nrow(ggplot2::diamonds), 2000), ]
)

BUILTIN_DESCRIPTIONS <- list(
    "iris" = "Fisher's Iris flower dataset (150 obs, 5 vars). Classic dataset for classification with sepal/petal measurements across 3 species.",
    "mtcars" = "Motor Trend car data (32 obs, 11 vars). Fuel consumption and 10 aspects of automobile design for 32 cars.",
    "diamonds" = "Diamond prices dataset (2,000 sampled obs, 10 vars). Prices and attributes of ~54K diamonds including carat, cut, color, clarity."
)

# =============================================================================
# Helper Functions
# =============================================================================

#' Backtick-escape column names for safe use in R formulas
#' Handles names with special characters like %, $, spaces, etc.
bt <- function(x) {
    ifelse(grepl("^[a-zA-Z.][a-zA-Z0-9._]*$", x), x, paste0("`", x, "`"))
}

#' Read a dataset from an uploaded file based on its extension
read_uploaded_file <- function(filepath, filename) {
    ext <- tolower(tools::file_ext(filename))
    tryCatch(
        {
            switch(ext,
                "csv" = readr::read_csv(filepath, show_col_types = FALSE),
                "xlsx" = readxl::read_excel(filepath),
                "xls" = readxl::read_excel(filepath),
                "json" = {
                    raw <- jsonlite::fromJSON(filepath)
                    if (is.data.frame(raw)) raw else as.data.frame(raw)
                },
                "rds" = readRDS(filepath),
                "tsv" = readr::read_tsv(filepath, show_col_types = FALSE),
                stop("Unsupported file format: .", ext)
            )
        },
        error = function(e) {
            message("Error reading file: ", e$message)
            NULL
        }
    )
}

#' Generate a data profile summary for a data.frame
generate_data_profile <- function(df) {
    profile <- data.frame(
        Column = names(df),
        Type = sapply(df, function(x) class(x)[1]),
        N_Missing = sapply(df, function(x) sum(is.na(x))),
        Pct_Missing = round(sapply(df, function(x) mean(is.na(x))) * 100, 1),
        N_Unique = sapply(df, function(x) length(unique(x))),
        N_Total = nrow(df),
        stringsAsFactors = FALSE
    )
    profile$Alert <- ifelse(
        profile$Pct_Missing > 50, "!! >50% missing - consider dropping",
        ifelse(profile$Pct_Missing > 20, "! >20% missing - imputation recommended",
            ifelse(profile$N_Unique == 1, "!! Constant column",
                ifelse(profile$N_Unique == profile$N_Total & profile$Type == "character",
                    "! All unique (possible ID column)", "OK"
                )
            )
        )
    )
    profile
}

#' Compute skewness and kurtosis for all numeric columns
compute_moments <- function(df) {
    num_cols <- names(df)[sapply(df, is.numeric)]
    if (length(num_cols) == 0) {
        return(data.frame())
    }

    results <- lapply(num_cols, function(col) {
        x <- na.omit(as.numeric(df[[col]]))
        if (length(x) < 4) {
            return(data.frame(
                Column = col, Skewness = NA_real_, Kurtosis = NA_real_,
                Skew_Direction = "Insufficient data", Normality_p = NA_real_,
                Suggestion = "Need >= 4 non-NA observations",
                stringsAsFactors = FALSE
            ))
        }
        sk <- moments::skewness(x)
        ku <- moments::kurtosis(x)
        sw_p <- tryCatch(shapiro.test(x[1:min(length(x), 5000)])$p.value, error = function(e) NA_real_)

        direction <- if (sk < -0.5) "Left-skewed" else if (sk > 0.5) "Right-skewed" else "Approx. symmetric"
        suggestion <- if (abs(sk) > 2) "Strong skew - try log or Box-Cox" else if (abs(sk) > 1) "Moderate skew - consider sqrt or log" else if (abs(sk) > 0.5) "Mild skew - may benefit from sqrt" else "No transform needed"

        data.frame(
            Column = col, Skewness = round(sk, 3), Kurtosis = round(ku, 3),
            Skew_Direction = direction, Normality_p = round(sw_p, 4),
            Suggestion = suggestion, stringsAsFactors = FALSE
        )
    })
    do.call(rbind, results)
}

#' Fit multiple distributions to a numeric vector and return comparison table
fit_distributions <- function(x) {
    x <- na.omit(as.numeric(x))
    if (length(x) < 10) {
        return(NULL)
    }

    dists <- c("norm")
    if (all(x > 0)) dists <- c(dists, "lnorm", "exp", "gamma", "weibull")

    fits <- list()
    comparison <- list()

    for (d in dists) {
        tryCatch(
            {
                f <- fitdistrplus::fitdist(x, d)
                fits[[d]] <- f
                comparison[[d]] <- data.frame(
                    Distribution = d,
                    AIC = round(f$aic, 2),
                    BIC = round(f$bic, 2),
                    LogLik = round(f$loglik, 2),
                    stringsAsFactors = FALSE
                )
            },
            error = function(e) NULL
        )
    }

    comp_df <- do.call(rbind, comparison)
    if (!is.null(comp_df)) comp_df <- comp_df[order(comp_df$AIC), ]

    list(fits = fits, comparison = comp_df)
}

#' Compute Cook's Distance and leverage for a linear model
compute_influence <- function(df, response, predictors) {
    # Backtick-escape column names with special characters (%, spaces, etc.)
    fml <- as.formula(paste(bt(response), "~", paste(bt(predictors), collapse = " + ")))
    model <- lm(fml, data = df, na.action = na.exclude)

    n <- nrow(model$model)
    p <- length(predictors) + 1
    cooks_threshold <- 4 / n
    leverage_threshold <- 2 * p / n

    cd <- cooks.distance(model)
    hv <- hatvalues(model)
    sr <- rstandard(model)
    df_val <- dffits(model)
    infl_flag <- cd > cooks_threshold
    lev_flag <- hv > leverage_threshold

    status <- ifelse(
        infl_flag & lev_flag, "High Influence + Leverage",
        ifelse(infl_flag, "High Influence",
            ifelse(lev_flag, "High Leverage", "Normal")
        )
    )

    result <- data.frame(
        Row = 1:n,
        Cooks_Distance = round(cd, 4),
        Leverage = round(hv, 4),
        Std_Residual = round(sr, 4),
        DFFITS = round(df_val, 4),
        Influential = infl_flag,
        High_Leverage = lev_flag,
        Status = status,
        stringsAsFactors = FALSE
    )

    list(
        diagnostics = result,
        model = model,
        cooks_threshold = cooks_threshold,
        leverage_threshold = leverage_threshold
    )
}

# =============================================================================
# Source Modules
# =============================================================================
modules_dir <- file.path(getwd(), "R")
if (dir.exists(modules_dir)) {
    module_files <- list.files(modules_dir, pattern = "\\.R$", full.names = TRUE)
    for (f in module_files) source(f, local = FALSE)
}
