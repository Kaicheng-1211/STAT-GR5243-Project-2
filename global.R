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

#' Read a dataset from an uploaded file based on its extension
#' @param filepath Path to the uploaded file
#' @param filename Original filename (used for extension detection)
#' @return A data.frame or NULL on failure
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
#' @param df A data.frame
#' @return A tibble with one row per column containing type, missing %, unique count, etc.
generate_data_profile <- function(df) {
    tibble::tibble(
        Column = names(df),
        Type = sapply(df, function(x) class(x)[1]),
        N_Missing = sapply(df, function(x) sum(is.na(x))),
        Pct_Missing = round(sapply(df, function(x) mean(is.na(x))) * 100, 1),
        N_Unique = sapply(df, function(x) length(unique(x))),
        N_Total = nrow(df)
    ) %>%
        mutate(
            Alert = case_when(
                Pct_Missing > 50 ~ "⚠️ >50% missing — consider dropping",
                Pct_Missing > 20 ~ "🔶 >20% missing — imputation recommended",
                N_Unique == 1 ~ "⚠️ Constant column",
                N_Unique == N_Total & Type == "character" ~ "🔶 All unique (possible ID column)",
                TRUE ~ "✅ OK"
            )
        )
}

#' Compute skewness and kurtosis for all numeric columns
#' @param df A data.frame
#' @return A tibble with skewness, kurtosis, and normality test results
compute_moments <- function(df) {
    num_cols <- names(df)[sapply(df, is.numeric)]
    if (length(num_cols) == 0) {
        return(tibble::tibble())
    }

    purrr::map_dfr(num_cols, function(col) {
        x <- na.omit(df[[col]])
        if (length(x) < 4) {
            return(tibble::tibble(
                Column = col, Skewness = NA_real_, Kurtosis = NA_real_,
                Skew_Direction = "Insufficient data", Normality_p = NA_real_,
                Suggestion = "Need ≥ 4 non-NA observations"
            ))
        }
        sk <- moments::skewness(x)
        ku <- moments::kurtosis(x)
        sw_p <- tryCatch(shapiro.test(x[1:min(length(x), 5000)])$p.value, error = function(e) NA_real_)

        direction <- dplyr::case_when(
            sk < -0.5 ~ "⬅️ Left-skewed",
            sk > 0.5 ~ "➡️ Right-skewed",
            TRUE ~ "↔️ Approximately symmetric"
        )

        suggestion <- dplyr::case_when(
            abs(sk) > 2 ~ "Strong skew — try log or Box-Cox transform",
            abs(sk) > 1 ~ "Moderate skew — consider sqrt or log transform",
            abs(sk) > 0.5 ~ "Mild skew — may benefit from sqrt transform",
            TRUE ~ "No transform needed"
        )

        tibble::tibble(
            Column = col,
            Skewness = round(sk, 3),
            Kurtosis = round(ku, 3),
            Skew_Direction = direction,
            Normality_p = round(sw_p, 4),
            Suggestion = suggestion
        )
    })
}

#' Fit multiple distributions to a numeric vector and return comparison table
#' @param x Numeric vector (non-NA, positive for some distributions)
#' @return A list with fit objects and comparison table
fit_distributions <- function(x) {
    x <- na.omit(x)
    if (length(x) < 10) {
        return(NULL)
    }

    # Determine which distributions to try
    dists <- c("norm")
    if (all(x > 0)) {
        dists <- c(dists, "lnorm", "exp", "gamma", "weibull")
    }
    if (all(x >= 0)) {
        dists <- c(dists, "pois") # only if integer-like
    }

    fits <- list()
    comparison <- list()

    for (d in dists) {
        tryCatch(
            {
                if (d == "pois") {
                    # Poisson only makes sense for non-negative integers
                    if (!all(x == floor(x))) next
                    f <- fitdistrplus::fitdist(as.integer(x), d)
                } else {
                    f <- fitdistrplus::fitdist(x, d)
                }
                fits[[d]] <- f
                comparison[[d]] <- tibble::tibble(
                    Distribution = d,
                    AIC = round(f$aic, 2),
                    BIC = round(f$bic, 2),
                    LogLik = round(f$loglik, 2)
                )
            },
            error = function(e) NULL
        )
    }

    list(
        fits = fits,
        comparison = dplyr::bind_rows(comparison) %>% dplyr::arrange(AIC)
    )
}

#' Compute Cook's Distance and leverage for a linear model
#' @param df Data frame
#' @param response Character: name of response variable
#' @param predictors Character vector: names of predictor variables
#' @return A list with influence measures and the model
compute_influence <- function(df, response, predictors) {
    # Build formula
    fml <- as.formula(paste(response, "~", paste(predictors, collapse = " + ")))
    model <- lm(fml, data = df, na.action = na.exclude)

    infl <- influence.measures(model)

    # Thresholds
    n <- nrow(model$model)
    p <- length(predictors) + 1
    cooks_threshold <- 4 / n
    leverage_threshold <- 2 * p / n

    result <- tibble::tibble(
        Row = 1:n,
        Cooks_Distance = round(cooks.distance(model), 4),
        Leverage = round(hatvalues(model), 4),
        Std_Residual = round(rstandard(model), 4),
        DFFITS = round(dffits(model), 4),
        Influential = cooks.distance(model) > cooks_threshold,
        High_Leverage = hatvalues(model) > leverage_threshold
    ) %>%
        mutate(
            Status = case_when(
                Influential & High_Leverage ~ "🔴 High Influence + Leverage",
                Influential ~ "🟠 High Influence",
                High_Leverage ~ "🟡 High Leverage",
                TRUE ~ "🟢 Normal"
            )
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
