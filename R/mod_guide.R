# =============================================================================
# mod_guide.R — User Guide Module
# =============================================================================

guideUI <- function(id) {
    ns <- NS(id)

    tagList(
        # Hero Section
        div(
            class = "hero-section",
            h2(icon("chart-line"), " Welcome to DataScope"),
            p("A comprehensive interactive data analysis platform. Upload your dataset,
         clean and preprocess it, engineer new features, explore with interactive
         visualizations, fit probability distributions, and run regression diagnostics —
         all without writing a single line of code.")
        ),

        # Quick Start Steps
        h4("Quick Start", style = "font-weight: 600; margin-bottom: 1rem;"),
        div(
            class = "step-card",
            div(class = "step-number", "1"),
            div(
                class = "step-content",
                h5("Upload Your Data"),
                p("Go to the Upload Data tab. Upload a CSV, Excel (.xlsx), JSON, or RDS file,
           or select one of the built-in demo datasets (Iris, Mtcars, or Diamonds) to
           get started immediately.")
            )
        ),
        div(
            class = "step-card",
            div(class = "step-number", "2"),
            div(
                class = "step-content",
                h5("Clean & Preprocess"),
                p("Handle missing values (remove or impute with mean/median/mode), remove
           duplicates, scale numeric features, encode categorical variables, and
           detect + treat outliers. See before/after comparisons in real time.")
            )
        ),
        div(
            class = "step-card",
            div(class = "step-number", "3"),
            div(
                class = "step-content",
                h5("Engineer Features"),
                p("Apply transformations (log, sqrt, Box-Cox, polynomial), create interaction
           terms, bin numeric variables, or write custom formulas. Watch distribution
           previews update live as you make changes.")
            )
        ),
        div(
            class = "step-card",
            div(class = "step-number", "4"),
            div(
                class = "step-content",
                h5("Explore (EDA)"),
                p("Generate interactive plots (histograms, scatter, box, violin, density, bar),
           view correlation heatmaps, compute summary statistics with skewness and
           kurtosis, and filter your data dynamically.")
            )
        ),
        div(
            class = "step-card",
            div(class = "step-number", "5"),
            div(
                class = "step-content",
                h5("Distribution Fitting Lab"),
                p("Select any numeric column and fit multiple probability distributions
           (Normal, Exponential, Gamma, Weibull, Log-Normal). Compare fits via AIC/BIC,
           view density overlays, and explore Cullen-Frey diagrams.")
            )
        ),
        div(
            class = "step-card",
            div(class = "step-number", "6"),
            div(
                class = "step-content",
                h5("Regression Diagnostics"),
                p("Pick a response and predictors to fit a linear model. View Cook's Distance,
           leverage plots, DFFITS, and identify influential observations with color-coded
           status indicators. Remove high-influence points with one click.")
            )
        ),
        br(),

        # FAQ / Tips Accordion
        h4("Tips & FAQ", style = "font-weight: 600; margin-bottom: 1rem;"),
        accordion(
            id = ns("faq"),
            open = FALSE,
            accordion_panel(
                title = "What file formats can I upload?",
                icon  = icon("file"),
                p("CSV (.csv), Excel (.xlsx, .xls), JSON (.json), TSV (.tsv), and R Data (.rds).
           The app auto-detects the format from the file extension.")
            ),
            accordion_panel(
                title = "How does missing value imputation work?",
                icon  = icon("puzzle-piece"),
                p("You can choose from several strategies: remove rows with NAs, remove columns
           above a missing-% threshold, or impute using mean, median, or mode. The
           imputation diagnostics panel shows you exactly which rows need attention and
           how the distributions change pre/post imputation.")
            ),
            accordion_panel(
                title = "What is Cook's Distance?",
                icon  = icon("bullseye"),
                p("Cook's Distance measures how much the fitted values of a regression model
           change when a single observation is removed. Points with Cook's D > 4/n are
           typically considered influential. The Diagnostics tab visualizes this and
           lets you remove those points interactively.")
            ),
            accordion_panel(
                title = "What distributions can I fit?",
                icon  = icon("chart-area"),
                p("Normal, Log-Normal, Exponential, Gamma, Weibull, and Poisson (for count data).
           The app uses Maximum Likelihood Estimation (MLE) via the fitdistrplus package
           and compares fits using AIC, BIC, and log-likelihood.")
            ),
            accordion_panel(
                title = "What does the skewness indicator mean?",
                icon  = icon("arrows-alt-h"),
                p("Skewness measures the asymmetry of a distribution. Values near 0 indicate
           symmetry; positive values indicate right-skew (long right tail); negative
           values indicate left-skew. The app suggests transformations when |skew| > 0.5.")
            ),
            accordion_panel(
                title = "Can I download my cleaned/transformed data?",
                icon  = icon("download"),
                p("Yes! The Data Cleaning tab has a download button that exports the current
           state of your cleaned data as a CSV file.")
            )
        )
    )
}

guideServer <- function(id) {
    moduleServer(id, function(input, output, session) {
        # Guide is read-only, no server logic needed
    })
}
