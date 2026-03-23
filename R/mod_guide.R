guideUI <- function(id) {
    ns <- NS(id)
    tagList(
        div(class = "hero-section",
            h2(icon("chart-line"), " Welcome to DataScope"),
            p("A comprehensive interactive data analysis platform. Upload your dataset,
         clean and preprocess it, engineer new features, explore with interactive
         visualizations, fit probability distributions, and run regression diagnostics —
         all without writing a single line of code.")
        ),

        h4("Quick Start", style = "font-weight: 600; margin-bottom: 1rem;"),
        
        # ── 修正點：onclick 裡的值必須對應 app.R 的 nav_panel value ──
        div(class = "step-card", style = "cursor: pointer;",
            onclick = sprintf("Shiny.setInputValue('%s', 'tab_upload', {priority: 'event'})", ns("goto_tab")),
            div(class = "step-number", "1"),
            div(class = "step-content", h5("Upload Your Data"), p("Go to the Upload Data tab. Upload a CSV, Excel (.xlsx), JSON, or RDS file,
           or select one of the built-in demo datasets (Iris, Mtcars, or Diamonds) to
           get started immediately."))
        ),
        
        div(class = "step-card", style = "cursor: pointer;",
            onclick = sprintf("Shiny.setInputValue('%s', 'tab_cleaning', {priority: 'event'})", ns("goto_tab")),
            div(class = "step-number", "2"),
            div(class = "step-content", h5("Clean & Preprocess"), p("Handle missing values (remove or impute with mean/median/mode), remove
           duplicates, scale numeric features, encode categorical variables, and
           detect + treat outliers. See before/after comparisons in real time."))
        ),

        div(class = "step-card", style = "cursor: pointer;",
            onclick = sprintf("Shiny.setInputValue('%s', 'tab_feature', {priority: 'event'})", ns("goto_tab")),
            div(class = "step-number", "3"),
            div(class = "step-content", h5("Engineer Features"), p("Apply transformations, create interaction terms, and generate new features to enhance model performance."))
        ),

        div(class = "step-card", style = "cursor: pointer;",
            onclick = sprintf("Shiny.setInputValue('%s', 'tab_eda', {priority: 'event'})", ns("goto_tab")),
            div(class = "step-number", "4"),
            div(class = "step-content", h5("Explore (EDA)"), p("Generate interactive plots (histograms, scatter, box, violin, density, bar),
           view correlation heatmaps, compute summary statistics with skewness and
           kurtosis, and filter your data dynamically."))
        ),

        div(class = "step-card", style = "cursor: pointer;",
            onclick = sprintf("Shiny.setInputValue('%s', 'tab_dist', {priority: 'event'})", ns("goto_tab")),
            div(class = "step-number", "5"),
            div(class = "step-content", h5("Distribution Fitting Lab"), p("Select any numeric column and fit multiple probability distributions
           (Normal, Exponential, Gamma, Weibull, Log-Normal). Compare fits via AIC/BIC,
           view density overlays, and explore Cullen-Frey diagrams."))
        ),

        div(class = "step-card", style = "cursor: pointer;",
            onclick = sprintf("Shiny.setInputValue('%s', 'tab_diag', {priority: 'event'})", ns("goto_tab")),
            div(class = "step-number", "6"),
            div(class = "step-content", h5("Regression Diagnostics"), p("Pick a response and predictors to fit a linear model. View Cook's Distance,
           leverage plots, DFFITS, and identify influential observations with color-coded
           status indicators. Remove high-influence points with one click."))
        )
    )
}

guideServer <- function(id) {
    moduleServer(id, function(input, output, session) {
        # 返回一个明确的 reactiveVal，并监听 namespaced input
        rv <- reactiveVal(NULL)
        observeEvent(input$goto_tab, {
            message("guideServer: received goto_tab = ", as.character(input$goto_tab))
            rv(input$goto_tab)
        }, ignoreNULL = FALSE)

        # 导出为 reactive 表达式
        reactive({ rv() })
    })
}