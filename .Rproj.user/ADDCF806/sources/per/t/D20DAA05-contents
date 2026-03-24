# =============================================================================
# app.R — Entry Point
# =============================================================================

source("global.R")

# =============================================================================
# UI
# =============================================================================
ui <- page_navbar(
    title = tags$span(
        icon("chart-line"), " DataScope",
        style = "font-weight: 700; letter-spacing: -0.02em;"
    ),
    id = "main_nav", # 必须指定 ID 才能执行跳转
    theme = app_theme,
    header = tags$head(
        tags$link(rel = "stylesheet", href = "custom.css"),
        useShinyjs(),
        useShinyFeedback(),
        useWaiter()
    ),

    # ── Tab 1: User Guide ──
    nav_panel(
        title = "User Guide",
        value = "tab_guide",
        icon  = icon("book-open"),
        guideUI("guide")
    ),

    # ── Tab 2: Upload Data ──
    nav_panel(
        title = "Upload Data",
        value = "tab_upload",
        icon  = icon("cloud-upload-alt"),
        uploadUI("upload")
    ),

    # ── Tab 3: Data Cleaning ──
    nav_panel(
        title = "Data Cleaning",
        value = "tab_cleaning",
        icon  = icon("broom"),
        cleaningUI("cleaning")
    ),

    # ── Tab 4: Feature Engineering ──
    nav_panel(
        title = "Feature Engineering",
        value = "tab_feature",
        icon  = icon("cogs"),
        featureUI("feature")
    ),

    # ── Tab 5: EDA ──
    nav_panel(
        title = "EDA",
        value = "tab_eda",
        icon  = icon("chart-bar"),
        edaUI("eda")
    ),

    # ── Tab 6: Distribution Lab ──
    nav_panel(
        title = "Distribution Lab",
        value = "tab_dist",
        icon  = icon("bell-curve", lib = "glyphicon"),
        distributionsUI("distributions")
    ),

    # ── Tab 7: Diagnostics ──
    nav_panel(
        title = "Diagnostics",
        value = "tab_diag",
        icon  = icon("stethoscope"),
        diagnosticsUI("diagnostics")
    )
)

# =============================================================================
# Server
# =============================================================================
server <- function(input, output, session) {
    
    # 1. 接收来自 Guide 模块的跳转信号
    # 注意：guideServer 现在会返回一个 reactive 对象
    target_tab <- guideServer("guide")
    
    # 2. 监听点击并执行跳转
    observeEvent(target_tab(), {
        req(target_tab()) # 确保有值
        # Debug: log and show a notification when Guide 请求跳转
        message("Guide requested navigation to: ", target_tab())
        try(showNotification(paste("Navigate to:", target_tab()), type = "message"), silent = TRUE)
        nav_select("main_nav", target_tab())
    })

    # ── 以下为你原有的数据流逻辑 ──
    uploaded_data <- uploadServer("upload")
    cleaned_data <- cleaningServer("cleaning", uploaded_data)
    engineered_data <- featureServer("feature", cleaned_data)

    edaServer("eda", engineered_data)
    distributionsServer("distributions", engineered_data)
    diagnosticsServer("diagnostics", engineered_data)
}

# =============================================================================
# Launch
# =============================================================================
shinyApp(ui, server)