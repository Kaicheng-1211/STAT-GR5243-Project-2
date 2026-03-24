# =============================================================================
# app.R — Entry Point
# STAT-GR5243 Project 2: Interactive Data Analysis App
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
  id = "main_nav",
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
    value = "guide",
    icon  = icon("book-open"),
    guideUI("guide")
  ),
  
  # ── Tab 2: Upload Data ──
  nav_panel(
    title = "Upload Data",
    value = "upload",
    icon  = icon("cloud-upload-alt"),
    uploadUI("upload")
  ),
  
  # ── Tab 3: Data Cleaning ──
  nav_panel(
    title = "Data Cleaning",
    value = "cleaning",
    icon  = icon("broom"),
    cleaningUI("cleaning")
  ),
  
  # ── Tab 4: Feature Engineering ──
  nav_panel(
    title = "Feature Engineering",
    value = "feature",
    icon  = icon("cogs"),
    featureUI("feature")
  ),
  
  # ── Tab 5: EDA ──
  nav_panel(
    title = "EDA",
    value = "eda",
    icon  = icon("chart-bar"),
    edaUI("eda")
  ),
  
  # ── Tab 6: Distribution Lab ──
  nav_panel(
    title = "Distribution Lab",
    value = "distributions",
    icon  = icon("bell-curve", lib = "glyphicon"),
    distributionsUI("distributions")
  ),
  
  # ── Tab 7: Diagnostics ──
  nav_panel(
    title = "Diagnostics",
    value = "diagnostics",
    icon  = icon("stethoscope"),
    diagnosticsUI("diagnostics")
  )
)

# =============================================================================
# Server
# =============================================================================
server <- function(input, output, session) {
  # ── Shared reactive: the current working dataset ──
  # This flows: upload → clean → feature → eda/distributions/diagnostics
  uploaded_data <- uploadServer("upload")
  cleaned_data <- cleaningServer("cleaning", uploaded_data)
  engineered_data <- featureServer("feature", cleaned_data)
  
  # ── Modules that consume the final dataset ──
  guideServer("guide", session)
  edaServer("eda", engineered_data)
  distributionsServer("distributions", engineered_data)
  diagnosticsServer("diagnostics", engineered_data)
}

# =============================================================================
# Launch
# =============================================================================
shinyApp(ui = ui, server = server)
