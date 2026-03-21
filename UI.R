library(shiny)
library(shinydashboard)
library(ggplot2)
library(dplyr)
library(readr)
library(readxl)
library(DT)
library(shinyjs)
library(shinycssloaders)
library(corrplot)

# UI
ui <- dashboardPage(
  
  dashboardHeader(title = "📊 Data Analysis App"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("User Guide", tabName = "guide", icon = icon("book")),
      menuItem("Upload Data", tabName = "upload", icon = icon("upload")),
      menuItem("Data Cleaning", tabName = "clean", icon = icon("broom")),
      menuItem("Feature Engineering", tabName = "feature", icon = icon("cogs")),
      menuItem("EDA", tabName = "eda", icon = icon("chart-bar"))
    ),
    br(),
    checkboxInput("dark_mode", "🌙 Dark Mode", FALSE)
  ),
  
  dashboardBody(
    useShinyjs(),
    
    tags$head(
      tags$style(HTML("
        body.dark-mode {
          background-color: #1e1e1e;
          color: #ffffff;
        }
        body.dark-mode .box {
          background-color: #2c2c2c;
          color: white;
        }
        .box {
          border-radius: 10px;
        }
      "))
    ),
    
    tabItems(
      
      # Guide
      tabItem(tabName = "guide",
              fluidRow(
                box(width = 12, title = "Welcome", status = "primary",
                    h4("How to use this app:"),
                    tags$ol(
                      tags$li("Upload your dataset"),
                      tags$li("Clean data"),
                      tags$li("Create features"),
                      tags$li("Explore with visualization")
                    )
                )
              )
      ),
      
      # Upload
      tabItem(tabName = "upload",
              fluidRow(
                box(width = 4, title = "Upload", status = "primary",
                    fileInput("file", "Upload File",
                              accept = c(".csv", ".xlsx",".json", ".rds")),
                    selectInput("dataset_builtin", "Or choose dataset",
                                choices = c("None", "iris", "mtcars"))
                ),
                box(width = 8, title = "Preview", status = "info",
                    DTOutput("data_preview")
                )
              )
      ),
      
      # Data Cleaning
      tabItem(tabName = "clean",
              fluidRow(
                box(width = 4, title = "Options", status = "warning",
                    checkboxInput("remove_na", "Remove NA"),
                    checkboxInput("remove_dup", "Remove Duplicates"),
                    checkboxInput("scale_data", "Scale Numeric Data"),
                    br(),
                    downloadButton("download_data", "📥 Download Data")
                ),
                box(width = 8, title = "Cleaned Data", status = "info",
                    DTOutput("clean_data")
                )
              )
      ),
      
      # Feature Engineering
      tabItem(tabName = "feature",
              fluidRow(
                box(width = 4, title = "Create Feature", status = "success",
                    textInput("new_col", "New Column Name"),
                    textInput("formula", "Formula"),
                    sliderInput("alpha_keep", "Alpha",
                                min = 0, max = 5, value = 1, step = 0.1),
                    actionButton("add_feature", "Add Feature"),
                    helpText("Example: Sepal.Length * alpha_keep")
                ),
                box(width = 8, title = "Updated Data", status = "info",
                    DTOutput("feature_data")
                )
              )
      ),
      
      # EDA
      tabItem(tabName = "eda",
              
              fluidRow(
                valueBoxOutput("num_rows"),
                valueBoxOutput("num_cols"),
                valueBoxOutput("missing_vals")
              ),
              
              fluidRow(
                box(width = 4, title = "Plot Settings", status = "primary",
                    selectInput("x", "X Variable", choices = NULL),
                    selectInput("y", "Y Variable", choices = NULL),
                    selectInput("plot_type", "Plot Type",
                                choices = c("Histogram", "Scatter", "Boxplot"))
                ),
                box(width = 8, title = "Visualization", status = "info",
                    withSpinner(plotOutput("plot", height = 300)),
                    verbatimTextOutput("summary")
                )
              ),
              
              fluidRow(
                box(width = 12, title = "Correlation Heatmap", status = "danger",
                    withSpinner(plotOutput("corrplot"))
                )
              )
      )
    )
  )
)

# Server
server <- function(input, output, session) {
  
  # Dark Mode
  observe({
    if (input$dark_mode) {
      runjs("document.body.classList.add('dark-mode');")
    } else {
      runjs("document.body.classList.remove('dark-mode');")
    }
  })
  
  #Load Data
  data <- reactive({
    if (!is.null(input$file)) {
      ext <- tools::file_ext(input$file$name)
      
      if (ext == "csv") {
        read_csv(input$file$datapath)
      } else if (ext == "xlsx") {
        read_excel(input$file$datapath)
      } else if (ext == "json") {
        jsonlite::fromJSON(input$file$datapath)
      } else if (ext == "rds") {
        readRDS(input$file$datapath)
      } else {
        showNotification("Unsupported file format.", type = "error")
        return(NULL)
      }
      
    } else if (input$dataset_builtin == "iris") {
      iris
    } else if (input$dataset_builtin == "mtcars") {
      mtcars
    } else {
      NULL
    }
  })
  
  output$data_preview <- renderDT({
    req(data())
    datatable(data())
  })
  
  # Cleaning
  clean_data <- reactive({
    df <- data()
    req(df)
    
    if (input$remove_na) df <- na.omit(df)
    if (input$remove_dup) df <- distinct(df)
    
    if (input$scale_data) {
      num_cols <- sapply(df, is.numeric)
      df[num_cols] <- scale(df[num_cols])
    }
    
    df
  })
  
  output$clean_data <- renderDT({
    datatable(clean_data())
  })
  
  # Download
  output$download_data <- downloadHandler(
    filename = function() {
      paste("cleaned_data_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(clean_data(), file, row.names = FALSE)
    }
  )
  
  # Feature Engineering
  feature_data <- reactiveVal()
  
  observe({
    feature_data(clean_data())
  })
  
  observeEvent(input$add_feature, {
    df <- feature_data()
    req(df, input$new_col, input$formula)
    
    tryCatch({
      env <- new.env()
      list2env(df, envir = env)
      env$alpha_keep <- input$alpha_keep
      
      new_feature <- eval(parse(text = input$formula), envir = env)
      
      if (length(new_feature) != nrow(df)) {
        stop("Length mismatch")
      }
      
      df[[input$new_col]] <- new_feature
      feature_data(df)
      
    }, error = function(e) {
      showNotification(paste("Error:", e$message), type = "error")
    })
  })
  
  output$feature_data <- renderDT({
    datatable(feature_data())
  })
  
  # Update selectors
  observe({
    df <- feature_data()
    if (!is.null(df)) {
      updateSelectInput(session, "x", choices = names(df))
      updateSelectInput(session, "y", choices = names(df))
    }
  })
  
  # Plot
  output$plot <- renderPlot({
    df <- feature_data()
    req(df, input$x)
    
    if (input$plot_type == "Histogram") {
      ggplot(df, aes_string(x = input$x)) + geom_histogram()
    } else if (input$plot_type == "Scatter") {
      req(input$y)
      ggplot(df, aes_string(x = input$x, y = input$y)) + geom_point()
    } else {
      ggplot(df, aes_string(y = input$x)) + geom_boxplot()
    }
  })
  
  output$summary <- renderPrint({
    summary(feature_data())
  })
  
  # KPI
  output$num_rows <- renderValueBox({
    valueBox(nrow(feature_data()), "Rows", icon = icon("table"), color = "blue")
  })
  
  output$num_cols <- renderValueBox({
    valueBox(ncol(feature_data()), "Columns", icon = icon("columns"), color = "green")
  })
  
  output$missing_vals <- renderValueBox({
    valueBox(sum(is.na(feature_data())), "Missing", icon = icon("exclamation"), color = "red")
  })
  
  # Correlation Heatmap
  output$corrplot <- renderPlot({
    df <- feature_data()
    num_df <- df[sapply(df, is.numeric)]
    req(ncol(num_df) > 1)
    
    corr <- cor(num_df, use = "complete.obs")
    corrplot(corr, method = "color", type = "upper")
  })
}

shinyApp(ui, server)