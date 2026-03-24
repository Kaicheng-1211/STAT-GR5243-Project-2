# =============================================================================
# mod_upload.R — Dataset Loading & Data Profiling Module
# =============================================================================

uploadUI <- function(id) {
    ns <- NS(id)

    layout_sidebar(
        sidebar = sidebar(
            title = "Data Source",
            width = 320,

            # File upload
            fileInput(
                ns("file"),
                label = tagList(icon("file-upload"), " Upload a File"),
                accept = c(".csv", ".xlsx", ".xls", ".json", ".rds", ".tsv"),
                placeholder = "CSV, Excel, JSON, RDS, TSV"
            ),
            tags$hr(),

            # Built-in datasets
            radioGroupButtons(
                inputId = ns("builtin"),
                label = tagList(icon("database"), " Or Use a Built-in Dataset"),
                choices = c("None", names(BUILTIN_DATASETS)),
                selected = "iris",
                direction = "vertical",
                justified = TRUE,
                size = "sm",
                status = "outline-primary"
            ),

            # Dataset description
            conditionalPanel(
                condition = sprintf("input['%s'] !== 'None'", ns("builtin")),
                div(
                    style = "margin-top: 10px; padding: 10px; background: rgba(102,126,234,0.05); border-radius: 8px;",
                    textOutput(ns("dataset_desc"))
                )
            ),
            tags$hr(),

            # Data info
            uiOutput(ns("data_info")),
            tags$hr(),

            # Reset Button
            actionButton(ns("reset_upload"), "Clear Data",
                class = "btn-outline-danger w-100", icon = icon("trash")
            )
        ),

        # Main panel
        navset_card_tab(
            id = ns("upload_tabs"),
            nav_panel(
                title = "Data Preview",
                icon  = icon("table"),
                DTOutput(ns("preview_table"))
            ),
            nav_panel(
                title = "Data Profile",
                icon  = icon("clipboard-check"),
                DTOutput(ns("profile_table"))
            ),
            nav_panel(
                title = "Missing Values",
                icon = icon("exclamation-triangle"),
                plotOutput(ns("missingness_plot"), height = "400px"),
                br(),
                DTOutput(ns("missing_rows_table"))
            )
        )
    )
}

uploadServer <- function(id) {
    moduleServer(id, function(input, output, session) {
        ns <- session$ns

        # ── Reactive: loaded data ──
        data <- reactive({
            # File upload takes priority
            if (!is.null(input$file)) {
                df <- read_uploaded_file(input$file$datapath, input$file$name)
                if (!is.null(df)) {
                    showNotification(
                        paste0("✅ Loaded '", input$file$name, "' (", nrow(df), " rows × ", ncol(df), " cols)"),
                        type = "message", duration = 4
                    )
                    return(as.data.frame(df))
                }
            }

            # Built-in dataset
            if (!is.null(input$builtin) && input$builtin != "None") {
                df <- BUILTIN_DATASETS[[input$builtin]]
                showNotification(
                    paste0("✅ Loaded '", input$builtin, "' (", nrow(df), " rows × ", ncol(df), " cols)"),
                    type = "message", duration = 4
                )
                return(as.data.frame(df))
            }

            NULL
        })

        # ── Reset / Clear Data ──
        observeEvent(input$reset_upload, {
            shinyjs::reset("file")
            shinyWidgets::updateRadioGroupButtons(session, "builtin", selected = "iris")
            showNotification("Uploaded data cleared.", type = "message")
        })

        # ── Built-in dataset description ──
        output$dataset_desc <- renderText({
            req(input$builtin != "None")
            BUILTIN_DESCRIPTIONS[[input$builtin]]
        })

        # ── Data info sidebar ──
        output$data_info <- renderUI({
            req(data())
            df <- data()
            num_cols <- sum(sapply(df, is.numeric))
            cat_cols <- ncol(df) - num_cols

            tagList(
                h6(icon("info-circle"), " Dataset Summary", style = "font-weight: 600;"),
                tags$div(
                    style = "font-size: 0.9rem;",
                    tags$p(tags$strong("Rows: "), format(nrow(df), big.mark = ",")),
                    tags$p(tags$strong("Columns: "), ncol(df)),
                    tags$p(tags$strong("Numeric: "), num_cols),
                    tags$p(tags$strong("Categorical: "), cat_cols),
                    tags$p(
                        tags$strong("Missing cells: "),
                        format(sum(is.na(df)), big.mark = ","),
                        paste0(" (", round(mean(is.na(df)) * 100, 1), "%)")
                    )
                )
            )
        })

        # ── Data preview table ──
        output$preview_table <- renderDT({
            req(data())
            datatable(
                data(),
                options = list(
                    pageLength = 15,
                    scrollX = TRUE,
                    dom = "lfrtip",
                    language = list(
                        emptyTable = "No data loaded. Upload a file or select a built-in dataset."
                    )
                ),
                rownames = FALSE,
                class = "compact stripe hover"
            )
        })

        # ── Data profile table ──
        output$profile_table <- renderDT({
            req(data())
            profile <- generate_data_profile(data())
            datatable(
                profile,
                options = list(
                    pageLength = 50,
                    scrollX = TRUE,
                    dom = "ft",
                    order = list(list(3, "desc")) # Sort by Pct_Missing descending
                ),
                rownames = FALSE,
                class = "compact stripe hover"
            ) %>%
                formatStyle(
                    "Pct_Missing",
                    background = styleColorBar(c(0, 100), "rgba(231,76,60,0.3)"),
                    backgroundSize = "100% 90%",
                    backgroundRepeat = "no-repeat",
                    backgroundPosition = "center"
                ) %>%
                formatStyle(
                    "Alert",
                    color = styleEqual(
                        c(
                            "✅ OK", "🔶 >20% missing — imputation recommended",
                            "⚠️ >50% missing — consider dropping",
                            "⚠️ Constant column",
                            "🔶 All unique (possible ID column)"
                        ),
                        c("#18bc9c", "#f39c12", "#e74c3c", "#e74c3c", "#f39c12")
                    ),
                    fontWeight = "bold"
                )
        })

        # ── Missingness visualization ──
        output$missingness_plot <- renderPlot({
            req(data())
            df <- data()
            if (sum(is.na(df)) == 0) {
                plot.new()
                text(0.5, 0.5, "No missing values detected! ✅", cex = 1.5, col = "#18bc9c")
            } else {
                naniar::vis_miss(df) +
                    theme_minimal(base_size = 14) +
                    theme(axis.text.x = element_text(angle = 45, hjust = 1))
            }
        })

        # ── Missing rows table ──
        output$missing_rows_table <- renderDT({
            req(data())
            df <- data()
            row_missing <- rowSums(is.na(df))

            if (sum(row_missing) == 0) {
                return(NULL)
            }

            missing_summary <- tibble::tibble(
                Row = which(row_missing > 0),
                N_Missing = row_missing[row_missing > 0],
                Pct_Missing = round(row_missing[row_missing > 0] / ncol(df) * 100, 1),
                Missing_Columns = sapply(which(row_missing > 0), function(i) {
                    paste(names(df)[is.na(df[i, ])], collapse = ", ")
                })
            ) %>%
                arrange(desc(N_Missing))

            datatable(
                missing_summary,
                options = list(pageLength = 10, scrollX = TRUE, dom = "lfrtip"),
                rownames = FALSE,
                caption = "Rows requiring imputation (sorted by missing count)",
                class = "compact stripe hover"
            ) %>%
                formatStyle(
                    "Pct_Missing",
                    background = styleColorBar(c(0, 100), "rgba(243,156,18,0.3)"),
                    backgroundSize = "100% 90%",
                    backgroundRepeat = "no-repeat",
                    backgroundPosition = "center"
                )
        })

        # Return the reactive data for downstream modules
        return(data)
    })
}
