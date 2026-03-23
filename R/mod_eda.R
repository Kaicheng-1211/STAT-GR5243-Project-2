# =============================================================================
# mod_eda.R — Exploratory Data Analysis Module
# =============================================================================

edaUI <- function(id) {
    ns <- NS(id)

    tagList(
        # ── KPI Row ──
        layout_column_wrap(
            width = 1 / 4,
            value_box(
                title = "Rows", value = textOutput(ns("kpi_rows")),
                showcase = icon("table"), theme = "primary"
            ),
            value_box(
                title = "Columns", value = textOutput(ns("kpi_cols")),
                showcase = icon("columns"), theme = "success"
            ),
            value_box(
                title = "Missing Cells", value = textOutput(ns("kpi_missing")),
                showcase = icon("exclamation-triangle"), theme = "warning"
            ),
            value_box(
                title = "Numeric / Categorical", value = textOutput(ns("kpi_types")),
                showcase = icon("tags"), theme = "info"
            )
        ),
        br(),

        # ── Main Content ──
        layout_sidebar(
            sidebar = sidebar(
                title = "Plot Controls",
                width = 300,
                selectInput(ns("plot_type"), "Chart Type",
                    choices = c(
                        "Histogram" = "histogram", "Density" = "density",
                        "Scatter" = "scatter", "Boxplot" = "boxplot",
                        "Violin" = "violin", "Bar Chart" = "bar",
                        "QQ-Plot" = "qq"
                    )
                ),
                selectizeInput(ns("x_var"), "X Variable", choices = NULL),
                conditionalPanel(
                    condition = sprintf("input['%s'] == 'scatter'", ns("plot_type")),
                    selectizeInput(ns("y_var"), "Y Variable", choices = NULL),
                    checkboxInput(ns("add_trend"), "Add Trend Line", value = FALSE)
                ),
                conditionalPanel(
                    condition = sprintf(
                        "['histogram','density','boxplot','violin'].includes(input['%s'])",
                        ns("plot_type")
                    ),
                    selectizeInput(ns("color_var"), "Color / Group By (optional)",
                        choices = NULL, options = list(placeholder = "None")
                    )
                ),
                conditionalPanel(
                    condition = sprintf("input['%s'] == 'histogram'", ns("plot_type")),
                    sliderInput(ns("n_bins_hist"), "Number of Bins",
                        min = 5, max = 100, value = 30
                    )
                ),
                tags$hr(),
                h6(icon("filter"), " Data Filter", style = "font-weight: 600;"),
                selectizeInput(ns("filter_col"), "Filter Column",
                    choices = NULL,
                    options = list(placeholder = "None")
                ),
                uiOutput(ns("filter_controls")),
                tags$hr(),

                # Downloads
                downloadButton(ns("download_plot"), "Download Current Plot",
                    class = "btn-outline-primary w-100"
                ),
                br(), br(),
                downloadButton(ns("download_summary"), "Download Summary",
                    class = "btn-outline-success w-100"
                )
            ),

            # Main area with tabs
            navset_card_tab(
                id = ns("eda_tabs"),
                nav_panel(
                    title = "Visualization",
                    icon = icon("chart-bar"),
                    plotly::plotlyOutput(ns("main_plot"), height = "450px")
                ),
                nav_panel(
                    title = "Correlation",
                    icon = icon("project-diagram"),
                    layout_sidebar(
                        sidebar = sidebar(
                            width = 200,
                            selectInput(ns("corr_method"), "Method",
                                choices = c(
                                    "Pearson" = "pearson", "Spearman" = "spearman",
                                    "Kendall" = "kendall"
                                )
                            ),
                            sliderInput(ns("corr_fontsize"), "Label Size", min = 0.4, max = 1.2, value = 0.7, step = 0.1)
                        ),
                        plotOutput(ns("corr_plot"), height = "600px")
                    )
                ),
                nav_panel(
                    title = "Summary Statistics",
                    icon  = icon("calculator"),
                    verbatimTextOutput(ns("skim_output"))
                ),
                nav_panel(
                    title = "Pairs Plot",
                    icon = icon("th"),
                    helpText("Select 2-6 numeric columns for the pairs plot:"),
                    selectizeInput(ns("pairs_cols"), "Columns",
                        choices = NULL,
                        multiple = TRUE, options = list(maxItems = 6)
                    ),
                    plotOutput(ns("pairs_plot"), height = "550px")
                ),
                nav_panel(
                    title = "Spectral Analysis",
                    icon = icon("wave-square"),
                    selectizeInput(ns("spectral_col"), "Numeric Column", choices = NULL),
                    checkboxInput(ns("spectral_smooth"), "Apply Daniell Smoothing", value = TRUE),
                    plotly::plotlyOutput(ns("spectral_plot"), height = "400px"),
                    helpText("The periodogram shows dominant frequencies/cycles in the data.")
                )
            )
        )
    )
}

edaServer <- function(id, engineered_data) {
    moduleServer(id, function(input, output, session) {
        ns <- session$ns

        # ── Filtered data ──
        filtered_data <- reactive({
            req(engineered_data())
            df <- engineered_data()

            if (!is.null(input$filter_col) && input$filter_col != "" &&
                input$filter_col %in% names(df)) {
                col <- input$filter_col

                if (is.numeric(df[[col]])) {
                    rng <- input$filter_range
                    if (!is.null(rng)) {
                        df <- df[!is.na(df[[col]]) & df[[col]] >= rng[1] & df[[col]] <= rng[2], , drop = FALSE]
                    }
                } else {
                    vals <- input$filter_cat_vals
                    if (!is.null(vals) && length(vals) > 0) {
                        df <- df[df[[col]] %in% vals, , drop = FALSE]
                    }
                }
            }
            df
        })

        # ── Update selectors ──
        observe({
            req(engineered_data())
            df <- engineered_data()
            all_cols <- names(df)
            num_cols <- names(df)[sapply(df, is.numeric)]
            cat_cols <- names(df)[sapply(df, function(x) is.character(x) | is.factor(x))]

            updateSelectizeInput(session, "x_var", choices = all_cols)
            updateSelectizeInput(session, "y_var", choices = num_cols)
            updateSelectizeInput(session, "color_var", choices = c("", cat_cols))
            updateSelectizeInput(session, "filter_col", choices = c("", all_cols))
            updateSelectizeInput(session, "pairs_cols", choices = num_cols, selected = head(num_cols, 4))
            updateSelectizeInput(session, "spectral_col", choices = num_cols)
        })

        # ── Dynamic filter controls ──
        output$filter_controls <- renderUI({
            req(input$filter_col, input$filter_col != "")
            df <- engineered_data()
            req(input$filter_col %in% names(df))
            col <- input$filter_col

            if (is.numeric(df[[col]])) {
                rng <- range(df[[col]], na.rm = TRUE)
                sliderInput(ns("filter_range"), "Range", min = rng[1], max = rng[2], value = rng)
            } else {
                vals <- unique(as.character(df[[col]]))
                checkboxGroupInput(ns("filter_cat_vals"), "Select Values", choices = vals, selected = vals)
            }
        })

        # ── KPIs ──
        output$kpi_rows <- renderText({
            req(filtered_data())
            format(nrow(filtered_data()), big.mark = ",")
        })
        output$kpi_cols <- renderText({
            req(filtered_data())
            ncol(filtered_data())
        })
        output$kpi_missing <- renderText({
            req(filtered_data())
            paste0(
                format(sum(is.na(filtered_data())), big.mark = ","),
                " (", round(mean(is.na(filtered_data())) * 100, 1), "%)"
            )
        })
        output$kpi_types <- renderText({
            req(filtered_data())
            n_num <- sum(sapply(filtered_data(), is.numeric))
            paste0(n_num, " / ", ncol(filtered_data()) - n_num)
        })

        # ── Reactive plot object (for download reuse) ──
        current_plot <- reactive({
            req(filtered_data(), input$x_var)
            df <- filtered_data()
            x_var <- input$x_var
            req(x_var %in% names(df))

            color_var <- if (!is.null(input$color_var) && input$color_var != "" &&
                input$color_var %in% names(df)) {
                input$color_var
            } else {
                NULL
            }

            p <- switch(input$plot_type,
                "histogram" = {
                    if (!is.null(color_var)) {
                        ggplot(df, aes(x = .data[[x_var]], fill = .data[[color_var]])) +
                            geom_histogram(bins = input$n_bins_hist, alpha = 0.7, position = "identity")
                    } else {
                        ggplot(df, aes(x = .data[[x_var]])) +
                            geom_histogram(bins = input$n_bins_hist, fill = "#667eea", alpha = 0.8)
                    }
                },
                "density" = {
                    if (!is.null(color_var)) {
                        ggplot(df, aes(x = .data[[x_var]], fill = .data[[color_var]])) +
                            geom_density(alpha = 0.5)
                    } else {
                        ggplot(df, aes(x = .data[[x_var]])) +
                            geom_density(fill = "#667eea", alpha = 0.6)
                    }
                },
                "scatter" = {
                    req(input$y_var, input$y_var %in% names(df))
                    p_base <- ggplot(df, aes(x = .data[[x_var]], y = .data[[input$y_var]])) +
                        geom_point(alpha = 0.6, color = "#667eea")
                    if (input$add_trend) p_base <- p_base + geom_smooth(method = "lm", color = "#e74c3c", se = TRUE)
                    p_base
                },
                "boxplot" = {
                    if (!is.null(color_var)) {
                        ggplot(df, aes(x = .data[[color_var]], y = .data[[x_var]], fill = .data[[color_var]])) +
                            geom_boxplot(alpha = 0.7, show.legend = FALSE)
                    } else {
                        ggplot(df, aes(y = .data[[x_var]])) +
                            geom_boxplot(fill = "#667eea", alpha = 0.7)
                    }
                },
                "violin" = {
                    if (!is.null(color_var)) {
                        ggplot(df, aes(x = .data[[color_var]], y = .data[[x_var]], fill = .data[[color_var]])) +
                            geom_violin(alpha = 0.7, show.legend = FALSE) +
                            geom_boxplot(width = 0.1, fill = "white", alpha = 0.8)
                    } else {
                        ggplot(df, aes(x = "", y = .data[[x_var]])) +
                            geom_violin(fill = "#667eea", alpha = 0.7) +
                            geom_boxplot(width = 0.1, fill = "white", alpha = 0.8)
                    }
                },
                "bar" = {
                    ggplot(df, aes(x = .data[[x_var]])) +
                        geom_bar(fill = "#667eea", alpha = 0.8) +
                        theme(axis.text.x = element_text(angle = 45, hjust = 1))
                },
                "qq" = {
                    ggplot(df, aes(sample = .data[[x_var]])) +
                        stat_qq(color = "#667eea", alpha = 0.6) +
                        stat_qq_line(color = "#e74c3c", linewidth = 1) +
                        labs(x = "Theoretical Quantiles", y = "Sample Quantiles")
                }
            )

            p + theme_minimal(base_size = 13) +
                labs(title = paste(tools::toTitleCase(input$plot_type), "of", x_var))
        })

        # ── Main Plot ──
        output$main_plot <- plotly::renderPlotly({
            plotly::ggplotly(current_plot())
        })

        # ── Download current plot as PNG ──
        output$download_plot <- downloadHandler(
            filename = function() paste0("eda_plot_", Sys.Date(), ".png"),
            content = function(file) {
                ggsave(file, plot = current_plot(), width = 10, height = 6, dpi = 150)
            }
        )

        # ── Download summary stats as CSV ──
        output$download_summary <- downloadHandler(
            filename = function() paste0("summary_stats_", Sys.Date(), ".csv"),
            content = function(file) {
                df <- filtered_data()
                summ <- compute_moments(df)
                write.csv(summ, file, row.names = FALSE)
            }
        )

        # ── Correlation Plot (BIGGER, with adjustable font) ──
        output$corr_plot <- renderPlot({
            req(filtered_data())
            df <- filtered_data()
            num_df <- df[sapply(df, is.numeric)]
            req(ncol(num_df) > 1)

            corr_mat <- cor(num_df, use = "pairwise.complete.obs", method = input$corr_method)

            corrplot::corrplot(corr_mat,
                method = "color", type = "upper",
                tl.cex = input$corr_fontsize, tl.col = "black",
                addCoef.col = "black", number.cex = input$corr_fontsize,
                col = colorRampPalette(c("#e74c3c", "white", "#667eea"))(200),
                title = paste(tools::toTitleCase(input$corr_method), "Correlation"),
                mar = c(0, 0, 2, 0)
            )
        })

        # ── Summary Statistics ──
        output$skim_output <- renderPrint({
            req(filtered_data())
            skimr::skim(filtered_data())
        })

        # ── Pairs Plot (FIX: only truly numeric, handle errors) ──
        output$pairs_plot <- renderPlot({
            req(filtered_data(), length(input$pairs_cols) >= 2)
            df <- filtered_data()

            # Only keep columns that are strictly numeric vectors (not factors/matrices)
            valid_cols <- input$pairs_cols[input$pairs_cols %in% names(df)]
            valid_cols <- valid_cols[sapply(df[valid_cols], function(x) is.numeric(x) && is.atomic(x))]
            req(length(valid_cols) >= 2)

            tryCatch(
                {
                    GGally::ggpairs(
                        df[valid_cols],
                        lower = list(continuous = GGally::wrap("points", alpha = 0.4, color = "#667eea")),
                        diag  = list(continuous = GGally::wrap("densityDiag", fill = "#667eea", alpha = 0.5)),
                        upper = list(continuous = GGally::wrap("cor", size = 4))
                    ) + theme_minimal(base_size = 11)
                },
                error = function(e) {
                    plot.new()
                    text(0.5, 0.5, paste("Pairs plot error:", e$message), cex = 1.2, col = "#e74c3c")
                }
            )
        })

        # ── Spectral Analysis ──
        output$spectral_plot <- plotly::renderPlotly({
            req(filtered_data(), input$spectral_col)
            df <- filtered_data()
            col <- input$spectral_col
            req(col %in% names(df))

            x <- na.omit(as.numeric(df[[col]]))
            req(length(x) > 10)

            if (input$spectral_smooth) {
                spec_result <- spectrum(x, spans = c(3, 5), plot = FALSE)
            } else {
                spec_result <- spectrum(x, plot = FALSE)
            }

            spec_df <- data.frame(Frequency = spec_result$freq, Power = spec_result$spec)

            p <- ggplot(spec_df, aes(x = Frequency, y = Power)) +
                geom_line(color = "#667eea", linewidth = 0.8) +
                geom_area(fill = "#667eea", alpha = 0.15) +
                scale_y_log10() +
                theme_minimal(base_size = 13) +
                labs(title = paste("Periodogram:", col), x = "Frequency", y = "Spectral Density (log)")

            plotly::ggplotly(p)
        })
    })
}
