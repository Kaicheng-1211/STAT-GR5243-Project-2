# =============================================================================
# mod_diagnostics.R — Regression Diagnostics Module (Cook's D, Leverage, etc.)
# =============================================================================

diagnosticsUI <- function(id) {
    ns <- NS(id)

    layout_sidebar(
        sidebar = sidebar(
            title = "Model Setup",
            width = 320,
            h6(icon("bullseye"), " Linear Model Specification", style = "font-weight: 600;"),
            selectizeInput(ns("response"), "Response Variable (Y)", choices = NULL),
            selectizeInput(ns("predictors"), "Predictor Variables (X)",
                choices = NULL,
                multiple = TRUE, options = list(placeholder = "Select one or more...")
            ),
            actionButton(ns("fit_model"), "Fit Model & Diagnose",
                class = "btn-primary w-100", icon = icon("play")
            ),
            tags$hr(),
            h6(icon("sliders-h"), " Thresholds", style = "font-weight: 600;"),
            helpText("Cook's D threshold: 4/n"),
            helpText("Leverage threshold: 2p/n"),
            helpText("Where n = observations, p = parameters"),
            tags$hr(),
            conditionalPanel(
                condition = sprintf("output['%s']", ns("has_model")),
                h6(icon("trash"), " Influential Point Treatment", style = "font-weight: 600;"),
                actionButton(ns("remove_influential"), "Remove High-Influence Points",
                    class = "btn-outline-danger w-100", icon = icon("times")
                ),
                br(), br(),
                verbatimTextOutput(ns("model_summary_compact"))
            )
        ),

        # Main panel
        navset_card_tab(
            id = ns("diag_tabs"),
            nav_panel(
                title = "Cook's Distance",
                icon = icon("chart-bar"),
                plotly::plotlyOutput(ns("cooks_plot"), height = "400px"),
                helpText("Points above the red dashed line (4/n) are considered influential.
                  Click a bar to see the corresponding row in the table below."),
                DTOutput(ns("cooks_table"))
            ),
            nav_panel(
                title = "Leverage Plot",
                icon = icon("expand-arrows-alt"),
                plotly::plotlyOutput(ns("leverage_plot"), height = "450px"),
                helpText("Points in the upper-right quadrant have both high residuals and high leverage —
                  these are the most problematic observations.")
            ),
            nav_panel(
                title = "Influence Table",
                icon  = icon("table"),
                DTOutput(ns("influence_table"))
            ),
            nav_panel(
                title = "Residual Diagnostics",
                icon = icon("chart-line"),
                plotOutput(ns("residual_plots"), height = "500px")
            ),
            nav_panel(
                title = "Model Summary",
                icon  = icon("file-alt"),
                verbatimTextOutput(ns("model_full_summary"))
            )
        )
    )
}

diagnosticsServer <- function(id, engineered_data) {
    moduleServer(id, function(input, output, session) {
        ns <- session$ns

        model_results <- reactiveVal(NULL)

        # Update column choices
        observe({
            req(engineered_data())
            df <- engineered_data()
            num_cols <- names(df)[sapply(df, is.numeric)]
            updateSelectizeInput(session, "response", choices = num_cols)
            updateSelectizeInput(session, "predictors", choices = num_cols)
        })

        # Flag for conditional panel
        output$has_model <- reactive({
            !is.null(model_results())
        })
        outputOptions(output, "has_model", suspendWhenHidden = FALSE)

        # ── Fit Model ──
        observeEvent(input$fit_model, {
            req(engineered_data(), input$response, length(input$predictors) > 0)

            df <- engineered_data()
            response <- input$response
            predictors <- input$predictors

            # Ensure response not in predictors
            predictors <- setdiff(predictors, response)
            if (length(predictors) == 0) {
                showNotification("Predictors must differ from response.", type = "error")
                return()
            }

            tryCatch(
                {
                    result <- compute_influence(df, response, predictors)
                    model_results(result)

                    n_influential <- sum(result$diagnostics$Influential)
                    n_leverage <- sum(result$diagnostics$High_Leverage)

                    showNotification(
                        paste0(
                            "Model fitted! Found ", n_influential, " influential points and ",
                            n_leverage, " high-leverage observations."
                        ),
                        type = "message", duration = 5
                    )
                },
                error = function(e) {
                    showNotification(paste0("Model error: ", e$message), type = "error")
                }
            )
        })

        # ── Cook's Distance Plot ──
        output$cooks_plot <- plotly::renderPlotly({
            req(model_results())
            res <- model_results()
            diag <- res$diagnostics

            diag$Color <- ifelse(diag$Influential, "#e74c3c", "#667eea")

            p <- ggplot(diag, aes(
                x = Row, y = Cooks_Distance,
                text = paste("Row:", Row, "\nCook's D:", Cooks_Distance)
            )) +
                geom_col(fill = diag$Color, alpha = 0.8) +
                geom_hline(
                    yintercept = res$cooks_threshold, color = "red",
                    linetype = "dashed", linewidth = 0.8
                ) +
                theme_minimal(base_size = 13) +
                labs(
                    title = "Cook's Distance", x = "Observation", y = "Cook's Distance",
                    subtitle = paste0("Threshold: ", round(res$cooks_threshold, 4), " (4/n)")
                )

            plotly::ggplotly(p, tooltip = "text")
        })

        # ── Cook's Distance Table ──
        output$cooks_table <- renderDT({
            req(model_results())
            diag <- model_results()$diagnostics %>%
                filter(Influential) %>%
                select(Row, Cooks_Distance, Leverage, Std_Residual, DFFITS, Status) %>%
                arrange(desc(Cooks_Distance))

            if (nrow(diag) == 0) {
                diag <- tibble::tibble(Message = "No influential points detected (all Cook's D < 4/n)")
            }

            datatable(diag,
                options = list(dom = "t", scrollX = TRUE, pageLength = 20),
                rownames = FALSE, class = "compact stripe hover",
                caption = "Influential Observations (Cook's D > 4/n)"
            )
        })

        # ── Leverage Plot (Studentized Residuals vs Hat Values) ──
        output$leverage_plot <- plotly::renderPlotly({
            req(model_results())
            res <- model_results()
            diag <- res$diagnostics

            p <- ggplot(diag, aes(
                x = Leverage, y = Std_Residual, color = Status,
                text = paste(
                    "Row:", Row,
                    "\nLeverage:", round(Leverage, 4),
                    "\nStd Residual:", round(Std_Residual, 4),
                    "\nCook's D:", round(Cooks_Distance, 4)
                )
            )) +
                geom_point(alpha = 0.7, size = 3) +
                geom_hline(yintercept = c(-2, 2), color = "#f39c12", linetype = "dashed") +
                geom_vline(xintercept = res$leverage_threshold, color = "#e74c3c", linetype = "dashed") +
                scale_color_manual(values = c(
                    "🔴 High Influence + Leverage" = "#e74c3c",
                    "🟠 High Influence" = "#f39c12",
                    "🟡 High Leverage" = "#f1c40f",
                    "🟢 Normal" = "#18bc9c"
                )) +
                theme_minimal(base_size = 13) +
                labs(
                    title = "Studentized Residuals vs Leverage",
                    x = paste0("Leverage (threshold: ", round(res$leverage_threshold, 4), ")"),
                    y = "Studentized Residual",
                    color = "Status"
                )

            plotly::ggplotly(p, tooltip = "text")
        })

        # ── Full Influence Table ──
        output$influence_table <- renderDT({
            req(model_results())
            diag <- model_results()$diagnostics %>%
                arrange(desc(Cooks_Distance))

            datatable(diag,
                options = list(pageLength = 15, scrollX = TRUE, dom = "lfrtip"),
                rownames = FALSE, class = "compact stripe hover"
            ) %>%
                formatStyle(
                    "Status",
                    backgroundColor = styleEqual(
                        c(
                            "🔴 High Influence + Leverage", "🟠 High Influence",
                            "🟡 High Leverage", "🟢 Normal"
                        ),
                        c(
                            "rgba(231,76,60,0.15)", "rgba(243,156,18,0.15)",
                            "rgba(241,196,15,0.15)", "rgba(24,188,156,0.05)"
                        )
                    )
                ) %>%
                formatStyle(
                    "Cooks_Distance",
                    background = styleColorBar(range(diag$Cooks_Distance), "rgba(231,76,60,0.25)"),
                    backgroundSize = "100% 90%",
                    backgroundRepeat = "no-repeat",
                    backgroundPosition = "center"
                )
        })

        # ── Residual Diagnostics Plots ──
        output$residual_plots <- renderPlot({
            req(model_results())
            model <- model_results()$model
            par(mfrow = c(2, 2))
            plot(model,
                which = 1:4, col = "#667eea", pch = 16,
                sub.caption = "", caption = c(
                    "Residuals vs Fitted", "Normal Q-Q",
                    "Scale-Location", "Cook's Distance"
                )
            )
        })

        # ── Model Summaries ──
        output$model_summary_compact <- renderPrint({
            req(model_results())
            model <- model_results()$model
            cat("R² =", round(summary(model)$r.squared, 4), "\n")
            cat("Adj R² =", round(summary(model)$adj.r.squared, 4), "\n")
            cat("F-stat p =", format.pval(pf(summary(model)$fstatistic[1],
                summary(model)$fstatistic[2],
                summary(model)$fstatistic[3],
                lower.tail = FALSE
            )), "\n")
        })

        output$model_full_summary <- renderPrint({
            req(model_results())
            summary(model_results()$model)
        })

        # ── Remove Influential Points ──
        observeEvent(input$remove_influential, {
            req(model_results(), engineered_data())

            diag <- model_results()$diagnostics
            influential_rows <- diag$Row[diag$Influential]

            if (length(influential_rows) == 0) {
                showNotification("No influential points to remove.", type = "warning")
                return()
            }

            # Re-fit without influential points
            df <- engineered_data()
            df_clean <- df[-influential_rows, , drop = FALSE]

            response <- all.vars(formula(model_results()$model))[1]
            predictors <- setdiff(all.vars(formula(model_results()$model)), response)

            tryCatch(
                {
                    result <- compute_influence(df_clean, response, predictors)
                    model_results(result)

                    showNotification(
                        paste0("Removed ", length(influential_rows), " influential points. Model re-fitted."),
                        type = "message"
                    )
                },
                error = function(e) {
                    showNotification(paste0("Error: ", e$message), type = "error")
                }
            )
        })
    })
}
