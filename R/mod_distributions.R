# =============================================================================
# mod_distributions.R — Distribution Fitting Lab Module
# =============================================================================

distributionsUI <- function(id) {
    ns <- NS(id)

    layout_sidebar(
        sidebar = sidebar(
            title = "Distribution Lab",
            width = 300,
            selectizeInput(ns("dist_col"), "Select Numeric Column", choices = NULL),
            tags$hr(),
            h6(icon("chart-area"), " Distributions to Fit", style = "font-weight: 600;"),
            checkboxGroupInput(ns("dist_choices"), NULL,
                choices = c(
                    "Normal" = "norm", "Log-Normal" = "lnorm",
                    "Exponential" = "exp", "Gamma" = "gamma",
                    "Weibull" = "weibull"
                ),
                selected = c("norm", "lnorm", "gamma")
            ),
            tags$hr(),
            actionButton(ns("fit_dists"), "Fit Distributions",
                class = "btn-primary w-100", icon = icon("play")
            ),
            br(), br(),
            actionButton(ns("reset_dists"), "Reset",
                class = "btn-outline-secondary w-100", icon = icon("undo")
            ),
            br(), br(),
            downloadButton(ns("download_fit"), "Download Fit Results",
                class = "btn-outline-success w-100"
            ),
            tags$hr(),
            helpText("Uses Maximum Likelihood Estimation (MLE) via fitdistrplus.
                AIC/BIC: lower = better fit.")
        ),

        # Main panel
        navset_card_tab(
            id = ns("dist_tabs"),
            nav_panel(
                title = "Density Overlay",
                icon = icon("chart-area"),
                plotly::plotlyOutput(ns("density_overlay"), height = "450px")
            ),
            nav_panel(
                title = "Fit Comparison",
                icon  = icon("table"),
                DTOutput(ns("fit_table")),
                br(),
                verbatimTextOutput(ns("fit_details"))
            ),
            nav_panel(
                title = "Cullen-Frey",
                icon = icon("crosshairs"),
                plotOutput(ns("cullen_frey"), height = "450px"),
                helpText("The Cullen-Frey (skewness-kurtosis) diagram shows where your data
                  falls relative to known distributions.")
            ),
            nav_panel(
                title = "GoF Diagnostics",
                icon = icon("check-double"),
                plotOutput(ns("gof_plots"), height = "500px")
            )
        )
    )
}

distributionsServer <- function(id, engineered_data) {
    moduleServer(id, function(input, output, session) {
        ns <- session$ns

        fit_results <- reactiveVal(NULL)

        observe({
            req(engineered_data())
            num_cols <- names(engineered_data())[sapply(engineered_data(), is.numeric)]
            updateSelectizeInput(session, "dist_col", choices = num_cols)
        })

        # ── Reset ──
        observeEvent(input$reset_dists, {
            fit_results(NULL)
            showNotification("Distribution fits reset.", type = "message")
        })

        # ── Fit distributions ──
        observeEvent(input$fit_dists, {
            req(engineered_data(), input$dist_col, length(input$dist_choices) > 0)

            df <- engineered_data()
            # Force to plain numeric vector (fixes Cullen-Frey "must be numeric vector" error)
            x <- as.numeric(na.omit(df[[input$dist_col]]))

            if (length(x) < 10) {
                showNotification("Need at least 10 non-NA observations.", type = "error")
                return()
            }

            fits <- list()
            comparison <- list()

            for (d in input$dist_choices) {
                tryCatch(
                    {
                        if (d %in% c("lnorm", "exp", "gamma", "weibull")) {
                            if (any(x <= 0)) {
                                x_fit <- x - min(x) + 0.001
                            } else {
                                x_fit <- x
                            }
                        } else {
                            x_fit <- x
                        }

                        f <- fitdistrplus::fitdist(x_fit, d)
                        fits[[d]] <- f

                        ks_p <- tryCatch(
                            {
                                do.call(ks.test, c(list(x = x_fit, y = paste0("p", d)), as.list(f$estimate)))$p.value
                            },
                            error = function(e) NA_real_
                        )

                        comparison[[d]] <- data.frame(
                            Distribution = d, AIC = round(f$aic, 2), BIC = round(f$bic, 2),
                            LogLik = round(f$loglik, 2), KS_pvalue = round(ks_p, 4),
                            Parameters = paste(names(f$estimate), "=", round(f$estimate, 4), collapse = ", "),
                            stringsAsFactors = FALSE
                        )
                    },
                    error = function(e) {
                        showNotification(paste0("Could not fit ", d, ": ", e$message), type = "warning")
                    }
                )
            }

            if (length(fits) > 0) {
                comp_df <- do.call(rbind, comparison)
                comp_df <- comp_df[order(comp_df$AIC), ]

                fit_results(list(fits = fits, comparison = comp_df, data = x, column = input$dist_col))
                showNotification(paste0("Fitted ", length(fits), " distributions!"), type = "message")
            }
        })

        # ── Density Overlay Plot (with PROPER LEGEND) ──
        output$density_overlay <- plotly::renderPlotly({
            req(fit_results())
            res <- fit_results()
            x <- res$data

            # Named color mapping for legend
            dist_labels <- c(
                norm = "Normal", lnorm = "Log-Normal", exp = "Exponential",
                gamma = "Gamma", weibull = "Weibull"
            )
            dist_colors <- c(
                norm = "#e74c3c", lnorm = "#f39c12", exp = "#18bc9c",
                gamma = "#3498db", weibull = "#9b59b6"
            )

            df_hist <- data.frame(value = x)
            x_seq <- seq(min(x), max(x), length.out = 200)

            # Build overlay data for all fitted distributions
            overlay_list <- list()
            for (d in names(res$fits)) {
                f <- res$fits[[d]]
                y_fitted <- tryCatch(
                    {
                        if (d %in% c("lnorm", "exp", "gamma", "weibull") && any(x <= 0)) {
                            x_shifted <- x_seq - min(x) + 0.001
                            do.call(paste0("d", d), c(list(x = x_shifted), as.list(f$estimate)))
                        } else {
                            do.call(paste0("d", d), c(list(x = x_seq), as.list(f$estimate)))
                        }
                    },
                    error = function(e) NULL
                )

                if (!is.null(y_fitted)) {
                    label <- ifelse(d %in% names(dist_labels), dist_labels[d], d)
                    overlay_list[[d]] <- data.frame(
                        x = x_seq, y = y_fitted, Distribution = label,
                        stringsAsFactors = FALSE
                    )
                }
            }

            overlay_df <- do.call(rbind, overlay_list)

            # Build ggplot with explicit color mapping for legend
            color_map <- setNames(dist_colors[names(res$fits)], dist_labels[names(res$fits)])

            p <- ggplot() +
                geom_histogram(
                    data = df_hist, aes(x = value, y = after_stat(density)),
                    bins = 40, fill = "gray70", alpha = 0.5, color = "white"
                ) +
                geom_line(data = overlay_df, aes(x = x, y = y, color = Distribution), linewidth = 1.2) +
                scale_color_manual(values = color_map) +
                theme_minimal(base_size = 13) +
                labs(
                    title = paste("Distribution Fits:", res$column),
                    x = res$column, y = "Density", color = "Fitted Distribution"
                )

            plotly::ggplotly(p)
        })

        # ── Fit Comparison Table ──
        output$fit_table <- renderDT({
            req(fit_results())
            comp <- fit_results()$comparison

            datatable(comp,
                options = list(dom = "t", scrollX = TRUE),
                rownames = FALSE, class = "compact stripe hover"
            ) %>%
                formatStyle("KS_pvalue",
                    color = styleInterval(0.05, c("red", "green")),
                    fontWeight = "bold"
                )
        })

        output$fit_details <- renderPrint({
            req(fit_results())
            for (d in names(fit_results()$fits)) {
                cat("\n===", toupper(d), "===\n")
                print(summary(fit_results()$fits[[d]]))
            }
        })

        # ── Cullen-Frey Diagram (FIX: force numeric vector) ──
        output$cullen_frey <- renderPlot({
            req(fit_results())
            x <- as.numeric(fit_results()$data) # ensure plain numeric vector
            tryCatch(
                {
                    fitdistrplus::descdist(x, boot = 500)
                },
                error = function(e) {
                    plot.new()
                    text(0.5, 0.5, paste("Cullen-Frey error:", e$message), cex = 1.2, col = "#e74c3c")
                }
            )
        })

        # ── Goodness-of-Fit Diagnostic Plots ──
        output$gof_plots <- renderPlot({
            req(fit_results())
            fits <- fit_results()$fits
            req(length(fits) > 0)

            tryCatch(
                {
                    if (length(fits) == 1) {
                        plot(fits[[1]])
                    } else {
                        fit_list <- unname(fits)
                        fitdistrplus::denscomp(fit_list, legendtext = names(fits), plotstyle = "ggplot") +
                            theme_minimal(base_size = 13)
                    }
                },
                error = function(e) {
                    plot(fits[[1]])
                }
            )
        })

        # ── Download fit results ──
        output$download_fit <- downloadHandler(
            filename = function() paste0("distribution_fits_", Sys.Date(), ".csv"),
            content = function(file) {
                req(fit_results())
                write.csv(fit_results()$comparison, file, row.names = FALSE)
            }
        )
    })
}
