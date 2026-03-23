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
            h6(icon("info-circle"), " About", style = "font-weight: 600;"),
            helpText("Uses Maximum Likelihood Estimation (MLE) via the fitdistrplus package.
                AIC/BIC are used for model comparison — lower values indicate better fit.")
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
                  falls relative to known distributions. This helps identify which
                  theoretical distribution family is most appropriate.")
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

        # Update column choices
        observe({
            req(engineered_data())
            num_cols <- names(engineered_data())[sapply(engineered_data(), is.numeric)]
            updateSelectizeInput(session, "dist_col", choices = num_cols)
        })

        # ── Fit distributions on button click ──
        observeEvent(input$fit_dists, {
            req(engineered_data(), input$dist_col, length(input$dist_choices) > 0)

            df <- engineered_data()
            x <- na.omit(df[[input$dist_col]])

            if (length(x) < 10) {
                showNotification("Need at least 10 non-NA observations.", type = "error")
                return()
            }

            fits <- list()
            comparison <- list()

            for (d in input$dist_choices) {
                tryCatch(
                    {
                        # Some distributions require positive values
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

                        # Kolmogorov-Smirnov test
                        ks_p <- tryCatch(
                            {
                                ks_args <- c(list(x_fit), as.list(f$estimate))
                                ks_args$y <- paste0("p", d)
                                do.call(ks.test, c(list(x = x_fit, y = paste0("p", d)), as.list(f$estimate)))$p.value
                            },
                            error = function(e) NA_real_
                        )

                        comparison[[d]] <- tibble::tibble(
                            Distribution = d,
                            AIC = round(f$aic, 2),
                            BIC = round(f$bic, 2),
                            LogLik = round(f$loglik, 2),
                            KS_pvalue = round(ks_p, 4),
                            Parameters = paste(names(f$estimate), "=", round(f$estimate, 4), collapse = ", ")
                        )
                    },
                    error = function(e) {
                        showNotification(paste0("Could not fit ", d, ": ", e$message), type = "warning")
                    }
                )
            }

            if (length(fits) > 0) {
                fit_results(list(
                    fits = fits,
                    comparison = dplyr::bind_rows(comparison) %>% dplyr::arrange(AIC),
                    data = x,
                    column = input$dist_col
                ))
                showNotification(paste0("Fitted ", length(fits), " distributions!"), type = "message")
            }
        })

        # ── Density Overlay Plot ──
        output$density_overlay <- plotly::renderPlotly({
            req(fit_results())
            res <- fit_results()
            x <- res$data

            # Base histogram
            df_hist <- data.frame(value = x)
            p <- ggplot(df_hist, aes(x = value)) +
                geom_histogram(aes(y = after_stat(density)),
                    bins = 40,
                    fill = "#667eea", alpha = 0.4, color = "white"
                ) +
                geom_density(linewidth = 1, color = "#2c3e50", linetype = "dashed")

            # Overlay fitted densities
            dist_colors <- c(
                norm = "#e74c3c", lnorm = "#f39c12", exp = "#18bc9c",
                gamma = "#3498db", weibull = "#9b59b6"
            )

            x_seq <- seq(min(x), max(x), length.out = 200)

            for (d in names(res$fits)) {
                f <- res$fits[[d]]

                # Compute fitted density
                y_fitted <- tryCatch(
                    {
                        do.call(paste0("d", d), c(list(x = x_seq), as.list(f$estimate)))
                    },
                    error = function(e) NULL
                )

                if (!is.null(y_fitted)) {
                    df_fit <- data.frame(x = x_seq, y = y_fitted, dist = d)
                    color <- dist_colors[d]
                    if (is.na(color)) color <- "gray50"
                    p <- p + geom_line(
                        data = df_fit, aes(x = x, y = y),
                        color = color, linewidth = 1.1
                    )
                }
            }

            p <- p + theme_minimal(base_size = 13) +
                labs(
                    title = paste("Distribution Fits:", res$column),
                    x = res$column, y = "Density",
                    caption = paste(
                        "Colors — Red: Normal, Orange: Log-Normal, Green: Exponential,",
                        "Blue: Gamma, Purple: Weibull"
                    )
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
                formatStyle(
                    "AIC",
                    background = styleColorBar(range(comp$AIC), "rgba(102,126,234,0.2)"),
                    backgroundSize = "100% 90%",
                    backgroundRepeat = "no-repeat",
                    backgroundPosition = "center"
                ) %>%
                formatStyle(
                    "KS_pvalue",
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

        # ── Cullen-Frey Diagram ──
        output$cullen_frey <- renderPlot({
            req(fit_results())
            x <- fit_results()$data
            fitdistrplus::descdist(x, boot = 500, obs.col = "#667eea", boot.col = "#e74c3c")
        })

        # ── Goodness-of-Fit Diagnostic Plots ──
        output$gof_plots <- renderPlot({
            req(fit_results())
            fits <- fit_results()$fits
            req(length(fits) > 0)

            if (length(fits) == 1) {
                plot(fits[[1]])
            } else {
                # Use fitdistrplus comparison plot
                fit_list <- unname(fits)
                tryCatch(
                    {
                        fitdistrplus::denscomp(fit_list,
                            legendtext = names(fits),
                            plotstyle = "ggplot"
                        ) +
                            theme_minimal(base_size = 13)
                    },
                    error = function(e) {
                        plot(fits[[1]])
                    }
                )
            }
        })
    })
}
