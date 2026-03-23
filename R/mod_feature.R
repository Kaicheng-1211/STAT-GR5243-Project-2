# =============================================================================
# mod_feature.R — Feature Engineering Module
# =============================================================================

featureUI <- function(id) {
    ns <- NS(id)

    layout_sidebar(
        sidebar = sidebar(
            title = "Feature Tools",
            width = 340,

            # ── Transformations ──
            h6(icon("exchange-alt"), " Column Transformations", style = "font-weight: 600;"),
            selectizeInput(ns("transform_col"), "Select Numeric Column", choices = NULL),
            selectInput(ns("transform_type"), "Transformation",
                choices = c(
                    "None" = "none", "Log (x+1)" = "log",
                    "Square Root" = "sqrt", "Square" = "square",
                    "Box-Cox" = "boxcox", "Reciprocal (1/x)" = "reciprocal",
                    "Cube Root" = "cbrt"
                )
            ),
            actionButton(ns("apply_transform"), "Apply Transform",
                class = "btn-primary w-100", icon = icon("bolt")
            ),
            tags$hr(),

            # ── Interaction Terms ──
            h6(icon("link"), " Interaction Terms", style = "font-weight: 600;"),
            selectizeInput(ns("interact_col1"), "Column A", choices = NULL),
            selectizeInput(ns("interact_col2"), "Column B", choices = NULL),
            selectInput(ns("interact_op"), "Operation",
                choices = c(
                    "Multiply (A × B)" = "multiply",
                    "Add (A + B)" = "add",
                    "Ratio (A / B)" = "divide"
                )
            ),
            actionButton(ns("create_interaction"), "Create Interaction",
                class = "btn-success w-100", icon = icon("plus")
            ),
            tags$hr(),

            # ── Binning ──
            h6(icon("th"), " Binning", style = "font-weight: 600;"),
            selectizeInput(ns("bin_col"), "Column to Bin", choices = NULL),
            sliderInput(ns("n_bins"), "Number of Bins", min = 2, max = 20, value = 5),
            selectInput(ns("bin_method"), "Method",
                choices = c(
                    "Equal Width" = "equal_width",
                    "Equal Frequency (Quantiles)" = "quantile"
                )
            ),
            actionButton(ns("apply_binning"), "Create Bins",
                class = "btn-info w-100", icon = icon("cut")
            ),
            tags$hr(),

            # ── Custom Formula ──
            h6(icon("calculator"), " Custom Formula", style = "font-weight: 600;"),
            textInput(ns("new_col_name"), "New Column Name", placeholder = "e.g., ratio_feature"),
            textInput(ns("formula_expr"), "R Expression",
                placeholder = "e.g., Sepal.Length / Sepal.Width"
            ),
            actionButton(ns("apply_formula"), "Create Column",
                class = "btn-warning w-100", icon = icon("code")
            ),
            helpText("Use column names directly. Supports R math: log(), sqrt(), abs(), +, -, *, /, ^"),
            tags$hr(),

            # ── Undo ──
            actionButton(ns("undo_last"), "Undo Last Change",
                class = "btn-outline-danger w-100", icon = icon("undo")
            )
        ),

        # Main panel
        navset_card_tab(
            id = ns("feature_tabs"),
            nav_panel(
                title = "Current Data",
                icon  = icon("table"),
                verbatimTextOutput(ns("feature_log")),
                DTOutput(ns("feature_table"))
            ),
            nav_panel(
                title = "Transform Preview",
                icon = icon("chart-area"),
                layout_column_wrap(
                    width = 1 / 2,
                    card(
                        card_header("Before"),
                        plotly::plotlyOutput(ns("preview_before"), height = "300px")
                    ),
                    card(
                        card_header("After"),
                        plotly::plotlyOutput(ns("preview_after"), height = "300px")
                    )
                )
            ),
            nav_panel(
                title = "Skewness Report",
                icon  = icon("arrows-alt-h"),
                DTOutput(ns("skewness_table"))
            )
        )
    )
}

featureServer <- function(id, cleaned_data) {
    moduleServer(id, function(input, output, session) {
        ns <- session$ns

        # ── Working data with undo stack ──
        feature_data <- reactiveVal(NULL)
        undo_stack <- reactiveVal(list())
        feature_log <- reactiveVal("")

        # Initialize from upstream
        observe({
            req(cleaned_data())
            feature_data(cleaned_data())
            undo_stack(list())
            feature_log("")
        })

        # Update select inputs when data changes
        observe({
            req(feature_data())
            df <- feature_data()
            num_cols <- names(df)[sapply(df, is.numeric)]
            all_cols <- names(df)

            updateSelectizeInput(session, "transform_col", choices = num_cols)
            updateSelectizeInput(session, "interact_col1", choices = num_cols)
            updateSelectizeInput(session, "interact_col2", choices = num_cols)
            updateSelectizeInput(session, "bin_col", choices = num_cols)
        })

        # Helper: save state for undo
        save_state <- function() {
            stack <- undo_stack()
            stack <- c(stack, list(feature_data()))
            if (length(stack) > 10) stack <- stack[-1] # keep last 10
            undo_stack(stack)
        }

        # ── Column Transformation ──
        observeEvent(input$apply_transform, {
            req(feature_data(), input$transform_col, input$transform_type != "none")
            save_state()

            df <- feature_data()
            col <- input$transform_col
            x <- df[[col]]
            new_name <- paste0(col, "_", input$transform_type)

            tryCatch(
                {
                    df[[new_name]] <- switch(input$transform_type,
                        "log" = log(x + 1),
                        "sqrt" = sqrt(abs(x)),
                        "square" = x^2,
                        "boxcox" = {
                            if (any(x <= 0, na.rm = TRUE)) {
                                x_shifted <- x - min(x, na.rm = TRUE) + 1
                            } else {
                                x_shifted <- x
                            }
                            # Optimal lambda via Box-Cox
                            lambda <- tryCatch(
                                {
                                    bc <- MASS::boxcox(lm(x_shifted ~ 1), plotit = FALSE)
                                    bc$x[which.max(bc$y)]
                                },
                                error = function(e) 1
                            )

                            if (abs(lambda) < 0.01) log(x_shifted) else (x_shifted^lambda - 1) / lambda
                        },
                        "reciprocal" = 1 / (x + ifelse(any(x == 0, na.rm = TRUE), 1e-6, 0)),
                        "cbrt" = sign(x) * abs(x)^(1 / 3)
                    )

                    feature_data(df)
                    log_msg <- paste0(
                        feature_log(), "\n✅ Created '", new_name, "' from '", col,
                        "' (", input$transform_type, ")"
                    )
                    feature_log(log_msg)
                    showNotification(paste0("Created column: ", new_name), type = "message")
                },
                error = function(e) {
                    showNotification(paste0("Error: ", e$message), type = "error")
                }
            )
        })

        # ── Interaction Term ──
        observeEvent(input$create_interaction, {
            req(feature_data(), input$interact_col1, input$interact_col2)
            save_state()

            df <- feature_data()
            a <- input$interact_col1
            b <- input$interact_col2

            new_name <- paste0(a, "_x_", b)

            tryCatch(
                {
                    df[[new_name]] <- switch(input$interact_op,
                        "multiply" = df[[a]] * df[[b]],
                        "add"      = df[[a]] + df[[b]],
                        "divide"   = df[[a]] / (df[[b]] + 1e-10)
                    )

                    feature_data(df)
                    feature_log(paste0(feature_log(), "\n✅ Created interaction '", new_name, "'"))
                    showNotification(paste0("Created: ", new_name), type = "message")
                },
                error = function(e) {
                    showNotification(paste0("Error: ", e$message), type = "error")
                }
            )
        })

        # ── Binning ──
        observeEvent(input$apply_binning, {
            req(feature_data(), input$bin_col)
            save_state()

            df <- feature_data()
            col <- input$bin_col
            new_name <- paste0(col, "_binned")

            tryCatch(
                {
                    if (input$bin_method == "equal_width") {
                        df[[new_name]] <- cut(df[[col]], breaks = input$n_bins, include.lowest = TRUE)
                    } else {
                        probs <- seq(0, 1, length.out = input$n_bins + 1)
                        df[[new_name]] <- cut(df[[col]],
                            breaks = quantile(df[[col]], probs, na.rm = TRUE),
                            include.lowest = TRUE, labels = FALSE
                        )
                    }

                    feature_data(df)
                    feature_log(paste0(
                        feature_log(), "\n✅ Binned '", col, "' into ",
                        input$n_bins, " bins (", input$bin_method, ")"
                    ))
                    showNotification(paste0("Binned: ", new_name), type = "message")
                },
                error = function(e) {
                    showNotification(paste0("Error: ", e$message), type = "error")
                }
            )
        })

        # ── Custom Formula ──
        observeEvent(input$apply_formula, {
            req(feature_data(), input$new_col_name, input$formula_expr)
            save_state()

            df <- feature_data()

            tryCatch(
                {
                    env <- new.env(parent = baseenv())
                    list2env(as.list(df), envir = env)

                    result <- eval(parse(text = input$formula_expr), envir = env)

                    if (length(result) != nrow(df)) {
                        stop("Result length (", length(result), ") doesn't match data rows (", nrow(df), ")")
                    }

                    df[[input$new_col_name]] <- result
                    feature_data(df)
                    feature_log(paste0(
                        feature_log(), "\n✅ Created '", input$new_col_name,
                        "' = ", input$formula_expr
                    ))
                    showNotification(paste0("Created: ", input$new_col_name), type = "message")
                },
                error = function(e) {
                    showNotification(paste0("Formula error: ", e$message), type = "error")
                }
            )
        })

        # ── Undo ──
        observeEvent(input$undo_last, {
            stack <- undo_stack()
            if (length(stack) > 0) {
                feature_data(stack[[length(stack)]])
                undo_stack(stack[-length(stack)])
                feature_log(paste0(feature_log(), "\n↩️ Undo applied"))
                showNotification("Undone last change", type = "warning")
            } else {
                showNotification("Nothing to undo", type = "warning")
            }
        })

        # ── Outputs ──
        output$feature_log <- renderText({
            feature_log()
        })

        output$feature_table <- renderDT({
            req(feature_data())
            datatable(feature_data(),
                options = list(pageLength = 15, scrollX = TRUE, dom = "lfrtip"),
                rownames = FALSE, class = "compact stripe hover"
            )
        })

        # ── Transform Preview (Before/After) ──
        output$preview_before <- plotly::renderPlotly({
            req(feature_data(), input$transform_col)
            df <- feature_data()
            col <- input$transform_col
            req(col %in% names(df))

            p <- ggplot(df, aes(x = .data[[col]])) +
                geom_histogram(aes(y = after_stat(density)),
                    bins = 30, fill = "#667eea", alpha = 0.7
                ) +
                geom_density(color = "#e74c3c", linewidth = 1) +
                theme_minimal(base_size = 12) +
                labs(title = paste("Original:", col), x = col, y = "Density")

            plotly::ggplotly(p)
        })

        output$preview_after <- plotly::renderPlotly({
            req(feature_data(), input$transform_col, input$transform_type != "none")
            df <- feature_data()
            col <- input$transform_col
            x <- df[[col]]

            transformed <- tryCatch(
                {
                    switch(input$transform_type,
                        "log"        = log(x + 1),
                        "sqrt"       = sqrt(abs(x)),
                        "square"     = x^2,
                        "reciprocal" = 1 / (x + 1e-6),
                        "cbrt"       = sign(x) * abs(x)^(1 / 3),
                        x
                    )
                },
                error = function(e) x
            )

            df_preview <- data.frame(value = transformed)

            p <- ggplot(df_preview, aes(x = value)) +
                geom_histogram(aes(y = after_stat(density)),
                    bins = 30, fill = "#18bc9c", alpha = 0.7
                ) +
                geom_density(color = "#e74c3c", linewidth = 1) +
                theme_minimal(base_size = 12) +
                labs(title = paste("After:", input$transform_type), x = "Transformed", y = "Density")

            plotly::ggplotly(p)
        })

        # ── Skewness Report ──
        output$skewness_table <- renderDT({
            req(feature_data())
            moments_df <- compute_moments(feature_data())
            req(nrow(moments_df) > 0)

            datatable(
                moments_df,
                options = list(
                    pageLength = 50, dom = "ft", scrollX = TRUE,
                    order = list(list(1, "desc"))
                ),
                rownames = FALSE,
                class = "compact stripe hover"
            ) %>%
                formatStyle(
                    "Skewness",
                    background = styleInterval(
                        c(-0.5, 0.5),
                        c("rgba(231,76,60,0.15)", "rgba(24,188,156,0.15)", "rgba(231,76,60,0.15)")
                    )
                ) %>%
                formatStyle(
                    "Normality_p",
                    color = styleInterval(0.05, c("red", "green")),
                    fontWeight = "bold"
                )
        })

        # Return for downstream
        return(reactive({
            feature_data()
        }))
    })
}
