# =============================================================================
# mod_cleaning.R — Data Cleaning & Preprocessing Module
# =============================================================================

cleaningUI <- function(id) {
    ns <- NS(id)

    layout_sidebar(
        sidebar = sidebar(
            title = "Cleaning Options",
            width = 340,

            # ── Missing Values ──
            h6(icon("puzzle-piece"), " Missing Values", style = "font-weight: 600;"),
            radioGroupButtons(
                inputId = ns("na_strategy"),
                label = NULL,
                choices = c(
                    "Keep" = "keep", "Remove Rows" = "remove_rows",
                    "Remove Cols (>%)" = "remove_cols", "Impute" = "impute"
                ),
                selected = "keep",
                size = "sm",
                status = "outline-primary",
                justified = TRUE
            ),
            conditionalPanel(
                condition = sprintf("input['%s'] == 'remove_cols'", ns("na_strategy")),
                sliderInput(ns("na_col_threshold"), "Max Missing %",
                    min = 10, max = 90, value = 50, step = 5
                )
            ),
            conditionalPanel(
                condition = sprintf("input['%s'] == 'impute'", ns("na_strategy")),
                selectInput(ns("impute_method"), "Imputation Method",
                    choices = c("Mean" = "mean", "Median" = "median", "Mode" = "mode")
                )
            ),
            tags$hr(),

            # ── Duplicates ──
            h6(icon("clone"), " Duplicates", style = "font-weight: 600;"),
            switchInput(ns("remove_dups"),
                label = "Remove Duplicates",
                value = FALSE, size = "small", onStatus = "success"
            ),
            tags$hr(),

            # ── Scaling ──
            h6(icon("expand-arrows-alt"), " Scaling", style = "font-weight: 600;"),
            selectInput(ns("scale_method"), "Scale Numeric Columns",
                choices = c(
                    "None" = "none", "Z-Score (Standardize)" = "zscore",
                    "Min-Max [0,1]" = "minmax", "Robust (Median/IQR)" = "robust"
                )
            ),
            tags$hr(),

            # ── Categorical Encoding ──
            h6(icon("tags"), " Encoding", style = "font-weight: 600;"),
            selectInput(ns("encode_method"), "Encode Categorical Columns",
                choices = c(
                    "None" = "none", "Label Encoding" = "label",
                    "One-Hot Encoding" = "onehot"
                )
            ),
            conditionalPanel(
                condition = sprintf("input['%s'] != 'none'", ns("encode_method")),
                selectizeInput(ns("encode_cols"), "Select Columns to Encode",
                    choices = NULL, multiple = TRUE
                )
            ),
            tags$hr(),

            # ── Outlier Detection ──
            h6(icon("exclamation-circle"), " Outlier Treatment", style = "font-weight: 600;"),
            selectInput(ns("outlier_method"), "Detection Method",
                choices = c(
                    "None" = "none", "IQR (1.5x)" = "iqr",
                    "Z-Score (|z| > 3)" = "zscore_outlier"
                )
            ),
            conditionalPanel(
                condition = sprintf("input['%s'] != 'none'", ns("outlier_method")),
                selectInput(ns("outlier_action"), "Treatment",
                    choices = c(
                        "Flag Only" = "flag", "Remove" = "remove",
                        "Cap (Winsorize)" = "cap"
                    )
                )
            ),
            tags$hr(),

            # ── Apply & Download ──
            actionButton(ns("apply_cleaning"), "Apply Cleaning",
                class = "btn-primary btn-block w-100", icon = icon("check")
            ),
            br(), br(),
            downloadButton(ns("download_data"), "Download Cleaned Data",
                class = "btn-outline-success btn-block w-100"
            )
        ),

        # Main panel
        navset_card_tab(
            id = ns("clean_tabs"),
            nav_panel(
                title = "Cleaned Data",
                icon  = icon("table"),
                verbatimTextOutput(ns("cleaning_log")),
                DTOutput(ns("clean_table"))
            ),
            nav_panel(
                title = "Before / After",
                icon = icon("exchange-alt"),
                layout_column_wrap(
                    width = 1 / 2,
                    card(
                        card_header("Before Cleaning"),
                        verbatimTextOutput(ns("before_summary"))
                    ),
                    card(
                        card_header("After Cleaning"),
                        verbatimTextOutput(ns("after_summary"))
                    )
                )
            ),
            nav_panel(
                title = "Outlier Report",
                icon = icon("exclamation-triangle"),
                plotly::plotlyOutput(ns("outlier_plot"), height = "350px"),
                br(),
                DTOutput(ns("outlier_table"))
            )
        )
    )
}

cleaningServer <- function(id, uploaded_data) {
    moduleServer(id, function(input, output, session) {
        ns <- session$ns

        # ── Working copy (starts as uploaded data) ──
        cleaned <- reactiveVal(NULL)
        cleaning_log <- reactiveVal("")

        # Initialize from upstream
        observe({
            req(uploaded_data())
            cleaned(uploaded_data())
            cleaning_log("")
        })

        # Update encoding column choices
        observe({
            req(uploaded_data())
            df <- uploaded_data()
            cat_cols <- names(df)[sapply(df, function(x) is.character(x) | is.factor(x))]
            updateSelectizeInput(session, "encode_cols", choices = cat_cols, selected = NULL)
        })

        # ── Apply cleaning pipeline ──
        observeEvent(input$apply_cleaning, {
            req(uploaded_data())
            df <- uploaded_data()
            log_lines <- c()

            # 1. Missing values
            if (input$na_strategy == "remove_rows") {
                n_before <- nrow(df)
                df <- na.omit(df)
                log_lines <- c(
                    log_lines,
                    paste0("✅ Removed ", n_before - nrow(df), " rows with missing values")
                )
            } else if (input$na_strategy == "remove_cols") {
                thresh <- input$na_col_threshold / 100
                pct_missing <- sapply(df, function(x) mean(is.na(x)))
                cols_to_drop <- names(pct_missing[pct_missing > thresh])
                if (length(cols_to_drop) > 0) {
                    df <- df[, !names(df) %in% cols_to_drop, drop = FALSE]
                    log_lines <- c(
                        log_lines,
                        paste0(
                            "✅ Removed ", length(cols_to_drop), " columns with >",
                            input$na_col_threshold, "% missing: ", paste(cols_to_drop, collapse = ", ")
                        )
                    )
                }
            } else if (input$na_strategy == "impute") {
                num_cols <- names(df)[sapply(df, is.numeric)]
                imputed_count <- 0

                for (col in num_cols) {
                    n_na <- sum(is.na(df[[col]]))
                    if (n_na > 0) {
                        val <- switch(input$impute_method,
                            "mean" = mean(df[[col]], na.rm = TRUE),
                            "median" = median(df[[col]], na.rm = TRUE),
                            "mode" = {
                                tab <- table(df[[col]])
                                as.numeric(names(tab[which.max(tab)]))
                            }
                        )
                        df[[col]][is.na(df[[col]])] <- val
                        imputed_count <- imputed_count + n_na
                    }
                }
                if (imputed_count > 0) {
                    log_lines <- c(
                        log_lines,
                        paste0("✅ Imputed ", imputed_count, " missing values using ", input$impute_method)
                    )
                }
            }

            # 2. Duplicates
            if (input$remove_dups) {
                n_before <- nrow(df)
                df <- distinct(df)
                n_removed <- n_before - nrow(df)
                if (n_removed > 0) {
                    log_lines <- c(
                        log_lines,
                        paste0("✅ Removed ", n_removed, " duplicate rows")
                    )
                }
            }

            # 3. Scaling
            if (input$scale_method != "none") {
                num_cols <- names(df)[sapply(df, is.numeric)]
                if (length(num_cols) > 0) {
                    if (input$scale_method == "zscore") {
                        df[num_cols] <- lapply(df[num_cols], scale)
                        log_lines <- c(log_lines, "✅ Applied Z-score standardization")
                    } else if (input$scale_method == "minmax") {
                        df[num_cols] <- lapply(df[num_cols], function(x) {
                            rng <- range(x, na.rm = TRUE)
                            if (rng[2] == rng[1]) {
                                return(rep(0, length(x)))
                            }
                            (x - rng[1]) / (rng[2] - rng[1])
                        })
                        log_lines <- c(log_lines, "✅ Applied Min-Max scaling [0, 1]")
                    } else if (input$scale_method == "robust") {
                        df[num_cols] <- lapply(df[num_cols], function(x) {
                            med <- median(x, na.rm = TRUE)
                            iqr_val <- IQR(x, na.rm = TRUE)
                            if (iqr_val == 0) {
                                return(x - med)
                            }
                            (x - med) / iqr_val
                        })
                        log_lines <- c(log_lines, "✅ Applied Robust scaling (median/IQR)")
                    }
                }
            }

            # 4. Encoding
            if (input$encode_method != "none" && length(input$encode_cols) > 0) {
                if (input$encode_method == "label") {
                    for (col in input$encode_cols) {
                        if (col %in% names(df)) {
                            df[[col]] <- as.numeric(as.factor(df[[col]]))
                        }
                    }
                    log_lines <- c(
                        log_lines,
                        paste0("✅ Label-encoded: ", paste(input$encode_cols, collapse = ", "))
                    )
                } else if (input$encode_method == "onehot") {
                    for (col in input$encode_cols) {
                        if (col %in% names(df)) {
                            dummies <- model.matrix(~ . - 1, data = df[, col, drop = FALSE])
                            colnames(dummies) <- paste0(col, "_", gsub(paste0("^", col), "", colnames(dummies)))
                            df <- cbind(df[, !names(df) %in% col, drop = FALSE], as.data.frame(dummies))
                        }
                    }
                    log_lines <- c(
                        log_lines,
                        paste0("✅ One-hot encoded: ", paste(input$encode_cols, collapse = ", "))
                    )
                }
            }

            # 5. Outlier treatment
            if (input$outlier_method != "none") {
                num_cols <- names(df)[sapply(df, is.numeric)]
                outlier_count <- 0

                for (col in num_cols) {
                    x <- df[[col]]
                    if (input$outlier_method == "iqr") {
                        q1 <- quantile(x, 0.25, na.rm = TRUE)
                        q3 <- quantile(x, 0.75, na.rm = TRUE)
                        iqr_val <- q3 - q1
                        is_outlier <- !is.na(x) & (x < q1 - 1.5 * iqr_val | x > q3 + 1.5 * iqr_val)
                    } else {
                        z <- abs(scale(x))
                        is_outlier <- !is.na(z) & z > 3
                    }

                    n_out <- sum(is_outlier)
                    if (n_out > 0 && input$outlier_action != "flag") {
                        if (input$outlier_action == "remove") {
                            df <- df[!is_outlier, , drop = FALSE]
                        } else if (input$outlier_action == "cap") {
                            if (input$outlier_method == "iqr") {
                                lower <- q1 - 1.5 * iqr_val
                                upper <- q3 + 1.5 * iqr_val
                            } else {
                                lower <- mean(x, na.rm = TRUE) - 3 * sd(x, na.rm = TRUE)
                                upper <- mean(x, na.rm = TRUE) + 3 * sd(x, na.rm = TRUE)
                            }
                            df[[col]] <- pmax(pmin(df[[col]], upper), lower)
                        }
                        outlier_count <- outlier_count + n_out
                    }
                }

                if (outlier_count > 0) {
                    action_label <- if (input$outlier_action == "remove") "removed" else "winsorized"
                    log_lines <- c(
                        log_lines,
                        paste0(
                            "✅ ", outlier_count, " outliers ", action_label,
                            " (", input$outlier_method, " method)"
                        )
                    )
                }
            }

            if (length(log_lines) == 0) {
                log_lines <- "ℹ️ No changes applied (all settings at default)."
            }

            cleaned(as.data.frame(df))
            cleaning_log(paste(log_lines, collapse = "\n"))
            showNotification("Cleaning pipeline applied!", type = "message")
        })

        # ── Outputs ──
        output$cleaning_log <- renderText({
            cleaning_log()
        })

        output$clean_table <- renderDT({
            req(cleaned())
            datatable(cleaned(),
                options = list(pageLength = 15, scrollX = TRUE, dom = "lfrtip"),
                rownames = FALSE, class = "compact stripe hover"
            )
        })

        output$before_summary <- renderPrint({
            req(uploaded_data())
            cat("Dimensions:", nrow(uploaded_data()), "rows ×", ncol(uploaded_data()), "cols\n")
            cat("Missing:", sum(is.na(uploaded_data())), "\n")
            cat("Duplicates:", nrow(uploaded_data()) - nrow(distinct(uploaded_data())), "\n\n")
            str(uploaded_data(), give.attr = FALSE)
        })

        output$after_summary <- renderPrint({
            req(cleaned())
            cat("Dimensions:", nrow(cleaned()), "rows ×", ncol(cleaned()), "cols\n")
            cat("Missing:", sum(is.na(cleaned())), "\n")
            cat("Duplicates:", nrow(cleaned()) - nrow(distinct(cleaned())), "\n\n")
            str(cleaned(), give.attr = FALSE)
        })

        # ── Outlier visualization ──
        output$outlier_plot <- plotly::renderPlotly({
            req(cleaned())
            df <- cleaned()
            num_cols <- names(df)[sapply(df, is.numeric)]
            req(length(num_cols) > 0)

            # Show boxplots for first 8 numeric columns
            cols_to_show <- head(num_cols, 8)
            df_long <- tidyr::pivot_longer(df[cols_to_show],
                cols = everything(),
                names_to = "Variable", values_to = "Value"
            )

            p <- ggplot(df_long, aes(x = Variable, y = Value, fill = Variable)) +
                geom_boxplot(alpha = 0.7, show.legend = FALSE) +
                theme_minimal(base_size = 13) +
                theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
                labs(title = "Outlier Overview (Boxplots)", x = "", y = "Value")

            plotly::ggplotly(p, tooltip = c("y"))
        })

        output$outlier_table <- renderDT({
            req(cleaned())
            df <- cleaned()
            num_cols <- names(df)[sapply(df, is.numeric)]

            outlier_summary <- purrr::map_dfr(num_cols, function(col) {
                x <- df[[col]]
                q1 <- quantile(x, 0.25, na.rm = TRUE)
                q3 <- quantile(x, 0.75, na.rm = TRUE)
                iqr_val <- q3 - q1
                n_iqr <- sum(!is.na(x) & (x < q1 - 1.5 * iqr_val | x > q3 + 1.5 * iqr_val))
                z <- abs(scale(x))
                n_zscore <- sum(!is.na(z) & z > 3)

                tibble::tibble(
                    Column = col,
                    IQR_Outliers = n_iqr,
                    ZScore_Outliers = n_zscore,
                    Min = round(min(x, na.rm = TRUE), 3),
                    Max = round(max(x, na.rm = TRUE), 3),
                    Mean = round(mean(x, na.rm = TRUE), 3),
                    SD = round(sd(x, na.rm = TRUE), 3)
                )
            })

            datatable(outlier_summary,
                options = list(dom = "ft", scrollX = TRUE),
                rownames = FALSE, class = "compact stripe hover"
            )
        })

        # ── Download handler ──
        output$download_data <- downloadHandler(
            filename = function() paste0("cleaned_data_", Sys.Date(), ".csv"),
            content = function(file) write.csv(cleaned(), file, row.names = FALSE)
        )

        # Return cleaned data for downstream
        return(reactive({
            cleaned()
        }))
    })
}
