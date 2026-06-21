# -*- coding: utf-8 -*-
# ---------------------------------------------------------------
# Structura2 – Structural Insights, Simplified
# Shiny app for Structural Equation Modeling with mean structures
# ---------------------------------------------------------------

options(
  shiny.fullstacktrace = TRUE,
  shiny.reactlog       = TRUE,
  shiny.sanitize.errors = TRUE
)

# ---- WebR / Parallel Compatibility Patch -------------------------
# Patch parallel::detectCores BEFORE loading any library to intercept lavaan's startup checks.
tryCatch({
  if (requireNamespace("parallel", quietly = TRUE)) {
    ns <- asNamespace("parallel")
    if (bindingIsLocked("detectCores", ns)) {
      unlockBinding("detectCores", ns)
    }
    assign("detectCores", function(...) 1L, envir = ns)
    lockBinding("detectCores", ns)
  }
}, error = function(e) NULL)

# ---- Libraries --------------------------------------------------

library(shiny)
library(shinyjs)
library(DT)
library(rhandsontable)
library(lavaan)
library(DiagrammeR)
library(ggplot2)
library(markdown)

# Patch the lavaan option cache to prevent NA bounds crashes during estimation checks
tryCatch({
  env <- lavaan:::lavaan_cache_env
  for (chk_name in c("opt_check", "opt.check")) {
    if (exists(chk_name, envir = env)) {
      opt_check <- get(chk_name, envir = env)
      if (!is.null(opt_check$ncpus) && !is.null(opt_check$ncpus$nm)) {
        bounds <- opt_check$ncpus$nm$bounds
        if (any(is.na(bounds))) {
          opt_check$ncpus$nm$bounds[is.na(bounds)] <- 1L
          assign(chk_name, opt_check, envir = env)
        }
      }
    }
  }
}, error = function(e) NULL)

# ---- Inlined Utilities (from utils.R) ----------------------------


# 2. semDiagram: Visual path diagrams robust against NA and multicollinearity
semDiagram <- function(
    fitted_model,
    digits            = 3,
    standardized      = TRUE,
    alpha             = 0.05,
    low_alpha         = 0.2,
    min_width         = 1,
    max_width         = 5,
    pos_color         = "blue",
    neg_color         = "red",
    fontname          = "Helvetica",
    node_fontsize     = 11,
    edge_fontsize     = 9,
    show_residuals    = FALSE,
    show_intercepts   = FALSE,
    show_fit          = TRUE,
    show_collinearity = TRUE,
    layout            = "LR",
    ratio             = "fill",
    curvature         = 0.3,
    engine            = "dot",
    twopi_compact     = TRUE) {

  engine <- match.arg(engine, c("dot","neato","fdp","circo","twopi"))

  alpha_color <- function(col, alpha_val) {
    if (is.na(alpha_val) || alpha_val < 0 || alpha_val > 1) alpha_val <- 1
    rgb_mat <- grDevices::col2rgb(col) / 255
    grDevices::rgb(rgb_mat[1,], rgb_mat[2,], rgb_mat[3,], alpha = alpha_val)
  }

  if (!inherits(fitted_model, "lavaan"))
    stop("`fitted_model` must be a lavaan object.")

  invisible(lapply(c("DiagrammeR","lavaan"), function(p)
    if (!requireNamespace(p, quietly = TRUE))
      stop(sprintf("Package '%s' is required but not installed.", p))))

  params <- lavaan::parameterEstimates(fitted_model, standardized = TRUE)
  scale_col <- if (standardized) "std.all" else "est"

  fit_measures <- lavaan::fitMeasures(
    fitted_model,
    c("pvalue","srmr","rmsea","gfi","agfi","nfi","cfi","aic","bic"))
  n_obs <- lavaan::lavInspect(fitted_model, "nobs")

  condition_number <- NA
  max_cond_index <- NA
  tryCatch({
    samp_cov <- lavaan::lavInspect(fitted_model, "sampstat")$cov
    if (!is.null(samp_cov) && nrow(samp_cov) > 0) {
      samp_cor <- stats::cov2cor(samp_cov)
      eig_vals <- eigen(samp_cor, symmetric = TRUE, only.values = TRUE)$values
      if (length(eig_vals) > 0 && min(eig_vals) > 1e-12) {
        condition_number <- max(eig_vals) / min(eig_vals)
        condition_indices <- sqrt(max(eig_vals) / eig_vals)
        max_cond_index <- max(condition_indices)
      }
    }
  }, error = function(e) NULL)

  edge_rows <- params[params$op %in% c("=~","~","~~") & params$lhs != params$rhs, ]
  vals <- abs(edge_rows[[scale_col]]); vals <- vals[!is.na(vals)]
  max_abs <- if (standardized) 1 else (if (length(vals) == 0 || !is.finite(max(vals))) 1 else max(vals))

  colorize_thresh <- function(v, thr, invert = FALSE) {
    if (is.na(v)) "gray50"
    else if (invert) {
      if (v > thr) "red" else "gray20"
    } else {
      if (v <= thr) "red" else "gray20"
    }
  }

  fit_block <- if (show_fit) {
    paste0(
      sprintf("N = %d | ", n_obs),
      sprintf("<font color='%s'>p = %.3f</font> | ",
              colorize_thresh(fit_measures["pvalue"], 0.05), fit_measures["pvalue"]),
      sprintf("<font color='%s'>SRMR = %.3f</font> | ",
              colorize_thresh(fit_measures["srmr"], 0.08, invert = TRUE), fit_measures["srmr"]),
      sprintf("<font color='%s'>RMSEA = %.3f</font> | ",
              colorize_thresh(fit_measures["rmsea"], 0.08, invert = TRUE), fit_measures["rmsea"]),
      sprintf("AIC = %.1f | BIC = %.1f<BR/>", fit_measures["aic"], fit_measures["bic"]),
      sprintf("<font color='%s'>GFI = %.3f</font> | ",
              colorize_thresh(fit_measures["gfi"], 0.90), fit_measures["gfi"]),
      sprintf("<font color='%s'>AGFI = %.3f</font> | ",
              colorize_thresh(fit_measures["agfi"], 0.90), fit_measures["agfi"]),
      sprintf("<font color='%s'>NFI = %.3f</font> | ",
              colorize_thresh(fit_measures["nfi"], 0.90), fit_measures["nfi"]),
      sprintf("<font color='%s'>CFI = %.3f</font>",
              colorize_thresh(fit_measures["cfi"], 0.90), fit_measures["cfi"])
    )
  } else ""

  coll_block <- if (show_collinearity) {
    paste0(
      sprintf("<font color='%s'>Condition Number = %.1f</font>  | ",
              colorize_thresh(condition_number, 30, invert = TRUE), condition_number),
      sprintf("<font color='%s'>Max Condition Index = %.1f</font>",
              colorize_thresh(max_cond_index, 30, invert = TRUE), max_cond_index)
    )
  } else ""

  coeff_text <- if (standardized) "Coefficients: <b>Standardized</b>" else "Coefficients: <b>Unstandardized</b>"
  intercept_text <- if (show_intercepts) "Intercepts: <b>Shown</b>" else "Intercepts: <b>Hidden</b>"
  annot_block <- sprintf("%s   | %s", coeff_text, intercept_text)

  label_parts <- c(annot_block, if (show_fit) fit_block else NULL, if (show_collinearity) coll_block else NULL)
  top_label <- sprintf("<%s>", paste(label_parts, collapse = "<BR/>"))

  latents   <- unique(params$lhs[params$op == "=~"])
  observeds <- setdiff(unique(c(params$lhs, params$rhs)), c(latents, "1", ""))
  nodes <- list()
  for (lv in latents) nodes[[lv]] <- list(
    shape = "ellipse", label = lv, style = "filled", fillcolor = "#F0F0F0",
    fontname = fontname, fontsize = node_fontsize)
  for (ov in observeds) nodes[[ov]] <- list(
    shape = "box", label = ov, fontname = fontname, fontsize = node_fontsize)

  edges <- list()
  for (i in seq_len(nrow(params))) {
    p <- params[i, ]
    if (p$op %in% c("=~","~","~~") && p$lhs != p$rhs) {
      value <- if (standardized) p$std.all else p$est
      if (is.na(value)) value <- p$est

      pen <- (abs(value) / max_abs) * (max_width - min_width) + min_width
      if (!is.finite(pen)) pen <- min_width

      alpha_edge <- if (is.na(p$pvalue)) low_alpha else if (p$pvalue < alpha) 1 else low_alpha
      col <- alpha_color(if (value >= 0) pos_color else neg_color, alpha_edge)

      e_base <- switch(p$op,
                       "=~" = list(from = p$lhs, to = p$rhs, arrowhead = "vee"),
                       "~"  = list(from = p$rhs, to = p$lhs, arrowhead = "vee"),
                       "~~" = list(from = p$lhs, to = p$rhs,
                                   arrowhead = "vee", arrowtail = "vee",
                                   dir = "both", style = "dashed"))
      e_base$label    <- sprintf("%.*f", digits, value)
      e_base$penwidth <- pen
      e_base$color    <- col
      e_base$fontsize <- edge_fontsize
      e_base$fontname <- fontname
      if (p$op == "~~") {
        e_base$constraint <- FALSE
        e_base$dir        <- "both"
      }
      edges[[length(edges) + 1]] <- e_base
    }
  }

  node_defs <- paste(vapply(names(nodes), function(n) {
    attrs <- paste(names(nodes[[n]]), vapply(nodes[[n]], function(x)
      if (is.numeric(x)) as.character(x) else sprintf("\"%s\"", x), character(1)),
      sep = "=", collapse = ", ")
    sprintf("  \"%s\" [%s];", n, attrs)
  }, character(1)), collapse = "\n")

  edge_defs <- paste(vapply(edges, function(e) {
    attrs <- paste(names(e)[-1:-2], vapply(e[-1:-2], function(x)
      if (is.numeric(x)) as.character(x) else sprintf("\"%s\"", x), character(1)),
      sep = "=", collapse = ", ")
    sprintf("  \"%s\" -> \"%s\" [%s];", e$from, e$to, attrs)
  }, character(1)), collapse = "\n")

  radial_opts <- if (engine == "circo") {
    c("splines=true", "nodesep=0.4", "sep=\"+4\"", "mindist=1")
  } else if (engine == "twopi" && twopi_compact) {
    c("splines=true", "nodesep=0.2", "sep=\"+2\"", "ranksep=0.5", "normalize=true")
  } else character(0)
  radial_opts <- paste(radial_opts, collapse = ", ")

  graph_code <- sprintf(
    "digraph {\n  rankdir=%s;\n  graph [layout=%s%s%s, overlap=false,\n         labelloc=\"t\", labeljust=\"c\", label=%s, ratio=%s];\n  node  [fontname=\"%s\", margin=0.05];\n  edge  [fontname=\"%s\", fontcolor=\"#333333\"];\n\n%s\n\n%s\n}",
    layout, engine, if (nchar(radial_opts)) ", " else "", radial_opts,
    top_label, ratio, fontname, fontname, node_defs, edge_defs)

  tryCatch({
    DiagrammeR::grViz(graph_code, engine = engine)
  }, error = function(e) {
    err_dot <- sprintf("digraph {\n  node [shape=box, color=red, fontname=\"%s\"];\n  \"Error\" [label=\"Path diagram rendering failed:\\n%s\"];\n}", 
                       fontname, gsub("\"", "'", e$message))
    tryCatch({
      DiagrammeR::grViz(err_dot)
    }, error = function(e2) {
      NULL
    })
  })
}



# ------------------------------------------------------------------

`%||%` <- function(x, y) if (!is.null(x)) x else y
tryCatch(Sys.setlocale("LC_CTYPE", "ja_JP.UTF-8"), error = function(e) NULL)

# ---- Helper: Approximate Equations -----------------------------
#   * Indicator  =  intercept + loading * Latent
#   * Dependent  =  intercept + Σ( slope * Predictor )
#   * All coefficients are generated in raw (non-standardized) form
# ----------------------------------------------------------------
lavaan_to_equations <- function(fit, digits = 3) {

  # ---- Extract coefficients (non-standardized) ------------------------------
  pe <- parameterEstimates(fit, standardized = FALSE, remove.def = TRUE)

  # ---- Number formatter --------------------------------------
  format_est <- function(x, digits = 3) {
    sapply(x, function(v) {
      if (is.na(v)) return("NA")
      if (abs(v) < 10^(-digits))
        format(v, digits = digits, scientific = TRUE)
      else
        format(round(v, digits), nsmall = digits)
    })
  }

  # ---- Split dataframes ------------------------------------
  meas_df      <- pe[pe$op == "=~",  ]   # Measurement equations
  reg_df       <- pe[pe$op == "~",   ]   # Structural equations
  intercept_df <- pe[pe$op == "~1",  ]   # Intercepts

  eq_lines <- character(0)

  # ---------- 1. Measurement equations ------------
  if (nrow(meas_df)) {
    for (i in seq_len(nrow(meas_df))) {
      ind     <- meas_df$rhs[i]                # Indicator
      lat     <- meas_df$lhs[i]                # Latent
      loading <- format_est(meas_df$est[i], digits)
      int_val <- intercept_df$est[intercept_df$lhs == ind]
      rhs     <- c(if (length(int_val))
        format_est(int_val, digits) else NULL,
        paste0(loading, "*", lat))
      eq_lines <- c(eq_lines,
                    paste(ind, "=", paste(rhs, collapse = " + ")))
    }
  }

  # ---------- 2. Structural equations ------------
  if (nrow(reg_df)) {
    reg_split <- split(reg_df, reg_df$lhs)
    for (lhs in names(reg_split)) {
      df  <- reg_split[[lhs]]
      int <- intercept_df$est[intercept_df$lhs == lhs]
      rhs <- paste0(format_est(df$est, digits), "*", df$rhs)
      rhs <- c(if (length(int))
        format_est(int, digits) else NULL,
        rhs)
      eq_lines <- c(eq_lines,
                    paste(lhs, "=", paste(rhs, collapse = " + ")))
    }
  }

  # ---------- Output ---------------------
  eq_lines
}



# ================================================================
# UI
# ================================================================

ui <- fluidPage(
  useShinyjs(),
  tags$head(
    tags$link(rel = "icon", type = "image/png", href = "logo.png"),
    tags$style(HTML("
#app-logo { position: absolute; top: 8px; right: 16px; }
.modal-header { background: #f8f9fa; }
.modal-title  { font-weight: bold; }
.htDimmed { background-color: #d9d9d9 !important; color: #777 !important; }
.shiny-modal .modal-content { border-radius: 8px; box-shadow: 0 4px 12px rgba(0,0,0,0.15); }
.shiny-modal .modal-body    { padding: 20px !important; }
.shiny-modal .modal-footer  { padding: 10px !important; }
.alert-box { background:#fff3cd;border:1px solid #ffeeba;border-radius:6px;padding:10px;margin-bottom:10px; }
#lavaan_model { white-space: pre; }
#approx_eq    { white-space: pre-wrap; }
")),
    tags$script(HTML("
      $(document).on('shiny:visualchange', function(event) {
        setTimeout(function() {
          window.dispatchEvent(new Event('resize'));
        }, 150);
      });
      $(document).on('shown.bs.tab', 'a[data-toggle=\"tab\"]', function(e) {
        setTimeout(function() {
          window.dispatchEvent(new Event('resize'));
        }, 150);
      });

      // Browser-side CSV file reader with encoding auto-detection (UTF-8 -> Shift-JIS -> GB18030 -> Big5 -> EUC-KR -> Fallback)
      $(document).on('change', '#datafile', function(e) {
        const file = e.target.files[0];
        if (!file) return;
        const reader = new FileReader();
        reader.onload = function(evt) {
          const arrayBuffer = evt.target.result;
          const encodings = ['utf-8', 'shift-jis', 'gb18030', 'big5', 'euc-kr'];
          let decodedText = '';
          let success = false;

          for (const enc of encodings) {
            try {
              const decoder = new TextDecoder(enc, { fatal: true });
              decodedText = decoder.decode(arrayBuffer);
              success = true;
              break;
            } catch (err) {
              // Try next encoding on failure
            }
          }

          if (!success) {
            // Last resort fallback to Latin1 (windows-1252)
            const latin1Decoder = new TextDecoder('windows-1252');
            decodedText = latin1Decoder.decode(arrayBuffer);
          }

          // Send decoded UTF-8 string directly to Shiny (WebR)
          Shiny.setInputValue('datafile_utf8', {
            name: file.name,
            content: decodedText
          }, { priority: 'event' });
        };
        reader.readAsArrayBuffer(file);
      });
    "))
  ),
  div(id = "app-logo",
      img(src = "logo.png", height = 40,
          title = "Structural Insights, Simplified")),
  title = "Structura2",

  tabsetPanel(

    # ---------------- Data tab -----------------------------------
    tabPanel("Data", h4("Uploaded Data"), DTOutput("datatable")),

    # -------------- Filtered tab ---------------------------------
    tabPanel("Filtered",
             # ---- Analysis Settings (moved from Model tab) -----------
             h4("Analysis Settings"),
             radioButtons("analysis_mode", "Analysis mode:",
                          choices  = c("Raw (unstandardized)"  = "raw",
                                       "Standardized (scaled)" = "std"),
                          selected = "std", inline = TRUE),
             selectInput("missing_method", "Missing Data Handling:",
                         choices = c(
                           "Listwise deletion"           = "listwise",
                           "FIML (ML)"                   = "ml",
                           "FIML including exogenous x"  = "ml.x",
                           "Two-stage ML"                = "two.stage",
                           "Robust two-stage ML"         = "robust.two.stage"
                         ), selected = "listwise"),
             tags$hr(),
             
             # ---- Data Transformation & Selection --------------------
             h4("Data Transformation & Selection"),
             uiOutput("log_transform_ui"),
             uiOutput("display_column_ui"),
             DTOutput("filtered_table"),
             tags$hr(),
             h4("Correlation Heatmap"),
             plotOutput("corr_heatmap", height = "300px")),

    # ---------------- Model tab ----------------------------------
    tabPanel("Model",
             fluidRow(
               # ---------- Left column (inputs) -------------------
               column(width = 7,
                      conditionalPanel(
                        condition = "input.analysis_mode == 'raw'",
                        checkboxInput("diagram_std",
                                      "Show standardized coefficients in diagram",
                                      value = TRUE)),
                      # -------------- Run button -------------------
                      actionButton("run_model", "Run / Update Model",
                                   class = "btn btn-success"),
                      tags$hr(),
                      h4("Measurement Model"),
                      div(style = "margin-top: 10px; margin-bottom: 15px;",
                          rHandsontableOutput("input_table"),
                          actionButton("add_row", "Add Row", class = "btn btn-primary", style = "margin-top: 10px;")
                      ),
                      tags$hr(),
                      h4("Structural Model"),
                      p("Color intensity indicates R² strength (white: low, red: high). ",
                        "Use as exploratory reference alongside theoretical knowledge.",
                        style = "font-size: 12px; color: #666; margin-bottom: 10px;"),
                      rHandsontableOutput("checkbox_matrix"),
                      tags$hr(),
                      h4("Manual Equations"),
                      div(style = "margin-top: 10px;",
                          textAreaInput("extra_eq",
                                        "Additional lavaan syntax (one formula per line):",
                                        value = "",
                                        placeholder = "y1 ~ x1 + x2\nlatent2 =~ y3 + y4",
                                        rows = 4,
                                        resize = "vertical")
                      ),
                      tags$hr(),
                      h4("lavaan Syntax"),
                      verbatimTextOutput("lavaan_model")
               ),

               # ---------- Right column (outputs) -----------------
               column(width = 5,
                      # ---------- Tabset for outputs ---------------
                      tabsetPanel(id = "right_tabs", type = "tabs",

                                  # ----- Diagnostics tab ---------------------
                                  tabPanel("Diagnostics",
                                           shinyjs::hidden(
                                             div(id = "fit_alert_box",
                                                 textOutput("fit_alert"),
                                                 class = "alert-box")
                                           ),
                                           h4("Fit Indices"),
                                           DTOutput("fit_indices")),

                                  # ----- Equations tab -----------------------
                                  tabPanel("Equations",
                                           h4("Approximate Equations"),
                                           verbatimTextOutput("approx_eq")),

                                  # ----- Diagram settings tab ---------------
                                  tabPanel("Diagram Settings",
                                           h4("Path Diagram Options"),
                                           selectInput("layout_style", "Layout & Engine:",
                                                       choices = c(
                                                         "Hierarchical Left → Right (dot)" = "dot_LR",
                                                         "Hierarchical Top → Bottom (dot)" = "dot_TB",
                                                         "Spring model layout (neato)"      = "neato",
                                                         "Force-Directed Placement (fdp)"   = "fdp",
                                                         "Circular layout (circo)"          = "circo",
                                                         "Radial layout (twopi)"            = "twopi"
                                                       ),
                                                       selected = "dot_LR"))
                      ),
                      # ---------- Path diagram --------------------
                      h4("Path Diagram"),
                      div(style = "height:60vh; overflow-y:auto; overflow-x:hidden; border:1px solid #ccc;",
                          uiOutput("sem_plot_ui"))
               )
             )),

    # ---------------- Details tab --------------------------------
    tabPanel("Details",
             h4("Parameter Estimates"),
             DTOutput("param_tbl"),
             tags$hr(),
             h4("Model Summary"),
             verbatimTextOutput("fit_summary")),

    # ---------------- Help tab -----------------------------------
    tabPanel("Help", includeMarkdown("help.md"))
  )
)

# ================================================================
# SERVER
# ================================================================

server <- function(input, output, session) {

  showModal(
    modalDialog(
      title = span(icon("upload"), "Load Data"),
      fileInput("datafile", NULL,
                buttonLabel = "Browse…",
                placeholder  = "Upload CSV",
                accept       = c(".csv", "text/csv", "application/csv")),
      tags$hr(),
      radioButtons("sample_ds", "Or choose a demo dataset:",
                   choices = c("None", "HolzingerSwineford1939",
                               "PoliticalDemocracy", "Demo.growth",
                               "Demo.twolevel", "FacialBurns")),
      easyClose = FALSE,
      footer    = NULL
    )
  )

  data <- reactiveVal(NULL)

  observeEvent(input$datafile_utf8, {
    req(input$datafile_utf8)
    tryCatch({
      df <- utils::read.csv(
        text = input$datafile_utf8$content,
        fileEncoding = "UTF-8",
        stringsAsFactors = FALSE,
        check.names = FALSE
      )
      if (is.null(df) || nrow(df) == 0) {
        stop("The loaded dataset has no data rows.")
      }
      names(df) <- make.names(names(df), unique = TRUE)
      data(df)
      updateRadioButtons(session, "sample_ds", selected = "None")
      removeModal()
    }, error = function(e) {
      showModal(modalDialog(
        title = "Data Load Error",
        div(class = "alert alert-danger", e$message),
        easyClose = TRUE,
        footer = modalButton("Dismiss")
      ))
    })
  })

  observeEvent(input$sample_ds, {
    req(input$sample_ds != "None")
    ds <- switch(input$sample_ds,
                 "HolzingerSwineford1939" = HolzingerSwineford1939,
                 "PoliticalDemocracy"    = PoliticalDemocracy,
                 "Demo.growth"           = Demo.growth,
                 "Demo.twolevel"         = Demo.twolevel,
                 "FacialBurns"           = FacialBurns)
    data(ds)
    removeModal()
  })

  output$datatable <- renderDT({
    req(data())
    datatable(data(), filter = "top", editable = FALSE,
              options = list(pageLength = 30, autoWidth = TRUE),
              rownames = FALSE)
  }, server = FALSE)

  # ---------- Filtering & preprocessing --------------------------

  output$log_transform_ui <- renderUI({
    req(data())
    nums  <- names(data())[sapply(data(), is.numeric)]
    valid <- nums[sapply(data()[nums], function(x) min(x, na.rm = TRUE) > 0)]
    if (!length(valid)) return()
    checkboxGroupInput("log_columns", "Log-transform columns (log10):",
                       choices = valid, inline = TRUE)
  })

  processed_data <- reactive({
    req(data())
    tryCatch({
      idx <- input$datatable_rows_all
      if (is.null(idx)) {
        df <- data()
      } else {
        idx_num <- as.numeric(unlist(idx))
        df <- data()[idx_num, , drop = FALSE]
      }
      df[] <- lapply(df, function(x) if (is.factor(x)) as.character(x) else x)

      # --- log10 transform -----------------------------------------
      if (!is.null(input$log_columns)) {
        col_order <- names(df)
        for (col in input$log_columns) {
          log_col      <- paste0("log_", col)
          df[[log_col]] <- log10(df[[col]])
          pos           <- match(col, col_order)
          col_order[pos] <- log_col
          df[[col]]      <- NULL
        }
        df <- df[, col_order, drop = FALSE]
      }

      # --- one-hot encode ------------------------------------------
      chars <- names(df)[vapply(df, is.character, logical(1))]
      multi <- chars[vapply(df[chars], function(x) {
        u <- unique(x); length(u) > 1 && length(u) < nrow(df)
      }, logical(1))]
      if (length(multi)) {
        mm <- model.matrix(~ . - 1, data = df[multi], na.action = na.pass)
        df <- cbind(df[setdiff(names(df), multi)],
                    as.data.frame(mm, check.names = TRUE))
      }
      names(df) <- make.names(names(df), unique = TRUE)

      # --- standardize if requested --------------------------------
      if (input$analysis_mode == "std") {
        num_cols <- names(df)[vapply(df, is.numeric, logical(1))]
        for (col in num_cols) {
          col_sd <- sd(df[[col]], na.rm = TRUE)
          if (is.na(col_sd) || col_sd < 1e-12) {
            col_mean <- mean(df[[col]], na.rm = TRUE)
            df[[col]] <- df[[col]] - col_mean
          } else {
            df[[col]] <- scale(df[[col]])[, 1]
          }
        }
      }
      df
    }, error = function(e) {
      warning(paste("Data preprocessing failed:", e$message))
      data.frame()
    })
  })

  output$display_column_ui <- renderUI({
    df <- processed_data(); req(df)
    
    # Identify columns with zero variance (constant columns)
    numeric_cols <- sapply(df, is.numeric)
    zero_var_cols <- names(df)[numeric_cols][sapply(df[numeric_cols], function(x) {
      var_val <- var(x, na.rm = TRUE)
      is.na(var_val) || var_val == 0
    })]
    
    # Exclude zero variance columns from available choices
    available_cols <- setdiff(names(df), zero_var_cols)
    
    # Set default selection from available columns only
    numeric_orig <- names(data())[sapply(data(), is.numeric)]
    logs <- if (!is.null(input$log_columns)) paste0("log_", input$log_columns) else NULL
    default <- intersect(c(numeric_orig, logs), available_cols)
    
    div(
      checkboxGroupInput("display_columns", "Display columns:",
                         choices = available_cols, selected = default, inline = TRUE),
      if (length(zero_var_cols) > 0) {
        div(style = "color: #666; font-size: 11px; margin-top: 5px;",
            paste("Note: Excluded", length(zero_var_cols), "constant variable(s):",
                  paste(zero_var_cols, collapse = ", ")))
      }
    )
  })

  output$filtered_table <- renderDT({
    df <- processed_data(); req(df)
    if (!is.null(input$display_columns))
      df <- df[, intersect(input$display_columns, names(df)), drop = FALSE]
    
    # Round numeric columns to 3 decimal places for better display
    numeric_cols <- sapply(df, is.numeric)
    df[numeric_cols] <- lapply(df[numeric_cols], function(x) round(x, 3))
    
    datatable(df, filter = "top", editable = FALSE,
              options = list(pageLength = 30, autoWidth = TRUE, scrollX = TRUE),
              rownames = FALSE)
  }, server = FALSE)

  output$corr_heatmap <- renderPlot({
    req(!is.null(input$display_columns))
    tryCatch({
      df <- processed_data()
      all_cols <- intersect(input$display_columns, names(df))
      num_cols <- all_cols[sapply(df[, all_cols, drop = FALSE], is.numeric)]
      if (length(num_cols) < 2) return(NULL)
      cm <- cor(df[, num_cols, drop = FALSE], use = "pairwise.complete.obs")
      cm[is.nan(cm)] <- NA
      cm_rounded <- round(cm, 3)
      mf <- as.data.frame(as.table(cm_rounded))
      names(mf) <- c("Var1", "Var2", "value")
      ggplot(mf, aes(x = Var2, y = Var1, fill = value)) +
        geom_tile() +
        geom_text(aes(label = ifelse(is.na(value), "NA", sprintf('%.3f', value)))) +
        scale_fill_gradient2(midpoint = 0) +
        theme_minimal() +
        labs(x = NULL, y = NULL, fill = "Correlation")
    }, error = function(e) {
      ggplot() + 
        annotate("text", x = 0.5, y = 0.5, label = paste("Correlation Heatmap Error:\n", e$message), color = "red", size = 4) + 
        theme_void()
    })
  })

  # ---------- Measurement table ----------------------------------

  input_table_data <- reactiveVal(NULL)
  input_table_trigger <- reactiveVal(0)

  observeEvent(data(), {
    req(data())
    inds <- names(data())[sapply(data(), is.numeric)]
    init <- data.frame(Latent    = "LatentVariable1",
                       Indicator = "",
                       Operator  = "=~",
                       matrix(FALSE, nrow = 1, ncol = length(inds)),
                       stringsAsFactors = FALSE)
    colnames(init) <- c("Latent", "Indicator", "Operator", inds)
    input_table_data(init)
    input_table_trigger(input_table_trigger() + 1)
  })

  observeEvent(input$display_columns, ignoreNULL = TRUE, {
    inds <- input$display_columns
    df <- input_table_data()
    if (!is.null(df)) {
      meta <- df[, c("Latent", "Indicator", "Operator"), drop = FALSE]
      new_checkboxes <- as.data.frame(matrix(FALSE, nrow = nrow(df), ncol = length(inds)))
      colnames(new_checkboxes) <- inds
      common_cols <- intersect(colnames(df), inds)
      if (length(common_cols) > 0) {
        new_checkboxes[, common_cols] <- df[, common_cols]
      }
      input_table_data(cbind(meta, new_checkboxes))
      input_table_trigger(input_table_trigger() + 1)
    }
  })

  output$input_table <- renderRHandsontable({
    input_table_trigger()
    df <- isolate(input_table_data()); req(df)
    rh <- rhandsontable(df, rowHeaders = FALSE) %>%
      hot_table(highlightReadOnly = TRUE)
    rh <- hot_col(rh, "Latent")
    rh <- hot_col(rh, "Indicator", readOnly = TRUE)
    rh <- hot_col(rh, "Operator",  readOnly = TRUE)
    for (nm in setdiff(colnames(df), c("Latent", "Indicator", "Operator")))
      rh <- hot_col(rh, nm, type = "checkbox")
    rh
  })

  observeEvent(input$input_table, {
    tbl <- hot_to_r(input$input_table); req(tbl)
    tbl$Latent    <- make.names(tbl$Latent, unique = FALSE)
    convs         <- make.unique(c(names(processed_data()), tbl$Latent))
    tbl$Indicator <- tail(convs, nrow(tbl))
    input_table_data(tbl)
  })

  observeEvent(input$add_row, {
    df <- input_table_data(); req(df)
    new_row            <- df[1, ]
    new_row[,]         <- FALSE
    new_row$Latent     <- ""
    new_row$Operator   <- "=~"
    input_table_data(rbind(df, new_row))
    input_table_trigger(input_table_trigger() + 1)
  })

  # ---------- Helper function for R² calculation -----------------
  
  compute_r2_matrix <- function(data, dep_vars, pred_vars) {
    r2_matrix <- matrix(0, nrow = length(dep_vars), ncol = length(pred_vars),
                        dimnames = list(dep_vars, pred_vars))
    
    for (i in seq_along(dep_vars)) {
      dep <- dep_vars[i]
      if (is.numeric(data[[dep]])) {
        for (j in seq_along(pred_vars)) {
          pred <- pred_vars[j]
          if (is.numeric(data[[pred]]) && dep != pred) {
            tryCatch({
              # Use pairwise correlation for better handling of missing data
              correlation <- cor(data[[dep]], data[[pred]], 
                               use = "pairwise.complete.obs")
              # Handle NA or infinite correlation values
              if (is.na(correlation) || !is.finite(correlation)) {
                r2_matrix[i, j] <- 0
              } else {
                r2_val <- correlation^2
                r2_matrix[i, j] <- ifelse(is.na(r2_val) || !is.finite(r2_val), 0, r2_val)
              }
            }, error = function(e) {
              r2_matrix[i, j] <- 0
            })
          }
        }
      }
    }
    r2_matrix
  }

  # ---------- Structural table -----------------------------------

  struct_table_data <- reactiveVal(NULL)

  model_items <- reactive({
    df <- processed_data(); req(df)
    deps <- as.character(input$display_columns %||% names(df))
    meas <- input_table_data(); req(meas)
    vars <- names(meas)[4:ncol(meas)]
    row_has_indicator <- apply(meas[vars], 1, function(x) any(as.logical(x)))
    convs <- setdiff(na.omit(unique(meas$Indicator[row_has_indicator])), "")
    unique(c(deps, convs))
  })

  observeEvent(model_items(), {
    items <- model_items()
    if (!length(items)) {
      struct_table_data(NULL)
      return()
    }
    
    old_tbl <- struct_table_data()
    
    # Create new matrix
    mat <- data.frame(Dependent = items, Operator = "~", stringsAsFactors = FALSE)
    for (col in items) mat[[col]] <- FALSE
    
    # Preserve old settings if available
    if (!is.null(old_tbl)) {
      common_deps <- intersect(old_tbl$Dependent, items)
      common_cols <- intersect(colnames(old_tbl), items)
      if (length(common_deps) > 0 && length(common_cols) > 0) {
        for (dep in common_deps) {
          old_row_idx <- which(old_tbl$Dependent == dep)
          new_row_idx <- which(mat$Dependent == dep)
          if (length(old_row_idx) == 1 && length(new_row_idx) == 1) {
            mat[new_row_idx, common_cols] <- old_tbl[old_row_idx, common_cols]
          }
        }
      }
    }
    struct_table_data(mat)
  })

  observeEvent(input$checkbox_matrix, {
    tbl <- hot_to_r(input$checkbox_matrix); req(tbl)
    struct_table_data(tbl)
  })

  output$checkbox_matrix <- renderRHandsontable({
    tryCatch({
      df <- processed_data(); req(df)
      meas <- input_table_data(); req(meas)
      mat <- struct_table_data(); req(mat)
      items <- mat$Dependent
      if (!length(items)) return()
      
      # Compute R² matrix for color coding
      r2_matrix <- compute_r2_matrix(df, items, items)
      
      rh <- rhandsontable(mat, rowHeaders = FALSE) %>%
        hot_table(highlightReadOnly = TRUE, fixedColumnsLeft = 2)
      rh <- hot_col(rh, "Dependent", readOnly = TRUE)
      rh <- hot_col(rh, "Operator",  readOnly = TRUE)
    
      # Set diagonal cells as readOnly before applying renderers
      for (i in seq_along(items)) {
        diag_col_index <- match(items[i], colnames(mat))
        if (!is.na(diag_col_index)) {
          rh <- hot_cell(rh, row = i, col = diag_col_index, readOnly = TRUE)
        }
      }
      
      # Apply color coding using custom renderer for each checkbox column
      for (col_name in items) {
        r2_colors <- sapply(seq_along(items), function(row_idx) {
          dep_var <- items[row_idx]
          if (dep_var == col_name) {
            return("#FFFFFF")
          }
          
          r2_val <- tryCatch({
            if (dep_var %in% rownames(r2_matrix) && col_name %in% colnames(r2_matrix)) {
              val <- r2_matrix[dep_var, col_name]
              if (is.na(val) || !is.finite(val)) 0 else val
            } else {
              0
            }
          }, error = function(e) 0)
          
          r2_val <- ifelse(is.na(r2_val) || !is.finite(r2_val), 0, r2_val)
          red_intensity <- min(1, max(0, r2_val))
          rgb(1, 1 - red_intensity * 0.7, 1 - red_intensity * 0.7)
        })
        
        colors_js <- paste0("['", paste(r2_colors, collapse = "','"), "']")
        diag_row <- match(col_name, items) - 1
        
        renderer_js <- paste0("
          function(instance, td, row, col, prop, value, cellProperties) {
            Handsontable.renderers.CheckboxRenderer.apply(this, arguments);
            var colors = ", colors_js, ";
            if (colors[row]) {
              td.style.backgroundColor = colors[row];
            }
            if (row === ", diag_row, ") {
              cellProperties.readOnly = true;
              td.style.backgroundColor = '#f0f0f0';
              td.style.cursor = 'not-allowed';
              td.classList.add('htDimmed');
            }
          }")
        
        rh <- hot_col(rh, col_name, type = "checkbox", renderer = renderer_js)
      }
      rh
    }, error = function(e) {
      error_mat <- data.frame(
        Dependent = "Error",
        Operator = "~",
        Error = paste("Failed to load:", e$message),
        stringsAsFactors = FALSE
      )
      rhandsontable(error_mat, rowHeaders = FALSE) %>%
        hot_table(highlightReadOnly = TRUE)
    })
  })

  # ---------- lavaan syntax --------------------------------------

  lavaan_model_str <- reactive({
    req(input$input_table, struct_table_data())
    meas <- hot_to_r(input$input_table)
    mlines <- lapply(seq_len(nrow(meas)), function(i) {
      lt   <- meas$Latent[i]; if (!nzchar(lt)) return(NULL)
      vars <- names(meas)[4:ncol(meas)]
      inds <- vars[as.logical(meas[i, vars])];
      if (!length(inds)) return(NULL)
      paste0(lt, " =~ ", paste(inds, collapse = " + "))
    })
    struc <- struct_table_data(); req(struc)
    slines <- lapply(seq_len(nrow(struc)), function(i) {
      dp    <- struc$Dependent[i]; if (!nzchar(dp)) return(NULL)
      preds <- names(struc)[3:ncol(struc)]
      ps    <- preds[as.logical(struc[i, preds])]
      if (!length(ps)) return(NULL)
      paste0(dp, " ~ ", paste(ps, collapse = " + "))
    })
    # ----- add manual equations (new) -----------------------------
    extra <- strsplit(input$extra_eq, "\\n")[[1]]
    extra <- trimws(extra)
    extra <- extra[nzchar(extra)]
    unlist(c(mlines, slines, extra))
  })

  output$lavaan_model <- renderText({
    ln <- lavaan_model_str()
    paste(if (length(ln) == 0)
      "Define a model to proceed."
      else
        ln, collapse = "\n")
  })

  # ---------- Fit model safely (eventReactive) -------------------

  fit_model_safe <- eventReactive(input$run_model, {
    ln <- isolate(lavaan_model_str())
    if (length(ln) == 0) {
      msg <- if (input$run_model > 0) "Define a model to proceed." else ""
      return(list(ok = FALSE,
                  msg_friendly = msg,
                  fit = NULL))
    }
    tryCatch({
      # Use meanstructure = TRUE if FIML is selected to prevent lavaan error
      needs_meanstructure <- (input$analysis_mode == "raw" || 
                              input$missing_method %in% c("ml", "ml.x", "two.stage", "robust.two.stage"))

      fm <- sem(paste(ln, collapse = "\n"),
                data          = processed_data(),
                missing       = input$missing_method,
                fixed.x       = FALSE,
                parser        = "old",
                meanstructure = needs_meanstructure,
                ncpus         = 1L)
      list(ok = lavInspect(fm, "converged"),
           msg_friendly = if (lavInspect(fm, "converged"))
             "" else
               "Model did not converge. Check for variables with correlation = 1 and remove or combine them.",
           fit = fm)
    }, error = function(e) {
      # Enhanced error message with specific diagnosis
      error_msg <- conditionMessage(e)
      
      # Check for common lavaan errors and provide specific guidance
      if (grepl("sample covariance matrix is not positive-definite|not positive definite", error_msg, ignore.case = TRUE)) {
        friendly_msg <- "Model estimation failed: Variables are too highly correlated (near perfect correlation). This creates numerical instability in the covariance matrix. Try: (1) Remove one variable from highly correlated pairs, (2) Use more data samples, or (3) Select different variables with lower correlations."
      } else if (grepl("convergence|converged", error_msg, ignore.case = TRUE)) {
        friendly_msg <- "Model did not converge: The estimation algorithm could not find a stable solution. Try: (1) Check for perfect correlations between variables, (2) Simplify the model structure, or (3) Use different starting values."
      } else if (grepl("identification|identified", error_msg, ignore.case = TRUE)) {
        friendly_msg <- "Model identification problem: The model is under-identified (too few constraints). Try: (1) Add more observed variables, (2) Reduce the number of parameters, or (3) Add equality constraints."
      } else if (grepl("degrees of freedom", error_msg, ignore.case = TRUE)) {
        friendly_msg <- "Insufficient degrees of freedom: The model has too many parameters for the available data. Try: (1) Reduce model complexity, (2) Add more variables, or (3) Use a simpler model structure."
      } else {
        friendly_msg <- paste0("Estimation failed: ", error_msg, ". Try: (1) Check for perfect correlations between variables, (2) Ensure sufficient sample size, or (3) Simplify the model structure.")
      }
      
      list(ok = FALSE,
           msg_friendly = paste0(friendly_msg, "\n\nTechnical details: ", error_msg),
           fit = NULL)
    })
  }, ignoreNULL = FALSE)  # Initial auto-execution

  output$fit_alert <- renderText({
    msg <- fit_model_safe()$msg_friendly
    if (nzchar(msg)) {
      shinyjs::show("fit_alert_box")
      msg
    } else {
      shinyjs::hide("fit_alert_box")
      ""
    }
  })

  output$fit_indices <- renderDT({
    model <- fit_model_safe()
    validate(need(model$ok, model$msg_friendly))
    fit <- model$fit
    ms  <- fitMeasures(fit, c("pvalue","srmr","rmsea","aic","bic",
                              "gfi","agfi","nfi","cfi"))
    vals <- round(as.numeric(ms), 3)
    names(vals) <- names(ms)
    thr <- c(pvalue = .05, srmr = .08, rmsea = .06,
             gfi = .90, agfi = .90, nfi = .90, cfi = .90)
    fmt <- function(idx, v) {
      ok <- switch(idx,
                   pvalue = v >= thr["pvalue"],
                   srmr   = v <= thr["srmr"],
                   rmsea  = v <= thr["rmsea"],
                   gfi    = v >= thr["gfi"],
                   agfi   = v >= thr["agfi"],
                   nfi    = v >= thr["nfi"],
                   cfi    = v >= thr["cfi"], TRUE)
      if (is.na(v)) "NA"
      else if (!ok) sprintf('<span style="color:red;">%.3f</span>', v)
      else sprintf('%.3f', v)
    }
    html_vals <- mapply(fmt, names(vals), vals, USE.NAMES = FALSE)
    tbl <- as.data.frame(t(html_vals), stringsAsFactors = FALSE)
    colnames(tbl) <- toupper(names(vals))
    datatable(tbl, escape = FALSE, rownames = FALSE,
              options = list(dom = 't'))
  })

  # ----------------- Approximate Equations ----------------------
  output$approx_eq <- renderText({
    if (input$analysis_mode == "std")
      return("— Hidden in Standardized mode —")
    model <- fit_model_safe()
    validate(need(model$ok, model$msg_friendly))
    paste(lavaan_to_equations(model$fit), collapse = "\n")
  })

  output$fit_summary <- renderPrint({
    model <- fit_model_safe()
    validate(need(model$ok, model$msg_friendly))
    summary(model$fit, fit.measures = TRUE)
  })

  output$param_tbl <- renderDT({
    model <- fit_model_safe()
    validate(need(model$ok, model$msg_friendly))
    datatable(parameterEstimates(model$fit), options = list(pageLength = 15))
  })

  # ----------------- Path diagram UI ----------------------------
  output$sem_plot_ui <- renderUI({
    ln <- lavaan_model_str()
    if (length(ln) == 0)
      return(div("Define a model to view the path diagram."))
    tryCatch(grVizOutput("sem_plot"),
             error = function(e)
               div(style = "color:red;",
                   paste("Model Error:", e$message)))
  })

  output$sem_plot <- renderGrViz({
    model <- fit_model_safe()
    validate(need(model$ok, model$msg_friendly))
    std_for_plot <- if (input$analysis_mode == "std") TRUE else input$diagram_std

    # ---- parse layout_style into engine / rankdir ---------------
    parts  <- strsplit(input$layout_style, "_", fixed = TRUE)[[1]]
    eng    <- parts[1]
    rank   <- ifelse(length(parts) == 2, parts[2], "LR")
    # -------------------------------------------------------------

    semDiagram(model$fit,
               standardized = std_for_plot,
               layout       = rank,
               engine       = eng)
  })
}

# ---- Run the application ---------------------------------------

shinyApp(ui, server)
