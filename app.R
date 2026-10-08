# -*- coding: utf-8 -*-
# ---------------------------------------------------------------
# Structura2 – Structural Insights, Simplified
# Shiny app for Structural Equation Modeling with mean structures
# ---------------------------------------------------------------

options(
  shiny.fullstacktrace = TRUE,
  shiny.reactlog       = FALSE,
  shiny.sanitize.errors = TRUE
)

# ---- Startup Progress Reporting ----------------------------------
# Sends real startup milestones to the outer ShinyLive page (loading overlay in index.html)
# through a BroadcastChannel. Silent no-op outside WebR or on any failure.
report_startup_stage <- function(stage) {
  tryCatch({
    if (!grepl("emscripten", R.version$platform, fixed = TRUE)) return(invisible(FALSE))
    eval_js <- get("eval_js", envir = asNamespace("webr"))
    eval_js(sprintf(
      "new BroadcastChannel('structura-progress').postMessage({type:'stage',stage:'%s'}); 0",
      stage))
    invisible(TRUE)
  }, error = function(e) invisible(FALSE))
}
report_startup_stage("app_start")

# ---- Deferred regsem Loader --------------------------------------
# In the ShinyLive build, regsem and its dependencies (future, Rsolnp, ...) are not mounted at
# startup (see export_shinylive.R); they are fetched on first use. No-op when regsem is already
# installed (local R) or already loaded. Returns TRUE when regsem is available.
# The package name is built dynamically on purpose: ShinyLive scans app.R for literal package
# names and would otherwise install regsem and its dependencies at startup.
regsem_pkg <- paste0("reg", "sem")
# Availability is checked with system.file(): requireNamespace() would trigger WebR's automatic
# download from repo.r-wasm.org instead of using the copies hosted next to the site.
ensure_regsem_loaded <- function() {
  if (nzchar(system.file(package = regsem_pkg))) return(requireNamespace(regsem_pkg, quietly = TRUE))
  if (!grepl("emscripten", R.version$platform, fixed = TRUE)) return(FALSE)
  tryCatch({
    list_file <- tempfile(fileext = ".txt")
    utils::download.file("packages/deferred.txt", list_file, quiet = TRUE)
    tgz_paths <- readLines(list_file, warn = FALSE)
    tgz_paths <- tgz_paths[nzchar(tgz_paths)]
    lib <- "/shinylive/webr/packages"
    for (tgz_path in tgz_paths) {
      tmp <- tempfile(fileext = ".tgz")
      utils::download.file(tgz_path, tmp, quiet = TRUE, mode = "wb")
      utils::untar(tmp, exdir = lib, tar = "internal", extras = "--no-same-permissions")
      unlink(tmp)
    }
    if (!(lib %in% .libPaths())) .libPaths(c(.libPaths(), lib))
    requireNamespace(regsem_pkg, quietly = TRUE)
  }, error = function(e) FALSE)
}

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

# ---- WebR: Skip the sass Shared Library --------------------------
# shiny requires bslib, which imports sass. Loading sass dyn.load()s its 2.3 MB libsass module,
# which costs about 7 seconds of startup in WebR. sass only uses native code to compile Sass
# (C_compile_data / C_compile_file), and this app uses the default Bootstrap 3 theme only, so
# Sass is never compiled. A copy of the installed sass package without libs/ and with its native
# routines removed from Meta/nsInfo.rds is put first on .libPaths(); R code and exports are
# unchanged. No-op outside WebR or when anything unexpected happens.
skip_sass_native_library <- function() {
  tryCatch({
    if (!grepl("emscripten", R.version$platform, fixed = TRUE)) return(invisible(FALSE))
    if ("sass" %in% loadedNamespaces()) return(invisible(FALSE))
    # Name built dynamically so ShinyLive does not treat sass as an app dependency.
    src <- system.file(package = paste0("sa", "ss"))
    if (!nzchar(src)) return(invisible(FALSE))
    lib <- file.path(tempdir(), "structura_lib")
    dst <- file.path(lib, "sass")
    dir.create(dst, recursive = TRUE, showWarnings = FALSE)
    for (f in setdiff(list.files(src, all.files = TRUE, no.. = TRUE), "libs")) {
      file.copy(file.path(src, f), dst, recursive = TRUE)
    }
    ns_info_file <- file.path(dst, "Meta", "nsInfo.rds")
    ns_info <- readRDS(ns_info_file)
    ns_info$dynlibs <- NULL
    ns_info$nativeRoutines <- list()
    saveRDS(ns_info, ns_info_file)
    .libPaths(c(lib, .libPaths()))
    invisible(TRUE)
  }, error = function(e) invisible(FALSE))
}
skip_sass_native_library()

# ---- Libraries --------------------------------------------------

library(shiny)
library(shinyjs)
library(DT)
library(rhandsontable)
# In the static site the Help tab is pre-rendered to help.html at build time (export_shinylive.R), so
# markdown (and litedown/xfun) are not needed at startup. library(markdown) stays visible to ShinyLive's
# dependency scan so that the packages are still bundled (they are then deferred, not mounted).
if (!file.exists("help.html")) library(markdown)
report_startup_stage("libs_attached")

# Parser bypass block to guarantee dependency packaging during Shinylive build.
# regsem is intentionally NOT listed here: ShinyLive would mount it at startup. It is bundled
# via a build-only hint file instead (see export_shinylive.R) and loaded on demand.
if (FALSE) {
  library(lavaan)
}

# Patch the lavaan option cache to prevent NA bounds crashes during estimation checks.
# This forces the lavaan namespace to load, so it is called from the server right after the
# Load Data dialog is shown (see the session-start observer) instead of at app start; this keeps
# lavaan out of the critical path to the first paint.
patch_lavaan_option_cache <- function() {
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
}

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
    twopi_compact     = TRUE,
    cached_params     = NULL,
    cached_fit_measures = NULL,
    cached_n_obs      = NULL,
    ident_df          = NULL) {

  engine <- match.arg(engine, c("dot","neato","fdp","circo","twopi"))

  alpha_color <- function(col, alpha_val) {
    if (is.na(alpha_val) || alpha_val < 0 || alpha_val > 1) alpha_val <- 1
    rgb_mat <- grDevices::col2rgb(col) / 255
    grDevices::rgb(rgb_mat[1,], rgb_mat[2,], rgb_mat[3,], alpha = alpha_val)
  }

  if (!inherits(fitted_model, "lavaan"))
    stop("`fitted_model` must be a lavaan object.")

  invisible(lapply(c("lavaan"), function(p)
    if (!requireNamespace(p, quietly = TRUE))
      stop(sprintf("Package '%s' is required but not installed.", p))))

  params <- if (!is.null(cached_params)) {
    cached_params
  } else {
    lavaan::parameterEstimates(fitted_model, standardized = TRUE)
  }
  scale_col <- if (standardized) "std.all" else "est"

  fit_measures <- if (!is.null(cached_fit_measures)) {
    cached_fit_measures
  } else {
    lavaan::fitMeasures(
      fitted_model,
      c("pvalue","srmr","rmsea","gfi","agfi","nfi","cfi","aic","bic"))
  }
  n_obs <- if (!is.null(cached_n_obs)) {
    cached_n_obs
  } else if (!is.null(cached_fit_measures) && "nobs" %in% names(cached_fit_measures)) {
    as.integer(cached_fit_measures["nobs"])
  } else {
    lavaan::lavInspect(fitted_model, "nobs")
  }

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

  # Not identified (ident_df given): the p-value, NFI and CFI cannot be computed. They are drawn in red as NA
  # (instead of gray) together with the df, so the violation is visible; other values stay as lavaan reports them.
  unidentified <- !is.null(ident_df)
  colorize_thresh <- function(v, thr, invert = FALSE) {
    if (is.na(v)) (if (unidentified) "red" else "gray50")
    else if (invert) {
      if (v > thr) "red" else "gray20"
    } else {
      if (v <= thr) "red" else "gray20"
    }
  }

  fit_block <- if (show_fit) {
    paste0(
      sprintf("N = %d | ", n_obs),
      if (unidentified) {
        sprintf("<font color='red'>p = %s (df = %s)</font> | ",
                if (is.na(fit_measures["pvalue"])) "NA" else sprintf("%.3f", fit_measures["pvalue"]),
                format(ident_df))
      } else {
        sprintf("<font color='%s'>p = %.3f</font> | ",
                colorize_thresh(fit_measures["pvalue"], 0.05), fit_measures["pvalue"])
      },
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

  # Only structural rows define nodes; defined parameters (:=), constraints and thresholds must not
  # appear as boxes in the diagram.
  node_params <- params[params$op %in% c("=~", "~", "~~", "~1"), , drop = FALSE]
  latents   <- unique(node_params$lhs[node_params$op == "=~"])
  observeds <- setdiff(unique(c(node_params$lhs, node_params$rhs)), c(latents, "1", ""))
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
      if (is.na(value)) value <- 0

      pen <-(abs(value) / max_abs) * (max_width - min_width) + min_width
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
    c("splines=true", "nodesep=0.4", "mindist=1")
  } else if (engine == "twopi" && twopi_compact) {
    c("splines=true", "ranksep=1.0", "normalize=true")
  } else character(0)
  radial_opts_str <- paste(radial_opts, collapse = ", ")

  eff_ratio <- if (engine %in% c("twopi", "circo", "neato", "fdp")) "auto" else ratio

  graph_code <- sprintf(
    "digraph {\n  rankdir=%s;\n  graph [layout=%s%s%s, overlap=false,\n         labelloc=\"t\", labeljust=\"c\", label=%s, ratio=%s];\n  node  [fontname=\"%s\", margin=0.05];\n  edge  [fontname=\"%s\", fontcolor=\"#333333\"];\n\n%s\n\n%s\n}",
    layout, engine, if (nchar(radial_opts_str)) ", " else "", radial_opts_str,
    top_label, eff_ratio, fontname, fontname, node_defs, edge_defs)

  # Return raw DOT graph code. Layout and rendering will be done on the client side via @hpcc-js/wasm
  return(graph_code)
}



# ------------------------------------------------------------------

`%||%` <- function(x, y) if (!is.null(x)) x else y

# ---- Helper: Approximate Equations -----------------------------
#   * Indicator  =  intercept + loading * Latent
#   * Dependent  =  intercept + Σ( slope * Predictor )
#   * All coefficients are generated in raw (non-standardized) form
# ----------------------------------------------------------------
lavaan_to_equations <- function(fit, digits = 3, cached_pe = NULL) {

  # ---- Extract coefficients (non-standardized) ------------------------------
  pe <- if (!is.null(cached_pe)) cached_pe else lavaan::parameterEstimates(fit, standardized = FALSE, remove.def = FALSE)

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

# ---------- Helper: Retained Path String Generator -----------------
build_retained_str <- function(struct_df) {
  if (is.null(struct_df) || ncol(struct_df) < 3) return("None (Empty Model)")
  pred_cols <- names(struct_df)[3:ncol(struct_df)]
  lines <- c()
  for (i in seq_len(nrow(struct_df))) {
    dp <- struct_df$Dependent[i]
    if (!nzchar(dp)) next
    ps <- pred_cols[vapply(struct_df[i, pred_cols], function(x) isTRUE(as.logical(x)), logical(1))]
    if (length(ps) > 0) {
      lines <- c(lines, paste0(dp, " ~ ", paste(ps, collapse = " + ")))
    }
  }
  if (length(lines) > 0) paste(lines, collapse = " ; ") else "None (Empty Model)"
}

# ---------- Helper: Variable Isolation Constraint Validator ----------
# Verifies that specified dependent variables retain at least one incoming path (in-degree >= 1)
# and specified predictor variables retain at least one outgoing path (out-degree >= 1).
# The app always passes every baseline dependent variable as `retain_deps`.
# `required_vars` are variables that must keep at least one path in EITHER direction: lavaan silently drops
# a variable that has lost every path, which changes the likelihood and makes AIC/BIC incomparable.
check_variable_isolation <- function(s_df, retain_deps, retain_preds, pred_cols, required_vars = character(0)) {
  if (is.null(s_df) || nrow(s_df) == 0) return(FALSE)

  if (length(required_vars) > 0) {
    edges <- struct_edges(s_df)
    if (!all(required_vars %in% c(edges$dep, edges$pred))) return(FALSE)
  }

  if (length(retain_deps) > 0) {
    for (dep in retain_deps) {
      dep_r <- which(s_df$Dependent == dep)
      if (length(dep_r) == 0) return(FALSE)
      has_in_path <- any(vapply(pred_cols, function(p) {
        if (!p %in% names(s_df)) return(FALSE)
        isTRUE(as.logical(s_df[dep_r[1], p]))
      }, logical(1)))
      if (!has_in_path) return(FALSE)
    }
  }
  
  if (length(retain_preds) > 0) {
    for (pred in retain_preds) {
      if (!pred %in% names(s_df)) return(FALSE)
      has_out_path <- any(vapply(seq_len(nrow(s_df)), function(r) {
        isTRUE(as.logical(s_df[r, pred]))
      }, logical(1)))
      if (!has_out_path) return(FALSE)
    }
  }
  
  TRUE
}

# ---------- Helper: Structural Table <-> lavaan Syntax / Keys ----------
# Single source of truth for turning the structural checkbox matrix into lavaan lines and
# canonical candidate keys (previously duplicated in several server observers).
struct_pred_cols <- function(struct_df) {
  if (is.null(struct_df) || ncol(struct_df) < 3) return(character(0))
  names(struct_df)[3:ncol(struct_df)]
}

# Measurement-table rows -> "Latent =~ ind1 + ind2" lines. Latent names are normalized with the same
# make.names() rule the Model tab applies, so they match the names used in the structural matrix.
# Returns character(0) when the table has no indicator columns.
build_meas_lines <- function(meas) {
  if (is.null(meas) || ncol(meas) < 4 || nrow(meas) == 0) return(character(0))
  vars <- names(meas)[4:ncol(meas)]
  lines <- lapply(seq_len(nrow(meas)), function(i) {
    lt <- trimws(as.character(meas$Latent[i]))
    if (!nzchar(lt)) return(NULL)
    lt <- make.names(lt)
    inds <- vars[vapply(meas[i, vars], function(x) isTRUE(as.logical(x)), logical(1))]
    if (!length(inds)) return(NULL)
    paste0(lt, " =~ ", paste(inds, collapse = " + "))
  })
  as.character(unlist(lines))
}

build_struct_lines <- function(struct_df) {
  pred_cols <- struct_pred_cols(struct_df)
  if (!length(pred_cols)) return(character(0))
  lines <- lapply(seq_len(nrow(struct_df)), function(i) {
    dp <- struct_df$Dependent[i]
    if (!nzchar(dp)) return(NULL)
    ps <- pred_cols[vapply(struct_df[i, pred_cols], function(x) isTRUE(as.logical(x)), logical(1))]
    if (!length(ps)) return(NULL)
    paste0(dp, " ~ ", paste(ps, collapse = " + "))
  })
  unlist(lines)
}

make_struct_key <- function(struct_df) {
  pred_cols <- struct_pred_cols(struct_df)
  lines <- c()
  for (i in seq_len(nrow(struct_df))) {
    dp <- struct_df$Dependent[i]
    ps <- pred_cols[vapply(struct_df[i, pred_cols], function(x) isTRUE(as.logical(x)), logical(1))]
    if (length(ps)) lines <- c(lines, paste0(dp, "~", paste(sort(ps), collapse = ",")))
  }
  res <- paste(sort(lines), collapse = ";")
  if (!nzchar(res)) "EMPTY_PATH" else res
}

# Variables that take part in at least one active structural path (as dependent or predictor).
active_struct_vars <- function(struct_df) {
  pred_cols <- struct_pred_cols(struct_df)
  if (!length(pred_cols)) return(character(0))
  vars <- character(0)
  for (i in seq_len(nrow(struct_df))) {
    ps <- pred_cols[vapply(struct_df[i, pred_cols], function(x) isTRUE(as.logical(x)), logical(1))]
    if (length(ps)) vars <- c(vars, struct_df$Dependent[i], ps)
  }
  unique(vars)
}

# Dependent variables that have at least one active incoming path.
struct_dependents <- function(struct_df) {
  pred_cols <- struct_pred_cols(struct_df)
  if (!length(pred_cols) || is.null(struct_df) || nrow(struct_df) == 0) return(character(0))
  has_path <- vapply(seq_len(nrow(struct_df)), function(i) {
    any(vapply(struct_df[i, pred_cols], function(x) isTRUE(as.logical(x)), logical(1)))
  }, logical(1))
  unique(struct_df$Dependent[has_path])
}

# Active structural paths as a data.frame(dep, pred).
struct_edges <- function(struct_df) {
  pred_cols <- struct_pred_cols(struct_df)
  out <- list()
  if (length(pred_cols)) {
    for (i in seq_len(nrow(struct_df))) {
      ps <- pred_cols[vapply(struct_df[i, pred_cols], function(x) isTRUE(as.logical(x)), logical(1))]
      if (length(ps) && nzchar(struct_df$Dependent[i])) out[[length(out) + 1]] <- data.frame(dep = struct_df$Dependent[i], pred = ps, stringsAsFactors = FALSE)
    }
  }
  if (length(out)) do.call(rbind, out) else data.frame(dep = character(0), pred = character(0), stringsAsFactors = FALSE)
}

# ---- Model Spec & Export Helpers ---------------------------------------------
# A "model spec" describes a model by names only (never data values): the measurement rows, the active
# structural paths, the manual equations and the analysis settings. It is what gets saved in the browser,
# exported/imported as JSON and bundled in the results ZIP. Specs cross the R/JS boundary as JSON strings
# so that length-1 vectors are never unboxed by accident.
SPEC_FORMAT  <- "structura2-model"
SPEC_VERSION <- 1L

as_chr <- function(x) as.character(unlist(x, use.names = FALSE))

# Build a spec from the measurement table, the structural matrix and the analysis settings.
# Vectors are wrapped in lists so that they serialize as JSON arrays even when they have length 1.
build_model_spec <- function(meas, struct_df, extra_eq, settings, data_info, name = "") {
  measurement <- list()
  if (!is.null(meas) && ncol(meas) >= 4 && nrow(meas) > 0) {
    vars <- names(meas)[4:ncol(meas)]
    for (i in seq_len(nrow(meas))) {
      lt   <- trimws(as.character(meas$Latent[i]))
      inds <- vars[vapply(meas[i, vars], function(x) isTRUE(as.logical(x)), logical(1))]
      if (!nzchar(lt) && !length(inds)) next
      measurement[[length(measurement) + 1]] <- list(latent = lt, indicators = as.list(inds))
    }
  }
  structural <- list()
  pred_cols <- struct_pred_cols(struct_df)
  if (length(pred_cols) && nrow(struct_df) > 0) {
    for (i in seq_len(nrow(struct_df))) {
      ps <- pred_cols[vapply(struct_df[i, pred_cols], function(x) isTRUE(as.logical(x)), logical(1))]
      if (length(ps) && nzchar(struct_df$Dependent[i])) {
        structural[[length(structural) + 1]] <- list(dependent = struct_df$Dependent[i], predictors = as.list(ps))
      }
    }
  }
  list(
    format           = SPEC_FORMAT,
    version          = SPEC_VERSION,
    name             = name,
    data             = list(name = data_info$name %||% "", columns = as.list(data_info$columns),
                            nrow_used = data_info$nrow_used %||% 0L),
    settings         = settings,
    measurement      = measurement,
    structural       = structural,
    manual_equations = extra_eq %||% ""
  )
}

spec_to_json <- function(spec) {
  as.character(jsonlite::toJSON(spec, auto_unbox = TRUE, null = "null", na = "null", digits = NA))
}

# Parse and validate spec JSON text (from the browser store or an imported file).
# Returns list(ok = TRUE, spec = <normalized spec>) or list(ok = FALSE, msg = <user-facing message>).
# Every field is normalized to plain character vectors / scalars and unknown setting values are dropped,
# so later code never has to guess the shape of what the browser sent.
parse_model_spec_json <- function(txt) {
  tryCatch({
    x <- jsonlite::fromJSON(txt, simplifyVector = FALSE)
    if (!is.list(x) || !identical(x$format, SPEC_FORMAT)) {
      return(list(ok = FALSE, msg = "This file is not a Structura2 model file (format marker is missing)."))
    }
    ver <- suppressWarnings(as.integer(x$version %||% NA))
    if (is.na(ver) || ver < 1L || ver > SPEC_VERSION) {
      return(list(ok = FALSE, msg = "This model file was written by a newer version of Structura2. Update Structura2 and try again."))
    }
    s <- x$settings %||% list()
    pick <- function(v, allowed) if (is.character(v) && length(v) == 1 && v %in% allowed) v else NULL
    num  <- function(v) if (is.numeric(v) && length(v) == 1 && is.finite(v)) v else NULL
    lgl  <- function(v) if (is.logical(v) && length(v) == 1 && !is.na(v)) v else NULL
    settings <- list(
      analysis_mode        = pick(s$analysis_mode, c("raw", "std")),
      missing_method       = pick(s$missing_method, c("listwise", "ml", "ml.x", "two.stage", "robust.two.stage")),
      log_columns          = as_chr(s$log_columns),
      display_columns      = as_chr(s$display_columns),
      layout_style         = pick(s$layout_style, c("dot_LR", "dot_TB", "neato", "fdp", "circo", "twopi")),
      diagram_std          = lgl(s$diagram_std),
      show_suggested_paths = lgl(s$show_suggested_paths),
      max_suggestions      = num(s$max_suggestions)
    )
    spec <- list(
      name = as.character((x$name %||% "")[1]),
      data = list(name = as.character((x$data$name %||% "")[1]),
                  columns = as_chr(x$data$columns),
                  nrow_used = suppressWarnings(as.integer((x$data$nrow_used %||% 0L)[1]))),
      settings = settings,
      measurement = lapply(x$measurement %||% list(), function(m) {
        list(latent = as.character((m$latent %||% "")[1]), indicators = as_chr(m$indicators))
      }),
      structural = lapply(x$structural %||% list(), function(m) {
        list(dependent = as.character((m$dependent %||% "")[1]), predictors = as_chr(m$predictors))
      }),
      manual_equations = as.character((x$manual_equations %||% "")[1])
    )
    list(ok = TRUE, spec = spec)
  }, error = function(e) {
    list(ok = FALSE, msg = paste("Could not read the model file:", conditionMessage(e),
                                 "Check that the file was exported from Structura2 and is not truncated."))
  })
}

# Measurement table for a spec: same layout as the initial table built when data is loaded
# (Latent / Indicator / Operator + one logical column per displayed variable). Indicators that are
# not among `inds` are reported in `dropped` instead of being silently lost.
spec_to_meas_table <- function(spec, inds, obs_names) {
  rows <- spec$measurement
  if (!length(rows)) rows <- list(list(latent = "LatentVariable1", indicators = character(0)))
  n <- length(rows)
  latent <- vapply(rows, function(r) {
    lt <- trimws(r$latent)
    if (nzchar(lt)) make.names(lt) else ""
  }, character(1))
  mat <- matrix(FALSE, nrow = n, ncol = length(inds))
  colnames(mat) <- inds
  dropped <- character(0)
  for (i in seq_len(n)) {
    for (ind in rows[[i]]$indicators) {
      if (ind %in% inds) mat[i, ind] <- TRUE
      else dropped <- c(dropped, ind)
    }
  }
  tbl <- data.frame(Latent = latent, Indicator = "", Operator = "=~",
                    mat, stringsAsFactors = FALSE, check.names = FALSE)
  colnames(tbl) <- c("Latent", "Indicator", "Operator", inds)
  valid_latents <- ifelse(nzchar(tbl$Latent), tbl$Latent, "latent")
  convs <- make.unique(c(obs_names, valid_latents))
  tbl$Indicator <- ifelse(nzchar(tbl$Latent), tail(convs, n), "")
  list(table = tbl, dropped = unique(dropped))
}

# Structural matrix for a spec over the current model items (observed variables + latent names).
# Paths that mention a variable that no longer exists, or a self-loop, are reported in `dropped`.
spec_to_struct_table <- function(spec, items) {
  mat <- data.frame(Dependent = items, Operator = "~", stringsAsFactors = FALSE)
  for (col in items) mat[[col]] <- FALSE
  dropped <- character(0)
  for (s in spec$structural) {
    for (p in s$predictors) {
      if (s$dependent %in% items && p %in% items && !identical(s$dependent, p)) {
        mat[mat$Dependent == s$dependent, p] <- TRUE
      } else {
        dropped <- c(dropped, paste(s$dependent, "~", p))
      }
    }
  }
  list(table = mat, dropped = dropped)
}

# CSV text built with paste() only (no file connections), so non-ASCII column names survive WebR's
# C locale. Numbers keep up to 15 significant digits; NA becomes an empty field.
df_to_csv_text <- function(df) {
  esc <- function(x) paste0("\"", gsub("\"", "\"\"", x, fixed = TRUE), "\"")
  cell <- function(col) {
    if (is.integer(col)) {
      ifelse(is.na(col), "", as.character(col))
    } else if (is.numeric(col)) {
      # element-wise "%.15g": a vector-wide format() would pad every value to the same number of decimals
      out <- ifelse(is.finite(col), formatC(col, digits = 15, format = "g"), "")
      trimws(out)
    } else if (is.logical(col)) {
      ifelse(is.na(col), "", ifelse(col, "TRUE", "FALSE"))
    } else {
      out <- esc(as.character(col))
      out[is.na(col)] <- ""
      out
    }
  }
  header <- paste(esc(names(df)), collapse = ",")
  if (nrow(df) == 0) return(paste0(header, "\r\n"))
  body <- do.call(paste, c(lapply(df, cell), sep = ","))
  paste0(header, "\r\n", paste(body, collapse = "\r\n"), "\r\n")
}

# Per-variable summary of the observed variables of a fitted model (valid / missing counts, moments).
compute_var_stats <- function(fit, df_proc) {
  ov_vars <- intersect(lavaan::lavNames(fit, "ov"), names(df_proc))
  if (!length(ov_vars)) return(NULL)
  rows <- lapply(ov_vars, function(v) {
    x <- df_proc[[v]]
    x_valid <- if (is.numeric(x)) x[!is.na(x)] else numeric(0)
    n_total <- length(x)
    n_valid <- length(x_valid)
    n_miss  <- n_total - n_valid
    out <- data.frame(Variable = v, ValidN = n_valid, MissingN = n_miss,
                      MissingPct = if (n_total > 0) n_miss / n_total * 100 else 0,
                      Mean = NA_real_, SD = NA_real_, Skewness = NA_real_, Kurtosis = NA_real_,
                      stringsAsFactors = FALSE)
    if (n_valid > 1) {
      m_val  <- mean(x_valid)
      sd_val <- sd(x_valid)
      z <- (x_valid - m_val) / ifelse(sd_val > 0, sd_val, 1)
      out$Mean <- m_val; out$SD <- sd_val
      out$Skewness <- mean(z^3); out$Kurtosis <- mean(z^4) - 3
    }
    out
  })
  do.call(rbind, rows)
}

# Cronbach's alpha, composite reliability (CR) and AVE per latent construct.
compute_latent_reliability <- function(fit, df_proc) {
  lv_vars <- lavaan::lavNames(fit, "lv")
  if (!length(lv_vars)) return(NULL)
  pe_std <- tryCatch(lavaan::parameterEstimates(fit, standardized = TRUE), error = function(e) NULL)
  if (is.null(pe_std)) return(NULL)
  rows <- lapply(lv_vars, function(lv) {
    meas_sub   <- pe_std[pe_std$lhs == lv & pe_std$op == "=~", ]
    indicators <- meas_sub$rhs
    k <- length(indicators)
    loadings <- meas_sub$std.all
    loadings <- loadings[!is.na(loadings)]

    alpha_val <- NA_real_
    if (k >= 2 && all(indicators %in% names(df_proc))) {
      ind_df <- na.omit(df_proc[, indicators, drop = FALSE])
      if (nrow(ind_df) > 2) {
        cov_mat   <- cov(ind_df)
        var_sum   <- sum(diag(cov_mat))
        total_var <- sum(cov_mat)
        if (total_var > 0 && var_sum > 0) alpha_val <- (k / (k - 1)) * (1 - (var_sum / total_var))
      }
    }
    cr_val <- NA_real_
    ave_val <- NA_real_
    if (length(loadings) > 0) {
      sum_lambda    <- sum(loadings)
      sum_lambda_sq <- sum(loadings^2)
      sum_theta     <- sum(1 - loadings^2)
      if ((sum_lambda^2 + sum_theta) > 0) cr_val <- (sum_lambda^2) / (sum_lambda^2 + sum_theta)
      if ((sum_lambda_sq + sum_theta) > 0) ave_val <- sum_lambda_sq / (sum_lambda_sq + sum_theta)
    }
    data.frame(Latent = lv, Indicators = paste(indicators, collapse = ", "), Count = k,
               Alpha = alpha_val, CR = cr_val, AVE = ave_val, stringsAsFactors = FALSE)
  })
  do.call(rbind, rows)
}

# ---- Candidate comparability helpers ----------------------------------------
# A candidate is the baseline syntax with some structural paths removed and is estimated with exactly
# the same lavaan call as the main model (run_lavaan_sem), so the same path diagram always gives the
# same AIC/BIC. lavaan's defaults are NOT overridden, which has two consequences that are detected here:
#  * a variable that has lost every path is silently dropped from the model, so its likelihood is
#    computed on different data and AIC/BIC are no longer comparable with the baseline;
#  * removing a path can make lavaan free a covariance instead (m ~ x1 turns into m ~~ x1 when m becomes
#    exogenous; y ~ m turns into m ~~ y when both stay dependent), i.e. the association is NOT removed.
# Both kinds of candidate are kept in the catalogue for transparency but can never rank as optimal.

# Single estimation entry point shared by the main model, the optimizer candidates and the MI helper fit.
run_lavaan_sem <- function(syntax_str, data, missing_method, needs_meanstructure, ...) {
  lavaan::sem(syntax_str,
              data          = data,
              missing       = missing_method,
              fixed.x       = FALSE,
              parser        = "old",
              meanstructure = needs_meanstructure,
              ncpus         = 1L,
              ...)
}

# ---- Beginner-oriented diagnostics ----
# Principle: block only what is mathematically impossible (errors); report "estimated but unreliable"
# situations as warnings. Detection uses lavInspect() values and linear algebra, never lavaan warning text.
fmt_var_list <- function(x) paste(x, collapse = ", ")

# TRUE when the structural matrix has at least one active path (what Auto-Optimize works on)
has_active_struct_path <- function(struct_df) {
  if (is.null(struct_df) || nrow(struct_df) == 0 || ncol(struct_df) < 3) return(FALSE)
  m <- struct_df[, 3:ncol(struct_df), drop = FALSE]
  any(vapply(m, function(col) any(as.logical(col), na.rm = TRUE), logical(1)))
}

# One-line pointer shown under an identification error when the model has structural paths to prune
optimize_tip <- function(struct_df) {
  if (has_active_struct_path(struct_df))
    "Tip: Optimize (next to Run) can remove structural paths automatically until the model is identified."
  else NULL
}

# Decides whether Auto-Optimize can start from a fit_model_safe() result. A model that is not identified
# (df < 0 / no standard errors) is a flagged result (ok = TRUE, identified = FALSE) and a valid starting point:
# removing paths is often what makes it identified.
optimization_baseline <- function(model) {
  if (isTRUE(model$ok) && !is.null(model$fit))
    return(list(usable = TRUE, fit = model$fit, identified = !isFALSE(model$identified), msg = ""))
  list(usable = FALSE, fit = NULL, identified = FALSE, msg = model$msg_friendly %||% "")
}

# Checks run BEFORE estimation. Returns a character vector of blocking messages (empty = OK) with an
# attribute `kind`: "data" (missing variable, too few rows, linear dependency: nothing can be estimated) or
# "identification" (df < 0: estimable, and Auto-Optimize can search for an identified submodel).
diagnose_fit_inputs <- function(syntax_str, data, missing_method, needs_meanstructure) {
  msgs <- character(0)
  kind <- ""
  tryCatch({
    # The parameter table is built without touching the data, so it works even when estimation would fail
    pt <- lavaan::lavaanify(syntax_str, model.type = "sem", fixed.x = FALSE,
                            meanstructure = isTRUE(needs_meanstructure), auto.var = TRUE,
                            auto.cov.lv.x = TRUE, auto.cov.y = TRUE, int.ov.free = TRUE,
                            auto.fix.first = TRUE, auto.fix.single = TRUE)
    # Variables used by the model but absent from the data: name them (lavaan's own wording varies by version)
    absent <- setdiff(lavaan::lavNames(pt, "ov"), names(data))
    if (length(absent) > 0) {
      return(structure(sprintf(
        "These variables are used in the model but not found in the data: %s. Check the spelling in Manual Equations (names are case-sensitive), or select the variable in Filtered > Display columns.",
        fmt_var_list(absent)), kind = "data"))
    }
    ov <- intersect(lavaan::lavNames(pt, "ov"), names(data))
    p <- length(ov)
    if (p == 0) return(structure(msgs, kind = ""))
    X <- data[, ov, drop = FALSE]
    num <- vapply(X, is.numeric, logical(1))
    X <- X[, num, drop = FALSE]
    n_used <- if (identical(missing_method, "listwise")) sum(stats::complete.cases(X)) else nrow(X)

    if (n_used < p) {
      kind <- "data"
      msgs <- c(msgs, sprintf(
        "Too few rows: the model uses %d variables but only %d complete rows are available. Remove variables from the model, or use a larger data set (or a missing-data method such as FIML).",
        p, n_used))
    } else if (ncol(X) >= 2) {
      # Exact linear dependency: smallest eigenvalue of the correlation matrix is ~0
      Xc <- stats::na.omit(X)
      sds <- vapply(Xc, stats::sd, numeric(1))
      Xc <- Xc[, !is.na(sds) & sds > 1e-12, drop = FALSE]
      if (nrow(Xc) > 1 && ncol(Xc) >= 2) {
        eg <- eigen(stats::cor(Xc), symmetric = TRUE)
        if (min(eg$values) < 1e-8) {
          v <- eg$vectors[, which.min(eg$values)]
          culprits <- colnames(Xc)[abs(v) > 0.1]
          kind <- "data"
          msgs <- c(msgs, sprintf(
            "These variables are exactly linearly dependent: %s. This happens when a column is duplicated or is a total/sum of other columns. Remove one of them from the model (Filtered tab > Display columns).",
            fmt_var_list(culprits)))
        }
      }
    }

    if (length(msgs) == 0) {
      npar <- max(pt$free)
      n_info <- p * (p + 1) / 2 + if (isTRUE(needs_meanstructure)) p else 0
      df <- n_info - npar
      if (is.finite(df) && df < 0) {
        few <- character(0)
        for (lv in lavaan::lavNames(pt, "lv")) {
          k <- sum(pt$op == "=~" & pt$lhs == lv)
          if (k < 3) few <- c(few, sprintf("%s (%d indicator%s)", lv, k, if (k == 1) "" else "s"))
        }
        covs <- pt$op == "~~" & pt$lhs != pt$rhs & pt$free > 0
        cov_txt <- if (any(covs)) paste0(pt$lhs[covs], " ~~ ", pt$rhs[covs]) else character(0)
        kind <- "identification"
        msgs <- c(msgs, paste0(
          sprintf("The model is not identified: it estimates %d parameters but the data supply only %d pieces of information (df = %d). Remove at least %d free parameter(s).",
                  npar, n_info, as.integer(df), as.integer(-df)),
          if (length(cov_txt)) paste0(" Candidates: residual covariances (", fmt_var_list(cov_txt), ") or paths."),
          if (length(few)) paste0(" Factors with few indicators: ", fmt_var_list(few), ". Add indicators or simplify the model.")))
      }
    }
  }, error = function(e) NULL)
  attr(msgs, "kind") <- kind
  msgs
}

# Checks run AFTER a converged fit. Returns list(errors, warnings, ident): `ident` is the not-identified
# message (NULL when identified). An unidentified fit is still shown, so it is not an error.
diagnose_fit_results <- function(fit, data) {
  errs <- character(0); warns <- character(0); ident_msg <- NULL
  tryCatch({
    pe <- lavaan::parameterEstimates(fit)
    if (!fit_identification(fit)$identified) {
      ident_msg <- "The model is not identified: standard errors could not be computed. Typical causes are a factor with too few indicators, feedback loops (a ~ b and b ~ a), or too many free covariances. Simplify the model or add constraints."
    } else if (!fit_is_proper(fit)) {
      th <- tryCatch(lavaan::lavInspect(fit, "est"), error = function(e) NULL)
      neg <- character(0)
      if (!is.null(th)) {
        for (m in c("theta", "psi")) {
          if (!is.null(th[[m]]) && is.matrix(th[[m]])) {
            d <- diag(th[[m]]); neg <- c(neg, names(d)[!is.na(d) & d < 0])
          }
        }
      }
      if (length(neg)) {
        warns <- c(warns, sprintf("Negative variance estimated for: %s (Heywood case). Results are not interpretable. Common causes: a factor with few indicators or a small sample.", fmt_var_list(unique(neg))))
      } else {
        cl <- tryCatch(lavaan::lavInspect(fit, "cor.lv"), error = function(e) NULL)
        pairs <- character(0)
        if (is.matrix(cl) && nrow(cl) >= 2) {
          for (i in seq_len(nrow(cl) - 1)) for (j in (i + 1):ncol(cl))
            if (!is.na(cl[i, j]) && abs(cl[i, j]) >= 0.99) pairs <- c(pairs, paste(rownames(cl)[i], "and", colnames(cl)[j]))
        }
        warns <- c(warns, if (length(pairs))
          sprintf("Factors are indistinguishable: %s (correlation ~ 1). Merge them into one factor.", fmt_var_list(pairs))
        else "The solution is improper (a covariance matrix is not positive definite). Treat the results with caution.")
      }
    }
    # Highly correlated predictors of the same dependent variable
    reg <- pe[pe$op == "~", , drop = FALSE]
    for (dv in unique(reg$lhs)) {
      preds <- intersect(reg$rhs[reg$lhs == dv], names(data))
      preds <- preds[vapply(data[preds], is.numeric, logical(1))]
      if (length(preds) < 2) next
      cm <- suppressWarnings(stats::cor(data[, preds, drop = FALSE], use = "pairwise.complete.obs"))
      for (i in seq_len(nrow(cm) - 1)) for (j in (i + 1):ncol(cm))
        if (!is.na(cm[i, j]) && abs(cm[i, j]) >= 0.95)
          warns <- c(warns, sprintf("%1$s and %2$s are highly correlated (r = %4$.3f) and both predict %3$s, so their coefficients and standard errors are unstable. Combine them as indicators of one latent factor, or keep only one.",
                                    preds[i], preds[j], dv, cm[i, j]))
    }
  }, error = function(e) NULL)
  list(errors = errs, warnings = unique(warns), ident = ident_msg)
}

# Free covariances between two different variables as sorted "a ~~ b" keys.
free_cov_pairs <- function(fit) {
  pt <- tryCatch(lavaan::parTable(fit), error = function(e) NULL)
  if (is.null(pt) || !nrow(pt)) return(character(0))
  pt <- pt[pt$op == "~~" & pt$lhs != pt$rhs & pt$free > 0, , drop = FALSE]
  if (!nrow(pt)) return(character(0))
  unique(vapply(seq_len(nrow(pt)), function(i) paste(sort(c(pt$lhs[i], pt$rhs[i])), collapse = " ~~ "), character(1)))
}

# Variables the baseline model contains and a candidate must keep: the structural variables that are
# neither latent nor measurement indicators (those stay in the model through the measurement part).
required_struct_vars <- function(struct_df, base_fit) {
  lv  <- tryCatch(lavaan::lavNames(base_fit, "lv"), error = function(e) character(0))
  ind <- tryCatch(lavaan::lavNames(base_fit, "ov.ind"), error = function(e) character(0))
  setdiff(active_struct_vars(struct_df), c(lv, ind))
}

# Structural comparability of a fitted candidate with the baseline described by `ctx`
# (ctx$base_ov = observed variables, ctx$base_cov_pairs = free covariances of the baseline fit).
candidate_structure_check <- function(fit, ctx) {
  vars_ok <- is.null(ctx$base_ov) || setequal(lavaan::lavNames(fit, "ov"), ctx$base_ov)
  added <- if (is.null(ctx$base_cov_pairs)) character(0) else setdiff(free_cov_pairs(fit), ctx$base_cov_pairs)
  list(vars_ok = vars_ok, added_covs = added, replaced = length(added) > 0)
}

# TRUE when the solution is admissible (no negative variances, non-positive-definite matrices, ...).
# If the check itself cannot be evaluated the fit is not penalised.
fit_is_proper <- function(fit) {
  tryCatch(isTRUE(suppressWarnings(lavaan::lavInspect(fit, "post.check"))), error = function(e) TRUE)
}

# Identification of a fitted model. A model with df < 0 converges and even reports (meaningless) AIC/BIC,
# but its standard errors cannot be computed, so "identified" needs df >= 0 AND an invertible information
# matrix (lavInspect(fit, "vcov") is not a matrix when the standard errors failed).
# `deficit` is how far the model is from being identified: the number of parameters to remove when df < 0
# (0.5 when only the standard errors fail), used to steer the repair phase of the optimizer.
fit_identification <- function(fit) {
  converged <- tryCatch(isTRUE(lavaan::lavInspect(fit, "converged")), error = function(e) FALSE)
  if (!converged) return(list(identified = FALSE, df = NA_real_, deficit = Inf))
  df <- tryCatch(as.numeric(lavaan::fitMeasures(fit, "df")), error = function(e) NA_real_)
  vc <- tryCatch(suppressWarnings(lavaan::lavInspect(fit, "vcov")), error = function(e) NULL)
  se_ok <- is.matrix(vc) && !anyNA(vc)
  deficit <- (if (isTRUE(df < 0)) -df else 0) + (if (!se_ok && isTRUE(df >= 0)) 0.5 else 0)
  list(identified = isTRUE(df >= 0) && se_ok, df = df, deficit = deficit)
}

# Which conventional fit cutoffs (CFI < .90, RMSEA > .08, SRMR > .08) a set of fit measures violates.
fit_cutoff_violations <- function(ms) {
  get <- function(nm) if (nm %in% names(ms)) as.numeric(ms[nm]) else NA_real_
  c(cfi   = isTRUE(get("cfi") < 0.90),
    rmsea = isTRUE(get("rmsea") > 0.08),
    srmr  = isTRUE(get("srmr") > 0.08))
}

# Score used for ranking/search. Non-converged or improper (e.g. negative variance) fits never win, and
# neither do candidates that are not a pure path reduction of the baseline (a variable was dropped, or
# lavaan replaced a removed path by a covariance).
candidate_score <- function(rec, criterion) {
  if (is.null(rec) || !isTRUE(rec$converged) || isFALSE(rec$proper)) return(Inf)
  if (isFALSE(rec$vars_ok) || isTRUE(rec$replaced)) return(Inf)
  # An unidentified model fits "perfectly" (df < 0) and would otherwise win on AIC/BIC
  if (isFALSE(rec$identified)) return(Inf)
  s <- if (criterion == "AIC") rec$aic else rec$bic
  if (is.null(s) || is.na(s)) Inf else s
}

# Fits one candidate structural model with the same syntax and options as the main model.
# `ctx` carries data, estimation options, measurement lines, extra lines and an optional warm-start
# fit. Returns NULL when estimation fails.
fit_candidate_model <- function(struct_df, ctx) {
  all_syntax <- unlist(c(ctx$meas_lines, build_struct_lines(struct_df), ctx$extra_lines))
  if (!length(all_syntax)) return(NULL)
  syntax_str <- paste(all_syntax, collapse = "\n")

  run_sem <- function(...) {
    tryCatch(
      run_lavaan_sem(syntax_str, ctx$data, ctx$missing_method, ctx$needs_meanstructure, ...),
      error = function(e) NULL)
  }

  fm <- NULL
  if (!is.null(ctx$base_fit)) fm <- run_sem(start = ctx$base_fit)
  # Safety fallback: cold start if warm start failed or did not converge
  if (is.null(fm) || !isTRUE(lavaan::lavInspect(fm, "converged"))) fm <- run_sem()
  fm
}

# ---------- Helper: Reachability on the structural graph (edges: predictor -> dependent) ----------
struct_descendants <- function(struct_df, from) {
  seen <- character(0)
  queue <- from
  while (length(queue)) {
    cur <- queue[1]; queue <- queue[-1]
    if (!cur %in% names(struct_df)) next
    flags <- vapply(struct_df[[cur]], function(x) isTRUE(as.logical(x)), logical(1))
    kids <- setdiff(struct_df$Dependent[flags], seen)
    seen <- c(seen, kids)
    queue <- c(queue, kids)
  }
  seen
}

# ---- Helper: Diagnostic fit used to generate suggestions ------------
# lavaan only evaluates modification indices for variables that already take part in the model's
# regressions/loadings, so a variable with no structural path yet can never be suggested. This refits the
# current syntax with each unused variable attached as an exogenous predictor through a fixed-zero
# regression (`anchor ~ 0*v`), which leaves every estimate of the real model unchanged but makes lavaan
# score all paths into/out of v. (The reverse direction, `v ~ 0*anchor`, does not work: lavaan then
# reports no MI for paths from v.) `anchor_var` should be an observed endogenous variable of the model.
# The result is for diagnostics only and is never shown as the user's model.
fit_suggestion_model <- function(syntax_lines, unused_vars, anchor_var, ctx) {
  if (!length(unused_vars) || is.null(anchor_var) || !nzchar(anchor_var)) return(NULL)
  syntax_str <- paste(c(syntax_lines, paste0(unused_vars, " ~ 0*", anchor_var)), collapse = "\n")
  fm <- tryCatch(
    run_lavaan_sem(syntax_str, ctx$data, ctx$missing_method, ctx$needs_meanstructure),
    error = function(e) NULL)
  if (is.null(fm) || !isTRUE(lavaan::lavInspect(fm, "converged"))) NULL else fm
}

# ---- Helper: Modification-index based suggestions ------------
# All MI rows (regressions, residual covariances, cross-loadings) passing the MI and |std.EPC| filters.
get_modification_suggestions <- function(fit, mi_threshold = 6.63, epc_threshold = 0) {
  if (is.null(fit) || !isTRUE(lavaan::lavInspect(fit, "converged"))) return(NULL)
  mi_res <- tryCatch(
    lavaan::modificationindices(fit, standardized = TRUE, sort. = TRUE, minimum.value = mi_threshold),
    error = function(e) NULL)
  if (is.null(mi_res) || nrow(mi_res) == 0) return(NULL)
  mi_res <- mi_res[mi_res$op %in% c("~", "~~", "=~") & mi_res$lhs != mi_res$rhs, , drop = FALSE]
  if (nrow(mi_res) == 0) return(NULL)
  if (epc_threshold > 0 && "sepc.all" %in% names(mi_res)) {
    keep <- is.na(mi_res$sepc.all) | abs(mi_res$sepc.all) >= epc_threshold
    mi_res <- mi_res[keep, , drop = FALSE]
  }
  if (nrow(mi_res) == 0) return(NULL)
  mi_res[order(-mi_res$mi), , drop = FALSE]
}

# Suggested regression paths mapped onto the structural checkbox matrix by variable NAME.
# Each cell is NULL or list(mi, epc, std_epc, rank, cyclic). `rank` orders suggestions by MI
# (1 = strongest); `cyclic` flags paths that would close a feedback loop with existing paths.
get_suggested_structural_paths <- function(fit, struct_df, mi_threshold = 6.63, epc_threshold = 0, max_paths = Inf) {
  mi_res <- get_modification_suggestions(fit, mi_threshold, epc_threshold)
  if (is.null(mi_res)) return(NULL)
  reg_mi <- mi_res[mi_res$op == "~", , drop = FALSE]
  if (nrow(reg_mi) == 0) return(NULL)

  deps <- struct_df$Dependent
  preds <- struct_pred_cols(struct_df)
  suggested_matrix <- replicate(nrow(struct_df), vector("list", length(preds)), simplify = FALSE)
  has_suggestions <- FALSE
  rank_counter <- 0L

  for (i in seq_len(nrow(reg_mi))) {
    row_dep <- as.character(reg_mi$lhs[i])
    col_pred <- as.character(reg_mi$rhs[i])
    r <- match(row_dep, deps)
    c <- match(col_pred, preds)
    if (is.na(r) || is.na(c)) next
    if (rank_counter >= max_paths) break

    # Exclude already active paths (self-loops are already removed by get_modification_suggestions)
    if (isTRUE(as.logical(struct_df[r, preds[c]]))) next

    std_val <- if ("sepc.all" %in% names(reg_mi)) as.numeric(reg_mi$sepc.all[i]) else NA_real_
    if (is.na(std_val)) std_val <- NULL
    rank_counter <- rank_counter + 1L

    suggested_matrix[[r]][[c]] <- list(
      mi = as.numeric(reg_mi$mi[i]),
      epc = as.numeric(reg_mi$epc[i]),
      std_epc = std_val,
      rank = rank_counter,
      cyclic = col_pred %in% struct_descendants(struct_df, row_dep)
    )
    has_suggestions <- TRUE
  }

  if (!has_suggestions) return(NULL)
  suggested_matrix
}

# ================================================================
# UI
# ================================================================

ui <- fluidPage(
  useShinyjs(),
  tags$head(
    tags$link(rel = "icon", type = "image/x-icon", href = "favicon.ico"),
    tags$style(HTML("
#app-logo { position: absolute; top: 8px; right: 16px; }
.modal-header { background: #f8f9fa; }
.modal-title  { font-weight: bold; }
.htDimmed { background-color: #d9d9d9 !important; color: #777 !important; }
.shiny-modal .modal-content { border-radius: 8px; box-shadow: 0 4px 12px rgba(0,0,0,0.15); }
.shiny-modal .modal-body    { padding: 20px !important; }
.shiny-modal .modal-footer  { padding: 10px !important; }
.alert-box { background:#fff3cd;border:1px solid #ffeeba;border-radius:6px;padding:10px;margin-bottom:10px; }
.alert-box.alert-box-error { background:#f8d7da;border-color:#f5c6cb;color:#721c24; }
.alert-box.alert-box-warning { background:#fff3cd;border-color:#ffeeba;color:#856404; }
.alert-box.alert-box-info { background:#e9ecef;border-color:#ced4da;color:#495057; }
/* A validate() message from an htmlwidget (DT) is absolutely positioned and overlaps the next element; keep it in the flow */
.htmlwidgets-error { position: static !important; height: auto !important; padding: 4px 0 8px; }
.html-widget-output[style*='visibility: hidden'] { height: 0 !important; overflow: hidden; }
#fit_alert { white-space: pre-wrap; }
#lavaan_model { white-space: pre; }
#approx_eq    { white-space: pre-wrap; }

/* Custom elegant splash preloader styles */
#structura-preload-container {
  position: fixed;
  top: 0; left: 0; width: 100%; height: 100%;
  background: linear-gradient(135deg, #1e293b 0%, #0f172a 100%);
  color: white;
  z-index: 99999;
  display: flex;
  flex-direction: column;
  justify-content: center;
  align-items: center;
  font-family: system-ui, -apple-system, sans-serif;
  transition: opacity 0.5s ease-out;
}
.structura-preload-spinner {
  border: 4px solid rgba(255,255,255,0.1);
  width: 50px; height: 50px;
  border-radius: 50%;
  border-left-color: #3b82f6;
  animation: structura-spin 1s linear infinite;
  margin-bottom: 24px;
}
@keyframes structura-spin { 0% { transform: rotate(0deg); } 100% { transform: rotate(360deg); } }
.structura-preload-progress {
  width: 280px;
  background-color: #334155;
  border-radius: 10px;
  padding: 3px;
  margin-top: 16px;
}
.structura-preload-bar {
  height: 8px;
  background-color: #3b82f6;
  border-radius: 8px;
  width: 0%;
  transition: width 0.3s ease;
}
.structura-preload-bar-indeterminate {
  position: relative;
  width: 35% !important;
  animation: structura-preload-slide 1.6s infinite ease-in-out;
}
@keyframes structura-preload-slide {
  0% { margin-left: 0%; } 50% { margin-left: 65%; } 100% { margin-left: 0%; }
}
.structura-embedded #structura-preload-container { display: none !important; }

/* Print Report Styling */
#structura-print-report {
  display: none;
}

@media print {
  body * {
    visibility: hidden;
  }
  #structura-print-report, #structura-print-report * {
    visibility: visible;
  }
  #structura-print-report {
    display: block !important;
    position: absolute;
    left: 0;
    top: 0;
    width: 100%;
    color: #1e293b;
    background: #ffffff;
    font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, 'Helvetica Neue', Arial, sans-serif;
  }
  @page {
    size: A4 portrait;
    margin: 15mm 12mm 15mm 12mm;
  }
  .print-page-break {
    page-break-before: always;
  }
  .print-avoid-break {
    page-break-inside: avoid;
  }
  .print-table {
    width: 100%;
    border-collapse: collapse;
    margin-bottom: 16px;
    font-size: 11px;
  }
  .print-table th, .print-table td {
    border: 1px solid #cbd5e1;
    padding: 5px 8px;
    text-align: left;
  }
  .print-table th {
    background-color: #f1f5f9 !important;
    font-weight: 600;
    -webkit-print-color-adjust: exact;
    print-color-adjust: exact;
  }
  .print-header {
    border-bottom: 2px solid #2563eb;
    padding-bottom: 8px;
    margin-bottom: 16px;
    display: flex;
    justify-content: space-between;
    align-items: flex-end;
  }
  .print-header h1 {
    font-size: 20px;
    font-weight: bold;
    color: #0f172a;
    margin: 0;
  }
  .print-header .meta {
    font-size: 11px;
    color: #64748b;
  }
  .print-section-title {
    font-size: 14px;
    font-weight: bold;
    color: #1e293b;
    border-bottom: 1px solid #e2e8f0;
    padding-bottom: 4px;
    margin-top: 14px;
    margin-bottom: 8px;
  }
  .print-diagram-box {
    text-align: center;
    max-height: 480px;
    overflow: hidden;
    margin: 10px 0;
  }
  .print-diagram-box svg {
    max-width: 100%;
    max-height: 460px;
    height: auto;
  }
  .print-syntax-box {
    background: #f8fafc;
    border: 1px solid #e2e8f0;
    border-radius: 4px;
    padding: 8px 12px;
    font-family: monospace;
    font-size: 10px;
    white-space: pre-wrap;
    -webkit-print-color-adjust: exact;
    print-color-adjust: exact;
  }
}
")),
    # Graphviz bundle (wasm embedded). Local runApp serves it from www/; in the ShinyLive static
    # site it is hosted next to index.html (site/hpcc-js/) so it bypasses the slow R/webR HTTP
    # emulation. Loaded with `defer` so it never blocks the first render.
    tags$script(
      defer = NA,
      src = if (file.exists("www/hpcc-js/graphviz.umd.js")) "hpcc-js/graphviz.umd.js"
            else "../hpcc-js/graphviz.umd.js"
    ),
    tags$script(HTML("
      // Progress bar logic (JavaScript-driven to avoid R blocking issues)
      (function() {
        var interval;
        var width = 0;

        // Guarantee function is registered immediately even if DOM elements aren't ready yet
        window.finishStructuraPreload = function(success, errorMsg) {
          if (interval) clearInterval(interval);
          var bar = document.getElementById('structura-preload-bar');
          var status = document.getElementById('structura-preload-status');
          
          if (success) {
            if (bar) { bar.classList.remove('structura-preload-bar-indeterminate'); bar.style.width = '100%'; }
            if (status) status.innerText = 'Ready!';
            setTimeout(function() {
              var container = document.getElementById('structura-preload-container');
              if (container) {
                container.style.opacity = '0';
                setTimeout(function() {
                  container.style.display = 'none';
                }, 500);
              }
            }, 300);
          } else {
            if (bar) {
              bar.classList.remove('structura-preload-bar-indeterminate');
              bar.style.backgroundColor = '#ef4444';
              bar.style.width = '100%';
            }
            if (status) {
              status.innerText = 'Error loading application: ' + errorMsg;
              status.style.color = '#f87171';
            }
          }
        };

        // Local runApp only: the real progress overlay lives in the ShinyLive host page, so this
        // inner overlay just shows an indeterminate bar (no fabricated percentage).
        if (window.parent !== window) {
          document.documentElement.classList.add('structura-embedded');
        }
        var startTimer = function() {
          var bar = document.getElementById('structura-preload-bar');
          var status = document.getElementById('structura-preload-status');
          if (!bar || !status) {
            setTimeout(startTimer, 100);
            return;
          }
          bar.classList.add('structura-preload-bar-indeterminate');
          status.innerText = 'Loading structural equation engine (lavaan)...';
        };

        startTimer();
      })();

      // Standalone client-side diagram export helpers
      // The on-screen SVG is sized 100% x 100% to fit its container; exports need absolute pixel sizes
      // (a percentage size cannot be drawn to a canvas in every browser), so a clone is sized from its viewBox.
      window.structuraDiagramSvg = function() {
        var container = document.getElementById('sem_plot_container');
        var svg = container ? container.querySelector('svg') : null;
        if (!svg) return null;
        var vb = svg.viewBox && svg.viewBox.baseVal;
        var width = (vb && vb.width > 0) ? vb.width : (svg.clientWidth || 800);
        var height = (vb && vb.height > 0) ? vb.height : (svg.clientHeight || 600);
        var clone = svg.cloneNode(true);
        clone.setAttribute('width', width + 'px');
        clone.setAttribute('height', height + 'px');
        if (!clone.getAttribute('xmlns')) clone.setAttribute('xmlns', 'http://www.w3.org/2000/svg');
        return { text: new XMLSerializer().serializeToString(clone), width: width, height: height };
      };

      // Renders the diagram to a PNG Blob (white background). The scale is reduced for very large diagrams
      // so the canvas stays below ~16 million pixels.
      window.structuraDiagramPng = function(scale) {
        return new Promise(function(resolve, reject) {
          var d = window.structuraDiagramSvg();
          if (!d) { reject(new Error('No path diagram available.')); return; }
          scale = scale || 2;
          var maxPixels = 16000000;
          if (d.width * d.height * scale * scale > maxPixels) scale = Math.sqrt(maxPixels / (d.width * d.height));
          var url = URL.createObjectURL(new Blob([d.text], { type: 'image/svg+xml;charset=utf-8' }));
          var img = new Image();
          img.onload = function() {
            try {
              var canvas = document.createElement('canvas');
              canvas.width = Math.max(1, Math.round(d.width * scale));
              canvas.height = Math.max(1, Math.round(d.height * scale));
              var ctx = canvas.getContext('2d');
              ctx.fillStyle = '#ffffff';
              ctx.fillRect(0, 0, canvas.width, canvas.height);
              ctx.drawImage(img, 0, 0, canvas.width, canvas.height);
              URL.revokeObjectURL(url);
              canvas.toBlob(function(b) { b ? resolve(b) : reject(new Error('PNG encoding failed.')); }, 'image/png');
            } catch (err) { URL.revokeObjectURL(url); reject(err); }
          };
          img.onerror = function() { URL.revokeObjectURL(url); reject(new Error('The diagram could not be rendered to an image.')); };
          img.src = url;
        });
      };

      window.structuraSaveBlob = function(blob, filename) {
        var url = URL.createObjectURL(blob);
        var a = document.createElement('a');
        a.href = url;
        a.download = filename;
        document.body.appendChild(a);
        a.click();
        document.body.removeChild(a);
        setTimeout(function() { URL.revokeObjectURL(url); }, 2000);
      };

      window.downloadSemDiagramSvg = function() {
        var d = window.structuraDiagramSvg();
        if (!d) {
          alert('No path diagram available to export. Please run and fit a model first.');
          return;
        }
        window.structuraSaveBlob(new Blob([d.text], { type: 'image/svg+xml;charset=utf-8' }), 'structura2_path_diagram.svg');
      };

      window.downloadSemDiagramPng = function(scale) {
        if (!window.structuraDiagramSvg()) {
          alert('No path diagram available to export. Please run and fit a model first.');
          return;
        }
        window.structuraDiagramPng(scale || 2).then(function(blob) {
          window.structuraSaveBlob(blob, 'structura2_path_diagram.png');
        }).catch(function(err) {
          alert('Could not export the PNG: ' + err.message);
        });
      };

      // Local timestamp for file names, e.g. 20261004_153012 (taken in the browser: WebR runs in UTC)
      window.structuraStamp = function() {
        var d = new Date();
        var p = function(n) { return (n < 10 ? '0' : '') + n; };
        return d.getFullYear() + p(d.getMonth() + 1) + p(d.getDate()) + '_' + p(d.getHours()) + p(d.getMinutes()) + p(d.getSeconds());
      };
      window.structuraSafeName = function(s) {
        var t = String(s || '').replace(/[\\\\/:*?\"<>|\\s]+/g, '_').replace(/^_+|_+$/g, '');
        return t.length ? t.slice(0, 60) : 'model';
      };

      // Minimal ZIP writer (STORE method = no compression; all payloads here are small text/PNG files).
      // entries: [{name: string, data: Uint8Array}]
      window.structuraBuildZip = function(entries) {
        var table = window.__structuraCrcTable;
        if (!table) {
          table = window.__structuraCrcTable = new Uint32Array(256);
          for (var n = 0; n < 256; n++) {
            var c = n;
            for (var k = 0; k < 8; k++) c = (c & 1) ? (0xEDB88320 ^ (c >>> 1)) : (c >>> 1);
            table[n] = c >>> 0;
          }
        }
        var crc32 = function(buf) {
          var c = 0xFFFFFFFF;
          for (var i = 0; i < buf.length; i++) c = table[(c ^ buf[i]) & 0xFF] ^ (c >>> 8);
          return (c ^ 0xFFFFFFFF) >>> 0;
        };
        var enc = new TextEncoder();
        var now = new Date();
        var dosTime = (now.getHours() << 11) | (now.getMinutes() << 5) | (now.getSeconds() >> 1);
        var dosDate = ((now.getFullYear() - 1980) << 9) | ((now.getMonth() + 1) << 5) | now.getDate();
        var parts = [], central = [], offset = 0;
        entries.forEach(function(e) {
          var name = enc.encode(e.name);
          var crc = crc32(e.data);
          var local = new DataView(new ArrayBuffer(30));
          local.setUint32(0, 0x04034b50, true); local.setUint16(4, 20, true); local.setUint16(6, 0x0800, true);
          local.setUint16(8, 0, true); local.setUint16(10, dosTime, true); local.setUint16(12, dosDate, true);
          local.setUint32(14, crc, true); local.setUint32(18, e.data.length, true); local.setUint32(22, e.data.length, true);
          local.setUint16(26, name.length, true); local.setUint16(28, 0, true);
          parts.push(new Uint8Array(local.buffer), name, e.data);
          var cd = new DataView(new ArrayBuffer(46));
          cd.setUint32(0, 0x02014b50, true); cd.setUint16(4, 20, true); cd.setUint16(6, 20, true); cd.setUint16(8, 0x0800, true);
          cd.setUint16(10, 0, true); cd.setUint16(12, dosTime, true); cd.setUint16(14, dosDate, true);
          cd.setUint32(16, crc, true); cd.setUint32(20, e.data.length, true); cd.setUint32(24, e.data.length, true);
          cd.setUint16(28, name.length, true); cd.setUint32(42, offset, true);
          central.push(new Uint8Array(cd.buffer), name);
          offset += 30 + name.length + e.data.length;
        });
        var cdSize = central.reduce(function(s, p) { return s + p.length; }, 0);
        var end = new DataView(new ArrayBuffer(22));
        end.setUint32(0, 0x06054b50, true); end.setUint16(8, entries.length, true); end.setUint16(10, entries.length, true);
        end.setUint32(12, cdSize, true); end.setUint32(16, offset, true);
        return new Blob(parts.concat(central, [new Uint8Array(end.buffer)]), { type: 'application/zip' });
      };

      // Browser-side model store (localStorage). Everything is wrapped in try/catch: storage can be missing
      // or blocked (private windows, site-data settings), and the app must keep working without it.
      window.structuraStore = (function() {
        var KEY = 'structura2.models.v1';
        var MAX_MODELS = 20;
        function read() {
          try {
            var t = window.localStorage.getItem(KEY);
            var o = t ? JSON.parse(t) : {};
            return { autosave: o.autosave || null, models: o.models || {} };
          } catch (e) { return null; }
        }
        function write(o) {
          try { window.localStorage.setItem(KEY, JSON.stringify(o)); return true; } catch (e) { return false; }
        }
        function label(spec) {
          if (!spec) return '';
          var dn = (spec.data && spec.data.name) ? spec.data.name : 'unknown data';
          return dn + (spec.saved_at ? ' (' + spec.saved_at + ')' : '');
        }
        function publish(extra) {
          if (!window.Shiny || !Shiny.setInputValue) return;
          var o = read();
          var info = o === null
            ? { available: false, names: [], autosave_label: '', autosave_columns: [] }
            : { available: true, names: Object.keys(o.models).sort(),
                autosave_label: label(o.autosave),
                autosave_columns: (o.autosave && o.autosave.data && o.autosave.data.columns) ? o.autosave.data.columns : [] };
          if (extra) Object.keys(extra).forEach(function(k) { info[k] = extra[k]; });
          Shiny.setInputValue('stored_models', JSON.stringify(info), { priority: 'event' });
        }
        function respond(spec, source) {
          Shiny.setInputValue('restore_spec_json', { nonce: Date.now(), source: source, json: JSON.stringify(spec) }, { priority: 'event' });
        }
        function handle(msg) {
          var o = read();
          if (o === null) { publish({ error: 'Browser storage is unavailable.' }); return; }
          var stamp = new Date().toLocaleString();
          if (msg.op === 'autosave') {
            var s = JSON.parse(msg.json); s.saved_at = stamp; o.autosave = s;
            if (!write(o)) publish({ error: 'Could not write to browser storage (it may be full or blocked).' }); else publish();
          } else if (msg.op === 'save') {
            var m = JSON.parse(msg.json); m.name = msg.name; m.saved_at = stamp;
            if (!(msg.name in o.models) && Object.keys(o.models).length >= MAX_MODELS) {
              publish({ error: 'At most ' + MAX_MODELS + ' models can be kept. Delete one first, or use Export JSON.' }); return;
            }
            o.models[msg.name] = m;
            if (!write(o)) publish({ error: 'Could not write to browser storage (it may be full or blocked).' }); else publish({ notice: 'Saved \"' + msg.name + '\".', saved_name: msg.name });
          } else if (msg.op === 'delete') {
            delete o.models[msg.name];
            write(o); publish({ notice: 'Deleted \"' + msg.name + '\".' });
          } else if (msg.op === 'get') {
            var spec = msg.kind === 'autosave' ? o.autosave : o.models[msg.name];
            if (!spec) publish({ error: 'That saved model was not found in this browser.' });
            else respond(spec, msg.kind === 'autosave' ? 'autosave' : msg.name);
          }
        }
        return { handle: handle, publish: publish };
      })();

      // Imported JSON model file -> R (the file input is reset so the same file can be imported again)
      $(document).on('change', '#model_import_file', function(e) {
        var file = e.target.files[0];
        if (!file) return;
        var reader = new FileReader();
        reader.onload = function(evt) {
          Shiny.setInputValue('model_import_json', { nonce: Date.now(), name: file.name, text: evt.target.result }, { priority: 'event' });
          e.target.value = '';
        };
        reader.onerror = function() {
          Shiny.setInputValue('model_import_json', { nonce: Date.now(), name: file.name, text: '' }, { priority: 'event' });
          e.target.value = '';
        };
        reader.readAsText(file, 'utf-8');
      });

      $(document).on('shiny:connected', function() {

        var notifyR = function(type, msg) {
          Shiny.setInputValue('client_notice', { nonce: Date.now(), type: type, msg: msg }, { priority: 'event' });
        };

        // Browser model store bridge (see structuraStore) and first status report
        Shiny.addCustomMessageHandler('structura_store', function(msg) {
          try { window.structuraStore.handle(msg); } catch (err) { notifyR('error', 'Model storage failed: ' + err.message); }
        });
        window.structuraStore.publish();

        // Generic text download (model JSON)
        Shiny.addCustomMessageHandler('structura_download_text', function(msg) {
          try {
            var name = msg.stem + (msg.label ? '_' + window.structuraSafeName(msg.label) : '') +
                       (msg.stamp ? '_' + window.structuraStamp() : '') + '.' + msg.ext;
            window.structuraSaveBlob(new Blob([msg.text], { type: msg.mime || 'text/plain;charset=utf-8' }), name);
          } catch (err) { notifyR('error', 'Download failed: ' + err.message); }
        });

        // Results ZIP: text files come from R; the path diagram (SVG + PNG) is taken from the page
        Shiny.addCustomMessageHandler('structura_build_zip', function(msg) {
          var enc = new TextEncoder();
          var entries = (msg.files || []).map(function(f) {
            var body = enc.encode(f.text);
            if (f.name.slice(-4).toLowerCase() === '.csv') {
              // UTF-8 BOM so that Excel reads non-ASCII (e.g. Japanese) headers correctly
              var withBom = new Uint8Array(body.length + 3);
              withBom.set([0xEF, 0xBB, 0xBF], 0); withBom.set(body, 3);
              body = withBom;
            }
            return { name: f.name, data: body };
          });
          var finish = function(note) {
            try {
              var blob = window.structuraBuildZip(entries);
              window.structuraSaveBlob(blob, 'structura2_results_' + window.structuraStamp() + '.zip');
              notifyR(note ? 'warning' : 'message', note || ('Results ZIP created (' + entries.length + ' files).'));
            } catch (err) { notifyR('error', 'Could not create the ZIP file: ' + err.message); }
          };
          var svg = window.structuraDiagramSvg();
          if (!svg) { finish('Results ZIP created without the path diagram (no diagram is displayed).'); return; }
          entries.push({ name: 'path_diagram.svg', data: enc.encode(svg.text) });
          window.structuraDiagramPng(2).then(function(blob) {
            return blob.arrayBuffer();
          }).then(function(buf) {
            entries.push({ name: 'path_diagram.png', data: new Uint8Array(buf) });
            finish(null);
          }).catch(function(err) {
            finish('Results ZIP created without the PNG diagram (' + err.message + '); the SVG is included.');
          });
        });

        // Printable PDF Report Assembly & Trigger
        Shiny.addCustomMessageHandler('prepare_and_print_pdf_report', function(msg) {
          var reportDiv = document.getElementById('structura-print-report');
          if (!reportDiv) return;
          
          var plotContainer = document.getElementById('sem_plot_container');
          var svgElem = plotContainer ? plotContainer.querySelector('svg') : null;
          var svgHtml = svgElem ? svgElem.outerHTML : '<p style=\"color:#666; font-style:italic;\">No diagram available</p>';

          var sectionIdx = 1;
          var html = '<div class=\"print-header\">' +
            '<div>' +
              '<h1>Structura2 Analysis Report</h1>' +
              '<div class=\"meta\" style=\"margin-top: 3px;\">Structural Insights, Simplified</div>' +
            '</div>' +
            '<div class=\"meta\" style=\"text-align: right;\">' +
              '<div><b>Generated:</b> ' + msg.timestamp + '</div>' +
              '<div><b>Mode:</b> ' + msg.analysis_mode + ' | <b>Missing:</b> ' + msg.missing_method + '</div>' +
            '</div>' +
          '</div>' +
          (msg.warning_html || '') +
          '<div class=\"print-avoid-break\">' +
            '<div class=\"print-section-title\">' + (sectionIdx++) + '. Model Fit Summary & Diagnostics</div>' +
            msg.fit_table_html +
          '</div>';

          if (msg.opt_history_html && msg.opt_history_html.trim() !== '') {
            html += '<div class=\"print-avoid-break\">' +
              '<div class=\"print-section-title\">Model Optimization History</div>' +
              msg.opt_history_html +
            '</div>';
          }

          if (msg.var_stats_html && msg.var_stats_html.trim() !== '') {
            html += '<div class=\"print-avoid-break\">' +
              '<div class=\"print-section-title\">' + (sectionIdx++) + '. Variable Summary Statistics</div>' +
              msg.var_stats_html +
            '</div>';
          }

          if (msg.latent_rel_html && msg.latent_rel_html.trim() !== '') {
            html += '<div class=\"print-avoid-break\">' +
              '<div class=\"print-section-title\">' + (sectionIdx++) + '. Latent Variable Reliability & Validity</div>' +
              msg.latent_rel_html +
            '</div>';
          }

          html += '<div class=\"print-avoid-break\">' +
            '<div class=\"print-section-title\">' + (sectionIdx++) + '. Path Diagram</div>' +
            '<div class=\"print-diagram-box\">' +
              svgHtml +
            '</div>' +
          '</div>' +
          '<div class=\"print-page-break\"></div>' +
          '<div class=\"print-avoid-break\">' +
            '<div class=\"print-section-title\">' + (sectionIdx++) + '. Parameter Estimates</div>' +
            msg.param_table_html +
          '</div>';

          if (msg.defined_effects_html && msg.defined_effects_html.trim() !== '') {
            html += '<div class=\"print-avoid-break\" style=\"margin-top: 14px;\">' +
              '<div class=\"print-section-title\">' + (sectionIdx++) + '. Defined & Indirect Effects</div>' +
              msg.defined_effects_html +
            '</div>';
          }

          html += '<div class=\"print-avoid-break\" style=\"margin-top: 14px;\">' +
            '<div class=\"print-section-title\">' + (sectionIdx++) + '. Model Syntax (lavaan)</div>' +
            '<pre class=\"print-syntax-box\">' + msg.syntax_text + '</pre>' +
          '</div>';

          if (msg.summary_text && msg.summary_text.trim() !== '') {
            html += '<div style=\"margin-top: 14px;\">' +
              '<div class=\"print-section-title\">' + (sectionIdx++) + '. Model Summary (lavaan)</div>' +
              '<pre class=\"print-syntax-box\">' + msg.summary_text + '</pre>' +
            '</div>';
          }

          reportDiv.innerHTML = html;

          setTimeout(function() {
            window.print();
          }, 150);
        });

        Shiny.addCustomMessageHandler('update_sem_plot', function(message) {
          var container = document.getElementById('sem_plot_container');
          if (!container) return;
          
          if (message.message) {
            container.style.display = 'flex';
            container.style.alignItems = 'center';
            container.style.justifyContent = 'center';
            if (message.error) {
              container.innerHTML = '<div style=\"color:red; padding:10px; text-align:center;\">' + message.message + '</div>';
            } else {
              container.innerHTML = '<div style=\"color:#666; padding:10px; text-align:center;\">' + message.message + '</div>';
            }
            return;
          }
          
          container.style.display = 'block';
          var hpccWasm = window['@hpcc-js/wasm/graphviz'];
          if (hpccWasm && hpccWasm.Graphviz) {
            hpccWasm.Graphviz.load().then(function(graphviz) {
              try {
                var svg = graphviz.layout(message.dot, 'svg', message.engine);
                container.innerHTML = svg;
                var svgElement = container.querySelector('svg');
                if (svgElement) {
                  svgElement.setAttribute('width', '100%');
                  svgElement.setAttribute('height', '100%');
                }
              } catch (err) {
                container.innerHTML = '<div style=\"color:red; padding:10px;\">Layout failed: ' + err.message + '</div>';
              }
            }).catch(function(err) {
              container.innerHTML = '<div style=\"color:red; padding:10px;\">Failed to load Graphviz WASM: ' + err.message + '</div>';
            });
          } else {
            container.innerHTML = '<div style=\"color:red; padding:10px;\">Graphviz library not loaded.</div>';
          }
        });

        Shiny.addCustomMessageHandler('update_prune_preview_plot', function(message) {
          var container = document.getElementById('prune_preview_container');
          if (!container) return;
          
          if (message.message) {
            container.style.display = 'flex';
            container.style.alignItems = 'center';
            container.style.justifyContent = 'center';
            if (message.error) {
              container.innerHTML = '<div style=\"color:red; padding:10px; text-align:center;\">' + message.message + '</div>';
            } else {
              container.innerHTML = '<div style=\"color:#666; padding:10px; text-align:center;\">' + message.message + '</div>';
            }
            return;
          }
          
          container.style.display = 'block';
          var hpccWasm = window['@hpcc-js/wasm/graphviz'];
          if (hpccWasm && hpccWasm.Graphviz) {
            hpccWasm.Graphviz.load().then(function(graphviz) {
              try {
                var svg = graphviz.layout(message.dot, 'svg', message.engine);
                container.innerHTML = svg;
                var svgElement = container.querySelector('svg');
                if (svgElement) {
                  svgElement.setAttribute('width', '100%');
                  svgElement.setAttribute('height', '100%');
                }
              } catch (err) {
                container.innerHTML = '<div style=\"color:red; padding:10px;\">Layout failed: ' + err.message + '</div>';
              }
            }).catch(function(err) {
              container.innerHTML = '<div style=\"color:red; padding:10px;\">Failed to load Graphviz WASM: ' + err.message + '</div>';
            });
          } else {
            container.innerHTML = '<div style=\"color:red; padding:10px;\">Graphviz library not loaded.</div>';
          }
        });

        // Real-time Optimization Live Canvas Line Chart Renderer
        Shiny.addCustomMessageHandler('update_optimization_live_chart', function(msg) {
          var canvas = document.getElementById('opt_chart_canvas');
          if (!canvas) return;
          var ctx = canvas.getContext('2d');
          var w = canvas.width, h = canvas.height;
          
          ctx.clearRect(0, 0, w, h);
          ctx.fillStyle = '#0f172a';
          ctx.fillRect(0, 0, w, h);
          
          var padLeft = 45, padRight = 25, padTop = 25, padBottom = 30;
          var graphW = w - padLeft - padRight;
          var graphH = h - padTop - padBottom;
          
          var scores = Array.isArray(msg.scores) ? msg.scores : (msg.scores !== null && msg.scores !== undefined ? [msg.scores] : []);
          var bestScores = Array.isArray(msg.best_scores) ? msg.best_scores : (msg.best_scores !== null && msg.best_scores !== undefined ? [msg.best_scores] : []);
          var allVals = scores.concat([msg.baseScore]).filter(function(v) { return typeof v === 'number' && !isNaN(v) && isFinite(v); });
          if (allVals.length === 0) allVals = [0, 100];
          
          var maxVal = Math.max.apply(null, allVals);
          var minVal = Math.min.apply(null, allVals);
          if (maxVal === minVal) { maxVal += 5; minVal -= 5; }
          var valRange = maxVal - minVal;
          
          ctx.strokeStyle = '#334155';
          ctx.lineWidth = 1;
          ctx.font = '10px sans-serif';
          ctx.fillStyle = '#94a3b8';
          
          for (var g = 0; g <= 4; g++) {
            var gy = padTop + (g / 4) * graphH;
            ctx.beginPath();
            ctx.moveTo(padLeft, gy);
            ctx.lineTo(w - padRight, gy);
            ctx.stroke();
            var gVal = maxVal - (g / 4) * valRange;
            ctx.fillText(gVal.toFixed(1), 5, gy + 3);
          }
          
          // No baseline line when the starting model has no score (it is not identified)
          if (typeof msg.baseScore === 'number' && isFinite(msg.baseScore)) {
            var baseY = padTop + (1 - (msg.baseScore - minVal) / valRange) * graphH;
            ctx.setLineDash([4, 4]);
            ctx.strokeStyle = '#64748b';
            ctx.beginPath();
            ctx.moveTo(padLeft, baseY);
            ctx.lineTo(w - padRight, baseY);
            ctx.stroke();
            ctx.setLineDash([]);
            ctx.fillText('Baseline', w - 50, baseY - 4);
          }
          
          var maxSteps = msg.maxIter || 80;
          
          if (scores.length > 0) {
            ctx.fillStyle = '#64748b';
            for (var i = 0; i < scores.length; i++) {
              if (isNaN(scores[i]) || !isFinite(scores[i])) continue;
              var sx = padLeft + ((i + 1) / maxSteps) * graphW;
              var sy = padTop + (1 - (scores[i] - minVal) / valRange) * graphH;
              ctx.beginPath();
              ctx.arc(sx, sy, 2, 0, 2 * Math.PI);
              ctx.fill();
            }
          }
          
          if (bestScores.length > 0) {
            ctx.strokeStyle = '#3b82f6';
            ctx.lineWidth = 2.5;
            ctx.beginPath();
            for (var j = 0; j < bestScores.length; j++) {
              if (isNaN(bestScores[j]) || !isFinite(bestScores[j])) continue;
              var bx = padLeft + (j / maxSteps) * graphW;
              var by = padTop + (1 - (bestScores[j] - minVal) / valRange) * graphH;
              if (j === 0) ctx.moveTo(bx, by); else ctx.lineTo(bx, by);
            }
            ctx.stroke();
            
            var lastIdx = bestScores.length - 1;
            var currX = padLeft + (lastIdx / maxSteps) * graphW;
            var currY = padTop + (1 - (bestScores[lastIdx] - minVal) / valRange) * graphH;
            ctx.fillStyle = '#60a5fa';
            ctx.beginPath();
            ctx.arc(currX, currY, 5, 0, 2 * Math.PI);
            ctx.fill();
          }
          
          ctx.fillStyle = '#94a3b8';
          ctx.fillText('0', padLeft, h - 10);
          ctx.fillText('Step ' + msg.step + ' / ' + maxSteps, padLeft + graphW / 2 - 25, h - 10);
          ctx.fillText(maxSteps, w - padRight - 15, h - 10);

          var badgeElem = document.getElementById('prune_iter_badge');
          if (badgeElem && msg.step !== undefined) {
            badgeElem.textContent = 'Step ' + msg.step + ' / ' + maxSteps;
          }
          
          var statusElem = document.getElementById('prune_progress_status');
          if (statusElem && msg.detail) {
            statusElem.innerHTML = msg.detail;
          }
          
          var barElem = document.getElementById('prune_progress_bar_inner');
          if (barElem) {
            var pct = Math.min(100, Math.round((msg.step / maxSteps) * 100));
            barElem.style.width = pct + '%';
          }
        });
      });

      // Ensure handsontable/DT recalculates layout when switching tabs
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
  title = "Structura2",

  # Preload overlay splash screen
  div(
    id = "structura-preload-container",
    div(class = "structura-preload-spinner"),
    h2("Structura2", style = "margin: 0; font-weight: 300; letter-spacing: 2px;"),
    p("Initializing WebR Environment...", id = "structura-preload-status", style = "color: #94a3b8; margin-top: 12px; font-size: 14px;"),
    div(
      class = "structura-preload-progress",
      div(id = "structura-preload-bar", class = "structura-preload-bar")
    )
  ),

  # Main application hidden behind this wrapper
  hidden(
    div(
      id = "structura-main-app",
      div(id = "app-logo",
          img(src = "logo.png", height = 40,
              title = "Structural Insights, Simplified")),

  tabsetPanel(id = "main_tabs",

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
             DTOutput("filtered_table")),

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
                       # -------------- Run & Auto-Optimize buttons -------------------
                       div(style = "display: flex; gap: 10px; align-items: center; margin-bottom: 10px; flex-wrap: wrap;",
                           actionButton("run_model", "Run",
                                        class = "btn btn-success",
                                        title = "Run / update the model (fit it with the current settings)"),
                           shinyjs::hidden(
                             actionButton("prune_model_btn", "Optimize",
                                          class = "btn btn-info",
                                          title = "Auto-Optimize Model: search for the most parsimonious structural paths")
                           ),
                           # PDF / ZIP / Saved Models are shown only when they are meaningful (see the visibility observer)
                           shinyjs::hidden(
                             actionButton("export_pdf_btn", "PDF",
                                          class = "btn btn-default",
                                          title = "Export PDF Report")
                           ),
                           shinyjs::hidden(
                             actionButton("export_zip_btn", "ZIP",
                                          class = "btn btn-default",
                                          title = "Download Results (ZIP): all result files and the model definition")
                           ),
                           # Opens the Saved Models dialog (browser storage + JSON); see saved_models_modal()
                           shinyjs::hidden(
                             actionButton("saved_models_btn", "Saved Models",
                                          class = "btn btn-default",
                                          title = "Save, load or delete models kept in this browser; import / export JSON")
                           )
                       ),
                      shinyjs::hidden(
                        div(id = "latent_error_box",
                            class = "alert alert-danger",
                            style = "margin-top: 10px; font-weight: bold;",
                            textOutput("latent_error_msg"))
                      ),
                      tags$hr(),
                      h4("Measurement Model"),
                      div(style = "margin-top: 10px; margin-bottom: 15px;",
                          rHandsontableOutput("input_table"),
                          actionButton("add_row", "Add Row", class = "btn btn-primary", style = "margin-top: 10px;")
                      ),
                      tags$hr(),
                      div(style = "display: flex; justify-content: space-between; align-items: center; margin-bottom: 4px; flex-wrap: wrap; gap: 8px;",
                          h4("Structural Model", style = "margin: 0; font-weight: 600;"),
                          div(style = "display: flex; align-items: center; gap: 12px; flex-wrap: wrap;",
                              div(style = "margin-bottom: 0;",
                                  checkboxInput("show_suggested_paths", "Highlight top N paths by Modification Indices", value = TRUE, width = "auto")),
                              conditionalPanel(
                                condition = "input.show_suggested_paths",
                                div(style = "display: flex; align-items: center; gap: 6px;",
                                    tags$span("N:", style = "font-weight: 600;"),
                                    div(style = "margin-bottom: -15px;",
                                        numericInput("max_suggestions", NULL, value = 5, min = 1, step = 1, width = "70px")))
                              )
                          )
                      ),
                      p("Color intensity indicates R² strength (white: low, red: high). ",
                        tags$span(style = "color: #2563eb; font-weight: 600;", "Blue border"),
                        " indicates recommended paths based on Modification Indices (thick border + #1 = strongest; ",
                        tags$span(style = "color: #d97706; font-weight: 600;", "orange border"),
                        " = would create a feedback loop). Add ONE path at a time and re-run the model, because MI values change after every change. ",
                        "Use as exploratory reference alongside theoretical knowledge.",
                        style = "font-size: 12px; color: #666; margin-bottom: 10px;"),
                      uiOutput("suggestion_status_ui"),
                      rHandsontableOutput("checkbox_matrix"),
                      tags$hr(),
                      h4("Manual Equations"),
                      div(style = "margin-top: 10px;",
                          textAreaInput("extra_eq",
                                        "Additional lavaan syntax (one formula per line):",
                                        value = "",
                                        placeholder = "y1 ~ x1 + x2\nlatent2 =~ y3 + y4",
                                        width = "100%",
                                        rows = 4,
                                        resize = "vertical")
                      ),
                      tags$hr(),
                      h4("lavaan Syntax"),
                      verbatimTextOutput("lavaan_model")
               ),

               # ---------- Right column (outputs) -----------------
               column(width = 5,
                      # ---------- Fit message (above the tabs so it is visible on every tab) ----------
                      shinyjs::hidden(
                        div(id = "fit_alert_box",
                            textOutput("fit_alert"),
                            class = "alert-box")
                      ),
                      # ---------- Tabset for outputs ---------------
                      tabsetPanel(id = "right_tabs", type = "tabs",

                                  # ----- Diagnostics tab ---------------------
                                  tabPanel("Diagnostics",
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
                      div(style = "display: flex; justify-content: space-between; align-items: center; margin-bottom: 6px; margin-top: 10px;",
                          h4("Path Diagram", style = "margin: 0; font-weight: 600;"),
                          div(style = "display: flex; gap: 6px;",
                              actionButton("save_diagram_svg", "Save SVG", class = "btn btn-default btn-xs",
                                           onclick = "downloadSemDiagramSvg()"),
                              actionButton("save_diagram_png", "Save PNG", class = "btn btn-default btn-xs",
                                           onclick = "downloadSemDiagramPng(2)")
                          )
                      ),
                      div(style = "height:60vh; overflow-y:auto; overflow-x:hidden; border:1px solid #ccc; position: relative;",
                          tags$div(id = "sem_plot_container", 
                                   style = "width:100%; height:100%; display: flex; align-items: center; justify-content: center; color: #666;",
                                   "Define a model to view the path diagram."))
               )
             )),

    # ---------------- Details tab --------------------------------
    tabPanel("Details",
             div(style = "display: flex; justify-content: space-between; align-items: center; margin-top: 10px; margin-bottom: 12px; flex-wrap: wrap; gap: 10px;",
                 h4("Parameter Estimates", style = "margin: 0; font-weight: 600;"),
                 div(style = "display: flex; gap: 20px; align-items: center;",
                     div(style = "margin-bottom: 0;",
                         checkboxInput("param_show_std", "Include Standardized (std.all)", value = FALSE)
                     ),
                     div(style = "display: flex; align-items: center; gap: 8px;",
                         tags$label("Decimals:", `for` = "param_digits", style = "margin: 0; font-size: 13px; color: #475569; font-weight: 500;"),
                         div(style = "width: 130px; margin-bottom: 0;",
                             selectInput("param_digits", NULL,
                                         choices = c("2" = "2", "3 (Default)" = "3", "4" = "4", "All (Raw)" = "all"),
                                         selected = "3", width = "100%")
                         )
                     )
                 )
             ),
             uiOutput("ident_note"),
             DTOutput("param_tbl"),
             tags$hr(),
             h4("Model Summary"),
             verbatimTextOutput("fit_summary")),

    # ---------------- Help tab -----------------------------------
    tabPanel("Help", if (file.exists("help.html")) {
      HTML(paste(readLines("help.html", encoding = "UTF-8", warn = FALSE), collapse = "
"))
    } else {
      includeMarkdown("help.md")
    })
  ), # end tabsetPanel
  div(id = "structura-print-report")
  ) # end div (structura-main-app)
  ) # end hidden
) # end fluidPage
report_startup_stage("ui_built")

# ================================================================
# SERVER
# ================================================================

server <- function(input, output, session) {
  report_startup_stage("session_start")

  # Session-start sequence triggered on Shiny session connection
  observeEvent(TRUE, {
    tryCatch({
      # Complete the progress bar and transition out successfully
      runjs("if (window.parent) { window.parent.postMessage({ type: 'structura-ready' }, '*'); }")
      runjs("if (window.finishStructuraPreload) { window.finishStructuraPreload(true); } else { $('#structura-preload-container').hide(); }")
      shinyjs::show("structura-main-app")
      
      # Show the initial load data modal dialog after dependencies are loaded
      showModal(
        modalDialog(
          title = "Load Data",
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

      # Attach lavaan only after the dialog has been sent to the browser. R is single-threaded,
      # so any click made while lavaan loads is queued and handled once the engine is ready.
      lavaan_nid <- showNotification("Loading SEM engine (lavaan)...", duration = NULL,
                                     closeButton = FALSE, session = session)
      session$onFlushed(function() {
        tryCatch({
          # Attach lavaan explicitly (direct call to bypass WebR VFS bugs)
          library(lavaan)
          patch_lavaan_option_cache()
          report_startup_stage("lavaan_loaded")
          removeNotification(lavaan_nid, session = session)
        }, error = function(e) {
          removeNotification(lavaan_nid, session = session)
          showNotification(
            paste("Could not load the SEM engine (lavaan):", conditionMessage(e),
                  "Please reload the page."),
            type = "error", duration = NULL, session = session)
        })
      }, once = TRUE)
    }, error = function(e) {
      err_msg <- gsub("'", "\\'", e$message, fixed = TRUE)
      err_msg <- gsub("\n", " ", err_msg, fixed = TRUE)
      runjs(sprintf("if (window.finishStructuraPreload) { window.finishStructuraPreload(false, '%s'); }", err_msg))
      runjs(sprintf("if (window.parent) { window.parent.postMessage({ type: 'structura-error', message: '%s' }, '*'); }", err_msg))
      warning("Structura2 startup failed: ", e$message)
    })
  }, once = TRUE)

  # Prefetch the deferred regsem packages as soon as the strategy is selected, so the
  # (synchronous) download happens while a notification is visible rather than mid-optimization.
  observeEvent(input$prune_strategy, {
    req(identical(input$prune_strategy, "regsem"))
    if (nzchar(system.file(package = regsem_pkg))) return()
    nid <- showNotification("Loading regularized SEM engine (regsem)...", duration = NULL,
                            closeButton = FALSE, session = session)
    session$onFlushed(function() {
      ok <- ensure_regsem_loaded()
      removeNotification(nid, session = session)
      if (!ok) {
        showNotification("Could not load regsem. Check your network connection and reload the page.",
                         type = "error", duration = 10, session = session)
      }
    }, once = TRUE)
  }, ignoreInit = TRUE)

  data <- reactiveVal(NULL)
  data_label <- reactiveVal("")   # file name or demo dataset name; stored in model specs for reference only

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
      if (!any(vapply(df, is.numeric, logical(1)))) {
        stop("The loaded dataset has no numeric columns. Check the delimiter and that the file is a CSV with numeric variables.")
      }
      data_label(as.character(input$datafile_utf8$name %||% ""))
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
    data_label(input$sample_ds)
    data(ds)
    removeModal()
  })

  output$datatable <- renderDT({
    req(data())
    df <- data()
    dt <- datatable(df, filter = "top", editable = FALSE,
                    options = list(pageLength = 30, autoWidth = TRUE, scrollX = TRUE),
                    rownames = FALSE)
    
    # Format floating-point numeric columns to 3 decimal places while preserving integers (e.g., ID, grade)
    is_float_col <- function(x) is.numeric(x) && any(!is.na(x) & (x %% 1 != 0))
    float_cols <- names(df)[sapply(df, is_float_col)]
    
    if (length(float_cols) > 0) {
      dt <- dt %>% formatRound(columns = float_cols, digits = 3)
    }
    dt
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
  # Rendered even while the Filtered tab is hidden, so a model restore can set the selection
  outputOptions(output, "log_transform_ui", suspendWhenHidden = FALSE)

  # Column-name shape (log10 renames / one-hot dummy names) computed from the full,
  # unfiltered dataset. Kept independent of input$datatable_rows_all so UI elements that
  # only need the resulting column set (e.g. display_column_ui) don't lose their selection
  # or re-render every time the user filters/searches the preview table.
  processed_columns_full <- reactive({
    req(data())
    tryCatch({
      df <- data()
      df[] <- lapply(df, function(x) if (is.factor(x)) as.character(x) else x)

      # --- log10 transform (renames columns only) -------------------
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

      # --- one-hot encode --------------------------------------------
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
      df
    }, error = function(e) {
      warning(paste("Data preprocessing failed:", e$message))
      data.frame()
    })
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
        if (length(num_cols) > 0) {
          mat_vals <- as.matrix(df[, num_cols, drop = FALSE])
          sds <- apply(mat_vals, 2, sd, na.rm = TRUE)
          zero_var <- which(is.na(sds) | sds < 1e-12)
          non_zero_var <- setdiff(seq_along(num_cols), zero_var)
          if (length(non_zero_var) > 0) {
            mat_vals[, non_zero_var] <- scale(mat_vals[, non_zero_var, drop = FALSE])
          }
          if (length(zero_var) > 0) {
            mat_vals[, zero_var] <- scale(mat_vals[, zero_var, drop = FALSE], scale = FALSE)
          }
          df[, num_cols] <- as.data.frame(mat_vals)
        }
      }
      df
    }, error = function(e) {
      warning(paste("Data preprocessing failed:", e$message))
      data.frame()
    })
  })

  output$display_column_ui <- renderUI({
    df <- processed_columns_full(); req(df)

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
  outputOptions(output, "display_column_ui", suspendWhenHidden = FALSE)

  output$filtered_table <- renderDT({
    df <- processed_data(); req(df)
    if (!is.null(input$display_columns))
      df <- df[, intersect(input$display_columns, names(df)), drop = FALSE]
    
    dt <- datatable(df, filter = "top", editable = FALSE,
                    options = list(pageLength = 30, autoWidth = TRUE, scrollX = TRUE),
                    rownames = FALSE)
    
    # Format floating-point numeric columns to 3 decimal places with aligned trailing zeros
    is_float_col <- function(x) is.numeric(x) && any(!is.na(x) & (x %% 1 != 0))
    float_cols <- names(df)[sapply(df, is_float_col)]
    
    if (length(float_cols) > 0) {
      dt <- dt %>% formatRound(columns = float_cols, digits = 3)
    }
    dt
  }, server = FALSE)

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
    df <- if (!is.null(input$input_table)) hot_to_r(input$input_table) else input_table_data()
    if (!is.null(df)) {
      current_inds <- setdiff(colnames(df), c("Latent", "Indicator", "Operator"))
      if (identical(sort(current_inds), sort(inds))) {
        return()
      }
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
    tbl$Latent    <- ifelse(nzchar(trimws(tbl$Latent)), make.names(tbl$Latent, unique = FALSE), "")
    
    obs_names <- names(processed_data())
    latent_names <- tbl$Latent[nzchar(tbl$Latent)]
    
    has_conflict <- FALSE
    conflict_msg <- ""
    
    # 1. Check duplicate name with observed variables in dataset
    conflicting_with_obs <- intersect(latent_names, obs_names)
    if (length(conflicting_with_obs) > 0) {
      has_conflict <- TRUE
      conflict_msg <- sprintf("Error: Latent variable names cannot be the same as observed variables in the dataset: %s", 
                              paste(conflicting_with_obs, collapse = ", "))
    }
    
    # 2. Check duplicate name with other latent variables
    if (!has_conflict && any(duplicated(latent_names))) {
      has_conflict <- TRUE
      duplicated_names <- unique(latent_names[duplicated(latent_names)])
      conflict_msg <- sprintf("Error: Latent variable names must be unique. Duplicate names found: %s", 
                              paste(duplicated_names, collapse = ", "))
    }
    
    # UI control for validation output and model execution button state
    if (has_conflict) {
      shinyjs::disable("run_model")
      output$latent_error_msg <- renderText(conflict_msg)
      shinyjs::show("latent_error_box")
    } else {
      shinyjs::enable("run_model")
      shinyjs::hide("latent_error_box")
    }
    
    valid_latents <- ifelse(nzchar(tbl$Latent), tbl$Latent, "latent")
    convs         <- make.unique(c(obs_names, valid_latents))
    tbl$Indicator <- ifelse(nzchar(tbl$Latent), tail(convs, nrow(tbl)), "")
    input_table_data(tbl)
  })

  observeEvent(input$add_row, {
    df <- if (!is.null(input$input_table)) hot_to_r(input$input_table) else input_table_data()
    req(df)
    new_row            <- df[1, ]
    new_row[,]         <- FALSE
    new_row$Latent     <- ""
    new_row$Indicator  <- ""
    new_row$Operator   <- "=~"
    combined           <- rbind(df, new_row)
    obs_names          <- names(processed_data())
    valid_latents      <- ifelse(nzchar(combined$Latent), combined$Latent, "latent")
    convs              <- make.unique(c(obs_names, valid_latents))
    combined$Indicator <- ifelse(nzchar(combined$Latent), tail(convs, nrow(combined)), "")
    input_table_data(combined)
    input_table_trigger(input_table_trigger() + 1)
  })

  # ---------- Helper function for R² calculation -----------------
  
  compute_r2_matrix <- function(data, dep_vars, pred_vars) {
    r2_matrix <- matrix(0, nrow = length(dep_vars), ncol = length(pred_vars),
                        dimnames = list(dep_vars, pred_vars))
    
    all_vars <- union(dep_vars, pred_vars)
    available_vars <- intersect(all_vars, names(data))
    numeric_vars <- available_vars[sapply(data[available_vars], is.numeric)]
    
    if (length(numeric_vars) > 1) {
      tryCatch({
        cor_matrix <- cor(data[numeric_vars], use = "pairwise.complete.obs")
        r2_full <- cor_matrix^2
        r2_full[!is.finite(r2_full)] <- 0
        
        dep_in <- intersect(dep_vars, numeric_vars)
        pred_in <- intersect(pred_vars, numeric_vars)
        if (length(dep_in) > 0 && length(pred_in) > 0) {
          r2_matrix[dep_in, pred_in] <- r2_full[dep_in, pred_in]
        }
      }, error = function(e) {
        # Keep 0 on error
      })
    }
    diag(r2_matrix) <- 0
    r2_matrix
  }

  cached_r2_matrix <- reactive({
    df <- processed_data(); req(df)
    items <- model_items()
    if (!length(items)) return(NULL)
    compute_r2_matrix(df, items, items)
  })

  # ---------- Structural table -----------------------------------

  struct_table_data <- reactiveVal(NULL)
  struct_table_trigger <- reactiveVal(0)

  model_items <- reactive({
    df <- processed_data(); req(df)
    deps <- as.character(input$display_columns %||% names(df))
    meas <- input_table_data(); req(meas)
    vars <- setdiff(names(meas), c("Latent", "Indicator", "Operator"))
    convs <- character(0)
    if (length(vars)) {
      row_has_indicator <- apply(meas[vars], 1, function(x) any(as.logical(x), na.rm = TRUE))
      convs <- setdiff(na.omit(unique(meas$Indicator[row_has_indicator])), "")
    }
    unique(c(deps, convs))
  })

  observeEvent(model_items(), {
    items <- model_items()
    if (!length(items)) {
      struct_table_data(NULL)
      struct_table_trigger(struct_table_trigger() + 1)
      return()
    }
    
    old_tbl <- if (!is.null(input$checkbox_matrix)) hot_to_r(input$checkbox_matrix) else struct_table_data()
    
    # Create new matrix
    mat <- data.frame(Dependent = items, Operator = "~", stringsAsFactors = FALSE)
    for (col in items) mat[[col]] <- FALSE
    
    # Preserve old settings if available
    if (!is.null(old_tbl)) {
      common_deps <- intersect(old_tbl$Dependent, items)
      common_cols <- intersect(setdiff(colnames(old_tbl), c("Dependent", "Operator")), items)
      if (length(common_deps) > 0 && length(common_cols) > 0) {
        for (dep in common_deps) {
          old_row_idx <- which(old_tbl$Dependent == dep)
          new_row_idx <- which(mat$Dependent == dep)
          if (length(old_row_idx) == 1 && length(new_row_idx) == 1) {
            for (cc in common_cols) {
              val <- old_tbl[old_row_idx, cc]
              mat[new_row_idx, cc] <- isTRUE(as.logical(val))
            }
          }
        }
      }
    }
    struct_table_data(mat)
    struct_table_trigger(struct_table_trigger() + 1)
  })

  observeEvent(input$checkbox_matrix, {
    tbl <- hot_to_r(input$checkbox_matrix); req(tbl)
    pred_cols <- setdiff(colnames(tbl), c("Dependent", "Operator"))
    for (col in pred_cols) {
      tbl[[col]] <- vapply(tbl[[col]], function(x) isTRUE(as.logical(x)), logical(1))
    }
    struct_table_data(tbl)
  })

  # Reactive cache for modification indices suggestions to eliminate heavy recalculation in checkbox_matrix
  cached_suggested_matrix <- reactiveVal(NULL)

  # Fit that modification indices are read from: the user's fitted model, extended with any unused
  # numeric items attached via fixed-zero regressions so those items can be suggested too.
  # Refitted only when the model is re-run, not when the MI/EPC thresholds change.
  suggestion_fit <- reactive({
    if (!isTRUE(input$show_suggested_paths)) return(NULL)
    model_res <- tryCatch(fit_model_safe(), error = function(e) NULL)
    if (is.null(model_res) || !isTRUE(model_res$ok) || is.null(model_res$fit)) return(NULL)
    # Modification indices of an unidentified fit are not meaningful
    if (isFALSE(model_res$identified)) return(NULL)
    items <- tryCatch(model_items(), error = function(e) character(0))
    df <- tryCatch(processed_data(), error = function(e) NULL)
    if (is.null(df)) return(model_res$fit)
    fitted_ov <- lavaan::lavNames(model_res$fit, "ov")
    unused <- setdiff(items, fitted_ov)
    unused <- unused[unused %in% names(df)]
    unused <- unused[vapply(unused, function(v) is.numeric(df[[v]]), logical(1))]
    if (!length(unused) || !length(fitted_ov)) return(model_res$fit)
    needs_meanstructure <- (isolate(input$analysis_mode) == "raw" ||
                            isolate(input$missing_method) %in% c("ml", "ml.x", "two.stage", "robust.two.stage"))
    ctx <- list(data = df, missing_method = isolate(input$missing_method),
                needs_meanstructure = needs_meanstructure)
    endo_ov <- lavaan::lavNames(model_res$fit, "ov.y")
    anchor_var <- if (length(endo_ov)) endo_ov[1] else fitted_ov[1]
    aug <- tryCatch(fit_suggestion_model(model_res$syntax, unused, anchor_var, ctx), error = function(e) NULL)
    if (is.null(aug)) model_res$fit else aug   # fall back to the plain fit if the helper fit fails
  })

  observe({
    show_sug <- isTRUE(input$show_suggested_paths)
    if (!show_sug) {
      cached_suggested_matrix(NULL)
      return()
    }
    model_res <- tryCatch(fit_model_safe(), error = function(e) NULL)
    sug_fit <- tryCatch(suggestion_fit(), error = function(e) NULL)
    model_items() # establish dependency so the cache is invalidated when structural items (rows/cols) change shape
    max_sug <- input$max_suggestions %||% 5
    if (!is.numeric(max_sug) || is.na(max_sug) || max_sug < 1) max_sug <- 5
    # Matching is by variable name against the current table, so paths the user has just ticked
    # (but not yet fitted) are no longer suggested; suggestion_status_ui flags the stale state.
    mat <- isolate(struct_table_data())
    if (!is.null(model_res) && isTRUE(model_res$ok) && !is.null(sug_fit) && !is.null(mat)) {
      sug <- tryCatch(get_suggested_structural_paths(sug_fit, mat, mi_threshold = 0, epc_threshold = 0,
                                                    max_paths = max_sug),
                      error = function(e) NULL)
      cached_suggested_matrix(sug)
    } else {
      cached_suggested_matrix(NULL)
    }
  })

  # Tells the user when the highlighted suggestions no longer describe the displayed structure
  output$suggestion_status_ui <- renderUI({
    if (!isTRUE(input$show_suggested_paths)) return(NULL)
    model_res <- tryCatch(fit_model_safe(), error = function(e) NULL)
    current <- tryCatch(lavaan_model_str(), error = function(e) NULL)
    if (is.null(model_res) || !isTRUE(model_res$ok) || is.null(model_res$syntax)) {
      return(div(class = "alert alert-info", style = "font-size: 12px; padding: 6px 10px; margin-bottom: 8px;",
                 "Suggestions appear after the model has been fitted successfully (Run)."))
    }
    if (!identical(model_res$syntax, current)) {
      return(div(class = "alert alert-warning", style = "font-size: 12px; padding: 6px 10px; margin-bottom: 8px;",
                 "The structure has changed since the last fit. Highlighted suggestions are from the previous fit; ",
                 "click ", tags$b("Run"), " to refresh them."))
    }
    NULL
  })

  output$checkbox_matrix <- renderRHandsontable({
    struct_table_trigger()
    tryCatch({
      df <- isolate(processed_data()); req(df)
      meas <- isolate(input_table_data()); req(meas)
      mat <- isolate(struct_table_data()); req(mat)
      items <- mat$Dependent
      if (!length(items)) return()
      
      # Use cached R2 matrix, preventing recalculations on checkbox click
      r2_matrix <- cached_r2_matrix(); req(r2_matrix)
      
      rh <- rhandsontable(mat, rowHeaders = FALSE) %>%
        hot_table(highlightReadOnly = TRUE, fixedColumnsLeft = 2)
      rh <- hot_col(rh, "Dependent", readOnly = TRUE)
      rh <- hot_col(rh, "Operator",  readOnly = TRUE)
      
      # Store R2 matrix once in the widget payload
      rh$x$r2_matrix <- r2_matrix
      
      # Use cached suggested paths matrix if enabled
      suggested_matrix <- if (isTRUE(input$show_suggested_paths)) cached_suggested_matrix() else NULL
      rh$x$suggested_matrix <- suggested_matrix
      
      # Define static JS renderer referencing the shared R2 matrix and suggested paths matrix
      # (Note: Col index offset is -2 because 'Dependent' and 'Operator' columns are on the left)
      renderer_js <- "
        function(instance, td, row, col, prop, value, cellProperties) {
          Handsontable.renderers.CheckboxRenderer.apply(this, arguments);
          var params = instance.params || instance.getSettings();
          var r2_matrix = params.r2_matrix;
          var suggested_matrix = params.suggested_matrix;
          var col_var_idx = col - 2;
          
          td.style.boxShadow = '';
          td.style.backgroundColor = '';
          td.style.cursor = '';
          td.classList.remove('htDimmed');
          if (td.title && td.title.indexOf('Suggested Path') !== -1) {
            td.title = '';
          }
          var chk = td.querySelector('input');
          if (chk) {
            chk.style.outline = '';
            chk.style.outlineOffset = '';
            chk.style.borderRadius = '';
          }
          
          if (r2_matrix && col_var_idx >= 0 && col_var_idx < r2_matrix.length) {
            var r2_val = r2_matrix[row][col_var_idx];
            if (r2_val !== undefined && r2_val !== null && r2_val > 0) {
              var red_intensity = Math.min(1, r2_val);
              var r = 255;
              var g = Math.round(255 - red_intensity * 0.7 * 255);
              var b = Math.round(255 - red_intensity * 0.7 * 255);
              td.style.backgroundColor = 'rgb(' + r + ',' + g + ',' + b + ')';
            }
          }
          
          if (row === col_var_idx) {
            cellProperties.readOnly = true;
            td.style.backgroundColor = '#f0f0f0';
            td.style.cursor = 'not-allowed';
            td.classList.add('htDimmed');
            return;
          }
          
          if (suggested_matrix && row < suggested_matrix.length && col_var_idx >= 0 && col_var_idx < suggested_matrix[row].length) {
            var sug = suggested_matrix[row][col_var_idx];
            if (sug && !value) {
              var is_top = (sug.rank === 1);
              var is_cyclic = (sug.cyclic === true);
              var sug_color = is_cyclic ? '#d97706' : '#2563eb';
              td.style.boxShadow = 'inset 0 0 0 ' + (is_top ? '4px ' : '2.5px ') + sug_color;
              if (chk) {
                chk.style.outline = '2px solid ' + sug_color;
                chk.style.outlineOffset = '1px';
                chk.style.borderRadius = '3px';
              }
              var tipText = 'Suggested Path to Add' + (sug.rank ? ' (#' + sug.rank + ' by MI)' : '') +
                            ':\\nMI: ' + Number(sug.mi).toFixed(2) + ' (Chi-sq drop)';
              if (sug.std_epc !== null && sug.std_epc !== undefined && !isNaN(sug.std_epc)) {
                tipText += '\\nstd.EPC: ' + Number(sug.std_epc).toFixed(3);
              }
              if (is_cyclic) {
                tipText += '\\nWARNING: would create a feedback loop (non-recursive model).';
              }
              tipText += '\\nAdd one path at a time, then re-run the model.';
              td.title = tipText;
            }
          }
        }"
      
      for (col_name in items) {
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
    mlines <- build_meas_lines(meas)
    struc <- struct_table_data(); req(struc)
    slines <- build_struct_lines(struc)
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

  # Analysis settings that belong to a model spec (read under isolate by callers)
  current_settings <- function() {
    list(
      analysis_mode        = input$analysis_mode %||% "std",
      missing_method       = input$missing_method %||% "listwise",
      log_columns          = as.list(as_chr(input$log_columns)),
      display_columns      = as.list(as_chr(input$display_columns)),
      layout_style         = input$layout_style %||% "dot_LR",
      diagram_std          = isTRUE(input$diagram_std),
      show_suggested_paths = isTRUE(input$show_suggested_paths),
      max_suggestions      = input$max_suggestions %||% 5
    )
  }

  # Spec of the model currently defined in the tables (used at fit time and for named saves)
  make_model_spec <- function(df_used, name = "") {
    meas <- if (!is.null(input$input_table)) hot_to_r(input$input_table) else input_table_data()
    build_model_spec(
      meas, struct_table_data(), input$extra_eq, current_settings(),
      list(name = data_label(), columns = names(data()), nrow_used = nrow(df_used)),
      name = name)
  }

  fit_model_safe <- eventReactive(input$run_model, {
    ln <- isolate(lavaan_model_str())
    if (length(ln) == 0) {
      msg <- if (input$run_model > 0) "Define a model to proceed." else ""
      return(list(ok = FALSE,
                  msg_friendly = msg,
                  msg_level = "info",
                  fail_kind = "empty",
                  fit = NULL,
                  syntax = NULL,
                  pe_std = NULL,
                  pe_raw = NULL,
                  fit_measures = NULL,
                  equations = NULL))
    }
    tryCatch({
      # Use meanstructure = TRUE if FIML is selected to prevent lavaan error
      needs_meanstructure <- (input$analysis_mode == "raw" || 
                              input$missing_method %in% c("ml", "ml.x", "two.stage", "robust.two.stage"))

      snapshot_error <- NULL
      df_fit <- processed_data()
      syntax_chr <- paste(ln, collapse = "\n")

      # Impossible situations are reported before estimation, naming the variables involved.
      # df < 0 is NOT one of them: lavaan still estimates, and the result is shown flagged as not identified.
      pre_errs <- diagnose_fit_inputs(syntax_chr, df_fit, input$missing_method, needs_meanstructure)
      pre_kind <- attr(pre_errs, "kind") %||% ""
      if (length(pre_errs) > 0 && !identical(pre_kind, "identification")) {
        return(list(ok = FALSE,
                    msg_friendly = paste(pre_errs, collapse = "

"),
                    fail_kind = "data",
                    fit = NULL, syntax = NULL, pe_std = NULL, pe_raw = NULL,
                    fit_measures = NULL, equations = NULL))
      }

      fm <- run_lavaan_sem(syntax_chr, df_fit,
                           input$missing_method, needs_meanstructure)

      converged <- isTRUE(lavInspect(fm, "converged"))
      post <- if (converged) diagnose_fit_results(fm, df_fit) else list(errors = character(0), warnings = character(0), ident = NULL)
      # Not identified: df < 0 (pre-check) or no standard errors (post-check); one message is enough
      ident_msg <- if (length(pre_errs) > 0) paste(pre_errs, collapse = "

") else post$ident
      identified <- !converged || is.null(ident_msg)
      pe_std <- NULL
      pe_raw <- NULL
      fit_meas <- NULL
      eqs <- NULL
      if (converged) {
        pe_std <- tryCatch(parameterEstimates(fm, standardized = TRUE), error = function(e) NULL)
        pe_raw <- tryCatch(parameterEstimates(fm, standardized = FALSE, remove.def = FALSE), error = function(e) NULL)
        fit_meas <- tryCatch(
          fitMeasures(fm, c("nobs", "chisq", "df", "pvalue", "srmr", "rmsea", "gfi", "agfi", "nfi", "cfi", "aic", "bic")),
          error = function(e) NULL
        )
        eqs <- tryCatch(lavaan_to_equations(fm, cached_pe = pe_raw), error = function(e) character(0))
      }
      other_warnings <- if (length(post$warnings) > 0) paste0("- ", paste(post$warnings, collapse = "
- ")) else NULL

      list(ok = converged,
           identified = identified,
           # Only a NEGATIVE df is a df violation to mark in red; no standard errors with df >= 0 is reported by the
           # message box alone (p, NFI and CFI can still be computed there)
           ident_df = if (!identified && isTRUE(fit_identification(fm)$df < 0)) fit_identification(fm)$df else NA_real_,
           fail_kind = if (converged) "" else "convergence",
           msg_title = if (!identified) "The model is not identified. Results are shown for inspection only." else NULL,
           msg_level = if (!identified) "error" else NULL,
           msg_friendly = if (!identified)
             paste(c(ident_msg, optimize_tip(struct_table_data()),
                     if (!is.null(other_warnings)) paste0("Other warnings:
", other_warnings)), collapse = "

")
           else if (converged)
             (if (!is.null(other_warnings))
               paste0("Results are shown but may be unreliable:
", other_warnings)
              else "")
           else
             paste(c(pre_errs, "Model did not converge. Check for variables with correlation = 1 and remove or combine them."),
                   collapse = "\n\n"),
           fit = fm,
           syntax = ln,
           pe_std = pe_std,
           pe_raw = pe_raw,
           fit_measures = fit_meas,
           equations = eqs,
           # Data and model spec as they were at fit time: autosave, ZIP and PDF describe this fit,
           # not whatever the inputs say when the export button is pressed
           fit_data = df_fit,
           snapshot = if (converged) tryCatch(make_model_spec(df_fit), error = function(e) {
             snapshot_error <<- conditionMessage(e)
             NULL
           }) else NULL,
           snapshot_error = snapshot_error)
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
      } else {
        friendly_msg <- paste0("Estimation failed: ", error_msg, ". Try: (1) Check for perfect correlations between variables, (2) Ensure sufficient sample size, or (3) Simplify the model structure.")
      }
      
      list(ok = FALSE,
           msg_friendly = paste0(friendly_msg, "\n\nTechnical details: ", error_msg),
           fail_kind = "error",
           fit = NULL,
           syntax = NULL,
           pe_std = NULL,
           pe_raw = NULL,
           fit_measures = NULL,
           equations = NULL)
    })
  }, ignoreNULL = FALSE)  # Initial auto-execution

  output$fit_alert <- renderText({
    model <- fit_model_safe()
    msg <- model$msg_friendly
    if (nzchar(msg)) {
      level <- model$msg_level %||% if (isTRUE(model$ok)) "warning" else "error"
      for (lv in c("error", "warning", "info")) {
        if (identical(lv, level)) shinyjs::addClass("fit_alert_box", paste0("alert-box-", lv))
        else shinyjs::removeClass("fit_alert_box", paste0("alert-box-", lv))
      }
      shinyjs::show("fit_alert_box")
      if (identical(level, "error")) paste0(model$msg_title %||% "Model could not be estimated.", "\n", msg) else msg
    } else {
      shinyjs::hide("fit_alert_box")
      ""
    }
  })
  # The box starts hidden; Shiny would never render a hidden output, so it could never reveal itself
  outputOptions(output, "fit_alert", suspendWhenHidden = FALSE)

  # Short placeholder for panels that cannot show results; the full text is in the message box above the tabs
  unavailable_msg <- function(model) {
    if (nzchar(model$msg_friendly) && !identical(model$msg_level, "info")) "Not available. See the message above." else ""
  }

  output$fit_indices <- renderDT({
    model <- fit_model_safe()
    validate(need(model$ok, unavailable_msg(model)))
    ms <- model$fit_measures
    if (is.null(ms)) {
      fit <- model$fit
      ms  <- fitMeasures(fit, c("pvalue","srmr","rmsea","aic","bic",
                                "gfi","agfi","nfi","cfi"))
    }
    vals <- round(as.numeric(ms[c("pvalue","srmr","rmsea","aic","bic","gfi","agfi","nfi","cfi")]), 3)
    names(vals) <- c("pvalue","srmr","rmsea","aic","bic","gfi","agfi","nfi","cfi")
    thr <- c(pvalue = .05, srmr = .08, rmsea = .08,
             gfi = .90, agfi = .90, nfi = .90, cfi = .90)
    # Not identified: lavaan's values are shown as they are; the ones that cannot be computed (p, NFI, CFI)
    # and the df are red
    unident <- isFALSE(model$identified) && is.finite(model$ident_df)
    fmt <- function(idx, v) {
      ok <- switch(idx,
                   pvalue = v >= thr["pvalue"],
                   srmr   = v <= thr["srmr"],
                   rmsea  = v <= thr["rmsea"],
                   gfi    = v >= thr["gfi"],
                   agfi   = v >= thr["agfi"],
                   nfi    = v >= thr["nfi"],
                   cfi    = v >= thr["cfi"], TRUE)
      if (is.na(v)) (if (unident) '<span style="color:red;">NA</span>' else "NA")
      else if (!ok) sprintf('<span style="color:red;">%.3f</span>', v)
      else sprintf('%.3f', v)
    }
    html_vals <- mapply(fmt, names(vals), vals, USE.NAMES = FALSE)
    tbl <- as.data.frame(t(html_vals), stringsAsFactors = FALSE)
    colnames(tbl) <- toupper(names(vals))
    if (unident) {
      df_txt <- if (is.finite(model$ident_df)) format(model$ident_df) else "NA"
      tbl <- cbind(DF = sprintf('<span style="color:red;font-weight:bold;">%s</span>', df_txt), tbl)
    }
    datatable(tbl, escape = FALSE, rownames = FALSE,
              options = list(dom = 't'))
  })

  # ----------------- Approximate Equations ----------------------
  output$approx_eq <- renderText({
    if (input$analysis_mode == "std")
      return("— Hidden in Standardized mode —")
    model <- fit_model_safe()
    validate(need(model$ok, unavailable_msg(model)))
    if (!is.null(model$equations)) {
      paste(model$equations, collapse = "\n")
    } else {
      paste(lavaan_to_equations(model$fit), collapse = "\n")
    }
  })

  # Details tab: the message box sits on the Model tab, so a not-identified model is flagged here as well
  output$ident_note <- renderUI({
    model <- tryCatch(fit_model_safe(), error = function(e) NULL)
    if (is.null(model) || !isFALSE(model$identified)) return(NULL)
    df_txt <- if (is.finite(model$ident_df)) sprintf(" (df = %s)", format(model$ident_df)) else ""
    div(style = "color: #b91c1c; font-weight: 600; font-size: 13px; margin-bottom: 8px;",
        paste0("The model is not identified", df_txt, ": standard errors and fit indices are unavailable or unreliable."))
  })

  output$fit_summary <- renderPrint({
    model <- fit_model_safe()
    validate(need(model$ok, model$msg_friendly))
    summary(model$fit, fit.measures = TRUE)
  })

  output$param_tbl <- renderDT({
    tryCatch({
      model <- fit_model_safe()
      validate(need(model$ok, model$msg_friendly))
      
      show_std <- isTRUE(input$param_show_std)
      pe <- if (show_std && !is.null(model$pe_std)) {
        model$pe_std
      } else if (!show_std && !is.null(model$pe_raw)) {
        model$pe_raw
      } else {
        tryCatch({
          parameterEstimates(model$fit, standardized = show_std)
        }, error = function(e) {
          parameterEstimates(model$fit)
        })
      }
      
      digits_input <- input$param_digits
      if (is.null(digits_input) || digits_input == "") digits_input <- "3"
      
      num_cols <- names(pe)[sapply(pe, is.numeric)]
      
      # Raw precision mode
      if (digits_input == "all") {
        return(datatable(pe,
                         extensions = 'Buttons',
                         options = list(pageLength = 15, dom = 'Bfrtip', buttons = c('copy', 'csv')),
                         rownames = FALSE))
      }
      
      digits_val <- as.integer(digits_input)
      if (is.na(digits_val) || digits_val < 1) digits_val <- 3
      
      std_num_cols <- setdiff(num_cols, "pvalue")
      
      # Custom JS renderer for p-value: formats < .001 (or according to digits) while keeping raw number for sorting
      p_threshold <- 10^(-digits_val)
      p_format_js <- JS(sprintf("
        function(data, type, row, meta) {
          if (type !== 'display') return data;
          if (data === null || data === undefined) return '';
          var num = parseFloat(data);
          if (isNaN(num)) return 'NA';
          if (num < %f) {
            return '< %s';
          }
          return num.toFixed(%d);
        }
      ", p_threshold, sprintf(paste0("%.", digits_val, "f"), p_threshold), digits_val))
      
      col_defs <- list()
      if ("pvalue" %in% names(pe)) {
        p_idx <- which(names(pe) == "pvalue") - 1  # 0-indexed column target for DataTables
        col_defs[[length(col_defs) + 1]] <- list(targets = p_idx, render = p_format_js)
      }
      
      dt <- datatable(pe,
                      extensions = 'Buttons',
                      options = list(
                        pageLength = 15,
                        dom = 'Bfrtip',
                        buttons = c('copy', 'csv'),
                        columnDefs = col_defs
                      ),
                      rownames = FALSE)
      
      if (length(std_num_cols) > 0) {
        dt <- dt %>% formatRound(columns = std_num_cols, digits = digits_val)
      }
      dt
    }, error = function(e) {
      validate(need(FALSE, paste("Error rendering parameter estimates table:", e$message)))
    })
  }, server = FALSE)  # client-side table: the Copy / CSV buttons then export every row, not only the visible page

  # ----------------- Path diagram server observer ---------------
  observe({
    # Reactive dependencies to trigger redraw
    model <- fit_model_safe()
    std_for_plot <- if (input$analysis_mode == "std") TRUE else input$diagram_std
    
    # Parse layout_style into engine / rankdir
    parts  <- strsplit(input$layout_style, "_", fixed = TRUE)[[1]]
    eng    <- parts[1]
    rank   <- ifelse(length(parts) == 2, parts[2], "LR")
    
    # If the model check fails or not run yet
    if (!model$ok) {
      session$sendCustomMessage("update_sem_plot", list(
        error = FALSE,
        message = if (nzchar(model$msg_friendly) && !identical(model$msg_level, "info"))
          "No path diagram: the model could not be estimated. See the message above."
        else "Define a model to view the path diagram."
      ))
      return()
    }

    # If model is empty
    ln <- lavaan_model_str()
    if (length(ln) == 0) {
      session$sendCustomMessage("update_sem_plot", list(
        error = FALSE,
        message = "Define a model to view the path diagram."
      ))
      return()
    }
    
    # Generate DOT code
    dot_code <- tryCatch(
      semDiagram(model$fit,
                 standardized        = std_for_plot,
                 layout              = rank,
                 engine              = eng,
                 cached_params       = if (std_for_plot) model$pe_std else model$pe_raw,
                 cached_fit_measures = model$fit_measures,
                 # Not identified: the incalculable values (p, NFI, CFI) and the df are drawn in red
                 ident_df            = if (isFALSE(model$identified) && is.finite(model$ident_df)) model$ident_df else NULL),
      error = function(e) e)
    if (inherits(dot_code, "error")) {
      session$sendCustomMessage("update_sem_plot", list(
        error = TRUE,
        message = htmltools::htmlEscape(paste("Could not draw the path diagram:", conditionMessage(dot_code)))
      ))
      return()
    }

    # Send DOT code to client JS
    session$sendCustomMessage("update_sem_plot", list(
      error = FALSE,
      dot = dot_code,
      engine = eng
    ))
  })

  # ----------------- Export PDF Analysis Report ------------------
  observeEvent(input$export_pdf_btn, {
    model_res <- fit_model_safe()
    if (!isTRUE(model_res$ok) || is.null(model_res$fit)) {
      showModal(modalDialog(
        title = "Export Warning",
        div(class = "alert alert-warning",
            "Please run and successfully fit a model before exporting the PDF report."),
        easyClose = TRUE,
        footer = modalButton("Dismiss")
      ))
      return()
    }

    tryCatch({
      fit <- model_res$fit
      ms <- lavaan::fitMeasures(fit, c("nobs", "chisq", "df", "pvalue", "cfi", "tli", "rmsea", "srmr", "aic", "bic"))
      
      # Multicollinearity calculation
      condition_number <- NA
      max_cond_index <- NA
      tryCatch({
        samp_cov <- lavaan::lavInspect(fit, "sampstat")$cov
        if (!is.null(samp_cov) && nrow(samp_cov) > 0) {
          samp_cor <- stats::cov2cor(samp_cov)
          eig_vals <- eigen(samp_cor, symmetric = TRUE, only.values = TRUE)$values
          if (length(eig_vals) > 0 && min(eig_vals) > 1e-12) {
            condition_number <- max(eig_vals) / min(eig_vals)
            max_cond_indices <- sqrt(max(eig_vals) / eig_vals)
            max_cond_index <- max(max_cond_indices)
          }
        }
      }, error = function(e) NULL)

      cond_num_str <- if (!is.na(condition_number)) sprintf("%.1f", condition_number) else "-"
      max_cond_str <- if (!is.na(max_cond_index)) sprintf("%.1f", max_cond_index) else "-"

      # Build Fit & Diagnostics HTML Table
      # fitMeasures() has no "nobs" measure, so N comes from lavInspect(). Values that cannot be computed are
      # shown as NA (red) instead of being turned into 0, which would look like a perfect fit.
      n_obs_val <- tryCatch(sum(lavaan::lavInspect(fit, "nobs")), error = function(e) NA_real_)
      na_cell <- "<span style='color:#b91c1c;font-weight:600;'>NA</span>"
      num_cell <- function(v, fmt) if (is.na(v)) na_cell else sprintf(fmt, v)
      unident_pdf <- isFALSE(model_res$identified)
      df_cell <- if (is.na(ms["df"])) na_cell else if (unident_pdf)
        sprintf("<span style='color:#b91c1c;font-weight:600;'>%d</span>", as.integer(ms["df"]))
      else sprintf("%d", as.integer(ms["df"]))

      fit_html <- paste0(
        "<table class='print-table'>",
        "<thead><tr><th>N</th><th>Chi-square</th><th>df</th><th>p-value</th><th>CFI</th><th>TLI</th><th>RMSEA</th><th>SRMR</th><th>AIC</th><th>BIC</th><th>Cond. No.</th><th>Max Cond. Index</th></tr></thead>",
        "<tbody><tr>",
        paste0("<td>", num_cell(n_obs_val, "%d"), "</td><td>", num_cell(ms["chisq"], "%.2f"), "</td><td>", df_cell, "</td><td>",
               num_cell(ms["pvalue"], "%.3f"), "</td><td>", num_cell(ms["cfi"], "%.3f"), "</td><td>",
               num_cell(ms["tli"], "%.3f"), "</td><td>", num_cell(ms["rmsea"], "%.3f"), "</td><td>",
               num_cell(ms["srmr"], "%.3f"), "</td><td>", num_cell(ms["aic"], "%.1f"), "</td><td>",
               num_cell(ms["bic"], "%.1f"), "</td><td>", cond_num_str, "</td><td>", max_cond_str, "</td>"),
        "</tr></tbody></table>"
      )

      # The report describes the model as it was fitted (data and settings captured at fit time)
      df_fit <- model_res$fit_data %||% processed_data()

      # Build Variable Summary Statistics Table
      var_stats_html <- tryCatch({
        vs <- compute_var_stats(fit, df_fit)
        if (!is.null(vs) && nrow(vs) > 0) {
          num_cell <- function(x) ifelse(is.na(x), "-", sprintf("%.3f", x))
          rows <- sprintf("<tr><td><b>%s</b></td><td style='text-align:right;'>%d</td><td style='text-align:right;'>%d (%.1f%%)</td><td style='text-align:right;'>%s</td><td style='text-align:right;'>%s</td><td style='text-align:right;'>%s</td><td style='text-align:right;'>%s</td></tr>",
                          htmltools::htmlEscape(vs$Variable), as.integer(vs$ValidN), as.integer(vs$MissingN), vs$MissingPct,
                          num_cell(vs$Mean), num_cell(vs$SD), num_cell(vs$Skewness), num_cell(vs$Kurtosis))
          paste0(
            "<table class='print-table'>",
            "<thead><tr><th>Variable</th><th style='text-align:right;'>Valid N</th><th style='text-align:right;'>Missing N (%)</th><th style='text-align:right;'>Mean</th><th style='text-align:right;'>Std.Dev</th><th style='text-align:right;'>Skewness</th><th style='text-align:right;'>Kurtosis</th></tr></thead>",
            "<tbody>", paste(rows, collapse = ""), "</tbody></table>"
          )
        } else ""
      }, error = function(e) "")

      # Build Latent Variable Reliability & Validity Table
      latent_rel_html <- tryCatch({
        rel <- compute_latent_reliability(fit, df_fit)
        if (!is.null(rel) && nrow(rel) > 0) {
          num_cell <- function(x) ifelse(is.na(x), "-", sprintf("%.3f", x))
          lv_rows <- sprintf("<tr><td><b>%s</b></td><td>%s</td><td style='text-align:right;'>%d</td><td style='text-align:right;'>%s</td><td style='text-align:right;'>%s</td><td style='text-align:right;'>%s</td></tr>",
                             htmltools::htmlEscape(rel$Latent), htmltools::htmlEscape(rel$Indicators), as.integer(rel$Count),
                             num_cell(rel$Alpha), num_cell(rel$CR), num_cell(rel$AVE))
          paste0(
            "<table class='print-table'>",
            "<thead><tr><th>Latent Construct</th><th>Indicators</th><th style='text-align:right;'>Count</th><th style='text-align:right;'>Cronbach's &alpha;</th><th style='text-align:right;'>CR (Composite Reliability)</th><th style='text-align:right;'>AVE (Average Variance Extracted)</th></tr></thead>",
            "<tbody>", paste(lv_rows, collapse = ""), "</tbody></table>"
          )
        } else ""
      }, error = function(e) "")

      # Build Parameter Estimates Table
      pe <- tryCatch({
        lavaan::parameterEstimates(fit, standardized = TRUE)
      }, error = function(e) lavaan::parameterEstimates(fit))
      
      main_pe <- pe[pe$op != ":=", ]
      param_rows <- vapply(seq_len(nrow(main_pe)), function(i) {
        r <- main_pe[i, ]
        p_val_str <- if (is.na(r$pvalue)) "-" else if (r$pvalue < 0.001) "< .001" else sprintf("%.3f", r$pvalue)
        std_val_str <- if ("std.all" %in% names(r) && !is.na(r$std.all)) sprintf("%.3f", r$std.all) else "-"
        se_str <- if (is.na(r$se)) "-" else sprintf("%.3f", r$se)
        z_str <- if (is.na(r$z)) "-" else sprintf("%.3f", r$z)
        sprintf("<tr><td>%s</td><td style='text-align:center;'>%s</td><td>%s</td><td style='text-align:right;'>%.3f</td><td style='text-align:right;'>%s</td><td style='text-align:right;'>%s</td><td style='text-align:right;'>%s</td><td style='text-align:right;'>%s</td></tr>",
                htmltools::htmlEscape(as.character(r$lhs)),
                htmltools::htmlEscape(as.character(r$op)),
                htmltools::htmlEscape(as.character(r$rhs)),
                r$est, se_str, z_str, p_val_str, std_val_str)
      }, character(1))
      
      param_html <- paste0(
        "<table class='print-table'>",
        "<thead><tr><th>LHS</th><th style='text-align:center;'>Op</th><th>RHS</th><th style='text-align:right;'>Estimate</th><th style='text-align:right;'>Std.Err</th><th style='text-align:right;'>z-value</th><th style='text-align:right;'>p-value</th><th style='text-align:right;'>Std.all</th></tr></thead>",
        "<tbody>", paste(param_rows, collapse = ""), "</tbody></table>"
      )

      # Build Defined & Indirect Effects Table
      defined_effects_html <- tryCatch({
        def_sub <- pe[pe$op == ":=", ]
        if (!is.null(def_sub) && nrow(def_sub) > 0) {
          def_rows <- vapply(seq_len(nrow(def_sub)), function(i) {
            r <- def_sub[i, ]
            p_val_str <- if (is.na(r$pvalue)) "-" else if (r$pvalue < 0.001) "< .001" else sprintf("%.3f", r$pvalue)
            std_val_str <- if ("std.all" %in% names(r) && !is.na(r$std.all)) sprintf("%.3f", r$std.all) else "-"
            se_str <- if (is.na(r$se)) "-" else sprintf("%.3f", r$se)
            z_str <- if (is.na(r$z)) "-" else sprintf("%.3f", r$z)
            sprintf("<tr><td><b>%s</b></td><td style='text-align:right;'>%.3f</td><td style='text-align:right;'>%s</td><td style='text-align:right;'>%s</td><td style='text-align:right;'>%s</td><td style='text-align:right;'>%s</td></tr>",
                    htmltools::htmlEscape(as.character(r$lhs)),
                    r$est, se_str, z_str, p_val_str, std_val_str)
          }, character(1))
          
          paste0(
            "<table class='print-table'>",
            "<thead><tr><th>Defined Parameter / Indirect Effect</th><th style='text-align:right;'>Estimate</th><th style='text-align:right;'>Std.Err</th><th style='text-align:right;'>z-value</th><th style='text-align:right;'>p-value</th><th style='text-align:right;'>Std.all</th></tr></thead>",
            "<tbody>", paste(def_rows, collapse = ""), "</tbody></table>"
          )
        } else ""
      }, error = function(e) "")

      # Build Auto-Optimization History Details
      opt_history_html <- tryCatch({
        res <- prune_results()
        if (!is.null(res) && !is.null(res$candidates) && length(res$candidates) > 0) {
          strat <- res$strategy_used %||% "Unknown"
          crit  <- res$criterion %||% "AIC"
          baseline_cand <- NULL
          optimal_cand  <- res$candidates[[1]]
          for (cand in res$candidates) {
            if (isTRUE(cand$status == "[Baseline]")) {
              baseline_cand <- cand; break
            }
          }
          
          # AIC/BIC are NA for models that are not identified (e.g. the starting model of an optimization)
          score_txt <- function(cand) {
            v <- if (is.null(cand)) NA_real_ else if (crit == "AIC") cand$aic else cand$bic
            if (is.finite(v)) sprintf("%.2f", v) else "n/a (not identified)"
          }
          base_score <- score_txt(baseline_cand)
          opt_score <- score_txt(optimal_cand)
          
          # removed_str lists the pruned paths as "dep ~ pred; dep ~ pred" (or the baseline label)
          pruned_paths_count <- if (!is.null(optimal_cand) && !is.null(optimal_cand$removed_str) &&
                                    !identical(optimal_cand$status, "[Baseline]")) {
            length(strsplit(optimal_cand$removed_str, ";", fixed = TRUE)[[1]])
          } else 0L

          sprintf(
            paste0(
              "<div style='background-color:#f8fafc; border:1px solid #e2e8f0; border-radius:4px; padding:8px 12px; font-size:11px; margin-bottom:12px;'>",
              "<div><b>Optimization Strategy:</b> %s | <b>Criterion:</b> %s</div>",
              "<div><b>Baseline %s Score:</b> %s &rarr; <b>Optimal %s Score:</b> %s</div>",
              "<div><b>Paths Pruned:</b> %d path(s)</div>",
              "</div>"),
            htmltools::htmlEscape(strat), htmltools::htmlEscape(crit),
            htmltools::htmlEscape(crit), base_score, htmltools::htmlEscape(crit), opt_score,
            pruned_paths_count
          )
        } else ""
      }, error = function(e) "")

      payload <- list(
        timestamp            = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
        analysis_mode        = if (identical(model_res$snapshot$settings$analysis_mode %||% input$analysis_mode, "std")) "Standardized" else "Raw",
        missing_method       = model_res$snapshot$settings$missing_method %||% input$missing_method,
        warning_html         = if (isFALSE(model_res$identified)) paste0(
          "<div style='border:2px solid #b91c1c; color:#b91c1c; padding:8px 12px; margin:10px 0; font-weight:600;'>",
          "The model is NOT IDENTIFIED",
          if (is.finite(model_res$ident_df)) paste0(" (df = ", format(model_res$ident_df), ")") else "",
          ": estimates are for inspection only; standard errors and fit indices are unavailable or unreliable.</div>") else "",
        fit_table_html       = fit_html,
        var_stats_html       = var_stats_html,
        latent_rel_html      = latent_rel_html,
        param_table_html     = param_html,
        defined_effects_html = defined_effects_html,
        opt_history_html     = opt_history_html,
        syntax_text          = htmltools::htmlEscape(paste(model_res$syntax, collapse = "\n")),
        summary_text         = htmltools::htmlEscape(paste(
          tryCatch(utils::capture.output(print(summary(fit, fit.measures = TRUE))),
                   error = function(e) "Model summary is not available."),
          collapse = "\n"))
      )

      session$sendCustomMessage("prepare_and_print_pdf_report", payload)
    }, error = function(e) {
      showModal(modalDialog(
        title = "Export Error",
        div(class = "alert alert-danger",
            paste("Failed to generate PDF report:", e$message)),
        easyClose = TRUE,
        footer = modalButton("Dismiss")
      ))
    })
  })

  # ----------------- Saved Models, Restore & Results ZIP ----------------
  # The model store lives in the browser (see structuraStore in the page script); R only asks it to put/get/delete
  # and receives a summary through input$stored_models. Models travel as JSON strings (see build_model_spec).

  store_info <- reactive({
    empty <- list(loaded = FALSE, available = FALSE, names = character(0), autosave_label = "",
                  autosave_columns = character(0))
    txt <- input$stored_models
    if (is.null(txt)) return(empty)
    x <- tryCatch(jsonlite::fromJSON(txt, simplifyVector = FALSE), error = function(e) NULL)
    if (is.null(x)) return(empty)
    list(loaded = TRUE, available = isTRUE(x$available), names = as_chr(x$names),
         autosave_label = as.character((x$autosave_label %||% "")[1]),
         autosave_columns = as_chr(x$autosave_columns),
         error = x$error, notice = x$notice, saved_name = x$saved_name)
  })

  observeEvent(input$stored_models, {
    info <- store_info()
    sel <- if (!is.null(info$saved_name) && info$saved_name %in% info$names) info$saved_name else NULL
    # An empty choice vector is not applied by updateSelectInput, so a blank placeholder stands in for "none saved"
    updateSelectInput(session, "saved_model_select",
                      choices = if (length(info$names)) info$names else c("(no saved models)" = ""),
                      selected = sel)
    if (!is.null(info$error)) {
      showNotification(as.character(info$error), type = "error", duration = 8)
    } else if (!is.null(info$notice)) {
      showNotification(as.character(info$notice), type = "message", duration = 4)
    }
  })

  # Saved Models dialog. The select input is created fresh on every open, so its choices come from the store summary.
  saved_models_modal <- function() {
    info <- store_info()
    modalDialog(
      title = "Saved Models",
      easyClose = TRUE,
      footer = modalButton("Close"),
      uiOutput("saved_models_status"),
      div(style = "display: flex; gap: 8px; align-items: flex-end; flex-wrap: wrap;",
          div(style = "width: 220px;",
              textInput("save_model_name", "Save current model as:", placeholder = "e.g. 3-factor CFA")),
          actionButton("save_model_btn", "Save", class = "btn btn-primary btn-sm",
                       style = "margin-bottom: 15px;")),
      div(style = "display: flex; gap: 8px; align-items: flex-end; flex-wrap: wrap;",
          div(style = "width: 220px;",
              selectInput("saved_model_select", "Saved models in this browser:",
                          choices = if (length(info$names)) info$names else c("(no saved models)" = ""))),
          actionButton("load_model_btn", "Load", class = "btn btn-default btn-sm",
                       style = "margin-bottom: 15px;"),
          actionButton("delete_model_btn", "Delete", class = "btn btn-default btn-sm",
                       style = "margin-bottom: 15px;")),
      div(style = "display: flex; gap: 8px; flex-wrap: wrap;",
          actionButton("export_model_json_btn", "Export JSON", class = "btn btn-default btn-sm"),
          actionButton("import_model_json_btn", "Import JSON", class = "btn btn-default btn-sm",
                       onclick = "document.getElementById('model_import_file').click();"),
          tags$input(id = "model_import_file", type = "file", accept = ".json,application/json",
                     style = "display: none;")),
      tags$p(style = "font-size: 11px; color: #666; margin: 8px 0 0 0;",
             "Models are saved by variable names only (never data values) in this browser's storage. ",
             "Storage can be cleared by the browser (private windows, site-data cleanup), so use ",
             tags$b("Export JSON"), " for a durable backup.")
    )
  }

  observeEvent(input$saved_models_btn, {
    tryCatch(showModal(saved_models_modal()), error = function(e) {
      showNotification(paste("Could not open Saved Models:", conditionMessage(e)), type = "error", duration = 8)
    })
  })

  output$saved_models_status <- renderUI({
    info <- store_info()
    if (!isTRUE(info$loaded)) return(NULL)
    if (!isTRUE(info$available)) {
      return(div(class = "alert alert-warning", style = "font-size: 12px; padding: 6px 10px; margin-bottom: 8px;",
                 "Browser storage is unavailable here (private window or blocked site data). ",
                 "Models cannot be kept in the browser; use Export JSON / Import JSON instead."))
    }
    if (nzchar(info$autosave_label)) {
      div(style = "font-size: 12px; color: #555; margin-bottom: 8px;",
          paste0("Last successful fit (autosaved): ", info$autosave_label, "  "),
          actionLink("restore_autosave_link", "Restore it"))
    } else {
      div(style = "font-size: 12px; color: #555; margin-bottom: 8px;",
          "The model is autosaved after every successful fit.")
    }
  })

  observeEvent(input$save_model_btn, {
    tryCatch({
      nm <- trimws(input$save_model_name %||% "")
      if (is.null(data())) {
        showNotification("Load a dataset and define a model before saving.", type = "warning", duration = 5)
        return()
      }
      if (!nzchar(nm)) {
        showNotification("Enter a name for the model first.", type = "warning", duration = 5)
        return()
      }
      spec <- make_model_spec(processed_data(), name = nm)
      session$sendCustomMessage("structura_store", list(op = "save", name = nm, json = spec_to_json(spec)))
    }, error = function(e) {
      showNotification(paste("Could not save the model:", conditionMessage(e)), type = "error", duration = 8)
    })
  })

  observeEvent(input$load_model_btn, {
    nm <- input$saved_model_select %||% ""
    if (!nzchar(nm)) {
      showNotification("There is no saved model to load. Save one first.", type = "warning", duration = 5)
      return()
    }
    session$sendCustomMessage("structura_store", list(op = "get", kind = "named", name = nm))
  })

  observeEvent(input$delete_model_btn, {
    nm <- input$saved_model_select %||% ""
    if (!nzchar(nm)) return()
    session$sendCustomMessage("structura_store", list(op = "delete", name = nm))
  })

  observeEvent(input$export_model_json_btn, {
    tryCatch({
      if (is.null(data())) {
        showNotification("Load a dataset and define a model before exporting.", type = "warning", duration = 5)
        return()
      }
      nm <- trimws(input$save_model_name %||% "")
      spec <- make_model_spec(processed_data(), name = nm)
      session$sendCustomMessage("structura_download_text", list(
        stem = "structura2_model", label = nm, ext = "json", stamp = TRUE,
        mime = "application/json;charset=utf-8", text = spec_to_json(spec)))
    }, error = function(e) {
      showNotification(paste("Could not export the model:", conditionMessage(e)), type = "error", duration = 8)
    })
  })

  observeEvent(input$model_import_json, {
    x <- input$model_import_json
    removeModal()
    txt <-as.character(x$text %||% "")
    if (!nzchar(txt)) {
      showNotification("The selected file could not be read or is empty.", type = "error", duration = 8)
      return()
    }
    parsed <- parse_model_spec_json(txt)
    if (!isTRUE(parsed$ok)) {
      showNotification(parsed$msg, type = "error", duration = 10)
      return()
    }
    start_restore(parsed$spec)
  })

  observeEvent(input$restore_spec_json, {
    x <- input$restore_spec_json
    removeModal()
    parsed <- parse_model_spec_json(as.character(x$json %||% ""))
    if (!isTRUE(parsed$ok)) {
      showNotification(parsed$msg, type = "error", duration = 10)
      return()
    }
    start_restore(parsed$spec)
  })

  # Autosave: the spec captured at fit time of every successful fit.
  # No ignoreInit here: before the first Run the fit reactive can sit in a silent "not ready" state, and with
  # ignoreInit = TRUE the first real fit would then be swallowed as the "initial" value. The initial value is
  # never ok, so nothing is saved by it anyway.
  observeEvent(fit_model_safe(), {
    res <- fit_model_safe()
    # An unidentified fit is not a "successful fit": "Last successful fit" must never point at it
    if (!isTRUE(res$ok) || isFALSE(res$identified)) return()
    tryCatch({
      if (is.null(res$snapshot)) stop(res$snapshot_error %||% "the model definition could not be read")
      session$sendCustomMessage("structura_store", list(op = "autosave", json = spec_to_json(res$snapshot)))
    }, error = function(e) {
      showNotification(paste("The model could not be autosaved:", conditionMessage(e)),
                       type = "warning", duration = 8)
    })
  })

  request_autosave_restore <- function() {
    removeNotification("restore_offer", session = session)
    session$sendCustomMessage("structura_store", list(op = "get", kind = "autosave"))
  }
  observeEvent(input$restore_autosave_link, request_autosave_restore())
  observeEvent(input$restore_autosave_notif, request_autosave_restore())

  # Offer to restore the autosaved model once, when a dataset with matching columns is loaded
  offered_key <- reactiveVal("")
  observe({
    df <- data(); req(df)
    info <- store_info()
    req(isTRUE(info$loaded), isTRUE(info$available), nzchar(info$autosave_label))
    key <- paste(data_label(), ncol(df), nrow(df), sep = "|")
    if (identical(isolate(offered_key()), key)) return()
    if (length(intersect(info$autosave_columns, names(df))) < 2) return()
    offered_key(key)
    showNotification(
      ui = div("A model from a previous session is available (", info$autosave_label, "). ",
               actionButton("restore_autosave_notif", "Restore it", class = "btn btn-primary btn-xs")),
      id = "restore_offer", duration = 25, type = "message", session = session)
  })

  # ---- Staged restore ----
  # Applying a spec is order-dependent: the log-transform selection changes the available columns, the displayed
  # columns reshape the measurement table, and the structural matrix is rebuilt from the measurement table. Each
  # stage therefore waits until the previous one has settled before the next piece is applied; if any stage does
  # not settle in time the restore is abandoned with a message. The model is not fitted automatically.
  restore_state <- reactiveVal(NULL)

  # Columns that can be shown after the current log-transform selection (mirrors output$display_column_ui)
  restore_display_choices <- function(logs) {
    full <- processed_columns_full()
    numeric_cols <- vapply(full, is.numeric, logical(1))
    zero_var <- names(full)[numeric_cols][vapply(full[numeric_cols], function(x) {
      v <- var(x, na.rm = TRUE); is.na(v) || v == 0
    }, logical(1))]
    available <- setdiff(names(full), zero_var)
    numeric_orig <- names(data())[vapply(data(), is.numeric, logical(1))]
    list(available = available,
         default = intersect(c(numeric_orig, if (length(logs)) paste0("log_", logs) else NULL), available))
  }

  start_restore <- function(spec) {
    if (is.null(data())) {
      showNotification("Load a dataset first, then restore the model.", type = "warning", duration = 6)
      return(invisible(FALSE))
    }
    tryCatch({
      s <- spec$settings
      df <- data()
      nums <- names(df)[vapply(df, is.numeric, logical(1))]
      loggable <- nums[vapply(df[nums], function(x) suppressWarnings(min(x, na.rm = TRUE)) > 0, logical(1))]
      target_logs <- intersect(s$log_columns, loggable)
      logs_changed <- !setequal(target_logs, as_chr(input$log_columns))
      dropped <- setdiff(s$log_columns, target_logs)

      restore_state(list(spec = spec, stage = if (logs_changed) "logs" else "display",
                         target_logs = target_logs, logs_changed = logs_changed, dropped = dropped,
                         t0 = Sys.time(), stage_t = Sys.time(), tries = 0L))
      removeNotification("restore_offer", session = session)
      showNotification("Restoring the model...", id = "restore_progress", duration = NULL,
                       closeButton = FALSE, type = "message")
      # Outputs of the Model tab only report their state while visible
      updateTabsetPanel(session, "main_tabs", selected = "Model")

      if (!is.null(s$analysis_mode))  updateRadioButtons(session, "analysis_mode", selected = s$analysis_mode)
      if (!is.null(s$missing_method)) updateSelectInput(session, "missing_method", selected = s$missing_method)
      if (!is.null(s$layout_style))   updateSelectInput(session, "layout_style", selected = s$layout_style)
      if (!is.null(s$diagram_std))    updateCheckboxInput(session, "diagram_std", value = s$diagram_std)
      if (!is.null(s$show_suggested_paths)) updateCheckboxInput(session, "show_suggested_paths", value = s$show_suggested_paths)
      if (!is.null(s$max_suggestions)) updateNumericInput(session, "max_suggestions", value = s$max_suggestions)
      updateTextAreaInput(session, "extra_eq", value = spec$manual_equations)
      if (logs_changed) updateCheckboxGroupInput(session, "log_columns", selected = target_logs)
      invisible(TRUE)
    }, error = function(e) {
      restore_state(NULL)
      removeNotification("restore_progress", session = session)
      showNotification(paste("Could not restore the model:", conditionMessage(e)), type = "error", duration = 10)
      invisible(FALSE)
    })
  }

  observe({
    st <- restore_state()
    if (is.null(st)) return()
    invalidateLater(250, session)

    secs_in_stage <- as.numeric(difftime(Sys.time(), st$stage_t, units = "secs"))
    go <- function(stage, ...) restore_state(modifyList(st, c(list(stage = stage, stage_t = Sys.time()), list(...))))
    abort <- function(msg) {
      restore_state(NULL)
      removeNotification("restore_progress", session = session)
      showNotification(msg, type = "error", duration = 12)
    }

    if (as.numeric(difftime(Sys.time(), st$t0, units = "secs")) > 25) {
      return(abort("Restoring the model timed out. Reload the data and try again, or rebuild the model manually."))
    }

    tryCatch({
      spec <- st$spec
      if (st$stage == "logs") {
        cur <- as_chr(input$log_columns)
        if (setequal(cur, st$target_logs)) {
          # The display-column list is re-rendered with a new default; wait for it (or give up waiting after 4 s)
          expected <- restore_display_choices(st$target_logs)$default
          if (setequal(as_chr(input$display_columns), expected) || secs_in_stage > 4) go("display")
        }
      } else if (st$stage == "display") {
        ch <- restore_display_choices(st$target_logs)
        wanted <- spec$settings$display_columns
        target <- intersect(wanted, ch$available)
        if (!length(target)) target <- as_chr(input$display_columns)
        if (!setequal(as_chr(input$display_columns), target)) {
          updateCheckboxGroupInput(session, "display_columns", selected = target)
        }
        go("display_wait", target_display = target, dropped = c(st$dropped, setdiff(wanted, ch$available)))
      } else if (st$stage == "display_wait") {
        tbl <- input_table_data()
        tbl_cols <- if (is.null(tbl)) NULL else setdiff(colnames(tbl), c("Latent", "Indicator", "Operator"))
        if (setequal(as_chr(input$display_columns), st$target_display) && setequal(tbl_cols, st$target_display)) {
          go("meas")
        } else if (secs_in_stage > 8) {
          abort("Could not apply the column selection of the saved model. Reload the data and try again.")
        }
      } else if (st$stage == "meas") {
        res <- spec_to_meas_table(spec, as_chr(input$display_columns), names(processed_data()))
        input_table_data(res$table)
        input_table_trigger(input_table_trigger() + 1)
        go("meas_wait", meas_table = res$table, dropped = c(st$dropped, res$dropped))
      } else if (st$stage == "meas_wait") {
        echo <- if (!is.null(input$input_table)) tryCatch(hot_to_r(input$input_table), error = function(e) NULL) else NULL
        if (!is.null(echo) && identical(as.character(echo$Latent), as.character(st$meas_table$Latent)) &&
            identical(build_meas_lines(echo), build_meas_lines(st$meas_table))) {
          go("struct")
        } else if (secs_in_stage > 8) {
          abort("Could not apply the measurement model of the saved model. Open the Model tab and try again.")
        }
      } else if (st$stage == "struct") {
        items <- tryCatch(model_items(), error = function(e) NULL)
        cur <- struct_table_data()
        # Let the structural matrix finish rebuilding after the measurement change before overwriting it
        if (!is.null(items) && !is.null(cur) && setequal(cur$Dependent, items) && secs_in_stage > 0.5) {
          res <- spec_to_struct_table(spec, items)
          struct_table_data(res$table)
          struct_table_trigger(struct_table_trigger() + 1)
          go("struct_wait", struct_key = make_struct_key(res$table),
             dropped = if (st$tries == 0L) c(st$dropped, res$dropped) else st$dropped)
        } else if (secs_in_stage > 8) {
          abort("Could not apply the structural model of the saved model. Open the Model tab and try again.")
        }
      } else if (st$stage == "struct_wait") {
        cur <- struct_table_data()
        if (!is.null(cur) && identical(make_struct_key(cur), st$struct_key)) {
          if (secs_in_stage > 0.8) {
            restore_state(NULL)
            removeNotification("restore_progress", session = session)
            msg <- "Model restored. Click Run to fit it."
            skipped <- unique(st$dropped)
            if (length(skipped)) {
              msg <- paste0(msg, " Skipped because they are not in the current data: ",
                            paste(head(skipped, 8), collapse = ", "), if (length(skipped) > 8) ", ..." else "", ".")
            }
            n_saved <- spec$data$nrow_used
            n_now <- tryCatch(nrow(processed_data()), error = function(e) NA_integer_)
            if (!is.na(n_saved) && !is.na(n_now) && n_saved > 0 && n_saved != n_now) {
              msg <- paste0(msg, " Note: the saved model was fitted on ", n_saved, " rows; the current data has ", n_now, ".")
            }
            showNotification(msg, type = if (length(skipped)) "warning" else "message", duration = 12)
          }
        } else if (secs_in_stage > 3 && st$tries < 4L) {
          # A stale update from the browser overwrote the matrix: apply it again
          go("struct", tries = st$tries + 1L)
        } else if (secs_in_stage > 8) {
          abort("Could not apply the structural model of the saved model. Open the Model tab and try again.")
        }
      }
    }, error = function(e) {
      if (!inherits(e, "shiny.silent.error")) abort(paste("Could not restore the model:", conditionMessage(e)))
    })
  })

  # Messages coming from client-side helpers (ZIP creation, ...)
  observeEvent(input$client_notice, {
    x <- input$client_notice
    type <- as.character(x$type %||% "message")
    if (!type %in% c("message", "warning", "error")) type <- "message"
    showNotification(as.character(x$msg %||% ""), type = type, duration = if (type == "error") 10 else 6)
  })

  # ---- Results ZIP ----
  observeEvent(input$export_zip_btn, {
    # Before the first Run the fit reactive can still be in a silent "not ready" state, hence the tryCatch
    model_res <- tryCatch(fit_model_safe(), error = function(e) NULL)
    if (is.null(model_res) || !isTRUE(model_res$ok) || is.null(model_res$fit)) {
      showModal(modalDialog(
        title = "Export Warning",
        div(class = "alert alert-warning",
            "Please run and successfully fit a model before downloading the results."),
        easyClose = TRUE,
        footer = modalButton("Dismiss")
      ))
      return()
    }
    tryCatch({
      fit <- model_res$fit
      df_fit <- model_res$fit_data %||% processed_data()
      files <- list()
      add <- function(name, text) files[[length(files) + 1]] <<- list(name = name, text = text)

      pe <- tryCatch(lavaan::parameterEstimates(fit, standardized = TRUE),
                     error = function(e) lavaan::parameterEstimates(fit))
      add("parameter_estimates.csv", df_to_csv_text(pe))

      fm <- tryCatch(lavaan::fitMeasures(fit), error = function(e) NULL)
      if (!is.null(fm)) {
        add("fit_measures.csv", df_to_csv_text(data.frame(measure = names(fm), value = as.numeric(fm),
                                                          stringsAsFactors = FALSE)))
      }
      vs <- tryCatch(compute_var_stats(fit, df_fit), error = function(e) NULL)
      if (!is.null(vs) && nrow(vs) > 0) add("variable_summary.csv", df_to_csv_text(vs))
      rel <- tryCatch(compute_latent_reliability(fit, df_fit), error = function(e) NULL)
      if (!is.null(rel) && nrow(rel) > 0) add("reliability.csv", df_to_csv_text(rel))

      add("model_syntax.txt", paste(model_res$syntax, collapse = "\n"))
      if (!is.null(model_res$snapshot)) add("model.json", spec_to_json(model_res$snapshot))
      summ <- tryCatch(paste(utils::capture.output(summary(fit, fit.measures = TRUE)), collapse = "\n"),
                       error = function(e) paste("Summary unavailable:", conditionMessage(e)))
      add("lavaan_summary.txt", summ)

      st <- model_res$snapshot$settings
      readme <- c(
        if (isFALSE(model_res$identified)) c(
          paste0("WARNING: the model is NOT IDENTIFIED",
                 if (is.finite(model_res$ident_df)) paste0(" (df = ", format(model_res$ident_df), ")") else "",
                 ". Estimates are for inspection only; standard errors and fit indices are unavailable or unreliable."),
          ""),
        "Structura2 results",
        "==================",
        paste0("Data: ", if (nzchar(data_label())) data_label() else "(unnamed)", "  (", nrow(df_fit), " rows used)"),
        paste0("Analysis mode: ", st$analysis_mode %||% input$analysis_mode,
               " | Missing data: ", st$missing_method %||% input$missing_method),
        paste0("lavaan version: ", as.character(utils::packageVersion("lavaan"))),
        "",
        "Files",
        "-----",
        "parameter_estimates.csv   all parameter estimates (full precision, includes std.all)",
        "fit_measures.csv          every fit measure reported by lavaan",
        "variable_summary.csv      valid / missing counts, mean, SD, skewness, kurtosis",
        "reliability.csv           Cronbach's alpha, CR, AVE per latent construct (if any)",
        "model_syntax.txt          lavaan model syntax",
        "model.json                the model definition: Saved Models > Import JSON restores it",
        "lavaan_summary.txt        lavaan summary() output",
        "path_diagram.svg / .png   the path diagram as displayed (if available)"
      )
      add("README.txt", paste(readme, collapse = "\n"))

      session$sendCustomMessage("structura_build_zip", list(files = files))
    }, error = function(e) {
      showModal(modalDialog(
        title = "Export Error",
        div(class = "alert alert-danger",
            paste("Failed to prepare the results ZIP:", conditionMessage(e))),
        easyClose = TRUE,
        footer = modalButton("Dismiss")
      ))
    })
  })

  # ----------------- Auto-Optimize Model Server Observers ---------------
  prune_lock_table_data <- reactiveVal(NULL)
  prune_results <- reactiveVal(NULL)
  selected_prune_cand <- reactiveVal(NULL)
  selected_prune_idx <- reactiveVal(1)

  # Dynamic visibility toggle for Auto-Optimize button based on structural path selection, model convergence, and syntax synchronization
  observe({
    has_active_path <- has_active_struct_path(struct_table_data())

    model_res <- fit_model_safe()
    current_syntax <- lavaan_model_str()
    # A model that only fails identification (df < 0) can still be optimized: removing paths may fix it
    is_model_ready <- !is.null(model_res) &&
                      isTRUE(optimization_baseline(model_res)$usable) &&
                      !is.null(model_res$syntax) &&
                      identical(model_res$syntax, current_syntax)
    
    if (has_active_path && is_model_ready) {
      shinyjs::show("prune_model_btn")
    } else {
      shinyjs::hide("prune_model_btn")
    }
  })

  # PDF / ZIP need a successfully fitted model; Saved Models needs loaded data (models store variable names only)
  observe({
    model_res <- tryCatch(fit_model_safe(), error = function(e) NULL)
    shinyjs::toggle("export_pdf_btn", condition = !is.null(model_res) && isTRUE(model_res$ok) && !is.null(model_res$fit))
    shinyjs::toggle("export_zip_btn", condition = !is.null(model_res) && isTRUE(model_res$ok) && !is.null(model_res$fit))
    df <- tryCatch(processed_data(), error = function(e) NULL)
    shinyjs::toggle("saved_models_btn", condition = is.data.frame(df) && ncol(df) > 0)
  })

  # Trigger Auto-Optimize Step 1 Modal
  observeEvent(input$prune_model_btn, {
    struct_df <- isolate(struct_table_data())
    if (is.null(struct_df) || nrow(struct_df) == 0) {
      showModal(modalDialog(
        title = "Auto-Optimize Warning",
        div(class = "alert alert-warning",
            "No structural model defined. Please set up structural paths first."),
        easyClose = TRUE,
        footer = modalButton("Dismiss")
      ))
      return()
    }

    base_model <- fit_model_safe()
    base_info <- optimization_baseline(base_model)

    if (!isTRUE(base_info$usable)) {
      showModal(modalDialog(
        title = "Auto-Optimize Warning",
        div(class = "alert alert-warning",
            paste0("Could not fit baseline model for optimization: ", base_info$msg)),
        easyClose = TRUE,
        footer = modalButton("Dismiss")
      ))
      return()
    }
    base_unidentified_info <- if (!isTRUE(base_info$identified)) {
      base_df <- fit_identification(base_info$fit)$df
      div(style = "background-color: #eff6ff; border: 1px solid #bfdbfe; border-radius: 6px; padding: 10px 14px; margin-bottom: 15px; font-size: 13px; color: #1e3a8a;",
          tags$b("The current model is not identified"),
          paste0(if (is.finite(base_df) && base_df < 0) sprintf(" (df = %d)", as.integer(base_df)) else "",
                 ". Paths are removed first until the model is identified, then the best model is searched. "),
          "Models that are not identified are never ranked. Locked paths are never removed, and every variable keeps ",
          "at least one path, so lock fewer paths if no identified model can be found.")
    } else NULL

    # Initialize lock table data (filtered to active rows and active predictor columns)
    pred_cols <- names(struct_df)[3:ncol(struct_df)]
    
    active_row_indices <- c()
    for (i in seq_len(nrow(struct_df))) {
      has_active <- FALSE
      for (col in pred_cols) {
        if (isTRUE(as.logical(struct_df[i, col]))) {
          has_active <- TRUE
          break
        }
      }
      if (has_active) active_row_indices <- c(active_row_indices, i)
    }
    
    active_pred_cols <- c()
    for (col in pred_cols) {
      has_active <- FALSE
      for (i in seq_len(nrow(struct_df))) {
        if (isTRUE(as.logical(struct_df[i, col]))) {
          has_active <- TRUE
          break
        }
      }
      if (has_active) active_pred_cols <- c(active_pred_cols, col)
    }
    
    if (length(active_row_indices) == 0 || length(active_pred_cols) == 0) {
      lock_df <- struct_df
      for (col in pred_cols) lock_df[[col]] <- FALSE
    } else {
      lock_df <- struct_df[active_row_indices, c("Dependent", "Operator", active_pred_cols), drop = FALSE]
      for (col in active_pred_cols) {
        lock_df[[col]] <- FALSE
      }
    }
    prune_lock_table_data(lock_df)

    active_preds <- active_pred_cols

    showModal(modalDialog(
      title = "Auto-Optimize Model: Step 1 - Strategy, Parameters & Path Locking",
      size = "l",
      div(
        style = "padding: 10px;",
        base_unidentified_info,
        div(
          style = "background-color: #f8fafc; border: 1px solid #cbd5e1; border-radius: 6px; padding: 12px; margin-bottom: 15px;",
          div(style = "display: flex; justify-content: space-between; align-items: center; margin-bottom: 6px;",
              tags$b("Variable Isolation Prevention (Keep in Model)", style = "color: #1e293b; font-size: 14px;"),
              span(style = "font-size: 11px; color: #64748b;", "At least one connecting path will be retained")
          ),
          p("Every variable of your model always keeps at least one path (lavaan would otherwise drop it silently and the scores could no longer be compared), ",
            "and every dependent variable always keeps at least one incoming path (otherwise it would become exogenous and lavaan would replace the paths with covariances). ",
            "Select predictor variables below to additionally require an outgoing path.",
            style = "font-size: 12px; color: #64748b; margin-bottom: 10px;"),
          fluidRow(
            column(width = 12,
                   div(style = "display: flex; justify-content: space-between; align-items: center; margin-bottom: 4px;",
                       tags$label("Predictor Variables:", style = "font-size: 12px; font-weight: 600; margin: 0; color: #334155;"),
                       div(actionLink("retain_preds_all", "All", style = "font-size: 11px; margin-right: 6px; cursor: pointer;"),
                           actionLink("retain_preds_none", "None", style = "font-size: 11px; cursor: pointer;"))
                   ),
                   div(style = "max-height: 110px; overflow-y: auto; background: #ffffff; border: 1px solid #e2e8f0; border-radius: 4px; padding: 6px 10px;",
                       checkboxGroupInput("prune_retain_preds", label = NULL, choices = active_preds, selected = character(0))
                   )
            )
          )
        ),
        p("Only paths from the structural matrix are optimized. Equations typed under Manual Equations are kept unchanged in every candidate.",
          style = "font-size: 12px; color: #64748b; margin-bottom: 8px;"),
        p("Select structural paths to lock (protect from pruning).", style = "font-size: 13px; color: #334155; margin-bottom: 4px;"),
        p("Highlighted cells represent active paths in your current model. Unchecked active paths will be evaluated for optimization.",
          style = "font-size: 12px; color: #64748b;"),
        rHandsontableOutput("prune_lock_table"),
        tags$hr(),
        fluidRow(
          column(width = 6,
                 selectInput("prune_criterion", "Optimization Criterion:",
                             choices = c("AIC (Predictive Accuracy / Balanced)" = "AIC",
                                         "BIC (Stronger Sparsity Penalty)" = "BIC"),
                             selected = "AIC")
          ),
          column(width = 6,
                 selectInput("prune_strategy", "Search Algorithm Strategy:",
                             choices = c("Adaptive Auto-Switch (Recommended)" = "adaptive",
                                         "Regularized SEM (Lasso / Elastic Net - Modern Standard)" = "regsem",
                                         "Stepwise Search (Fast & Deterministic)" = "stepwise",
                                         "Exhaustive Search (100% Exact All-Subset)" = "exhaustive",
                                         "Simulated Annealing (SA - Fast Trajectory Search)" = "sa"),
                             selected = "adaptive")
          )
        ),
        tags$details(
          style = "margin-top: 15px; border: 1px solid #ddd; padding: 10px; border-radius: 4px; background-color: #fafafa;",
          tags$summary(
            style = "font-weight: bold; cursor: pointer; color: #333;",
            "Advanced Algorithm Hyper-Parameters"
          ),
          div(
            style = "margin-top: 10px;",
            conditionalPanel(
              condition = "input.prune_strategy == 'adaptive' || input.prune_strategy == 'exhaustive'",
              numericInput("max_exhaustive_comb", "Exhaustive Search Max Combinations Threshold:",
                           value = 1024, min = 64, max = 8192, step = 64)
            ),
            conditionalPanel(
              condition = "input.prune_strategy == 'sa'",
              fluidRow(
                column(4, numericInput("sa_max_iter", "SA Max Iterations:", value = 150, min = 20, max = 500, step = 10)),
                column(4, numericInput("sa_temp_init", "SA Initial Temp (in AIC/BIC units):", value = 10.0, min = 1.0, max = 100.0, step = 1.0)),
                column(4, numericInput("sa_seed", "Random Seed:", value = 1, min = 1, step = 1))
              ),
              helpText("The cooling rate is derived automatically so the temperature falls to 0.1 at the final iteration.")
            ),
            conditionalPanel(
              condition = "input.prune_strategy == 'regsem'",
              fluidRow(
                column(6, selectInput("regsem_type", "Regularization Penalty:",
                                     choices = c("Lasso (L1)" = "lasso", "Elastic Net" = "enet"),
                                     selected = "lasso")),
                column(6, numericInput("regsem_n_lambda", "Number of Lambda Steps:", value = 30, min = 10, max = 100, step = 5))
              ),
              helpText("Regularized structural equation modeling using L1/L2 penalties across varying lambda thresholds.")
            )
          )
        )
      ),
      footer = tagList(
        modalButton("Cancel"),
        actionButton("run_prune_explore", "Run Optimization", class = "btn btn-primary")
      )
    ))
  })

  # Render Lock Table in Modal 1
  output$prune_lock_table <- renderRHandsontable({
    lock_df <- prune_lock_table_data(); req(lock_df)
    struct_df <- struct_table_data(); req(struct_df)
    
    lock_pred_cols <- setdiff(names(lock_df), c("Dependent", "Operator"))
    
    struct_submatrix <- matrix(FALSE, nrow = nrow(lock_df), ncol = length(lock_pred_cols))
    for (r in seq_len(nrow(lock_df))) {
      dep <- lock_df$Dependent[r]
      struct_r_idx <- which(struct_df$Dependent == dep)
      if (length(struct_r_idx) > 0) {
        for (c_idx in seq_along(lock_pred_cols)) {
          col_name <- lock_pred_cols[c_idx]
          if (col_name %in% names(struct_df)) {
            struct_submatrix[r, c_idx] <- isTRUE(as.logical(struct_df[struct_r_idx[1], col_name]))
          }
        }
      }
    }
    
    rh <- rhandsontable(lock_df, rowHeaders = FALSE) %>%
      hot_table(highlightReadOnly = TRUE, fixedColumnsLeft = 2)
    rh <- hot_col(rh, "Dependent", readOnly = TRUE)
    rh <- hot_col(rh, "Operator",  readOnly = TRUE)
    
    rh$x$struct_matrix <- struct_submatrix
    
    renderer_js <- "
      function(instance, td, row, col, prop, value, cellProperties) {
        Handsontable.renderers.CheckboxRenderer.apply(this, arguments);
        var params = instance.params || instance.getSettings();
        var struct_matrix = params.struct_matrix;
        var col_var_idx = col - 2;
        
        if (struct_matrix && row >= 0 && row < struct_matrix.length && col_var_idx >= 0 && col_var_idx < struct_matrix[0].length) {
          var is_active = struct_matrix[row][col_var_idx];
          if (is_active === true || is_active === 'TRUE' || is_active === 'true') {
            td.style.backgroundColor = '#e0f2fe';
            td.style.fontWeight = 'bold';
            td.style.cursor = '';
            td.classList.remove('htDimmed');
            cellProperties.readOnly = false;
          } else {
            cellProperties.readOnly = true;
            td.style.backgroundColor = '#f0f0f0';
            td.style.cursor = 'not-allowed';
            td.classList.add('htDimmed');
          }
        }
      }"
    
    for (col_name in lock_pred_cols) {
      rh <- hot_col(rh, col_name, type = "checkbox", renderer = renderer_js)
    }
    rh
  })

  observeEvent(input$prune_lock_table, {
    tbl <- hot_to_r(input$prune_lock_table); req(tbl)
    prune_lock_table_data(tbl)
  })

  # Observers for Variable Isolation All / None quick select links (predictors only; dependents are always retained)
  observeEvent(input$retain_preds_all, {
    lock_df <- prune_lock_table_data(); req(lock_df)
    active_preds <- setdiff(names(lock_df), c("Dependent", "Operator"))
    updateCheckboxGroupInput(session, "prune_retain_preds", selected = active_preds)
  })
  observeEvent(input$retain_preds_none, {
    updateCheckboxGroupInput(session, "prune_retain_preds", selected = character(0))
  })

  # Reactive state variables for stepwise optimization stepper engine
  opt_running <- reactiveVal(FALSE)
  opt_step <- reactiveVal(0)
  opt_state <- reactiveVal(NULL)
  pending_prune_check <- reactiveVal(NULL)   # AIC/BIC of an applied candidate, verified after the main refit

  finalize_prune_results <- function(st, current_step = 0, stopped_early = FALSE) {
    opt_running(FALSE)
    
    candidates_list <- unname(st$candidates_map)
    scores <- vapply(candidates_list, candidate_score, numeric(1), criterion = st$criterion)

    ord <- order(scores, decreasing = FALSE)
    sorted_candidates <- candidates_list[ord]
    scores <- scores[ord]
    if (length(sorted_candidates) > 0 && is.finite(scores[1]) && sorted_candidates[[1]]$status != "[Baseline]") {
      sorted_candidates[[1]]$status <- "[Optimal]"
    }

    # Distance from the best candidate and Akaike-type weights (exp(-delta/2), normalized).
    # Candidates within 2 units of the best are statistically hard to distinguish from it.
    best_score <- if (length(scores) > 0) scores[1] else Inf
    if (is.finite(best_score)) {
      delta_best <- scores - best_score
      w_raw <- ifelse(is.finite(delta_best), exp(-delta_best / 2), 0)
      weights <- w_raw / sum(w_raw)
      for (k in seq_along(sorted_candidates)) {
        sorted_candidates[[k]]$delta_best <- delta_best[k]
        sorted_candidates[[k]]$weight <- weights[k]
        if (k > 1 && is.finite(delta_best[k]) && delta_best[k] < 2 &&
            sorted_candidates[[k]]$status %in% c("[Good]", "[Improved]")) {
          sorted_candidates[[k]]$status <- "[Equivalent]"
        }
      }
    }

    # Post-compute detailed fit measures (CFI, RMSEA, SRMR) for the top candidates only. Keeping
    # every lavaan object (up to 2^M of them for the exhaustive search) exhausts memory in WebR,
    # so lower-ranked fits are released and re-estimated on demand when previewed.
    max_keep_fits <- 20L
    base_violations <- fit_cutoff_violations(st$base_ms)
    if (length(sorted_candidates) > 0) {
      for (k in seq_along(sorted_candidates)) {
        cand_k <- sorted_candidates[[k]]
        if (k > max_keep_fits) {
          sorted_candidates[[k]]$fit <- NULL
          next
        }
        if (cand_k$converged && !is.null(cand_k$fit) && is.na(cand_k$cfi)) {
          ms_k <- tryCatch(lavaan::fitMeasures(cand_k$fit, c("cfi", "rmsea", "srmr")), error = function(e) NULL)
          if (!is.null(ms_k)) {
            sorted_candidates[[k]]$cfi <- as.numeric(ms_k["cfi"])
            sorted_candidates[[k]]$rmsea <- as.numeric(ms_k["rmsea"])
            sorted_candidates[[k]]$srmr <- as.numeric(ms_k["srmr"])
            # Flag only a NEW violation: a cutoff the baseline met but this candidate breaks. If the
            # baseline already violates a cutoff, every candidate would otherwise be flagged as well.
            # (no baseline fit measures exist when the start was not identified, so nothing can be "new")
            if (isTRUE(st$base_identified %||% TRUE) && any(fit_cutoff_violations(ms_k) & !base_violations)) {
              if (!sorted_candidates[[k]]$status %in% c("[Baseline]", "[Optimal]", "[Replaced]", "[Variable Dropped]", "[Not Identified]")) {
                sorted_candidates[[k]]$status <- "[Degraded Fit]"
              }
            }
          }
        }
      }
    }
    
    iso_parts <- c()
    if (length(st$retain_preds) > 0) iso_parts <- c(iso_parts, paste0("Pred: ", paste(st$retain_preds, collapse = ", ")))
    isolation_summary <- if (length(iso_parts) > 0) paste(iso_parts, collapse = " | ") else ""

    res <- list(
      candidates = sorted_candidates,
      criterion = st$criterion,
      strategy_used = st$eff_strategy,
      isolation_summary = isolation_summary,
      stopped_early = stopped_early,
      stopped_at_step = current_step,
      max_steps = st$max_steps,
      ctx = st$ctx,
      fallback_note = st$fallback_note %||% "",
      base_identified = isTRUE(st$base_identified %||% TRUE),
      repair_note = st$repair_note %||% "",
      any_identified = any(is.finite(scores)),
      message = "Success"
    )
    
    prune_results(res)
    selected_prune_cand(res$candidates[[1]])
    selected_prune_idx(1)
    
    stopped_badge <- if (stopped_early) {
      sprintf(" [Stopped Early at Step %d / %d]", current_step, st$max_steps)
    } else ""
    
    showModal(modalDialog(
      title = div(
        style = "display: flex; justify-content: space-between; align-items: center; width: calc(100% - 30px); margin-right: 15px;",
        span(paste0("Auto-Optimize Model: Step 2 - Candidate Ranking Catalog", stopped_badge), style = "font-weight: bold;"),
        div(
          style = "display: flex; gap: 8px; align-items: center;",
          modalButton("Close / Cancel"),
          actionButton("apply_pruned_model", "Apply Selected Model to UI", class = "btn btn-success", style = "font-weight: 600; white-space: nowrap;")
        )
      ),
      size = "l",
      div(
        style = "padding: 10px;",
        div(
          style = if (stopped_early) {
            "background-color: #fffbeb; padding: 10px 14px; border-radius: 6px; border: 1px solid #fef3c7; margin-bottom: 15px;"
          } else {
            "background-color: #f8fafc; padding: 10px 14px; border-radius: 6px; border: 1px solid #e2e8f0; margin-bottom: 15px;"
          },
          p(sprintf("Strategy Used: %s%s%s. Click any candidate row in the table below or use navigation arrows to preview path diagrams. Models with degraded fit indices are flagged.",
                    toupper(res$strategy_used),
                    if (nzchar(res$isolation_summary %||% "")) paste0(" [Protected: ", res$isolation_summary, "]") else "",
                    if (stopped_early) sprintf(" (Exploration halted early at step %d of %d; best candidates evaluated so far are shown)", current_step, st$max_steps) else ""),
            style = if (stopped_early) "margin: 0; font-size: 13px; color: #92400e;" else "margin: 0; font-size: 13px; color: #334155;"),
          p("Note: the top-ranked model was selected using the same data it is evaluated on, so its fit is optimistic. ",
            "Candidates marked [Equivalent] are within 2 ", res$criterion, " units of the best and cannot be reliably distinguished from it; ",
            "prefer the one that is theoretically most defensible. [Improper] models (e.g. negative variances) are never ranked first. ",
            "Candidates are estimated with lavaan's default rules, exactly like a model you build by hand. A [Replaced] candidate is one where ",
            "lavaan frees a covariance in place of a removed path (see Added Cov.), so the association is not actually removed; ",
            "[Variable Dropped] means a variable lost all of its paths and was left out of the model. Neither can be ranked as optimal. ",
            "[Not Identified] models (df < 0 or no standard errors) show no AIC/BIC and are never ranked.",
            style = "margin: 6px 0 0 0; font-size: 12px; color: #64748b;"),
          if (nzchar(res$fallback_note %||% ""))
            p(res$fallback_note, style = "margin: 6px 0 0 0; font-size: 12px; color: #b45309; font-weight: 600;")
        ),
        if (!res$any_identified)
          div(class = "alert alert-warning", style = "font-size: 13px;",
              "No identified model was found. ",
              if (nzchar(res$repair_note)) res$repair_note
              else "Unlock paths or simplify the model (every variable keeps at least one path), then run the optimization again."),
        if (!res$base_identified && res$any_identified)
          p("The starting model was not identified, so ΔAIC/ΔBIC against it are not shown; compare candidates with \"Δ vs Best\" and Weight.",
            style = "margin: 0 0 8px 0; font-size: 12px; color: #1e3a8a;"),
        DTOutput("prune_candidates_table"),
        tags$hr(style = "margin: 15px 0;"),
        div(
          style = "display: flex; justify-content: space-between; align-items: center; margin-top: 0; margin-bottom: 10px;",
          h5("Path Diagram Preview for Selected Candidate:", style = "margin: 0; font-weight: 600;"),
          uiOutput("prune_cand_indicator_ui", inline = TRUE)
        ),
        div(style = "height: 320px; border: 1px solid #ccc; position: relative; border-radius: 4px; overflow: hidden; background-color: #ffffff;",
            actionButton("prune_prev_cand", label = "<", class = "btn btn-default btn-sm",
                         style = "position: absolute; left: 12px; top: 50%; transform: translateY(-50%); z-index: 10; width: 38px; height: 38px; padding: 0; border-radius: 50%; background: rgba(255, 255, 255, 0.9); border: 1px solid #cbd5e1; box-shadow: 0 2px 5px rgba(0,0,0,0.15); display: flex; align-items: center; justify-content: center;",
                         title = "Previous Candidate"),
            actionButton("prune_next_cand", label = ">", class = "btn btn-default btn-sm",
                         style = "position: absolute; right: 12px; top: 50%; transform: translateY(-50%); z-index: 10; width: 38px; height: 38px; padding: 0; border-radius: 50%; background: rgba(255, 255, 255, 0.9); border: 1px solid #cbd5e1; box-shadow: 0 2px 5px rgba(0,0,0,0.15); display: flex; align-items: center; justify-content: center;",
                         title = "Next Candidate"),
            tags$div(id = "prune_preview_container", 
                     style = "width:100%; height:100%; display: flex; align-items: center; justify-content: center; color: #666;",
                     "Select a candidate row above to view preview."))
      ),
      footer = NULL
    ))
  }

  observeEvent(input$stop_prune_explore, {
    if (!isTRUE(opt_running())) return()
    st <- opt_state()
    if (is.null(st)) return()
    curr_s <- opt_step()
    finalize_prune_results(st, current_step = curr_s, stopped_early = TRUE)
  })

  observeEvent(input$cancel_prune_explore, {
    opt_running(FALSE)
    opt_state(NULL)
    removeModal()
    showNotification("Model optimization cancelled.", type = "message", duration = 3)
  })

  # 2. Run Candidate Search & Display Step 2 Modal via Stepwise Stepper Engine
  observeEvent(input$run_prune_explore, {
    base_model <- fit_model_safe()
    base_info <- optimization_baseline(base_model)
    if (!isTRUE(base_info$usable)) {
      showModal(modalDialog(
        title = "Auto-Optimize Warning",
        div(class = "alert alert-warning",
            paste0("Could not fit baseline model for optimization: ", base_info$msg)),
        easyClose = TRUE,
        footer = modalButton("Dismiss")
      ))
      return()
    }
    base_fit <- base_info$fit
    base_identified <- isTRUE(base_info$identified)
    if (!base_identified && identical(input$prune_strategy, "regsem")) {
      showModal(modalDialog(
        title = "Auto-Optimize Warning",
        div(class = "alert alert-warning",
            "Regularized SEM needs an identified starting model, but the current model is not identified. ",
            "Choose Adaptive, Stepwise, Exhaustive or Simulated Annealing: they remove paths until the model is identified."),
        easyClose = TRUE,
        footer = modalButton("Dismiss")
      ))
      return()
    }

    struct_df <- struct_table_data()
    lock_df <- prune_lock_table_data()
    pred_cols <- names(struct_df)[3:ncol(struct_df)]
    active_paths <- list()

    for (i in seq_len(nrow(struct_df))) {
      dep <- struct_df$Dependent[i]
      if (!nzchar(dep)) next
      for (p in pred_cols) {
        if (isTRUE(as.logical(struct_df[i, p]))) {
          is_locked <- FALSE
          if (!is.null(lock_df) && dep %in% lock_df$Dependent && p %in% names(lock_df)) {
            lock_r_idx <- which(lock_df$Dependent == dep)
            if (length(lock_r_idx) > 0) {
              is_locked <- isTRUE(as.logical(lock_df[lock_r_idx[1], p]))
            }
          }
          active_paths[[length(active_paths) + 1]] <- list(
            dep = dep, pred = p, locked = is_locked
          )
        }
      }
    }

    removable_paths <- Filter(function(x) !x$locked, active_paths)
    M <- length(removable_paths)

    if (M == 0) {
      showModal(modalDialog(
        title = "Auto-Optimize Warning",
        div(class = "alert alert-warning", "No unlocked structural paths available for optimization. All active paths are locked."),
        easyClose = TRUE,
        footer = modalButton("Dismiss")
      ))
      return()
    }

    # Every dependent variable of the baseline always keeps an incoming path: losing all of them makes it
    # exogenous and lavaan replaces the paths with covariances, so such candidates are never comparable.
    retain_deps <- unique(struct_edges(struct_df)$dep)
    retain_preds <- input$prune_retain_preds %||% character(0)

    if (!check_variable_isolation(struct_df, retain_deps, retain_preds, pred_cols)) {
      showModal(modalDialog(
        title = "Auto-Optimize Warning",
        div(class = "alert alert-warning",
            "The current baseline model does not satisfy the specified variable isolation constraints."),
        easyClose = TRUE,
        footer = modalButton("Dismiss")
      ))
      return()
    }

    criterion <- input$prune_criterion
    strategy <- input$prune_strategy
    total_comb <- 2^M
    # An empty or invalid threshold field arrives as NA; fall back to the default instead of failing in `if`
    max_comb <- suppressWarnings(as.numeric(input$max_exhaustive_comb %||% 1024))
    if (length(max_comb) != 1 || !is.finite(max_comb) || max_comb < 1) max_comb <- 1024

    eff_strategy <- strategy
    if (strategy == "adaptive") {
      if (total_comb <= max_comb) {
        eff_strategy <- "exhaustive"
      } else {
        eff_strategy <- "stepwise"
        showNotification(
          sprintf("Search space (%s combinations) exceeds the threshold (%s): using Stepwise Search, which does not guarantee the global optimum.",
                  format(total_comb, big.mark = ",", scientific = FALSE), format(max_comb, big.mark = ",", scientific = FALSE)),
          type = "message", duration = 8)
      }
    } else if (strategy == "exhaustive" && total_comb > max_comb) {
      showModal(modalDialog(
        title = "Auto-Optimize Warning",
        div(class = "alert alert-warning",
            sprintf("Exhaustive Search would need %s combinations (%d removable paths), which exceeds the threshold of %s. ",
                    format(total_comb, big.mark = ",", scientific = FALSE), M, format(max_comb, big.mark = ",", scientific = FALSE)),
            "Lock more paths, raise the threshold, or choose another search strategy (e.g. Stepwise or Adaptive)."),
        easyClose = TRUE,
        footer = modalButton("Dismiss")
      ))
      return()
    }

    # An unidentified start has no meaningful AIC/BIC: there is no baseline score to compare candidates with
    base_ms <- if (base_identified) {
      lavaan::fitMeasures(base_fit, c("aic", "bic", "cfi", "rmsea", "srmr"))
    } else {
      c(aic = NA_real_, bic = NA_real_, cfi = NA_real_, rmsea = NA_real_, srmr = NA_real_)
    }
    base_score <- if (criterion == "AIC") as.numeric(base_ms["aic"]) else as.numeric(base_ms["bic"])

    meas_syntax <- hot_to_r(input$input_table)
    mlines <- build_meas_lines(meas_syntax)

    extra <- strsplit(input$extra_eq, "\\n")[[1]]
    extra <- trimws(extra); extra <- extra[nzchar(extra)]
    needs_meanstructure <- (input$analysis_mode == "raw" || 
                            input$missing_method %in% c("ml", "ml.x", "two.stage", "robust.two.stage"))

    # Display Progress Modal with live HTML5 Canvas Chart
    showModal(modalDialog(
      title = "Auto-Optimize Model: Optimizing Model Space...",
      footer = div(
        style = "display: flex; justify-content: space-between; align-items: center; width: 100%;",
        actionButton("cancel_prune_explore", "Cancel", class = "btn btn-default", `data-dismiss` = "modal"),
        actionButton("stop_prune_explore", "Stop & View Results", class = "btn btn-warning")
      ),
      div(
        style = "text-align: center; padding: 15px;",
        div(style = "display: flex; justify-content: space-between; align-items: center; margin-bottom: 8px;",
            span(style = "font-weight: bold; color: #475569; font-size: 13px;",
                 sprintf("Strategy: %s | Criterion: %s", toupper(eff_strategy), criterion)),
            span(id = "prune_iter_badge", class = "badge badge-primary", style = "font-size: 12px; background-color: #2563eb;", "Step 0")
        ),
        tags$canvas(id = "opt_chart_canvas", width = "540", height = "200",
                    style = "border-radius: 8px; box-shadow: 0 4px 6px rgba(0,0,0,0.15); background-color: #0f172a; width: 100%; max-width: 540px; height: 200px;"),
        div(style = "width: 100%; background-color: #e2e8f0; border-radius: 6px; height: 8px; overflow: hidden; margin-top: 12px;",
            div(id = "prune_progress_bar_inner", style = "width: 0%; height: 100%; background-color: #3b82f6; transition: width 0.15s ease;")
        ),
        div(id = "prune_progress_status", style = "margin-top: 10px; font-weight: 600; color: #1e293b; font-size: 13px;",
            "Initializing baseline model and structural constraints...")
      )
    ))

    # Initialize stepper state
    sa_max_iter <- as.integer(input$sa_max_iter %||% 150)
    regsem_n_lambda <- as.integer(input$regsem_n_lambda %||% 30)

    # Upper bound on the number of candidate evaluations of a backward-elimination run:
    # at most M + (M-1) + ... + 1 fits (one per tick, so Stop/Cancel stay responsive).
    max_steps <- if (eff_strategy == "exhaustive") {
      total_comb
    } else if (eff_strategy == "stepwise") {
      M * (M + 1) / 2
    } else if (eff_strategy == "sa") {
      sa_max_iter
    } else {
      regsem_n_lambda
    }
    grid_matrix <- if (eff_strategy == "exhaustive") expand.grid(replicate(M, c(FALSE, TRUE), simplify = FALSE)) else NULL

    # Reproducible stochastic search; temperature is annealed from T0 to 0.1 over the whole run.
    set.seed(as.integer(input$sa_seed %||% 1))
    sa_T0 <- max(input$sa_temp_init %||% 10.0, 0.2)
    sa_alpha <- (0.1 / sa_T0)^(1 / max(sa_max_iter, 1))

    # Candidates are estimated exactly like the main model. They stay comparable with the baseline only if
    # no variable disappears (required_vars always keep a path) and no removed path is silently replaced
    # by a covariance (checked after each fit against base_ov / base_cov_pairs).
    ctx <- list(
      data = processed_data(),
      missing_method = input$missing_method,
      needs_meanstructure = needs_meanstructure,
      meas_lines = mlines,
      extra_lines = extra,
      required_vars = required_struct_vars(struct_df, base_fit),
      base_ov = lavaan::lavNames(base_fit, "ov"),
      base_cov_pairs = free_cov_pairs(base_fit),
      base_fit = base_fit
    )
    base_ident <- fit_identification(base_fit)

    state_obj <- list(
      eff_strategy = eff_strategy,
      criterion = criterion,
      ctx = ctx,
      base_fit = base_fit,
      base_identified = base_identified,
      base_ms = base_ms,
      base_score = base_score,
      removable_paths = removable_paths,
      M = M,
      struct_df = struct_df,
      pred_cols = pred_cols,
      max_steps = max_steps,
      grid_matrix = grid_matrix,
      candidates_map = list(),
      scores_hist = numeric(0),
      best_scores_hist = if (is.finite(base_score)) base_score else numeric(0),
      curr_vec = rep(TRUE, M),
      curr_df = struct_df,
      T_val = sa_T0,
      sa_alpha = sa_alpha,
      # Stepwise sub-step state: one candidate fit per tick
      sw_idx = 1L,
      sw_best_score = NULL,
      sw_best_df = NULL,
      sw_improved = FALSE,
      regsem_type = input$regsem_type %||% "lasso",
      regsem_n_lambda = regsem_n_lambda,
      retain_deps = retain_deps,
      retain_preds = retain_preds,
      # Repair phase: remove paths until the model is identified (Stepwise and SA only; Exhaustive
      # enumerates every combination, so the identified ones simply rank)
      repair = !base_identified && eff_strategy %in% c("stepwise", "sa"),
      repair_df = struct_df,
      repair_deficit = base_ident$deficit,
      repair_idx = 1L,
      repair_best = NULL,
      repair_ticks = 0L
    )

    base_key <- make_struct_key(struct_df)
    state_obj$candidates_map[[base_key]] <- list(
      removed_str = "None (Baseline Model)",
      retained_str = build_retained_str(struct_df),
      struct_df = struct_df,
      fit = base_fit,
      aic = as.numeric(base_ms["aic"]),
      bic = as.numeric(base_ms["bic"]),
      delta_aic = if (base_identified) 0.0 else NA_real_,
      delta_bic = if (base_identified) 0.0 else NA_real_,
      cfi = as.numeric(base_ms["cfi"]),
      rmsea = as.numeric(base_ms["rmsea"]),
      srmr = as.numeric(base_ms["srmr"]),
      converged = TRUE,
      proper = fit_is_proper(base_fit),
      identified = base_identified, deficit = base_ident$deficit,
      vars_ok = TRUE, replaced = FALSE, added_covs = character(0),
      status = "[Baseline]"
    )

    opt_state(state_obj)
    opt_step(0)
    opt_running(TRUE)
  })

  # Stepwise optimization execution observer with real-time UI yielding
  observe({
    req(opt_running())
    invalidateLater(30, session)
    
    step <- isolate(opt_step())
    st <- isolate(opt_state())
    req(st)

    if (step >= st$max_steps) {
      finalize_prune_results(st, current_step = step, stopped_early = FALSE)
      return()
    }

    make_key_local <- make_struct_key

    build_candidate_record_local <- function(curr_s_df, removed_str) {
      fm <- fit_candidate_model(curr_s_df, st$ctx)
      ret_str <- build_retained_str(curr_s_df)
      if (is.null(fm) || !isTRUE(lavaan::lavInspect(fm, "converged"))) {
        return(list(
          removed_str = if (nzchar(removed_str)) removed_str else "None (Baseline Model)",
          retained_str = ret_str,
          struct_df = curr_s_df, fit = NULL,
          aic = NA_real_, bic = NA_real_, delta_aic = NA_real_, delta_bic = NA_real_,
          cfi = NA_real_, rmsea = NA_real_, srmr = NA_real_,
          converged = FALSE, proper = FALSE, identified = FALSE, deficit = Inf, raw_aic = NA_real_,
          status = "[Non-converged]"
        ))
      }
      # Ultra-fast score extraction using stats::AIC and stats::BIC (skips baseline model fitting)
      c_aic <- tryCatch(as.numeric(stats::AIC(fm)), error = function(e) NA_real_)
      c_bic <- tryCatch(as.numeric(stats::BIC(fm)), error = function(e) NA_real_)
      d_aic <- c_aic - as.numeric(st$base_ms["aic"])
      d_bic <- c_bic - as.numeric(st$base_ms["bic"])
      proper <- fit_is_proper(fm)
      chk <- candidate_structure_check(fm, st$ctx)
      ident <- fit_identification(fm)
      raw_aic <- c_aic   # kept for the repair phase, which still has to compare unidentified fits
      if (!chk$vars_ok || !ident$identified) {
        # Fitted on a different set of variables, or not identified: its AIC/BIC are meaningless and
        # cannot be compared with the baseline at all
        c_aic <- c_bic <- d_aic <- d_bic <- NA_real_
      }

      stat <- "[Good]"
      if (isTRUE((st$criterion == "AIC" && d_aic < -0.01) || (st$criterion == "BIC" && d_bic < -0.01))) stat <- "[Improved]"
      if (!proper) stat <- "[Improper]"
      if (!ident$identified) stat <- "[Not Identified]"
      if (chk$replaced) stat <- "[Replaced]"
      if (!chk$vars_ok) stat <- "[Variable Dropped]"

      list(
        removed_str = if (nzchar(removed_str)) removed_str else "None (Baseline Model)",
        retained_str = ret_str,
        struct_df = curr_s_df, fit = fm,
        aic = c_aic, bic = c_bic, delta_aic = d_aic, delta_bic = d_bic,
        cfi = NA_real_, rmsea = NA_real_, srmr = NA_real_,
        converged = TRUE, proper = proper, status = stat,
        identified = ident$identified, deficit = ident$deficit, raw_aic = raw_aic,
        vars_ok = chk$vars_ok, replaced = chk$replaced, added_covs = chk$added_covs
      )
    }

    # Score of a record under the active criterion; Inf for non-converged or improper fits
    score_of <- function(rec) candidate_score(rec, st$criterion)

    curr_score_step <- st$base_score
    # Without a baseline score (unidentified start) nothing has been achieved yet
    best_curr <- if (length(st$best_scores_hist) > 0) tail(st$best_scores_hist, 1)
                 else if (is.finite(st$base_score)) st$base_score else Inf

    step_advance <- 1L

    removed_label <- function(df) {
      rem_vec <- c()
      for (jp in st$removable_paths) {
        if (!isTRUE(as.logical(df[df$Dependent == jp$dep, jp$pred]))) rem_vec <- c(rem_vec, paste0(jp$dep, " ~ ", jp$pred))
      }
      paste(rem_vec, collapse = "; ")
    }

    if (isTRUE(st$repair)) {
      # Repair phase (unidentified start, Stepwise/SA): remove one path per sweep until the model is identified.
      # One uncached candidate fit per tick, like the stepwise sweep. The removal that leaves the smallest
      # identification deficit (ties: lowest raw AIC) is adopted at the end of each sweep.
      st$repair_ticks <- st$repair_ticks + 1L
      evaluated <- FALSE
      while (!evaluated && st$repair_idx <= length(st$removable_paths)) {
        rp <- st$removable_paths[[st$repair_idx]]
        st$repair_idx <- st$repair_idx + 1L
        if (!isTRUE(as.logical(st$repair_df[st$repair_df$Dependent == rp$dep, rp$pred]))) next
        test_s_df <- st$repair_df
        test_s_df[test_s_df$Dependent == rp$dep, rp$pred] <- FALSE
        if (!check_variable_isolation(test_s_df, st$retain_deps, st$retain_preds, st$pred_cols, st$ctx$required_vars)) next

        k_str <- make_key_local(test_s_df)
        if (!k_str %in% names(st$candidates_map)) {
          st$candidates_map[[k_str]] <- build_candidate_record_local(test_s_df, removed_label(test_s_df))
          evaluated <- TRUE   # only a real model fit consumes the tick; cached candidates are free
        }
        rec <- st$candidates_map[[k_str]]
        # Only fits that converged and keep the variables/paths comparable can be walked through
        if (isTRUE(rec$converged) && !isFALSE(rec$vars_ok) && !isTRUE(rec$replaced)) {
          cand_def <- rec$deficit
          cand_aic <- if (is.finite(rec$raw_aic %||% NA_real_)) rec$raw_aic else Inf
          best <- st$repair_best
          if (is.null(best) || cand_def < best$deficit - 1e-9 ||
              (abs(cand_def - best$deficit) <= 1e-9 && cand_aic < best$raw_aic)) {
            st$repair_best <- list(df = test_s_df, deficit = cand_def, raw_aic = cand_aic,
                                   identified = is.finite(score_of(rec)))
          }
        }
      }

      if (st$repair_idx > length(st$removable_paths)) {
        # Sweep finished
        best <- st$repair_best
        if (is.null(best) || st$repair_ticks > st$M * (st$M + 1) / 2 + st$M) {
          st$repair_note <- paste0("Removing paths could not make the model identified: the locked paths and the rule that ",
                                   "every variable keeps a path leave no identified submodel. Unlock paths or simplify the model.")
          finalize_prune_results(st, current_step = isolate(opt_step()), stopped_early = FALSE)
          return()
        }
        st$repair_df <- best$df
        st$repair_deficit <- best$deficit
        st$repair_idx <- 1L
        st$repair_best <- NULL
        if (isTRUE(best$identified)) {
          # Identified: continue with the chosen strategy from this model
          st$repair <- FALSE
          st$curr_df <- best$df
          st$curr_vec <- vapply(st$removable_paths, function(rp) {
            isTRUE(as.logical(best$df[best$df$Dependent == rp$dep, rp$pred]))
          }, logical(1))
          # The repaired model is the first scored model: it is the best found so far
          s0 <- score_of(st$candidates_map[[make_key_local(best$df)]])
          if (is.finite(s0)) {
            st$scores_hist <- c(st$scores_hist, s0)
            st$best_scores_hist <- c(st$best_scores_hist, s0)
          }
        }
      }

      if (!isTRUE(isolate(opt_running()))) return()
      opt_state(st)
      n_removed <- sum(!vapply(st$removable_paths, function(rp) {
        isTRUE(as.logical(st$repair_df[st$repair_df$Dependent == rp$dep, rp$pred]))
      }, logical(1)))
      session$sendCustomMessage("update_optimization_live_chart", list(
        step = isolate(opt_step()),
        maxIter = st$max_steps,
        scores = st$scores_hist,
        best_scores = st$best_scores_hist,
        baseScore = NULL,
        detail = if (isTRUE(st$repair))
          sprintf("Repairing identification: %d path(s) removed so far (remaining deficit %.1f)", n_removed, st$repair_deficit)
        else "Identified model reached. Searching for the best model..."
      ))
      return()   # repair ticks do not use up the strategy's step budget
    }

    if (st$eff_strategy == "stepwise") {
      # Backward elimination, one candidate fit per tick so Stop/Cancel stay responsive in the
      # single-threaded WebR runtime. A sweep tries removing each remaining active path from the
      # current model; at the end of the sweep the best improving removal (if any) is adopted.
      if (is.null(st$sw_best_score)) {
        st$sw_best_score <- score_of(st$candidates_map[[make_key_local(st$curr_df)]])
        st$sw_best_df <- st$curr_df
        st$sw_start_score <- st$sw_best_score
        st$sw_tie_df <- NULL
        st$sw_improved <- FALSE
        st$sw_idx <- 1L
      }

      evaluated <- FALSE
      last_cand_score <- NA_real_
      while (!evaluated && st$sw_idx <= length(st$removable_paths)) {
        rp <- st$removable_paths[[st$sw_idx]]
        st$sw_idx <- st$sw_idx + 1L
        if (!isTRUE(as.logical(st$curr_df[st$curr_df$Dependent == rp$dep, rp$pred]))) next

        test_s_df <- st$curr_df
        test_s_df[test_s_df$Dependent == rp$dep, rp$pred] <- FALSE

        # Check variable isolation constraints
        if (!check_variable_isolation(test_s_df, st$retain_deps, st$retain_preds, st$pred_cols, st$ctx$required_vars)) next

        rem_vec <- c()
        for (j in seq_along(st$removable_paths)) {
          jp <- st$removable_paths[[j]]
          if (!isTRUE(as.logical(test_s_df[test_s_df$Dependent == jp$dep, jp$pred]))) {
            rem_vec <- c(rem_vec, paste0(jp$dep, " ~ ", jp$pred))
          }
        }
        k_str <- make_key_local(test_s_df)
        if (!k_str %in% names(st$candidates_map)) {
          st$candidates_map[[k_str]] <- build_candidate_record_local(test_s_df, paste(rem_vec, collapse = "; "))
          evaluated <- TRUE   # only a real model fit consumes the tick; cached candidates are free
        }
        cand_score <- score_of(st$candidates_map[[k_str]])
        last_cand_score <- cand_score
        if (is.finite(cand_score) && cand_score < st$sw_best_score - 0.01) {
          st$sw_best_score <- cand_score
          st$sw_best_df <- test_s_df
          st$sw_improved <- TRUE
        } else if (is.finite(cand_score) && is.null(st$sw_tie_df) &&
                   abs(cand_score - st$sw_start_score) <= 0.01) {
          # Equal fit with one path fewer (typical for saturated structural parts, where removing a
          # path just frees a covariance). Remembered so the search can walk across such plateaus.
          st$sw_tie_df <- test_s_df
        }
      }

      if (st$sw_idx > length(st$removable_paths)) {
        # Sweep finished
        if (st$sw_improved || !is.null(st$sw_tie_df)) {
          # Prefer a strictly improving removal; otherwise accept the first equal-fit removal (parsimony)
          st$curr_df <- if (st$sw_improved) st$sw_best_df else st$sw_tie_df
          st$sw_best_score <- NULL   # start a fresh sweep from the adopted model on the next tick
        } else {
          step_advance <- st$max_steps + 1   # no improving removal left: search converged
        }
      }

      curr_score_step <- if (is.finite(last_cand_score)) last_cand_score else
        if (!is.null(st$sw_best_score) && is.finite(st$sw_best_score)) st$sw_best_score else st$base_score
      st$scores_hist <- c(st$scores_hist, curr_score_step)
      if (is.finite(curr_score_step) && curr_score_step < best_curr) {
        best_curr <- curr_score_step
      }
      st$best_scores_hist <- c(st$best_scores_hist, best_curr)
    } else if (st$eff_strategy == "exhaustive") {
      chunk_size <- min(4L, st$max_steps - step)
      for (ci in seq_len(chunk_size)) {
        row_i <- step + ci
        if (row_i > st$max_steps) break
        state_vec <- as.logical(st$grid_matrix[row_i, ])
        test_s_df <- st$struct_df
        removed_paths_vec <- c()
        for (idx in seq_along(st$removable_paths)) {
          rp <- st$removable_paths[[idx]]
          if (!state_vec[idx]) {
            test_s_df[test_s_df$Dependent == rp$dep, rp$pred] <- FALSE
            removed_paths_vec <- c(removed_paths_vec, paste0(rp$dep, " ~ ", rp$pred))
          }
        }
        # Check variable isolation constraints
        if (!check_variable_isolation(test_s_df, st$retain_deps, st$retain_preds, st$pred_cols, st$ctx$required_vars)) {
          curr_score_step <- Inf
        } else {
          k_str <- make_key_local(test_s_df)
          if (!k_str %in% names(st$candidates_map)) {
            rem_label <- paste(removed_paths_vec, collapse = "; ")
            st$candidates_map[[k_str]] <- build_candidate_record_local(test_s_df, rem_label)
          }
          curr_score_step <- score_of(st$candidates_map[[k_str]])
        }
        st$scores_hist <- c(st$scores_hist, curr_score_step)
        if (is.finite(curr_score_step) && curr_score_step < best_curr) {
          best_curr <- curr_score_step
        }
        st$best_scores_hist <- c(st$best_scores_hist, best_curr)
      }
      step_advance <- chunk_size
    } else if (st$eff_strategy == "sa") {
      chunk_size <- min(4L, st$max_steps - step)
      for (ci in seq_len(chunk_size)) {
        flip_pos <- sample.int(st$M, 1)
        cand_vec <- st$curr_vec
        cand_vec[flip_pos] <- !cand_vec[flip_pos]
        
        test_s_df <- st$struct_df
        rem_vec <- c()
        for (idx in seq_along(st$removable_paths)) {
          rp <- st$removable_paths[[idx]]
          if (!cand_vec[idx]) {
            test_s_df[test_s_df$Dependent == rp$dep, rp$pred] <- FALSE
            rem_vec <- c(rem_vec, paste0(rp$dep, " ~ ", rp$pred))
          }
        }
        if (!check_variable_isolation(test_s_df, st$retain_deps, st$retain_preds, st$pred_cols, st$ctx$required_vars)) {
          c_score <- Inf
        } else {
          k_str <- make_key_local(test_s_df)
          if (!k_str %in% names(st$candidates_map)) {
            rem_label <- paste(rem_vec, collapse = "; ")
            st$candidates_map[[k_str]] <- build_candidate_record_local(test_s_df, rem_label)
          }
          c_score <- score_of(st$candidates_map[[k_str]])
        }

        if (is.finite(c_score)) {
          curr_score_step <- c_score
          curr_score <- score_of(st$candidates_map[[make_key_local(st$curr_df)]])

          dE <- as.numeric(c_score - curr_score)
          if (!is.na(dE) && !is.nan(dE)) {
            eff_T <- max(st$T_val, 1e-6)
            prob <- if (dE < 0) 1.0 else exp(-dE / eff_T)
            if (!is.na(prob) && !is.nan(prob) && runif(1) < prob) {
              st$curr_vec <- cand_vec
              st$curr_df <- test_s_df
            }
          }
        } else {
          curr_score_step <- Inf
        }
        st$T_val <- max(st$T_val * st$sa_alpha, 1e-6)
        st$scores_hist <- c(st$scores_hist, curr_score_step)
        if (is.finite(curr_score_step) && curr_score_step < best_curr) {
          best_curr <- curr_score_step
        }
        st$best_scores_hist <- c(st$best_scores_hist, best_curr)
      }
      step_advance <- chunk_size
    } else if (st$eff_strategy == "regsem") {
      # Regularized SEM (cv_regsem / regsem)
      if (is.null(st$reg_params_mat)) {
        pen_type <- st$regsem_type %||% "lasso"
        n_lambda <- st$regsem_n_lambda %||% 30

        # Resolve regsem functions dynamically so ShinyLive does not detect it as a startup dependency
        regsem_loaded <- ensure_regsem_loaded()
        reg_obj <- NULL
        if (regsem_loaded) {
          reg_obj <- tryCatch({
            getExportedValue(regsem_pkg, "cv_regsem")(st$base_fit, type = pen_type, pars_pen = "regressions",
                                                    n.lambda = n_lambda, jump = 0.04)
          }, error = function(e) {
            tryCatch({
              getExportedValue(regsem_pkg, "regsem")(st$base_fit, type = pen_type, pars_pen = "regressions", lambda = 0.05)
            }, error = function(e2) NULL)
          })
        }

        params_mat <- NULL
        if (!is.null(reg_obj)) {
          if (!is.null(reg_obj$parameters)) {
            params_mat <- reg_obj$parameters
          } else if (!is.null(reg_obj$coefficients)) {
            params_mat <- matrix(reg_obj$coefficients, nrow = 1, dimnames = list(NULL, names(reg_obj$coefficients)))
          }
        }

        # Fallback if regsem is unavailable or failed: soft-threshold the STANDARDIZED regression
        # coefficients so the result does not depend on variable scales. Clearly labelled as approximate.
        # Column names follow regsem's "predictor -> dependent" convention.
        if (is.null(params_mat)) {
          note <- if (!regsem_loaded) {
            "Regularized SEM engine (regsem) could not be loaded"
          } else {
            "regsem could not estimate this model (for example because of its missing-data setting)"
          }
          st$fallback_note <- paste0(note, "; results use an approximate soft-threshold path on standardized coefficients, NOT regsem.")
          showNotification(st$fallback_note, type = "warning", duration = 10)
          ss <- tryCatch(lavaan::standardizedSolution(st$base_fit), error = function(e) NULL)
          if (!is.null(ss)) {
            reg_ss <- ss[ss$op == "~", , drop = FALSE]
            lambdas <- seq(0.01, 0.5, length.out = n_lambda)
            params_mat <- matrix(0, nrow = n_lambda, ncol = nrow(reg_ss))
            colnames(params_mat) <- paste0(reg_ss$rhs, " -> ", reg_ss$lhs)
            for (li in seq_along(lambdas)) {
              for (ci in seq_len(nrow(reg_ss))) {
                b <- reg_ss$est.std[ci]
                params_mat[li, ci] <- sign(b) * max(0, abs(b) - lambdas[li])
              }
            }
          }
        }

        st$reg_params_mat <- params_mat
        st$reg_n_steps <- if (!is.null(params_mat)) nrow(params_mat) else 1L
        st$max_steps <- max(1L, st$reg_n_steps)
      }

      curr_lambda_idx <- step + 1L
      curr_score_step <- Inf

      if (!is.null(st$reg_params_mat) && curr_lambda_idx <= nrow(st$reg_params_mat)) {
        p_row <- st$reg_params_mat[curr_lambda_idx, ]
        p_names <- names(p_row)
        test_s_df <- st$struct_df

        for (rp in st$removable_paths) {
          # Exact (not regex) match on regsem's "predictor -> dependent" parameter names
          match_idx <- which(p_names == paste0(rp$pred, " -> ", rp$dep))
          if (length(match_idx) > 0) {
            val <- abs(p_row[match_idx[1]])
            if (!is.na(val) && val < 1e-4) {
              test_s_df[test_s_df$Dependent == rp$dep, rp$pred] <- FALSE
            }
          }
        }

        if (check_variable_isolation(test_s_df, st$retain_deps, st$retain_preds, st$pred_cols, st$ctx$required_vars)) {
          k_str <- make_key_local(test_s_df)
          if (!k_str %in% names(st$candidates_map)) {
            rem_vec <- c()
            for (rp in st$removable_paths) {
              if (!isTRUE(as.logical(test_s_df[test_s_df$Dependent == rp$dep, rp$pred]))) {
                rem_vec <- c(rem_vec, paste0(rp$dep, " ~ ", rp$pred))
              }
            }
            st$candidates_map[[k_str]] <- build_candidate_record_local(test_s_df, paste(rem_vec, collapse = "; "))
          }
          curr_score_step <- score_of(st$candidates_map[[k_str]])
        }
      }

      if (!is.finite(curr_score_step)) {
        curr_score_step <- if (length(st$scores_hist) > 0) tail(st$scores_hist, 1) else st$base_score
      }

      st$scores_hist <- c(st$scores_hist, curr_score_step)
      if (is.finite(curr_score_step) && curr_score_step < best_curr) {
        best_curr <- curr_score_step
      }
      st$best_scores_hist <- c(st$best_scores_hist, best_curr)
      step_advance <- 1L
    }
    
    if (!isTRUE(isolate(opt_running()))) {
      return()
    }

    opt_state(st)

    cur_step_display <- min(step + step_advance, st$max_steps)
    fmt_score <- function(x) if (is.finite(x)) sprintf("%.2f", x) else "-"
    detail_msg <- if (is.finite(st$base_score)) {
      sprintf(
        "Step %d / %d | Current %s: %.2f | Best: %.2f (Δ %+.2f)",
        cur_step_display, st$max_steps, st$criterion,
        curr_score_step, best_curr, best_curr - st$base_score
      )
    } else {
      sprintf("Step %d / %d | Current %s: %s | Best: %s",
              cur_step_display, st$max_steps, st$criterion, fmt_score(curr_score_step), fmt_score(best_curr))
    }

    session$sendCustomMessage("update_optimization_live_chart", list(
      step = cur_step_display,
      maxIter = st$max_steps,
      scores = st$scores_hist,
      best_scores = st$best_scores_hist,
      baseScore = st$base_score,
      detail = detail_msg
    ))

    opt_step(step + step_advance)
  })

  # Render Candidate Ranking Table
  output$prune_candidates_table <- renderDT({
    res <- prune_results(); req(res)
    cands <- res$candidates
    if (!length(cands)) return(NULL)
    
    df_list <- lapply(seq_along(cands), function(i) {
      c_item <- cands[[i]]
      ret_str <- if (is.null(c_item$retained_str)) build_retained_str(c_item$struct_df) else c_item$retained_str
      data.frame(
        Rank = i,
        Status = if (identical(c_item$status, "[Baseline]") && isFALSE(c_item$identified)) "[Baseline] [Not Identified]" else c_item$status,
        `Retained Paths` = ret_str,
        AIC = if (is.na(c_item$aic)) "—" else sprintf("%.2f", c_item$aic),
        BIC = if (is.na(c_item$bic)) "—" else sprintf("%.2f", c_item$bic),
        `ΔAIC` = if (is.na(c_item$delta_aic)) "—" else sprintf("%+.2f", c_item$delta_aic),
        `ΔBIC` = if (is.na(c_item$delta_bic)) "—" else sprintf("%+.2f", c_item$delta_bic),
        `Δ vs Best` = if (is.null(c_item$delta_best) || !is.finite(c_item$delta_best)) "—" else sprintf("%.2f", c_item$delta_best),
        `Added Cov.` = if (length(c_item$added_covs) > 0) paste(c_item$added_covs, collapse = "; ") else "—",
        Weight = if (is.null(c_item$weight) || !is.finite(c_item$weight)) "—" else sprintf("%.3f", c_item$weight),
        CFI = if (is.na(c_item$cfi)) "—" else sprintf("%.3f", c_item$cfi),
        RMSEA = if (is.na(c_item$rmsea)) "—" else sprintf("%.3f", c_item$rmsea),
        SRMR = if (is.na(c_item$srmr)) "—" else sprintf("%.3f", c_item$srmr),
        check.names = FALSE,
        stringsAsFactors = FALSE
      )
    })
    
    tbl <- do.call(rbind, df_list)
    
    datatable(
      tbl,
      selection = list(mode = "single", selected = isolate(selected_prune_idx())),
      rownames = FALSE,
      options = list(pageLength = 6, dom = 'tp', scrollX = TRUE)
    )
  }, server = FALSE)

  # Candidate Indicator Badge
  output$prune_cand_indicator_ui <- renderUI({
    res <- prune_results()
    if (is.null(res) || !length(res$candidates)) return(NULL)
    idx <- selected_prune_idx()
    if (is.null(idx) || idx < 1) idx <- 1
    total <- length(res$candidates)
    span(sprintf("Candidate %d of %d (Rank %d)", idx, total, idx),
         style = "font-size: 13px; font-weight: 600; color: #475569; background-color: #f1f5f9; padding: 3px 10px; border-radius: 12px; border: 1px solid #cbd5e1;")
  })

  # Previous Candidate Button Click
  observeEvent(input$prune_prev_cand, {
    res <- prune_results(); req(res)
    curr_idx <- selected_prune_idx()
    if (is.null(curr_idx)) curr_idx <- 1
    if (curr_idx > 1) {
      new_idx <- curr_idx - 1
      selected_prune_idx(new_idx)
      selected_prune_cand(res$candidates[[new_idx]])
      target_page <- ceiling(new_idx / 6)
      dataTableProxy("prune_candidates_table") %>% 
        selectRows(new_idx) %>% 
        selectPage(target_page)
    }
  })

  # Next Candidate Button Click
  observeEvent(input$prune_next_cand, {
    res <- prune_results(); req(res)
    curr_idx <- selected_prune_idx()
    if (is.null(curr_idx)) curr_idx <- 1
    if (curr_idx < length(res$candidates)) {
      new_idx <- curr_idx + 1
      selected_prune_idx(new_idx)
      selected_prune_cand(res$candidates[[new_idx]])
      target_page <- ceiling(new_idx / 6)
      dataTableProxy("prune_candidates_table") %>% 
        selectRows(new_idx) %>% 
        selectPage(target_page)
    }
  })

  # Candidate Row Selection Observer -> Updates selected candidate (rendering handled by observe below)
  observeEvent(input$prune_candidates_table_rows_selected, {
    res <- prune_results(); req(res)
    sel_idx <- input$prune_candidates_table_rows_selected
    if (is.null(sel_idx) || sel_idx > length(res$candidates)) return()
    
    selected_prune_idx(sel_idx)
    cand <- res$candidates[[sel_idx]]
    selected_prune_cand(cand)
  })

  # Initial trigger for selected preview on Step 2 Modal open
  observe({
    cand <- selected_prune_cand()
    if (is.null(cand)) return()
    
    if (!cand$converged) {
      session$sendCustomMessage("update_prune_preview_plot", list(
        error = TRUE,
        message = "Candidate model did not converge."
      ))
      return()
    }

    # Lower-ranked candidates release their lavaan object to save memory; re-estimate on demand.
    if (is.null(cand$fit)) {
      ctx <- isolate(prune_results())$ctx
      refit <- if (!is.null(ctx)) tryCatch(fit_candidate_model(cand$struct_df, ctx), error = function(e) NULL) else NULL
      if (is.null(refit)) {
        session$sendCustomMessage("update_prune_preview_plot", list(
          error = TRUE,
          message = "Could not re-estimate this candidate for preview."
        ))
        return()
      }
      cand$fit <- refit
      selected_prune_cand(cand)   # cache so Apply and re-selection reuse it
      return()
    }
    
    std_for_plot <- if (input$analysis_mode == "std") TRUE else input$diagram_std
    parts <- strsplit(input$layout_style, "_", fixed = TRUE)[[1]]
    eng   <- parts[1]
    rank  <- ifelse(length(parts) == 2, parts[2], "LR")
    
    dot_code <- tryCatch(
      semDiagram(cand$fit,
                 standardized = std_for_plot,
                 layout       = rank,
                 engine       = eng),
      error = function(e) e)
    if (inherits(dot_code, "error")) {
      session$sendCustomMessage("update_prune_preview_plot", list(
        error = TRUE,
        message = htmltools::htmlEscape(paste("Could not draw the path diagram:", conditionMessage(dot_code)))
      ))
      return()
    }

    session$sendCustomMessage("update_prune_preview_plot", list(
      error = FALSE,
      dot = dot_code,
      engine = eng
    ))
  })

  # Apply Selected Model to Main UI with Instant Sync & Automatic Model Refitting
  observeEvent(input$apply_pruned_model, {
    cand <- selected_prune_cand()
    if (is.null(cand) || is.null(cand$struct_df)) {
      showNotification("Please select a valid candidate model to apply.", type = "error")
      return()
    }
    
    if (isFALSE(cand$identified)) {
      showNotification("This candidate is not identified (df < 0 or no standard errors). Select an identified candidate.",
                       type = "warning", duration = 8)
      return()
    }
    if (isTRUE(cand$replaced)) {
      showNotification(
        paste0("This candidate is not a pure path reduction: lavaan adds ", paste(cand$added_covs, collapse = ", "),
               " in place of the removed path(s). The association is still in the model."),
        type = "warning", duration = 10)
    }
    # The refit below uses the same syntax and options as the candidate, so its AIC/BIC must match the table.
    pending_prune_check(if (isTRUE(cand$converged) && isTRUE(cand$vars_ok) && !is.na(cand$aic))
                          list(aic = cand$aic, bic = cand$bic) else NULL)

    # 1. Update structural data frame
    struct_table_data(cand$struct_df)
    
    # 2. Trigger reactive update for checkbox_matrix rhandsontable
    struct_table_trigger(struct_table_trigger() + 1)
    
    # 3. Close Modal
    removeModal()
    
    # 4. Trigger automatic model re-fitting so path diagram, fit indices, and params update immediately
    shinyjs::click("run_model")

    showNotification("Selected model applied to the structural UI; refitting now...", type = "message", duration = 4)
  })

  # Consistency check after applying a candidate: the main refit must reproduce the scores shown in the
  # candidate table (same syntax, same options). A mismatch means the table does not describe the applied model.
  observeEvent(fit_model_safe(), {
    chk <- isolate(pending_prune_check())
    if (is.null(chk)) return()
    pending_prune_check(NULL)
    res <- fit_model_safe()
    if (!isTRUE(res$ok) || is.null(res$fit)) return()
    ms <- tryCatch(lavaan::fitMeasures(res$fit, c("aic", "bic")), error = function(e) NULL)
    if (is.null(ms) || anyNA(ms)) return()
    if (abs(ms["aic"] - chk$aic) > 0.01 || abs(ms["bic"] - chk$bic) > 0.01) {
      showNotification(
        sprintf("The refitted model differs from the candidate table (AIC %.2f vs %.2f). Treat the table scores for this model with caution.",
                ms["aic"], chk$aic),
        type = "warning", duration = 12)
    }
  }, ignoreInit = TRUE)
}

# ---- Run the application ---------------------------------------

shinyApp(ui, server)
