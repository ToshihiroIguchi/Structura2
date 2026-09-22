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
library(markdown)

# Parser bypass block to guarantee dependency packaging during Shinylive build
# while deferring execution to the dynamic lazy-loader at runtime.
if (FALSE) {
  library(lavaan)
}

# Offline assets warning (assets should be prepared at build time)
tryCatch({
  js_path <- "www/hpcc-js/graphviz.umd.js"
  wasm_path <- "www/hpcc-js/graphvizlib.wasm"
  if (!file.exists(js_path) || !file.exists(wasm_path)) {
    warning("Graphviz assets are missing in www/hpcc-js/. They should be prepared at build time.")
  }
}, error = function(e) NULL)

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

  invisible(lapply(c("lavaan"), function(p)
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

  # Return raw DOT graph code. Layout and rendering will be done on the client side via @hpcc-js/wasm
  return(graph_code)
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



# ---- Helper: Multi-Algorithm Model Optimization Engine ---------------------------
# Evaluates candidate path-pruned models using Exhaustive Search, Simulated Annealing (SA),
# Genetic Algorithm (GA), or Adaptive Auto-Switch without artificial p-value pre-filtering.
sem_optimize_hybrid <- function(base_fit, data, meas_lines, struct_df, lock_df, extra_lines = character(0),
                                criterion = c("AIC", "BIC"), missing_method = "listwise",
                                needs_meanstructure = FALSE, strategy = c("adaptive", "exhaustive", "sa", "ga"),
                                max_exhaustive_comb = 1024,
                                sa_max_iter = 200, sa_alpha = 0.90,
                                ga_pop_size = 20, ga_max_gen = 15, ga_pmut = 0.10) {
  criterion <- match.arg(criterion)
  strategy  <- match.arg(strategy)
  
  if (is.null(base_fit) || !isTRUE(lavaan::lavInspect(base_fit, "converged"))) {
    stop("The baseline model must be successfully fitted before optimization.")
  }
  
  base_ms <- lavaan::fitMeasures(base_fit, c("aic", "bic", "cfi", "rmsea", "srmr", "pvalue"))
  base_score <- if (criterion == "AIC") base_ms["aic"] else base_ms["bic"]
  
  pred_cols <- names(struct_df)[3:ncol(struct_df)]
  active_paths <- list()
  
  for (i in seq_len(nrow(struct_df))) {
    dep <- struct_df$Dependent[i]
    if (!nzchar(dep)) next
    for (p in pred_cols) {
      if (isTRUE(as.logical(struct_df[i, p]))) {
        is_locked <- FALSE
        if (!is.null(lock_df) && p %in% names(lock_df) && i <= nrow(lock_df)) {
          is_locked <- isTRUE(as.logical(lock_df[i, p]))
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
    return(list(
      candidates = list(),
      message = "No unlocked structural paths available for optimization. All active paths are locked."
    ))
  }

  fit_candidate <- function(curr_struct_df) {
    slines <- lapply(seq_len(nrow(curr_struct_df)), function(i) {
      dp <- curr_struct_df$Dependent[i]
      if (!nzchar(dp)) return(NULL)
      preds <- names(curr_struct_df)[3:ncol(curr_struct_df)]
      ps <- preds[as.logical(curr_struct_df[i, preds])]
      if (!length(ps)) return(NULL)
      paste0(dp, " ~ ", paste(ps, collapse = " + "))
    })
    all_syntax <- unlist(c(meas_lines, slines, extra_lines))
    if (!length(all_syntax)) return(NULL)
    
    tryCatch({
      lavaan::sem(paste(all_syntax, collapse = "\n"),
                  data          = data,
                  missing       = missing_method,
                  fixed.x       = FALSE,
                  parser        = "old",
                  meanstructure = needs_meanstructure,
                  ncpus         = 1L)
    }, error = function(e) NULL)
  }

  build_candidate_record <- function(curr_s_df, removed_str) {
    fm <- fit_candidate(curr_s_df)
    if (is.null(fm) || !lavaan::lavInspect(fm, "converged")) {
      return(list(
        removed_str = if (nzchar(removed_str)) removed_str else "None (Baseline Model)",
        struct_df = curr_s_df,
        fit = NULL,
        aic = NA_real_, bic = NA_real_, delta_aic = NA_real_, delta_bic = NA_real_,
        cfi = NA_real_, rmsea = NA_real_, srmr = NA_real_,
        converged = FALSE,
        status = "[Non-converged]"
      ))
    }
    
    ms <- lavaan::fitMeasures(fm, c("aic", "bic", "cfi", "rmsea", "srmr"))
    c_aic <- as.numeric(ms["aic"])
    c_bic <- as.numeric(ms["bic"])
    d_aic <- c_aic - base_ms["aic"]
    d_bic <- c_bic - base_ms["bic"]
    c_cfi <- as.numeric(ms["cfi"])
    c_rmsea <- as.numeric(ms["rmsea"])
    c_srmr <- as.numeric(ms["srmr"])
    
    stat <- "[Good]"
    if ((criterion == "AIC" && d_aic < -0.01) || (criterion == "BIC" && d_bic < -0.01)) {
      stat <- "[Improved]"
    }
    if ((!is.na(c_cfi) && c_cfi < 0.90) || (!is.na(c_rmsea) && c_rmsea > 0.08) || (!is.na(c_srmr) && c_srmr > 0.08)) {
      stat <- "[Degraded Fit]"
    }
    
    list(
      removed_str = if (nzchar(removed_str)) removed_str else "None (Baseline Model)",
      struct_df = curr_s_df,
      fit = fm,
      aic = c_aic, bic = c_bic,
      delta_aic = d_aic, delta_bic = d_bic,
      cfi = c_cfi, rmsea = c_rmsea, srmr = c_srmr,
      converged = TRUE,
      status = stat
    )
  }

  total_comb <- 2^M
  
  eff_strategy <- strategy
  if (strategy == "adaptive") {
    if (total_comb <= max_exhaustive_comb) {
      eff_strategy <- "exhaustive"
    } else if (M <= 20) {
      eff_strategy <- "sa"
    } else {
      eff_strategy <- "ga"
    }
  }

  candidates_map <- list()
  
  make_key <- function(s_df) {
    lines <- c()
    for (i in seq_len(nrow(s_df))) {
      dp <- s_df$Dependent[i]
      ps <- pred_cols[as.logical(s_df[i, pred_cols])]
      if (length(ps)) lines <- c(lines, paste0(dp, "~", paste(sort(ps), collapse = ",")))
    }
    paste(sort(lines), collapse = ";")
  }

  base_key <- make_key(struct_df)
  candidates_map[[base_key]] <- list(
    removed_str = "None (Baseline Model)",
    struct_df = struct_df,
    fit = base_fit,
    aic = as.numeric(base_ms["aic"]),
    bic = as.numeric(base_ms["bic"]),
    delta_aic = 0.0,
    delta_bic = 0.0,
    cfi = as.numeric(base_ms["cfi"]),
    rmsea = as.numeric(base_ms["rmsea"]),
    srmr = as.numeric(base_ms["srmr"]),
    converged = TRUE,
    status = "[Baseline]"
  )

  if (eff_strategy == "exhaustive") {
    grid <- expand.grid(replicate(M, c(FALSE, TRUE), simplify = FALSE))
    for (row_i in seq_len(nrow(grid))) {
      state <- as.logical(grid[row_i, ])
      test_s_df <- struct_df
      removed_paths_vec <- c()
      for (idx in seq_along(removable_paths)) {
        rp <- removable_paths[[idx]]
        keep <- state[idx]
        if (!keep) {
          test_s_df[test_s_df$Dependent == rp$dep, rp$pred] <- FALSE
          removed_paths_vec <- c(removed_paths_vec, paste0(rp$dep, " ~ ", rp$pred))
        }
      }
      k_str <- make_key(test_s_df)
      if (!k_str %in% names(candidates_map)) {
        rem_label <- paste(removed_paths_vec, collapse = "; ")
        candidates_map[[k_str]] <- build_candidate_record(test_s_df, rem_label)
      }
    }
  } else if (eff_strategy == "sa") {
    curr_vec <- rep(TRUE, M)
    curr_df <- struct_df
    
    T_val <- 10.0
    for (iter in seq_len(sa_max_iter)) {
      flip_pos <- sample.int(M, 1)
      cand_vec <- curr_vec
      cand_vec[flip_pos] <- !cand_vec[flip_pos]
      
      test_s_df <- struct_df
      rem_vec <- c()
      for (idx in seq_along(removable_paths)) {
        rp <- removable_paths[[idx]]
        if (!cand_vec[idx]) {
          test_s_df[test_s_df$Dependent == rp$dep, rp$pred] <- FALSE
          rem_vec <- c(rem_vec, paste0(rp$dep, " ~ ", rp$pred))
        }
      }
      
      k_str <- make_key(test_s_df)
      if (!k_str %in% names(candidates_map)) {
        rem_label <- paste(rem_vec, collapse = "; ")
        candidates_map[[k_str]] <- build_candidate_record(test_s_df, rem_label)
      }
      
      c_rec <- candidates_map[[k_str]]
      curr_rec <- candidates_map[[make_key(curr_df)]]
      
      if (c_rec$converged) {
        c_score <- if (criterion == "AIC") c_rec$aic else c_rec$bic
        curr_score <- if (!is.null(curr_rec) && curr_rec$converged) {
          if (criterion == "AIC") curr_rec$aic else curr_rec$bic
        } else Inf
        
        dE <- c_score - curr_score
        if (dE < 0 || runif(1) < exp(-dE / T_val)) {
          curr_vec <- cand_vec
          curr_df <- test_s_df
        }
      }
      T_val <- T_val * sa_alpha
    }
  } else if (eff_strategy == "ga") {
    pop <- matrix(sample(c(TRUE, FALSE), ga_pop_size * M, replace = TRUE),
                  nrow = ga_pop_size, ncol = M)
    pop[1, ] <- TRUE
    
    evaluate_chrom <- function(chrom_vec) {
      test_s_df <- struct_df
      rem_vec <- c()
      for (idx in seq_along(removable_paths)) {
        rp <- removable_paths[[idx]]
        if (!chrom_vec[idx]) {
          test_s_df[test_s_df$Dependent == rp$dep, rp$pred] <- FALSE
          rem_vec <- c(rem_vec, paste0(rp$dep, " ~ ", rp$pred))
        }
      }
      k_str <- make_key(test_s_df)
      if (!k_str %in% names(candidates_map)) {
        rem_label <- paste(rem_vec, collapse = "; ")
        candidates_map[[k_str]] <<- build_candidate_record(test_s_df, rem_label)
      }
      rec <- candidates_map[[k_str]]
      if (!rec$converged) return(Inf)
      if (criterion == "AIC") rec$aic else rec$bic
    }

    for (gen in seq_len(ga_max_gen)) {
      scores <- apply(pop, 1, evaluate_chrom)
      
      best_idx <- which.min(scores)
      best_chrom <- pop[best_idx, ]
      
      new_pop <- pop
      new_pop[1, ] <- best_chrom
      
      for (p in seq(2, ga_pop_size, by = 2)) {
        i1 <- sample.int(ga_pop_size, 2); parent1 <- pop[i1[which.min(scores[i1])], ]
        i2 <- sample.int(ga_pop_size, 2); parent2 <- pop[i2[which.min(scores[i2])], ]
        
        if (M > 1 && runif(1) < 0.80) {
          x_pt <- sample.int(M - 1, 1)
          child1 <- c(parent1[1:x_pt], parent2[(x_pt + 1):M])
          child2 <- c(parent2[1:x_pt], parent1[(x_pt + 1):M])
        } else {
          child1 <- parent1
          child2 <- parent2
        }
        
        mut1 <- runif(M) < ga_pmut; child1[mut1] <- !child1[mut1]
        mut2 <- runif(M) < ga_pmut; child2[mut2] <- !child2[mut2]
        
        new_pop[p, ] <- child1
        if (p + 1 <= ga_pop_size) new_pop[p + 1, ] <- child2
      }
      pop <- new_pop
    }
  }

  candidates_list <- unname(candidates_map)
  
  scores <- vapply(candidates_list, function(x) {
    if (!x$converged) return(Inf)
    if (criterion == "AIC") x$aic else x$bic
  }, numeric(1))
  
  ord <- order(scores, decreasing = FALSE)
  sorted_candidates <- candidates_list[ord]
  
  if (length(sorted_candidates) > 0 && sorted_candidates[[1]]$converged && sorted_candidates[[1]]$status != "[Baseline]") {
    sorted_candidates[[1]]$status <- "[Optimal]"
  }
  
  list(
    candidates = sorted_candidates,
    criterion = criterion,
    strategy_used = eff_strategy,
    message = "Success"
  )
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
")),
    tags$script(src = if (file.exists("www/hpcc-js/graphviz.umd.js")) "hpcc-js/graphviz.umd.js" else "https://cdn.jsdelivr.net/npm/@hpcc-js/wasm/dist/graphviz.umd.js"),
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
            if (bar) bar.style.width = '100%';
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
              bar.style.backgroundColor = '#ef4444';
              bar.style.width = '100%';
            }
            if (status) {
              status.innerText = 'Error loading application: ' + errorMsg;
              status.style.color = '#f87171';
            }
          }
        };

        // Poll for DOM elements before starting the progress bar updates
        var startTimer = function() {
          var bar = document.getElementById('structura-preload-bar');
          var status = document.getElementById('structura-preload-status');
          if (!bar || !status) {
            setTimeout(startTimer, 100);
            return;
          }

          interval = setInterval(function() {
            if (width >= 90) {
              clearInterval(interval);
              return;
            }
            var step = (90 - width) * 0.08;
            if (step < 0.2) step = 0.2;
            width += step;
            bar.style.width = width + '%';
            
            if (width < 30) {
              status.innerText = 'Connecting to analysis environment...';
            } else if (width < 65) {
              status.innerText = 'Initializing R runtime and utilities...';
            } else {
              status.innerText = 'Loading structural equation engine (lavaan)...';
            }
          }, 150);
        };

        startTimer();
      })();

      window.__hpcc_wasmFolder = 'hpcc-js';

      $(document).on('shiny:connected', function() {

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
      });

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
             rHandsontableOutput("corr_heatmap")),

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
                       div(style = "display: flex; gap: 10px; align-items: center; margin-bottom: 10px;",
                           actionButton("run_model", "Run / Update Model",
                                        class = "btn btn-success"),
                           actionButton("prune_model_btn", "Auto-Optimize Model", icon = icon("cogs"),
                                        class = "btn btn-info")
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
                      div(style = "height:60vh; overflow-y:auto; overflow-x:hidden; border:1px solid #ccc; position: relative;",
                          tags$div(id = "sem_plot_container", 
                                   style = "width:100%; height:100%; display: flex; align-items: center; justify-content: center; color: #666;",
                                   "Define a model to view the path diagram."))
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
  ) # end tabsetPanel
  ) # end div (structura-main-app)
  ) # end hidden
) # end fluidPage

# ================================================================
# SERVER
# ================================================================

server <- function(input, output, session) {

  # Dynamic Lazy Loader sequence triggered on Shiny session connection
  observeEvent(TRUE, {
    tryCatch({
      # Load lavaan (only package we defer now, direct call to bypass WebR VFS bugs)
      library(lavaan)
      
      # Complete the progress bar and transition out successfully
      runjs("if (window.finishStructuraPreload) { window.finishStructuraPreload(true); } else { $('#structura-preload-container').hide(); }")
      shinyjs::show("structura-main-app")
      
      # Show the initial load data modal dialog after dependencies are loaded
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
    }, error = function(e) {
      err_msg <- gsub("'", "\\'", e$message, fixed = TRUE)
      err_msg <- gsub("\n", " ", err_msg, fixed = TRUE)
      runjs(sprintf("if (window.finishStructuraPreload) { window.finishStructuraPreload(false, '%s'); }", err_msg))
      warning("Structura2 lazy loading failed: ", e$message)
    })
  }, once = TRUE)

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

  output$corr_heatmap <- renderRHandsontable({
    req(!is.null(input$display_columns))
    tryCatch({
      df <- processed_data()
      all_cols <- intersect(input$display_columns, names(df))
      num_cols <- all_cols[sapply(df[, all_cols, drop = FALSE], is.numeric)]
      if (length(num_cols) < 2) return(NULL)
      cm <- cor(df[, num_cols, drop = FALSE], use = "pairwise.complete.obs")
      cm[is.nan(cm)] <- NA
      cm_rounded <- round(cm, 3)
      cm_df <- as.data.frame(cm_rounded)
      
      color_renderer <- "
        function (instance, td, row, col, prop, value, cellProperties) {
          Handsontable.renderers.TextRenderer.apply(this, arguments);
          if (value !== null) {
            var val = parseFloat(value);
            if (!isNaN(val)) {
              var r = 255, g = 255, b = 255;
              var absVal = Math.min(Math.abs(val), 1);
              var intensity = Math.round(255 * (1 - absVal));
              if (val > 0) {
                g = intensity;
                b = intensity;
              } else if (val < 0) {
                r = intensity;
                g = intensity;
              }
              td.style.background = 'rgb(' + r + ',' + g + ',' + b + ')';
              if (absVal > 0.5) {
                td.style.color = '#ffffff';
              } else {
                td.style.color = '#000000';
              }
              td.style.textAlign = 'center';
            } else {
              td.style.background = '#eeeeee';
              td.style.color = '#999999';
              td.style.textAlign = 'center';
            }
          } else {
            td.style.background = '#eeeeee';
            td.style.color = '#999999';
            td.style.textAlign = 'center';
          }
        }
      "
      
      rhandsontable(cm_df, rowHeaders = rownames(cm_rounded), readOnly = TRUE,
                    manualColumnResize = TRUE, manualRowResize = TRUE) %>%
        hot_cols(renderer = color_renderer)
    }, error = function(e) {
      error_df <- data.frame(Error = paste("Correlation Heatmap Error:", e$message))
      rhandsontable(error_df, rowHeaders = FALSE, readOnly = TRUE)
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
    
    convs         <- make.unique(c(obs_names, tbl$Latent))
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
    struct_table_trigger()
    tryCatch({
      df <- processed_data(); req(df)
      meas <- input_table_data(); req(meas)
      mat <- struct_table_data(); req(mat)
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
      
      # Define static JS renderer referencing the shared R2 matrix
      # (Note: Col index offset is -2 because 'Dependent' and 'Operator' columns are on the left)
      renderer_js <- "
        function(instance, td, row, col, prop, value, cellProperties) {
          Handsontable.renderers.CheckboxRenderer.apply(this, arguments);
          var params = instance.params || instance.getSettings();
          var r2_matrix = params.r2_matrix;
          var col_var_idx = col - 2;
          
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
        error = TRUE,
        message = model$msg_friendly
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
    dot_code <- semDiagram(model$fit,
                           standardized = std_for_plot,
                           layout       = rank,
                           engine       = eng)
    
    # Send DOT code to client JS
    session$sendCustomMessage("update_sem_plot", list(
      error = FALSE,
      dot = dot_code,
      engine = eng
    ))
  })

  # ----------------- Auto-Optimize Model Server Observers ---------------
  struct_table_trigger <- reactiveVal(0)
  prune_lock_table_data <- reactiveVal(NULL)
  prune_results <- reactiveVal(NULL)
  selected_prune_cand <- reactiveVal(NULL)

  # Trigger Auto-Optimize Step 1 Modal
  observeEvent(input$prune_model_btn, {
    base_model <- fit_model_safe()
    if (!isTRUE(base_model$ok)) {
      showModal(modalDialog(
        title = span(icon("exclamation-triangle"), "Auto-Optimize Warning"),
        div(class = "alert alert-warning",
            "Please define and fit a valid baseline model before running Auto-Optimize."),
        easyClose = TRUE,
        footer = modalButton("Dismiss")
      ))
      return()
    }

    struct_df <- isolate(struct_table_data())
    if (is.null(struct_df) || nrow(struct_df) == 0) {
      showModal(modalDialog(
        title = span(icon("exclamation-triangle"), "Auto-Optimize Warning"),
        div(class = "alert alert-warning",
            "No structural model defined. Please set up structural paths first."),
        easyClose = TRUE,
        footer = modalButton("Dismiss")
      ))
      return()
    }

    # Initialize lock table data (FALSE = unlocked by default)
    lock_df <- struct_df
    pred_cols <- names(struct_df)[3:ncol(struct_df)]
    for (col in pred_cols) {
      lock_df[[col]] <- FALSE
    }
    prune_lock_table_data(lock_df)

    showModal(modalDialog(
      title = span(icon("cogs"), "Auto-Optimize Model: Step 1 - Strategy, Parameters & Path Locking"),
      size = "l",
      div(
        style = "padding: 10px;",
        p("Select structural paths to ", tags$b("LOCK [x] (protect from pruning)"), "."),
        p("Highlighted cells represent active paths in your current model. Unchecked active paths will be evaluated for optimization.",
          style = "font-size: 13px; color: #555;"),
        rHandsontableOutput("prune_lock_table"),
        tags$hr(),
        fluidRow(
          column(width = 6,
                 radioButtons("prune_criterion", "Optimization Criterion:",
                              choices = c("AIC (Predictive Accuracy / Balanced)" = "AIC",
                                          "BIC (Stronger Sparsity Penalty)" = "BIC"),
                              selected = "AIC")
          ),
          column(width = 6,
                 radioButtons("prune_strategy", "Search Algorithm Strategy:",
                              choices = c("Adaptive Auto-Switch (Recommended)" = "adaptive",
                                          "Exhaustive Search (100% Exact All-Subset)" = "exhaustive",
                                          "Simulated Annealing (SA - Fast Trajectory Search)" = "sa",
                                          "Genetic Algorithm (GA - Evolutionary Search)" = "ga"),
                              selected = "adaptive")
          )
        ),
        tags$hr(),
        h5(icon("sliders-h"), " Algorithm Hyper-Parameters:"),
        conditionalPanel(
          condition = "input.prune_strategy == 'adaptive' || input.prune_strategy == 'exhaustive'",
          numericInput("max_exhaustive_comb", "Exhaustive Search Max Combinations Threshold:",
                       value = 1024, min = 64, max = 8192, step = 64)
        ),
        conditionalPanel(
          condition = "input.prune_strategy == 'sa' || (input.prune_strategy == 'adaptive')",
          fluidRow(
            column(width = 6,
                   numericInput("sa_max_iter", "SA Max Iterations:", value = 200, min = 50, max = 1000)
            ),
            column(width = 6,
                   numericInput("sa_alpha", "SA Cooling Rate (Alpha):", value = 0.90, min = 0.50, max = 0.99, step = 0.01)
            )
          )
        ),
        conditionalPanel(
          condition = "input.prune_strategy == 'ga' || (input.prune_strategy == 'adaptive')",
          fluidRow(
            column(width = 4,
                   numericInput("ga_pop_size", "GA Population Size:", value = 20, min = 10, max = 100)
            ),
            column(width = 4,
                   numericInput("ga_max_gen", "GA Generations:", value = 15, min = 5, max = 50)
            ),
            column(width = 4,
                   numericInput("ga_pmut", "GA Mutation Rate:", value = 0.10, min = 0.01, max = 0.50, step = 0.01)
            )
          )
        )
      ),
      footer = tagList(
        modalButton("Cancel"),
        actionButton("run_prune_explore", "Run Optimization", icon = icon("play"), class = "btn btn-primary")
      )
    ))
  })

  # Render Lock Table in Modal 1
  output$prune_lock_table <- renderRHandsontable({
    lock_df <- prune_lock_table_data(); req(lock_df)
    struct_df <- struct_table_data(); req(struct_df)
    
    rh <- rhandsontable(lock_df, rowHeaders = FALSE) %>%
      hot_table(highlightReadOnly = TRUE, fixedColumnsLeft = 2)
    rh <- hot_col(rh, "Dependent", readOnly = TRUE)
    rh <- hot_col(rh, "Operator",  readOnly = TRUE)
    
    pred_cols <- names(struct_df)[3:ncol(struct_df)]
    rh$x$struct_matrix <- as.matrix(struct_df[, pred_cols, drop = FALSE])
    
    renderer_js <- "
      function(instance, td, row, col, prop, value, cellProperties) {
        Handsontable.renderers.CheckboxRenderer.apply(this, arguments);
        var params = instance.params || instance.getSettings();
        var struct_matrix = params.struct_matrix;
        var col_var_idx = col - 2;
        
        if (struct_matrix && col_var_idx >= 0 && col_var_idx < struct_matrix[0].length) {
          var is_active = struct_matrix[row][col_var_idx];
          if (is_active === true || is_active === 'TRUE' || is_active === 'true') {
            td.style.backgroundColor = '#e0f2fe';
            td.style.fontWeight = 'bold';
            cellProperties.readOnly = false;
          } else {
            cellProperties.readOnly = true;
            td.style.backgroundColor = '#f0f0f0';
            td.style.cursor = 'not-allowed';
            td.classList.add('htDimmed');
          }
        }
      }"
    
    for (col_name in pred_cols) {
      rh <- hot_col(rh, col_name, type = "checkbox", renderer = renderer_js)
    }
    rh
  })

  observeEvent(input$prune_lock_table, {
    tbl <- hot_to_r(input$prune_lock_table); req(tbl)
    prune_lock_table_data(tbl)
  })

  # 2. Run Candidate Search & Display Step 2 Modal
  observeEvent(input$run_prune_explore, {
    removeModal() # Close Modal 1
    
    showNotification("Running automated structural optimization...", type = "message", duration = 3)
    
    base_model <- fit_model_safe()
    meas_syntax <- hot_to_r(input$input_table)
    mlines <- unlist(lapply(seq_len(nrow(meas_syntax)), function(i) {
      lt   <- meas_syntax$Latent[i]; if (!nzchar(lt)) return(NULL)
      vars <- names(meas_syntax)[4:ncol(meas_syntax)]
      inds <- vars[as.logical(meas_syntax[i, vars])]
      if (!length(inds)) return(NULL)
      paste0(lt, " =~ ", paste(inds, collapse = " + "))
    }))
    
    extra <- strsplit(input$extra_eq, "\\n")[[1]]
    extra <- trimws(extra)
    extra <- extra[nzchar(extra)]
    
    needs_meanstructure <- (input$analysis_mode == "raw" || 
                            input$missing_method %in% c("ml", "ml.x", "two.stage", "robust.two.stage"))

    res <- tryCatch({
      sem_optimize_hybrid(
        base_fit            = base_model$fit,
        data                = processed_data(),
        meas_lines          = mlines,
        struct_df           = struct_table_data(),
        lock_df             = prune_lock_table_data(),
        extra_lines         = extra,
        criterion           = input$prune_criterion,
        missing_method      = input$missing_method,
        needs_meanstructure = needs_meanstructure,
        strategy            = input$prune_strategy,
        max_exhaustive_comb = input$max_exhaustive_comb %||% 1024,
        sa_max_iter         = input$sa_max_iter %||% 200,
        sa_alpha            = input$sa_alpha %||% 0.90,
        ga_pop_size         = input$ga_pop_size %||% 20,
        ga_max_gen          = input$ga_max_gen %||% 15,
        ga_pmut             = input$ga_pmut %||% 0.10
      )
    }, error = function(e) {
      list(candidates = list(), message = paste("Optimization error:", e$message))
    })

    if (length(res$candidates) == 0) {
      showModal(modalDialog(
        title = span(icon("info-circle"), "Auto-Optimize Result"),
        div(class = "alert alert-warning", res$message),
        easyClose = TRUE,
        footer = modalButton("Dismiss")
      ))
      return()
    }

    prune_results(res)
    selected_prune_cand(res$candidates[[1]])

    # Open Step 2 Modal (Candidate Ranking Catalog)
    showModal(modalDialog(
      title = span(icon("list"), "Auto-Optimize Model: Step 2 - Candidate Ranking Catalog"),
      size = "l",
      div(
        style = "padding: 10px;",
        p(sprintf("Strategy Used: %s. Click any candidate row in the table below to preview its path diagram. Models with degraded fit indices are flagged.", toupper(res$strategy_used))),
        DTOutput("prune_candidates_table"),
        tags$hr(),
        h5("Path Diagram Preview for Selected Candidate:"),
        div(style = "height: 320px; border: 1px solid #ccc; position: relative; border-radius: 4px; overflow: hidden;",
            tags$div(id = "prune_preview_container", 
                     style = "width:100%; height:100%; display: flex; align-items: center; justify-content: center; color: #666;",
                     "Select a candidate row above to view preview."))
      ),
      footer = tagList(
        modalButton("Close / Cancel"),
        actionButton("apply_pruned_model", "Apply Selected Model to UI", icon = icon("check"), class = "btn btn-success")
      )
    ))
  })

  # Render Candidate Ranking Table
  output$prune_candidates_table <- renderDT({
    res <- prune_results(); req(res)
    cands <- res$candidates
    if (!length(cands)) return(NULL)
    
    crit <- res$criterion
    df_list <- lapply(seq_along(cands), function(i) {
      c_item <- cands[[i]]
      data.frame(
        Rank = i,
        Status = c_item$status,
        `Removed Paths` = c_item$removed_str,
        AIC = if (is.na(c_item$aic)) "—" else sprintf("%.2f", c_item$aic),
        BIC = if (is.na(c_item$bic)) "—" else sprintf("%.2f", c_item$bic),
        `ΔAIC` = if (is.na(c_item$delta_aic)) "—" else sprintf("%+.2f", c_item$delta_aic),
        `ΔBIC` = if (is.na(c_item$delta_bic)) "—" else sprintf("%+.2f", c_item$delta_bic),
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
      selection = "single",
      rownames = FALSE,
      options = list(pageLength = 6, dom = 'tp', scrollX = TRUE)
    )
  }, server = FALSE)

  # Candidate Row Selection Observer -> Redraw Preview Path Diagram
  observeEvent(input$prune_candidates_table_rows_selected, {
    res <- prune_results(); req(res)
    sel_idx <- input$prune_candidates_table_rows_selected
    if (is.null(sel_idx) || sel_idx > length(res$candidates)) return()
    
    cand <- res$candidates[[sel_idx]]
    selected_prune_cand(cand)
    
    if (!cand$converged || is.null(cand$fit)) {
      session$sendCustomMessage("update_prune_preview_plot", list(
        error = TRUE,
        message = "Candidate model did not converge."
      ))
      return()
    }
    
    std_for_plot <- if (input$analysis_mode == "std") TRUE else input$diagram_std
    parts <- strsplit(input$layout_style, "_", fixed = TRUE)[[1]]
    eng   <- parts[1]
    rank  <- ifelse(length(parts) == 2, parts[2], "LR")
    
    dot_code <- semDiagram(cand$fit,
                           standardized = std_for_plot,
                           layout       = rank,
                           engine       = eng)
    
    session$sendCustomMessage("update_prune_preview_plot", list(
      error = FALSE,
      dot = dot_code,
      engine = eng
    ))
  })

  # Initial trigger for selected preview on Step 2 Modal open
  observe({
    cand <- selected_prune_cand()
    if (is.null(cand)) return()
    
    if (!cand$converged || is.null(cand$fit)) {
      session$sendCustomMessage("update_prune_preview_plot", list(
        error = TRUE,
        message = "Candidate model did not converge."
      ))
      return()
    }
    
    std_for_plot <- if (input$analysis_mode == "std") TRUE else input$diagram_std
    parts <- strsplit(input$layout_style, "_", fixed = TRUE)[[1]]
    eng   <- parts[1]
    rank  <- ifelse(length(parts) == 2, parts[2], "LR")
    
    dot_code <- semDiagram(cand$fit,
                           standardized = std_for_plot,
                           layout       = rank,
                           engine       = eng)
    
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
    
    # 1. Update structural data frame
    struct_table_data(cand$struct_df)
    
    # 2. Trigger reactive update for checkbox_matrix rhandsontable
    struct_table_trigger(struct_table_trigger() + 1)
    
    # 3. Close Modal
    removeModal()
    
    # 4. Trigger automatic model re-fitting so path diagram, fit indices, and params update immediately
    shinyjs::click("run_model")
    
    showNotification("Selected model applied to structural UI and refitted successfully!", type = "message", duration = 4)
  })
}

# ---- Run the application ---------------------------------------

shinyApp(ui, server)
