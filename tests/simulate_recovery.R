# Simulation study: does Auto-Optimize recover the true paths, and do MI suggestions find omitted ones?
# Uses the app's own helper functions (fit_candidate_model, candidate_score, get_suggested_structural_paths, ...).
# Run from the project root:  Rscript tests/simulate_recovery.R [reps]
suppressMessages(library(lavaan))
args <- commandArgs(trailingOnly = TRUE)
reps <- if (length(args)) as.integer(args[1]) else 40L
exprs <- parse("app.R")
wanted <- c("struct_pred_cols", "build_struct_lines", "make_struct_key", "active_struct_vars", "struct_dependents", "struct_edges",
            "run_lavaan_sem", "free_cov_pairs", "required_struct_vars", "candidate_structure_check",
            "check_variable_isolation", "fit_is_proper", "fit_cutoff_violations", "candidate_score", "fit_candidate_model",
            "struct_descendants", "fit_suggestion_model", "get_modification_suggestions",
            "get_suggested_structural_paths", "%||%")
for (e in exprs) if (is.call(e) && identical(e[[1]], as.name("<-")) && as.character(e[[2]]) %in% wanted) eval(e)

items <- c("x1", "x2", "x3", "m", "y")
gen <- function(n, b_m = c(x1 = .5, x2 = .4, x3 = 0), b_y = c(m = .5, x1 = 0, x2 = 0, x3 = .3)) {
  d <- data.frame(x1 = rnorm(n), x2 = rnorm(n), x3 = rnorm(n))
  d$m <- b_m["x1"] * d$x1 + b_m["x2"] * d$x2 + b_m["x3"] * d$x3 + rnorm(n)
  d$y <- b_y["m"] * d$m + b_y["x1"] * d$x1 + b_y["x2"] * d$x2 + b_y["x3"] * d$x3 + rnorm(n)
  d
}
mk <- function(paths) {
  s <- data.frame(Dependent = items, Operator = "~", stringsAsFactors = FALSE)
  for (i in items) s[[i]] <- FALSE
  for (p in paths) s[s$Dependent == p[1], p[2]] <- TRUE
  s
}

# Candidates are estimated like the main model. As in the app, a candidate that drops a variable or in which
# lavaan frees a covariance in place of a removed path is infeasible (score Inf) and can never be optimal.
make_ctx <- function(d, base_df) {
  c0 <- list(data = d, missing_method = "listwise", needs_meanstructure = FALSE, meas_lines = NULL,
             extra_lines = NULL, base_fit = NULL)
  b <- fit_candidate_model(base_df, c0)
  c0$required_vars <- required_struct_vars(base_df, b)
  c0$base_ov <- lavNames(b, "ov"); c0$base_cov_pairs <- free_cov_pairs(b)
  c0
}
cand_scores <- function(df, ctx) {      # c(AIC, BIC); Inf for infeasible, failed or improper candidates
  out <- c(Inf, Inf)
  if (!check_variable_isolation(df, character(0), character(0), struct_pred_cols(df), ctx$required_vars)) return(out)
  fm <- fit_candidate_model(df, ctx)
  if (is.null(fm) || !isTRUE(lavInspect(fm, "converged")) || !fit_is_proper(fm)) return(out)
  chk <- candidate_structure_check(fm, ctx)
  if (!chk$vars_ok || chk$replaced) return(out)
  c(AIC(fm), BIC(fm))
}

# ---------- A. Exhaustive pruning from an over-specified model ----------
full_paths <- list(c("m","x1"), c("m","x2"), c("m","x3"), c("y","m"), c("y","x1"), c("y","x2"), c("y","x3"))
true_idx   <- c(1, 2, 4, 7)                      # m~x1, m~x2, y~m, y~x3 are real; the rest are false
M <- length(full_paths)
grid <- as.matrix(expand.grid(replicate(M, c(FALSE, TRUE), simplify = FALSE)))
full_df <- mk(full_paths)

run_pruning <- function(n) {
  d <- gen(n)
  ctx <- make_ctx(d, full_df)
  aic <- bic <- rep(Inf, nrow(grid))
  for (g in seq_len(nrow(grid))) {
    keep <- grid[g, ]
    df <- mk(full_paths[keep])
    if (!length(active_struct_vars(df))) next
    sc <- cand_scores(df, ctx)
    aic[g] <- sc[1]; bic[g] <- sc[2]
  }
  res <- lapply(list(AIC = aic, BIC = bic), function(sc) {
    keep <- grid[which.min(sc), ]
    c(sens = mean(keep[true_idx]), spec = mean(!keep[-true_idx]), exact = all(keep[true_idx]) && !any(keep[-true_idx]))
  })
  res
}

cat(sprintf("A. Exhaustive pruning (%d candidate models, %d reps per N)\n", nrow(grid), reps))
set.seed(2026)
out <- list()
for (n in c(200, 600)) {
  r <- replicate(reps, run_pruning(n), simplify = FALSE)
  for (crit in c("AIC", "BIC")) {
    m <- do.call(rbind, lapply(r, function(x) x[[crit]]))
    out[[paste(crit, n)]] <- colMeans(m)
    cat(sprintf("  N=%4d %s: true-path retention=%.2f  false-path removal=%.2f  exact model recovered=%.2f\n",
                n, crit, mean(m[, "sens"]), mean(m[, "spec"]), mean(m[, "exact"])))
  }
}

# ---------- B. MI suggestions for an omitted true path ----------
cat(sprintf("\nB. MI suggestions (omitted true path y ~ x3; %d reps per N)\n", reps * 3))
cur_paths <- list(c("m","x1"), c("m","x2"), c("y","m"))
cur_df <- mk(cur_paths); cur_lines <- build_struct_lines(cur_df)
omit_hit <- function(n, truth_has_x3) {
  d <- gen(n, b_y = c(m = .5, x1 = 0, x2 = 0, x3 = if (truth_has_x3) .3 else 0))
  ctx <- list(data = d, missing_method = "listwise", needs_meanstructure = FALSE)
  sf <- fit_suggestion_model(cur_lines, "x3", "y", ctx)
  if (is.null(sf)) return(c(any = NA, top1 = NA))
  sug <- get_suggested_structural_paths(sf, cur_df, mi_threshold = 6.63, epc_threshold = 0.1)
  if (is.null(sug)) return(c(any = FALSE, top1 = FALSE))
  r <- match("y", cur_df$Dependent); cc <- match("x3", struct_pred_cols(cur_df))
  cell <- sug[[r]][[cc]]
  c(any = TRUE, top1 = !is.null(cell) && identical(cell$rank, 1L))
}
for (n in c(200, 600)) {
  h <- t(replicate(reps * 3, omit_hit(n, TRUE)))
  fpr <- t(replicate(reps * 3, omit_hit(n, FALSE)))
  cat(sprintf("  N=%4d  omitted path present: #1 suggestion is y~x3 in %.0f%% of reps\n", n, 100 * mean(h[, "top1"], na.rm = TRUE)))
  cat(sprintf("  N=%4d  model correct (no omission): >=1 suggestion shown in %.0f%% of reps (false alarm rate)\n", n, 100 * mean(fpr[, "any"], na.rm = TRUE)))
}
