# Edge-case tests: degenerate data must never crash the optimizer / suggestion helpers.
# Run: Rscript tests/test_edge_cases.R   (from the project root)
suppressMessages(library(lavaan))
exprs <- parse("app.R")
wanted <- c("struct_pred_cols", "build_struct_lines", "make_struct_key", "active_struct_vars",
            "build_anchor_lines", "fit_is_proper", "candidate_score", "fit_candidate_model",
            "struct_descendants", "struct_dependents", "struct_edges", "fit_suggestion_model",
            "fit_cutoff_violations", "get_modification_suggestions", "get_suggested_structural_paths", "%||%")
for (e in exprs) {
  if (is.call(e) && identical(e[[1]], as.name("<-")) && as.character(e[[2]]) %in% wanted) eval(e)
}
ok <- function(cond, msg) { cat(if (isTRUE(cond)) "PASS" else "FAIL", "-", msg, "\n"); if (!isTRUE(cond)) quit(status = 1) }

items <- c("x1", "x2", "m", "y")
mk <- function(paths) {
  s <- data.frame(Dependent = items, Operator = "~", stringsAsFactors = FALSE)
  for (i in items) s[[i]] <- FALSE
  for (p in paths) s[s$Dependent == p[1], p[2]] <- TRUE
  s
}
base_df <- mk(list(c("m", "x1"), c("y", "m"), c("y", "x2")))
cand_df <- mk(list(c("m", "x1"), c("y", "m")))
mkctx <- function(d) list(data = d, missing_method = "listwise", needs_meanstructure = FALSE,
  meas_lines = NULL, extra_lines = NULL, anchor_vars = active_struct_vars(base_df),
  baseline_dvs = struct_dependents(base_df), baseline_edges = struct_edges(base_df), base_fit = NULL)

# Each call must return without an uncaught error; the result may be NULL / non-proper.
safe <- function(expr) tryCatch({ force(expr); TRUE }, error = function(e) { cat("  error:", conditionMessage(e), "\n"); FALSE })

set.seed(1); n <- 200
good <- data.frame(x1 = rnorm(n), x2 = rnorm(n)); good$m <- good$x1 + rnorm(n); good$y <- good$m + rnorm(n)

cases <- list(
  empty_rows   = good[0, ],
  one_row      = good[1, ],
  constant_col = transform(good, x2 = 1),
  collinear    = transform(good, x2 = x1),
  all_na_col   = transform(good, x2 = NA_real_)
)
for (nm in names(cases)) {
  d <- cases[[nm]]
  f <- NULL
  ok(safe(f <- suppressWarnings(fit_candidate_model(cand_df, mkctx(d)))), paste(nm, ": fit_candidate_model does not throw"))
  ok(safe(candidate_score(list(converged = !is.null(f) && isTRUE(lavInspect(f, "converged")), proper = !is.null(f) && fit_is_proper(f), aic = 1, bic = 1), "AIC")),
     paste(nm, ": candidate_score does not throw"))
  ok(safe(suppressWarnings(get_modification_suggestions(f))), paste(nm, ": get_modification_suggestions does not throw"))
  ok(safe(suppressWarnings(get_suggested_structural_paths(f, base_df))), paste(nm, ": get_suggested_structural_paths does not throw"))
  ok(safe(suppressWarnings(fit_suggestion_model(build_struct_lines(base_df), "x2", "y", mkctx(d)))), paste(nm, ": fit_suggestion_model does not throw"))
}
ok(is.na(candidate_score(NULL, "AIC")) || is.infinite(candidate_score(NULL, "AIC")), "NULL record scores Inf")
cat("All edge-case tests done\n")
