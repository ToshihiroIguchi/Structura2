# Consistency of Auto-Optimize candidates with the main model, on the classic y1/y2 example:
#   y1 ~ x1 + x2 ;  y2 ~ y1 + x1 + x2   (5 structural paths, 32 subsets).
# Checks that (1) a candidate's estimates equal those of a hand-built model with the same diagram,
# (2) the app's feasibility rules accept exactly the pure path reductions (variable set and covariances
# unchanged) and (3) reject the rest.   Run: Rscript tests/test_candidate_consistency.R  (project root)
suppressMessages(library(lavaan))
exprs <- parse("app.R")
wanted <- c("struct_pred_cols", "build_struct_lines", "make_struct_key", "active_struct_vars", "struct_edges",
            "run_lavaan_sem", "free_cov_pairs", "required_struct_vars", "candidate_structure_check",
            "check_variable_isolation", "fit_is_proper", "candidate_score", "fit_candidate_model", "%||%")
for (e in exprs) {
  if (is.call(e) && identical(e[[1]], as.name("<-")) && as.character(e[[2]]) %in% wanted) eval(e)
}
ok <- function(cond, msg) { cat(if (isTRUE(cond)) "PASS" else "FAIL", "-", msg, "\n"); if (!isTRUE(cond)) quit(status = 1) }

set.seed(2)
n <- 300
x1 <- rnorm(n); x2 <- rnorm(n)
y1 <- 0.5 * x1 + 0.3 * x2 + rnorm(n)
y2 <- 0.4 * y1 + 0.2 * x1 + rnorm(n)
d <- data.frame(x1, x2, y1, y2)

items <- c("x1", "x2", "y1", "y2")
all_paths <- list(c("y1", "x1"), c("y1", "x2"), c("y2", "y1"), c("y2", "x1"), c("y2", "x2"))
M <- length(all_paths)
mk <- function(paths) {
  s <- data.frame(Dependent = items, Operator = "~", stringsAsFactors = FALSE)
  for (i in items) s[[i]] <- FALSE
  for (p in paths) s[s$Dependent == p[1], p[2]] <- TRUE
  s
}
full_df <- mk(all_paths)

ctx0 <- list(data = d, missing_method = "listwise", needs_meanstructure = FALSE,
             meas_lines = NULL, extra_lines = NULL, base_fit = NULL)
base_fit <- fit_candidate_model(full_df, ctx0)
ctx <- c(ctx0, list(required_vars = required_struct_vars(full_df, base_fit),
                    base_ov = lavNames(base_fit, "ov"), base_cov_pairs = free_cov_pairs(base_fit)))
ctx_warm <- ctx; ctx_warm$base_fit <- base_fit

ok(setequal(ctx$required_vars, items), "all four variables are required")

n_feasible <- 0L; feasible_keys <- character(0)
for (code in (2^M - 1):1) {
  keep <- as.logical(bitwAnd(code, 2^(0:(M - 1))))
  df <- mk(all_paths[keep])
  label <- paste(vapply(all_paths[!keep], function(p) paste0(p[1], "~", p[2]), ""), collapse = ",")

  # The same diagram built "by hand" (main-model syntax) must give identical estimates.
  syn <- paste(build_struct_lines(df), collapse = "\n")
  hand <- run_lavaan_sem(syn, d, "listwise", FALSE)
  cand <- fit_candidate_model(df, ctx)
  ok(abs(AIC(cand) - AIC(hand)) < 1e-8 && abs(BIC(cand) - BIC(hand)) < 1e-8,
     paste0("[-", label, "] candidate AIC/BIC == hand-built model"))
  ok(max(abs(coef(cand) - coef(hand)[names(coef(cand))])) < 1e-6, paste0("[-", label, "] identical parameter estimates"))

  # Warm start (as used by the optimizer) must land on the same solution.
  warm <- fit_candidate_model(df, ctx_warm)
  ok(abs(AIC(warm) - AIC(hand)) < 1e-3, paste0("[-", label, "] warm start reproduces the cold-start AIC"))

  # Feasibility: variables kept AND no covariance freed in place of a path.
  pre  <- check_variable_isolation(df, character(0), character(0), struct_pred_cols(df), ctx$required_vars)
  chk  <- candidate_structure_check(cand, ctx)
  feasible <- pre && chk$vars_ok && !chk$replaced
  # the cheap pre-check must be consistent with the post-fit check on the variable set
  ok(pre == chk$vars_ok, paste0("[-", label, "] pre-check agrees with the fitted variable set"))
  score <- candidate_score(list(converged = TRUE, proper = TRUE, aic = AIC(cand), bic = BIC(cand),
                                vars_ok = chk$vars_ok, replaced = chk$replaced), "AIC")
  ok(is.finite(score) == (chk$vars_ok && !chk$replaced), paste0("[-", label, "] score is finite only for feasible candidates"))
  if (feasible) {
    n_feasible <- n_feasible + 1L
    feasible_keys <- c(feasible_keys, label)
    # a pure reduction removes exactly one free parameter per removed path
    ok(as.numeric(fitMeasures(cand, "npar")) == as.numeric(fitMeasures(base_fit, "npar")) - sum(!keep),
       paste0("[-", label, "] feasible candidate drops one parameter per removed path"))
  }
}
cat("feasible candidates:", n_feasible, "of", 2^M - 1, "\n")
ok(n_feasible == 8L, "exactly the 8 pure reductions are feasible")
# y2 ~ y1 can never be removed purely: lavaan frees y1 ~~ y2 in its place
ok(!any(grepl("y2~y1", feasible_keys, fixed = TRUE)), "y2 ~ y1 is never part of a feasible reduction")
cat("ALL PASSED\n")
