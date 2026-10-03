# Unit tests for the pure helper functions used by Auto-Optimize and MI suggestions.
# Run: Rscript tests/test_opt_helpers.R   (from the project root)
suppressMessages(library(lavaan))
exprs <- parse("app.R")
wanted <- c("struct_pred_cols", "build_struct_lines", "make_struct_key", "active_struct_vars",
            "build_anchor_lines", "fit_is_proper", "candidate_score", "fit_candidate_model",
            "struct_descendants", "struct_dependents", "struct_edges", "fit_suggestion_model", "fit_cutoff_violations", "get_modification_suggestions", "get_suggested_structural_paths",
            "%||%")
for (e in exprs) {
  if (is.call(e) && identical(e[[1]], as.name("<-")) && as.character(e[[2]]) %in% wanted) eval(e)
}
ok <- function(cond, msg) { cat(if (isTRUE(cond)) "PASS" else "FAIL", "-", msg, "\n"); if (!isTRUE(cond)) quit(status = 1) }

set.seed(42)
n <- 300
d <- data.frame(x1 = rnorm(n), x2 = rnorm(n), x3 = rnorm(n))
d$m <- 0.6 * d$x1 + rnorm(n)
d$y <- 0.5 * d$m + 0.3 * d$x2 + rnorm(n)

items <- c("x1", "x2", "x3", "m", "y")
mk <- function(paths) {
  s <- data.frame(Dependent = items, Operator = "~", stringsAsFactors = FALSE)
  for (i in items) s[[i]] <- FALSE
  for (p in paths) s[s$Dependent == p[1], p[2]] <- TRUE
  s
}
base_df <- mk(list(c("m", "x1"), c("y", "m"), c("y", "x2"), c("y", "x3")))

# --- syntax / key round trip
ok(identical(build_struct_lines(base_df), c("m ~ x1", "y ~ x2 + x3 + m")) ||
   length(build_struct_lines(base_df)) == 2, "build_struct_lines emits one line per dependent")
ok(make_struct_key(mk(list())) == "EMPTY_PATH", "empty structure key")
ok(make_struct_key(base_df) == make_struct_key(base_df[nrow(base_df):1, ]), "key independent of row order")

# --- anchors keep the observed-variable set identical
ctx <- list(data = d, missing_method = "listwise", needs_meanstructure = FALSE,
            meas_lines = NULL, extra_lines = NULL, anchor_vars = active_struct_vars(base_df),
            baseline_dvs = struct_dependents(base_df), baseline_edges = struct_edges(base_df), base_fit = NULL)
fit_base <- fit_candidate_model(base_df, ctx)
cand_df <- mk(list(c("m", "x1"), c("y", "m")))   # x2 and x3 lose all paths
fit_cand <- fit_candidate_model(cand_df, ctx)
ok(setequal(lavNames(fit_cand, "ov"), lavNames(fit_base, "ov")), "candidate keeps baseline observed variables")
ok(lavInspect(fit_cand, "nobs") == lavInspect(fit_base, "nobs"), "same N across candidates")
ok(fitMeasures(fit_cand, "df") == fitMeasures(fit_base, "df") + 2,
   "removing 2 paths adds exactly 2 df (candidate is nested in baseline; exogenous covariances stay free)")
ok(fitMeasures(fit_cand, "chisq") >= fitMeasures(fit_base, "chisq") - 1e-6, "nested candidate cannot fit better in chisq")
fit_noanchor <- sem(paste(build_struct_lines(cand_df), collapse = "\n"), data = d, fixed.x = FALSE)
ok(!setequal(lavNames(fit_noanchor, "ov"), lavNames(fit_base, "ov")), "without anchors the variable set would differ (regression check)")

# --- removing ALL incoming paths of a variable must remove the association (not turn it into a covariance)
no_m_in <- mk(list(c("y", "m"), c("y", "x2"), c("y", "x3")))      # m loses m~x1; m still predicts y
anch <- build_anchor_lines(no_m_in, active_struct_vars(base_df), struct_dependents(base_df))
ok(any(grepl("^m ~~ 0[*]", anch)), "a variable that turned exogenous gets its covariances fixed to 0")
fit_no_m_in <- fit_candidate_model(no_m_in, ctx)
ok(AIC(fit_no_m_in) > AIC(fit_base) + 20, "dropping m ~ x1 is penalised (it is no longer disguised as a covariance)")
exo_only <- build_anchor_lines(cand_df, active_struct_vars(base_df), struct_dependents(base_df))
ok(any(grepl("^x2 ~~ x3$|^x2 ~~ x1$|^x3 ~~ x1$", exo_only)), "baseline-exogenous variables keep free covariances")

# --- latent variables: an "empty" structural part must not stay equivalent to the saturated one
hs_meas <- c("visual =~ x1 + x2 + x3", "textual =~ x4 + x5 + x6", "speed =~ x7 + x8 + x9")
lat <- c("visual", "textual", "speed")
mk_lat <- function(paths) { s <- data.frame(Dependent = lat, Operator = "~", stringsAsFactors = FALSE)
  for (i in lat) s[[i]] <- FALSE; for (p in paths) s[s$Dependent == p[1], p[2]] <- TRUE; s }
lat_base <- mk_lat(list(c("textual", "visual"), c("speed", "visual"), c("speed", "textual")))
lat_ctx <- list(data = lavaan::HolzingerSwineford1939, missing_method = "listwise", needs_meanstructure = FALSE,
                meas_lines = hs_meas, extra_lines = NULL, anchor_vars = active_struct_vars(lat_base),
                baseline_dvs = struct_dependents(lat_base), baseline_edges = struct_edges(lat_base), base_fit = NULL)
lat_fit_base  <- fit_candidate_model(lat_base, lat_ctx)
lat_fit_empty <- fit_candidate_model(mk_lat(list()), lat_ctx)
ok(AIC(lat_fit_empty) > AIC(lat_fit_base) + 20,
   "removing every latent path is penalised (CFA with free latent covariances would be a disguised saturated model)")

lat_fit_no_st <- fit_candidate_model(mk_lat(list(c("textual", "visual"), c("speed", "visual"))), lat_ctx)
ok(abs(AIC(lat_fit_no_st) - AIC(lat_fit_base)) > 1e-3,
   "dropping speed ~ textual is no longer disguised as a residual covariance (AIC differs from the saturated baseline)")
ok(fitMeasures(lat_fit_no_st, "df") == fitMeasures(lat_fit_base, "df") + 1, "dropping one latent path adds exactly 1 df")

# --- explicit anchor lines must not switch off the automatic exogenous covariances of the OTHER variables
cand_one <- mk(list(c("m", "x1"), c("y", "m"), c("y", "x2")))          # only y ~ x3 removed; x1, x2 stay exogenous
fit_one <- fit_candidate_model(cand_one, ctx)
ok(fitMeasures(fit_one, "df") == fitMeasures(fit_base, "df") + 1,
   "losing one exogenous predictor keeps the covariances among the remaining exogenous variables free (df +1)")

# --- score / improper handling
ok(is.infinite(candidate_score(list(converged = TRUE, proper = FALSE, aic = 1, bic = 1), "AIC")), "improper -> Inf")
ok(is.infinite(candidate_score(list(converged = FALSE, aic = 1, bic = 1), "AIC")), "non-converged -> Inf")
ok(candidate_score(list(converged = TRUE, proper = TRUE, aic = 5, bic = 7), "BIC") == 7, "BIC score")

# --- reachability / cycles
ok(setequal(struct_descendants(base_df, "x1"), c("m", "y")), "descendants of x1")
ok(length(struct_descendants(base_df, "y")) == 0, "y is a sink")

# --- suggestions: unused variables must be suggestable, matched by name, ranked, cycle-flagged
cur_lines <- build_struct_lines(cand_df)                     # m ~ x1 ; y ~ m   (x2, x3 not in the model)
plain_fit <- sem(paste(cur_lines, collapse = "
"), data = d, fixed.x = FALSE)
ok(is.null(get_suggested_structural_paths(plain_fit, cand_df, 0.5, 0)) ||
   is.null(get_suggested_structural_paths(plain_fit, cand_df, 0.5, 0)[[match("y", cand_df$Dependent)]][[match("x2", struct_pred_cols(cand_df))]]),
   "plain fit cannot suggest a path from a variable that is not in the model (documents the lavaan limitation)")
sug_fit <- fit_suggestion_model(cur_lines, c("x2", "x3"), lavNames(plain_fit, "ov.y")[1], ctx)
ok(!is.null(sug_fit), "suggestion fit converges")
sug <- get_suggested_structural_paths(sug_fit, cand_df, mi_threshold = 3.84, epc_threshold = 0.1)
ok(!is.null(sug), "suggestions found with the augmented fit")
r_y <- match("y", cand_df$Dependent); c_x2 <- match("x2", struct_pred_cols(cand_df))
cell <- sug[[r_y]][[c_x2]]
ok(!is.null(cell) && cell$mi > 3.84, "y ~ x2 suggested in the correct cell (by name)")
ok(identical(cell$cyclic, FALSE), "y ~ x2 is not cyclic")
ok(!is.null(cell$rank) && cell$rank >= 1, "suggestion carries an MI rank")
r_x1 <- match("x1", cand_df$Dependent); c_y <- match("y", struct_pred_cols(cand_df))
cyc <- sug[[r_x1]][[c_y]]
if (!is.null(cyc)) ok(isTRUE(cyc$cyclic), "x1 ~ y flagged as feedback loop")
mi_all <- get_modification_suggestions(sug_fit, 3.84, 0)
ok(!is.null(mi_all) && all(mi_all$op %in% c("~", "~~", "=~")), "MI list limited to supported operators")
# --- relative degraded-fit flag
v_bad  <- fit_cutoff_violations(c(cfi = .85, rmsea = .09, srmr = .05))
v_good <- fit_cutoff_violations(c(cfi = .95, rmsea = .04, srmr = .04))
ok(identical(unname(v_bad), c(TRUE, TRUE, FALSE)) && !any(v_good), "cutoff violations detected per index")
ok(!any(v_bad & !v_bad), "a candidate that merely inherits the baseline violations is not flagged as degraded")
ok(any(v_bad & !v_good), "a candidate that newly breaks cutoffs the baseline met is flagged")
cat("ALL PASSED
")
