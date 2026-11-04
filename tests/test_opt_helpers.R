# Unit tests for the pure helper functions used by Auto-Optimize and MI suggestions.
# Run: Rscript tests/test_opt_helpers.R   (from the project root)
suppressMessages(library(lavaan))
exprs <- parse("app.R")
wanted <- c("struct_pred_cols", "build_struct_lines", "make_struct_key", "active_struct_vars",
            "run_lavaan_sem", "free_cov_pairs", "required_struct_vars", "candidate_structure_check",
            "check_variable_isolation", "fit_is_proper", "candidate_score", "fit_candidate_model",
            "droppable_struct_vars", "orphaned_struct_vars", "orphan_zero_lines",
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

# --- candidates are estimated exactly like the main model (same syntax, same options)
# ctx for a baseline structure: the baseline fit defines the comparable variable set / covariances.
mk_ctx <- function(dat, df, meas = NULL) {
  c0 <- list(data = dat, missing_method = "listwise", needs_meanstructure = FALSE,
             meas_lines = meas, extra_lines = NULL, base_fit = NULL)
  b <- fit_candidate_model(df, c0)
  c0$required_vars <- required_struct_vars(df, b)
  c0$base_ov <- lavNames(b, "ov")
  c0$base_cov_pairs <- free_cov_pairs(b)
  c0
}
ctx <- mk_ctx(d, base_df)
fit_base <- fit_candidate_model(base_df, ctx)
cand_df <- mk(list(c("m", "x1"), c("y", "m")))   # x2 and x3 lose all paths
main_style <- run_lavaan_sem(paste(build_struct_lines(cand_df), collapse = "\n"), d, "listwise", FALSE)
fit_cand <- fit_candidate_model(cand_df, ctx)
ok(abs(AIC(fit_cand) - AIC(main_style)) < 1e-6 && abs(BIC(fit_cand) - BIC(main_style)) < 1e-6,
   "candidate AIC/BIC equal the main-model fit of the same diagram (no extra syntax)")
ok(identical(sort(names(coef(fit_cand))), sort(names(coef(main_style)))), "candidate has exactly the main model's parameters")

# --- a variable that loses every path is detected (lavaan drops it, so AIC is not comparable)
chk_drop <- candidate_structure_check(fit_cand, ctx)
ok(!chk_drop$vars_ok, "a dropped variable (x2, x3 lost every path) is detected")
ok(AIC(fit_cand) < AIC(fit_base) - 100, "documents why: the dropped-variable model looks spuriously better")
ok(is.infinite(candidate_score(list(converged = TRUE, proper = TRUE, aic = 1, bic = 1, vars_ok = FALSE, replaced = FALSE), "AIC")),
   "a candidate on a different variable set can never win")
ok(!check_variable_isolation(cand_df, character(0), character(0), struct_pred_cols(cand_df), ctx$required_vars),
   "required variables must keep a path")
ok(check_variable_isolation(base_df, character(0), character(0), struct_pred_cols(base_df), ctx$required_vars),
   "the baseline satisfies the required-variable rule")
ok(setequal(ctx$required_vars, c("x1", "x2", "x3", "m", "y")), "every observed structural variable is required")
base_deps <- unique(struct_edges(base_df)$dep)
ok(check_variable_isolation(base_df, base_deps, character(0), struct_pred_cols(base_df), ctx$required_vars),
   "the baseline keeps an incoming path for every dependent variable")

# --- a model where every variable has several paths (saturated: 10 free parameters on 4 variables)
rich_df  <- mk(list(c("m", "x1"), c("m", "x2"), c("y", "m"), c("y", "x1"), c("y", "x2")))
rich_ctx <- mk_ctx(d, rich_df)
fit_rich <- fit_candidate_model(rich_df, rich_ctx)

# --- a clean reduction: one path removed, the variable set and covariances are unchanged
one_df  <- mk(list(c("m", "x1"), c("m", "x2"), c("y", "m"), c("y", "x1")))      # only y ~ x2 removed
fit_one <- fit_candidate_model(one_df, rich_ctx)
chk_one <- candidate_structure_check(fit_one, rich_ctx)
ok(chk_one$vars_ok && !chk_one$replaced, "removing y ~ x2 is a pure reduction")
ok(fitMeasures(fit_one, "df") == fitMeasures(fit_rich, "df") + 1, "a pure reduction adds exactly 1 df")
ok(fitMeasures(fit_one, "chisq") >= fitMeasures(fit_rich, "chisq") - 1e-6, "nested candidate cannot fit better in chisq")
ok(check_variable_isolation(one_df, character(0), character(0), struct_pred_cols(one_df), rich_ctx$required_vars),
   "removing y ~ x2 keeps every variable in the model")
rich_deps <- unique(struct_edges(rich_df)$dep)
ok(check_variable_isolation(one_df, rich_deps, character(0), struct_pred_cols(one_df), rich_ctx$required_vars),
   "removing y ~ x2 keeps an incoming path for every dependent variable")
ok(!check_variable_isolation(mk(list(c("y", "m"), c("y", "x1"), c("y", "x2"))), rich_deps, character(0),
                             struct_pred_cols(rich_df), rich_ctx$required_vars),
   "removing every incoming path of m violates the dependent-variable constraint")

# --- lavaan frees a covariance instead of a removed path: detected, never optimal
no_m_in <- mk(list(c("y", "m"), c("y", "x1"), c("y", "x2")))      # m loses both incoming paths and becomes exogenous
fit_no_m_in <- fit_candidate_model(no_m_in, rich_ctx)
chk_m <- candidate_structure_check(fit_no_m_in, rich_ctx)
ok(chk_m$vars_ok && chk_m$replaced && all(c("m ~~ x1", "m ~~ x2") %in% chk_m$added_covs),
   "m loses all incoming paths -> lavaan frees m ~~ x1 and m ~~ x2 instead (detected)")
ok(abs(AIC(fit_no_m_in) - AIC(fit_rich)) < 1e-6, "the replaced model is likelihood-equivalent to the baseline (nothing was removed)")
ok(is.infinite(candidate_score(list(converged = TRUE, proper = TRUE, aic = 1, bic = 1, vars_ok = TRUE, replaced = TRUE), "AIC")),
   "a replaced candidate can never win")

no_ym <- mk(list(c("m", "x1"), c("m", "x2"), c("y", "x1"), c("y", "x2")))      # y ~ m removed, both stay dependent
chk_ym <- candidate_structure_check(fit_candidate_model(no_ym, rich_ctx), rich_ctx)
ok(chk_ym$vars_ok && chk_ym$replaced && "m ~~ y" %in% chk_ym$added_covs,
   "y ~ m removed between two dependents -> m ~~ y freed (detected)")

# --- latent variables stay in the model through the measurement part
hs_meas <- c("visual =~ x1 + x2 + x3", "textual =~ x4 + x5 + x6", "speed =~ x7 + x8 + x9")
lat <- c("visual", "textual", "speed")
mk_lat <- function(paths) { s <- data.frame(Dependent = lat, Operator = "~", stringsAsFactors = FALSE)
  for (i in lat) s[[i]] <- FALSE; for (p in paths) s[s$Dependent == p[1], p[2]] <- TRUE; s }
lat_base <- mk_lat(list(c("textual", "visual"), c("speed", "visual"), c("speed", "textual")))
lat_ctx <- mk_ctx(lavaan::HolzingerSwineford1939, lat_base, hs_meas)
ok(length(lat_ctx$required_vars) == 0, "latent variables are not 'required' (the measurement model keeps them)")
lat_empty <- fit_candidate_model(mk_lat(list()), lat_ctx)
chk_lat <- candidate_structure_check(lat_empty, lat_ctx)
ok(chk_lat$vars_ok && chk_lat$replaced, "removing every latent path -> lavaan frees the latent covariances (detected)")
lat_no_st <- fit_candidate_model(mk_lat(list(c("textual", "visual"), c("speed", "visual"))), lat_ctx)
chk_lat2 <- candidate_structure_check(lat_no_st, lat_ctx)
ok(chk_lat2$replaced && "speed ~~ textual" %in% chk_lat2$added_covs, "dropping speed ~ textual -> speed ~~ textual freed (detected)")
lat_ok <- fit_candidate_model(mk_lat(list(c("textual", "visual"), c("speed", "textual"))), lat_ctx)
ok(!candidate_structure_check(lat_ok, lat_ctx)$replaced, "dropping speed ~ visual keeps speed dependent -> pure reduction")

# --- exogenous predictors may lose every path: scored with fixed-zero paths (variable selection)
hs <- lavaan::HolzingerSwineford1939[, paste0("x", 1:6)]
hs_items <- paste0("x", 1:6)
mk_hs <- function(preds) {
  s <- data.frame(Dependent = hs_items, Operator = "~", stringsAsFactors = FALSE)
  for (i in hs_items) s[[i]] <- FALSE
  for (p in preds) s[s$Dependent == "x6", p] <- TRUE
  s
}
hs_base <- mk_hs(paste0("x", 1:5))
for (ms in c(FALSE, TRUE)) for (mm in c("listwise", "ml")) {
  tag <- sprintf("[meanstructure=%s, missing=%s]", ms, mm)
  c0 <- list(data = hs, missing_method = mm, needs_meanstructure = ms || mm == "ml",
             meas_lines = NULL, extra_lines = NULL, base_fit = NULL)
  b <- fit_candidate_model(hs_base, c0)
  hctx <- c0
  hctx$droppable_vars <- droppable_struct_vars(hs_base, b)
  hctx$base_edges <- struct_edges(hs_base)
  hctx$required_vars <- setdiff(required_struct_vars(hs_base, b), hctx$droppable_vars)
  hctx$base_ov <- lavNames(b, "ov")
  hctx$base_cov_pairs <- free_cov_pairs(b)
  ok(setequal(hctx$droppable_vars, paste0("x", 1:5)), paste(tag, "exogenous predictors are droppable"))
  ok(length(hctx$required_vars) == 1 && hctx$required_vars == "x6", paste(tag, "the dependent variable stays required"))

  hs_cand <- mk_hs(c("x2", "x3", "x4", "x5"))                     # x1 loses its only path
  ok(identical(orphaned_struct_vars(hs_cand, hctx), "x1"), paste(tag, "x1 is orphaned"))
  ok(identical(orphan_zero_lines(hs_cand, hctx), "x6 ~ 0*x1"), paste(tag, "fixed-zero line for x1"))
  ok(!length(orphan_zero_lines(hs_base, hctx)), paste(tag, "no extra lines for the baseline"))
  fc <- fit_candidate_model(hs_cand, hctx)
  chk <- candidate_structure_check(fc, hctx)
  ok(chk$vars_ok && !chk$replaced, paste(tag, "orphaned predictor is a pure reduction (same variables, no added covariances)"))
  ok(fitMeasures(fc, "df") == fitMeasures(b, "df") + 1, paste(tag, "one path removed -> exactly 1 df"))
  # AIC difference equals that of the plain regression with the predictor removed (listwise data are complete)
  l_full <- lm(x6 ~ x1 + x2 + x3 + x4 + x5, hs); l_red <- lm(x6 ~ x2 + x3 + x4 + x5, hs)
  ok(abs((AIC(fc) - AIC(b)) - (AIC(l_red) - AIC(l_full))) < 1e-6, paste(tag, "delta AIC equals the regression variable-selection delta"))
  ok(check_variable_isolation(hs_cand, "x6", character(0), struct_pred_cols(hs_cand), hctx$required_vars),
     paste(tag, "an orphaned exogenous predictor passes the isolation check"))
  ok(!check_variable_isolation(mk_hs(character(0)), "x6", character(0), struct_pred_cols(hs_base), hctx$required_vars),
     paste(tag, "the dependent variable may never lose all incoming paths"))
  # several predictors orphaned at once, and a non-adjacent one
  cand2 <- mk_hs(c("x2", "x4"))
  ok(setequal(orphaned_struct_vars(cand2, hctx), c("x1", "x3", "x5")), paste(tag, "several orphaned predictors"))
  ok(identical(orphan_zero_lines(cand2, hctx), "x6 ~ 0*x1 + 0*x3 + 0*x5"), paste(tag, "one fixed-zero line per dependent"))
  fc2 <- fit_candidate_model(cand2, hctx)
  ok(candidate_structure_check(fc2, hctx)$vars_ok, paste(tag, "several orphans keep the variable set"))
  l_red2 <- lm(x6 ~ x2 + x4, hs)
  ok(abs((AIC(fc2) - AIC(b)) - (AIC(l_red2) - AIC(l_full))) < 1e-6, paste(tag, "multi-orphan delta AIC equals the regression delta"))
}

# exogenous variable used by several dependents: orphaned only when every path is gone
multi_df <- mk(list(c("m", "x1"), c("y", "x1"), c("y", "m"), c("y", "x2")))
mctx <- mk_ctx(d, multi_df)
mctx$droppable_vars <- droppable_struct_vars(multi_df, fit_candidate_model(multi_df, mctx))
mctx$base_edges <- struct_edges(multi_df)
ok(setequal(mctx$droppable_vars, c("x1", "x2")), "x1 and x2 are exogenous -> droppable (m and y are dependent)")
ok(!"x1" %in% orphaned_struct_vars(mk(list(c("m", "x1"), c("y", "m"))), mctx), "x1 keeps one path -> not orphaned")
no_x1 <- mk(list(c("y", "m"), c("y", "x2")))
no_x1_dep <- unique(struct_edges(multi_df)$dep)
ok(identical(orphaned_struct_vars(no_x1, mctx), "x1"), "x1 lost both its paths -> orphaned")
ok(identical(sort(orphan_zero_lines(no_x1, mctx)), c("m ~ 0*x1", "y ~ 0*x1")), "fixed-zero lines for every removed path of x1")
ok(!check_variable_isolation(no_x1, no_x1_dep, character(0), struct_pred_cols(no_x1), setdiff(required_struct_vars(multi_df, fit_candidate_model(multi_df, mctx)), mctx$droppable_vars)),
   "m lost its only incoming path -> still rejected (dependent-variable constraint)")

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
