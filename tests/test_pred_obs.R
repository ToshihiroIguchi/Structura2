# Unit tests for pred_obs_data() (Predicted vs Observed plot helper).
# Run: Rscript tests/test_pred_obs.R   (from the project root)
suppressMessages(library(lavaan))
exprs <- parse("app.R")
wanted <- c("pred_obs_data", "run_lavaan_sem", "%||%")
for (e in exprs) {
  if (is.call(e) && identical(e[[1]], as.name("<-")) && as.character(e[[2]]) %in% wanted) eval(e)
}
ok <- function(cond, msg) { cat(if (isTRUE(cond)) "PASS" else "FAIL", "-", msg, "\n"); if (!isTRUE(cond)) quit(status = 1) }

set.seed(1)
n <- 300
d <- data.frame(x = rnorm(n), x2 = rnorm(n))
d$m <- 0.6 * d$x + rnorm(n)
d$y <- 0.5 * d$m + 0.3 * d$x2 + rnorm(n)
chain <- "m ~ x\ny ~ m + x2"

# --- observed-only path model, listwise, no mean structure (Std-like) ---
fit <- run_lavaan_sem(chain, d, "listwise", FALSE)
r <- pred_obs_data(fit, "direct")
ok(setequal(names(r$items), c("m", "y")), "targets are the observed outcomes m and y")
yy <- r$items$y
ok(!isTRUE(all.equal(yy$obs, yy$pred)), "predictions differ from observations (no phantom reproduction)")
lm_r2 <- summary(lm(y ~ m + x2, d))$r.squared
ok(abs(yy$r2_emp - lm_r2) < 1e-3, "direct r2 matches lm() R-squared")
ok(abs(yy$r2_model - lm_r2) < 1e-2, "direct r2 is close to lavaan's R-squared")
ok(yy$n == n && !yy$uses_factor_scores, "n and factor-score flag")

# --- reduced form depends on exogenous variables only ---
rr <- pred_obs_data(fit, "reduced")
ry <- rr$items$y
cf <- coef(lm(ry$pred ~ d$x + d$x2))
ok(summary(lm(ry$pred ~ d$x + d$x2))$r.squared > 0.9999, "reduced-form y_hat is a linear function of x and x2")
ok(ry$r2_emp < yy$r2_emp + 1e-9, "reduced-form r2 is not larger than direct r2")

# --- mean structure (raw, FIML) with missing values ---
dm <- d
dm$m[sample(n, 30)] <- NA
fit_ml <- run_lavaan_sem(chain, dm, "ml", TRUE)
rm <- pred_obs_data(fit_ml, "direct")
ok(rm$items$y$n < n && rm$items$y$n > n - 40, "rows with missing predictors are dropped (FIML)")
ok(rm$items$m$n == n - 30 || rm$items$m$n == n, "outcome m keeps its own non-missing rows")
fit_mean <- run_lavaan_sem(chain, d, "listwise", TRUE)
ok(abs(pred_obs_data(fit_mean, "direct")$items$y$r2_emp - lm_r2) < 1e-3, "mean-structure fit gives the same r2")

# --- latent predictor: factor scores flagged ---
set.seed(2)
f <- rnorm(n)
dl <- data.frame(a1 = f + rnorm(n, sd = .5), a2 = f + rnorm(n, sd = .5), a3 = f + rnorm(n, sd = .5))
dl$z <- 0.7 * f + rnorm(n)
fit_l <- run_lavaan_sem("F =~ a1 + a2 + a3\nz ~ F", dl, "listwise", FALSE)
rl <- pred_obs_data(fit_l, "direct")
ok(isTRUE(rl$items$z$uses_factor_scores), "latent predictor sets uses_factor_scores")

# --- feedback loop: reduced form is disabled with a note, direct still works ---
fit_loop <- tryCatch(run_lavaan_sem("y ~ m + x2\nm ~ y + x", d, "listwise", FALSE), error = function(e) NULL, warning = function(w) NULL)
if (!is.null(fit_loop)) {
  rloop <- pred_obs_data(fit_loop, "reduced")
  ok(length(rloop$items) == 0 && any(grepl("feedback loop", rloop$notes)), "feedback loop disables reduced form with a note")
  ok(length(pred_obs_data(fit_loop, "direct")$items) == 2, "direct mode works with a feedback loop")
} else cat("SKIP - feedback-loop model could not be estimated\n")

# --- edge cases ---
dc <- d; dc$x2 <- 1
fit_c <- tryCatch(run_lavaan_sem("y ~ m + x2", dc, "listwise", FALSE), error = function(e) NULL, warning = function(w) NULL)
ok(is.null(fit_c) || is.list(pred_obs_data(fit_c, "direct")), "constant column does not crash the helper")
ok(is.list(pred_obs_data(NULL, "direct")) && length(pred_obs_data(NULL, "direct")$notes) > 0, "invalid fit returns a note instead of an error")
cat("All pred_obs tests passed\n")
