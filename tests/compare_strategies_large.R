# Strategy comparison at M = 10 structural paths (1024 candidates, still exhaustively solvable).
# Same mirrored search rules as compare_strategies.R, plus hybrids.  Run: Rscript tests/compare_strategies_large.R [reps] [N]
suppressMessages(library(lavaan))
args <- commandArgs(trailingOnly = TRUE); reps <- if (length(args)) as.integer(args[1]) else 20L
N <- if (length(args) > 1) as.integer(args[2]) else 300L
src <- readLines("tests/simulate_recovery.R"); cut <- grep("^# ---------- A", src)[1]
eval(parse(text = src[1:(cut - 1)]))
items <- c("x1","x2","x3","x4","x5","m","y")
mk <- function(paths) { s <- data.frame(Dependent = items, Operator = "~", stringsAsFactors = FALSE)
  for (i in items) s[[i]] <- FALSE; for (p in paths) s[s$Dependent == p[1], p[2]] <- TRUE; s }
full_paths <- c(lapply(c("x1","x2","x3","x4"), function(v) c("m", v)), lapply(c("m","x1","x2","x3","x4","x5"), function(v) c("y", v)))
true_flag <- vapply(full_paths, function(p) paste(p, collapse = "~") %in% c("m~x1","m~x2","y~m","y~x4"), TRUE)
M <- length(full_paths); full_df <- mk(full_paths); full <- rep(TRUE, M)
gen_large <- function(n) {        # correlated predictors (r = .5), weak-to-moderate effects
  z <- matrix(rnorm(n * 5), n); x <- z; for (j in 2:5) x[, j] <- .5 * z[, 1] + sqrt(1 - .25) * z[, j]
  d <- as.data.frame(x); names(d) <- paste0("x", 1:5)
  d$m <- .4 * d$x1 + .3 * d$x2 + rnorm(n); d$y <- .4 * d$m + .3 * d$x4 + rnorm(n); d
}

run_rep <- function(n) {
  d <- gen_large(n)
  ctx <- list(data = d, missing_method = "listwise", needs_meanstructure = FALSE, meas_lines = NULL, extra_lines = NULL,
              anchor_vars = active_struct_vars(full_df), baseline_dvs = struct_dependents(full_df),
              baseline_edges = struct_edges(full_df), base_fit = NULL)
  cache <- new.env()
  sc <- function(keep) { key <- paste(as.integer(keep), collapse = ""); if (!is.null(cache[[key]])) return(cache[[key]])
    out <- c(Inf, Inf)
    if (any(keep)) { fm <- fit_candidate_model(mk(full_paths[keep]), ctx)
      if (!is.null(fm) && isTRUE(lavInspect(fm, "converged")) && fit_is_proper(fm)) out <- c(AIC(fm), BIC(fm)) }
    cache[[key]] <- out; out }
  S <- function(k, ci) sc(k)[ci]
  grid <- as.matrix(expand.grid(replicate(M, c(FALSE, TRUE), simplify = FALSE)))
  all_sc <- t(apply(grid, 1, sc)); opt <- apply(all_sc, 2, min)
  best_of <- function(ev, ci) { ev <- unique(ev); ev[[which.min(vapply(ev, function(k) S(k, ci), 0))]] }
  stepwise <- function(ci) { ev <- list(full); cur <- full
    repeat { start <- S(cur, ci); best <- start; bv <- NULL; tv <- NULL
      for (i in which(cur)) { t <- cur; t[i] <- FALSE; ev[[length(ev) + 1]] <- t; s <- S(t, ci)
        if (is.finite(s) && s < best - .01) { best <- s; bv <- t } else if (is.finite(s) && is.null(tv) && abs(s - start) <= .01) tv <- t }
      nx <- if (!is.null(bv)) bv else tv; if (is.null(nx)) break; cur <- nx }
    ev }
  sa <- function(ci, iters, start = full) { T <- 10; al <- (0.1 / T)^(1 / iters); cur <- start; ev <- list(start)
    for (it in seq_len(iters)) { cand <- cur; j <- sample.int(M, 1); cand[j] <- !cand[j]; ev[[length(ev) + 1]] <- cand
      cs <- S(cand, ci); if (is.finite(cs)) { dE <- cs - S(cur, ci); if (runif(1) < (if (dE < 0) 1 else exp(-dE / max(T, 1e-6)))) cur <- cand }
      T <- max(T * al, 1e-6) }
    ev }
  res <- list()
  rec <- function(name, ev, ci) { b <- best_of(ev, ci)
    res[[paste(name, c("AIC","BIC")[ci])]] <<- c(sens = mean(b[true_flag]), spec = mean(!b[!true_flag]),
      exact = all(b[true_flag]) && !any(b[!true_flag]), hit = (S(b, ci) - opt[ci]) < .01, gap = S(b, ci) - opt[ci], fits = length(unique(ev))) }
  for (ci in 1:2) {
    ev_sw <- stepwise(ci); rec("stepwise", ev_sw, ci)
    ev_sa150 <- sa(ci, 150); rec("SA 150", ev_sa150, ci)
    ev_sa400 <- sa(ci, 400); rec("SA 400", ev_sa400, ci)
    rec("stepwise + SA 150", c(ev_sw, ev_sa150), ci)
    sw_best <- best_of(ev_sw, ci); rec("stepwise -> SA 150 (seeded)", c(ev_sw, sa(ci, 150, start = sw_best)), ci)
    rec("exhaustive", as.list(as.data.frame(t(grid))), ci)
  }
  res
}
set.seed(2027)
out <- replicate(reps, run_rep(N), simplify = FALSE)
cat(sprintf("M = %d paths (%d candidates), N = %d, %d replicates; correlated predictors r=.5\n", M, 2^M, N, reps))
cat(sprintf("%-30s %9s %9s %8s %11s %9s %7s\n", "method", "true kept", "false cut", "exact", "found opt.", "mean gap", "fits"))
for (m in unique(unlist(lapply(out, names)))) { mm <- do.call(rbind, lapply(out, function(x) x[[m]]))
  cat(sprintf("%-30s %9.2f %9.2f %8.2f %10.0f%% %9.2f %7.0f\n", m, mean(mm[,"sens"]), mean(mm[,"spec"]), mean(mm[,"exact"]),
              100 * mean(mm[,"hit"]), mean(mm[,"gap"]), mean(mm[,"fits"]))) }
