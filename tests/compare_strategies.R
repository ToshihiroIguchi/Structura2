# Compares the Auto-Optimize search strategies against the exact (exhaustive) optimum on simulated data.
# The search rules below mirror the stepper in app.R (stepwise with plateau crossing, simulated annealing,
# regsem lambda path) but run on the app's own helpers (fit_candidate_model, candidate_score, ...).
# Every candidate is fitted once per replicate and shared by all strategies, so the comparison is exact.
# Run from the project root:  Rscript tests/compare_strategies.R [reps]
suppressMessages(library(lavaan))
args <- commandArgs(trailingOnly = TRUE); reps <- if (length(args)) as.integer(args[1]) else 30L
src <- readLines("tests/simulate_recovery.R"); cut <- grep("^# ---------- A", src)[1]
eval(parse(text = src[1:(cut - 1)]))
has_regsem <- requireNamespace("regsem", quietly = TRUE)

full_paths <- list(c("m","x1"), c("m","x2"), c("m","x3"), c("y","m"), c("y","x1"), c("y","x2"), c("y","x3"))
true_flag <- c(TRUE, TRUE, FALSE, TRUE, FALSE, FALSE, TRUE)
M <- length(full_paths); full_df <- mk(full_paths)
path_names <- vapply(full_paths, function(p) paste0(p[2], " -> ", p[1]), "")   # regsem's naming

run_rep <- function(n, gen_fun = gen) {
  d <- gen_fun(n)
  ctx <- make_ctx(d, full_df)
  cache <- new.env()
  sc <- function(keep) {                       # c(AIC, BIC); Inf for empty / failed / improper models
    key <- paste(as.integer(keep), collapse = "")
    if (!is.null(cache[[key]])) return(cache[[key]])
    out <- c(Inf, Inf)
    if (any(keep)) {
      out <- cand_scores(mk(full_paths[keep]), ctx)
    }
    cache[[key]] <- out; out
  }
  S <- function(keep, ci) sc(keep)[ci]

  # exact optimum
  grid <- as.matrix(expand.grid(replicate(M, c(FALSE, TRUE), simplify = FALSE)))
  all_sc <- t(apply(grid, 1, sc))
  opt <- c(min(all_sc[, 1]), min(all_sc[, 2]))
  pick <- function(evaluated, ci) { # best of the evaluated candidates (what the ranking table shows first)
    s <- vapply(evaluated, function(k) S(k, ci), 0); evaluated[[which.min(s)]]
  }
  metrics <- function(keep, ci, nfit) c(sens = mean(keep[true_flag]), spec = mean(!keep[!true_flag]),
      exact = all(keep[true_flag]) && !any(keep[!true_flag]), gap = S(keep, ci) - opt[ci], hit = (S(keep, ci) - opt[ci]) < 0.01, fits = nfit)

  res <- list()
  full <- rep(TRUE, M)

  # --- exhaustive
  for (ci in 1:2) res[[paste("exhaustive", c("AIC","BIC")[ci])]] <- metrics(grid[which.min(all_sc[, ci]), ], ci, nrow(grid))

  # --- stepwise (backward, accepts ties when nothing strictly improves)
  for (ci in 1:2) {
    ev <- list(full); cur <- full
    repeat {
      start <- S(cur, ci); best <- start; best_vec <- NULL; tie_vec <- NULL
      for (i in which(cur)) {
        t <- cur; t[i] <- FALSE; ev[[length(ev) + 1]] <- t; s <- S(t, ci)
        if (is.finite(s) && s < best - 0.01) { best <- s; best_vec <- t }
        else if (is.finite(s) && is.null(tie_vec) && abs(s - start) <= 0.01) tie_vec <- t
      }
      nxt <- if (!is.null(best_vec)) best_vec else tie_vec
      if (is.null(nxt)) break
      cur <- nxt
    }
    ev <- unique(ev)
    res[[paste("stepwise", c("AIC","BIC")[ci])]] <- metrics(pick(ev, ci), ci, length(ev))
  }

  # --- simulated annealing (150 iterations, T0 = 10, cooling derived from the iteration count)
  for (ci in 1:2) {
    iters <- 150L; T <- 10; alpha <- (0.1 / T)^(1 / iters)
    cur <- full; ev <- list(full)
    for (it in seq_len(iters)) {
      cand <- cur; j <- sample.int(M, 1); cand[j] <- !cand[j]; ev[[length(ev) + 1]] <- cand
      cs <- S(cand, ci)
      if (is.finite(cs)) {
        dE <- cs - S(cur, ci); prob <- if (dE < 0) 1 else exp(-dE / max(T, 1e-6))
        if (!is.na(prob) && runif(1) < prob) cur <- cand
      }
      T <- max(T * alpha, 1e-6)
    }
    ev <- unique(ev)
    res[[paste("sa", c("AIC","BIC")[ci])]] <- metrics(pick(ev, ci), ci, length(ev))
  }

  # --- regsem lasso path (cv_regsem on the full model, paths with |coef| < 1e-4 are removed)
  if (has_regsem) {
    base <- sem(paste(build_struct_lines(full_df), collapse = "\n"), data = d, fixed.x = FALSE, parser = "old")
    pm <- tryCatch({ o <- NULL; capture.output(o <- suppressWarnings(suppressMessages(
            regsem::cv_regsem(base, type = "lasso", pars_pen = "regressions", n.lambda = 30, jump = 0.04))))
            o$parameters }, error = function(e) NULL)
    if (!is.null(pm)) {
      ev <- list(full)
      for (r in seq_len(nrow(pm))) {
        keep <- vapply(path_names, function(nm) { j <- which(colnames(pm) == nm); !(length(j) && abs(pm[r, j[1]]) < 1e-4) }, TRUE)
        ev[[length(ev) + 1]] <- unname(keep)
      }
      ev <- unique(ev)
      for (ci in 1:2) res[[paste("regsem", c("AIC","BIC")[ci])]] <- metrics(pick(ev, ci), ci, length(ev))
    }
  }
  res
}

# Hard scenario: strongly correlated predictors (r = .7) and weaker effects, so that suppression and
# near-equivalent models make greedy search easier to mislead.
gen_hard <- function(n) {
  x1 <- rnorm(n); x2 <- .7 * x1 + sqrt(1 - .49) * rnorm(n); x3 <- .7 * x1 + sqrt(1 - .49) * rnorm(n)
  d <- data.frame(x1 = x1, x2 = x2, x3 = x3)
  d$m <- .35 * x1 + .3 * x2 + rnorm(n)
  d$y <- .4 * d$m + .25 * x3 + rnorm(n)
  d
}
scenarios <- list(`independent predictors, medium effects` = gen, `correlated predictors (r=.7), weak effects` = gen_hard)

set.seed(99)
for (sc_name in names(scenarios)) for (n in c(200, 600)) {
  cat(sprintf("
=== %s ===", sc_name))
  out <- replicate(reps, run_rep(n, scenarios[[sc_name]]), simplify = FALSE)
  methods <- unique(unlist(lapply(out, names)))
  cat(sprintf("\nN = %d, %d replicates, %d candidate models\n", n, reps, 2^M))
  cat(sprintf("%-16s %9s %9s %9s %11s %9s %8s\n", "method", "true kept", "false cut", "exact", "found opt.", "mean gap", "fits"))
  for (m in methods) {
    mm <- do.call(rbind, lapply(out, function(x) x[[m]]))
    if (is.null(mm)) next
    cat(sprintf("%-16s %9.2f %9.2f %9.2f %10.0f%% %9.2f %8.0f\n", m, mean(mm[, "sens"]), mean(mm[, "spec"]),
                mean(mm[, "exact"]), 100 * mean(mm[, "hit"]), mean(mm[, "gap"]), mean(mm[, "fits"])))
  }
}
