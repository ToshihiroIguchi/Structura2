# Power / false-alarm trade-off of the MI suggestion thresholds (see simulate_recovery.R, part B).
# Run from the project root:  Rscript tests/simulate_mi_thresholds.R [reps]
suppressMessages(library(lavaan))
args <- commandArgs(trailingOnly = TRUE); reps <- if (length(args)) as.integer(args[1]) else 150L
src <- readLines("tests/simulate_recovery.R"); cut <- grep("^# ---------- A", src)[1]
eval(parse(text = src[1:(cut - 1)]))
cur_df <- mk(list(c("m","x1"), c("m","x2"), c("y","m"))); cur_lines <- build_struct_lines(cur_df)
r <- match("y", cur_df$Dependent); cc <- match("x3", struct_pred_cols(cur_df))
grid <- expand.grid(mi = c(3.84, 6.63, 10.83), epc = c(0.1, 0.2))
set.seed(11)
cat(sprintf("%d reps per cell; omitted true path y ~ x3 (beta = .3)\n", reps))
for (n in c(200, 600)) {
  hit <- matrix(NA, reps, nrow(grid)); alarm <- matrix(NA, reps, nrow(grid))
  for (i in seq_len(reps)) {
    for (truth in c(TRUE, FALSE)) {
      d <- gen(n, b_y = c(m = .5, x1 = 0, x2 = 0, x3 = if (truth) .3 else 0))
      sf <- fit_suggestion_model(cur_lines, "x3", "y", list(data = d, missing_method = "listwise", needs_meanstructure = FALSE))
      for (g in seq_len(nrow(grid))) {
        sug <- if (is.null(sf)) NULL else get_suggested_structural_paths(sf, cur_df, grid$mi[g], grid$epc[g])
        if (truth) { cell <- if (is.null(sug)) NULL else sug[[r]][[cc]]; hit[i, g] <- !is.null(cell) && identical(cell$rank, 1L) }
        else alarm[i, g] <- !is.null(sug)
      }
    }
  }
  for (g in seq_len(nrow(grid)))
    cat(sprintf("  N=%4d  MI>=%5.2f |std.EPC|>=%.1f : found as #1 = %3.0f%%   false alarm = %3.0f%%\n",
                n, grid$mi[g], grid$epc[g], 100 * mean(hit[, g]), 100 * mean(alarm[, g])))
}
