# Unit tests for the shared helpers extracted from the server code (data preparation, report formatting,
# candidate records). Run: Rscript tests/test_shared_helpers.R   (from the project root)
suppressMessages(library(lavaan))
exprs <- parse("app.R")
wanted <- c("preprocess_columns", "display_column_choices", "is_constant_col", "num_dash", "pe_cell_strings", "ident_df_suffix",
            "model_usable", "print_table_html", "with_indicator_names", "fit_converged", "fit_failure",
            "candidate_record", "build_retained_str", "build_struct_lines", "struct_edges", "struct_pred_cols",
            "FIT_INDEX_NAMES", "collinearity_stats", "pe_with_std", "%||%")
for (e in exprs) {
  if (is.call(e) && identical(e[[1]], as.name("<-")) && as.character(e[[2]]) %in% wanted) eval(e)
}
ok <- function(cond, msg) { cat(if (isTRUE(cond)) "PASS" else "FAIL", "-", msg, "\n"); if (!isTRUE(cond)) quit(status = 1) }

# --- preprocess_columns
d <- data.frame(a = c(1, 2, 3, 4), b = c(10, 20, 30, 40), g = factor(c("u", "v", "u", "w")),
                id = c("p", "q", "r", "s"), stringsAsFactors = FALSE)
p <- preprocess_columns(d, "b")
ok(identical(names(p)[1:2], c("a", "log_b")), "preprocess: log10 column is renamed in place")
ok(isTRUE(all.equal(p$log_b, log10(d$b))), "preprocess: log10 values")
ok(all(c("gv", "gw") %in% names(p)) && !"g" %in% names(p), "preprocess: factor with 2+ levels is one-hot encoded")
ok("id" %in% names(p), "preprocess: a column unique per row is kept as text, not encoded")
ok(identical(names(preprocess_columns(data.frame(`a b` = 1:2, check.names = FALSE))), "a.b"), "preprocess: names are made syntactic")
ok(identical(names(preprocess_columns(d[0, ])), names(d)), "preprocess: zero-row data does not fail")

pf <- preprocess_columns(d[d$g == "u", ], "b", reference = d)
ok(identical(names(pf), names(p)), "preprocess: filtered rows keep the full column set when a reference is given")
ok(all(pf$gv == 0) && all(pf$gw == 0), "preprocess: a level absent from the subset is an all-zero column")
ok(identical(preprocess_columns(d, "b", reference = d), p), "preprocess: reference = the data itself changes nothing")

# --- display_column_choices
dd <- data.frame(a = c(1, 2, 3), const = c(5, 5, 5), z = c(2, 4, 9))
ch <- display_column_choices(dd, dd, NULL)
ok(identical(ch$available, c("a", "z")) && identical(ch$zero_var, "const"), "display choices: constant column excluded")
ok(identical(ch$default, c("a", "z")), "display choices: default is the numeric columns")
full <- preprocess_columns(dd, "z")
ok(identical(display_column_choices(full, dd, "z")$default, c("a", "log_z")), "display choices: log-transformed column under its log_ name")
ok(identical(display_column_choices(data.frame(), data.frame(), NULL)$available, character(0)), "display choices: empty data")
ok(is_constant_col(c(0.3, 0.1 + 0.2)) && is_constant_col(c(NA_real_, NA)) && is_constant_col(5) &&
     !is_constant_col(c(1, 2)) && is_constant_col(c(1, NA, 1)), "is_constant_col: single value, all NA, float noise")
ok("w" %in% display_column_choices(data.frame(w = c(0.3, 0.1 + 0.2), z = c(1, 2)), NULL, NULL)$zero_var,
   "display choices: float-noise column counts as constant")

# --- formatting helpers
ok(identical(num_dash(c(1.23456, NA)), c("1.235", "-")), "num_dash: NA becomes '-'")
ok(identical(num_dash(NA_real_, "%.1f"), "-") && identical(num_dash(2, "%.1f"), "2.0"), "num_dash: custom format")
ok(identical(ident_df_suffix(-2), " (df = -2)") && identical(ident_df_suffix(NA_real_), "") && identical(ident_df_suffix(NULL), ""),
   "ident_df_suffix")
r <- data.frame(pvalue = 0.0004, std.all = 0.5, se = NA_real_, z = 1.5)
s <- pe_cell_strings(r)
ok(identical(s$p, "< .001") && identical(s$std, "0.500") && identical(s$se, "-") && identical(s$z, "1.500"), "pe_cell_strings")
ok(identical(pe_cell_strings(data.frame(pvalue = NA_real_, se = 0.1, z = 2))$std, "-"), "pe_cell_strings: no std.all column")
h <- print_table_html(c("A", "B &alpha;"), "<tr><td>1</td><td>2</td></tr>", align = c("left", "right"))
ok(grepl("<th>A</th>", h, fixed = TRUE) && grepl("<th style='text-align:right;'>B &alpha;</th>", h, fixed = TRUE) &&
     grepl("<table class='print-table'>", h, fixed = TRUE) && grepl("<tr><td>1</td>", h, fixed = TRUE), "print_table_html")

# --- model_usable / fit_failure / fit_converged
ok(!model_usable(NULL) && !model_usable(list(ok = FALSE, fit = 1)) && !model_usable(list(ok = TRUE)) &&
     model_usable(list(ok = TRUE, fit = 1)), "model_usable")
f <- fit_failure("msg", "data")
ok(!f$ok && f$fail_kind == "data" && is.null(f$fit) && is.null(f$msg_level), "fit_failure: defaults")
ok(identical(fit_failure("m", "empty", level = "info")$msg_level, "info"), "fit_failure: level")
set.seed(1)
fd <- data.frame(x = rnorm(60)); fd$y <- 0.5 * fd$x + rnorm(60)
fit <- sem("y ~ x", data = fd)
ok(fit_converged(fit) && !fit_converged(NULL) && !fit_converged("not a fit"), "fit_converged")
cs <- collinearity_stats(fit)
ok(is.finite(cs$condition_number) && cs$max_cond_index >= 1, "collinearity_stats: finite for a regular fit")
ok(is.na(collinearity_stats(NULL)$condition_number), "collinearity_stats: NA when the fit cannot be read")
ok(is.data.frame(pe_with_std(fit)) && "std.all" %in% names(pe_with_std(fit)), "pe_with_std")
ok(all(FIT_INDEX_NAMES %in% names(fitMeasures(fit))), "FIT_INDEX_NAMES are valid lavaan measures")

# --- with_indicator_names: latent names become unique indicator names
tbl <- data.frame(Latent = c("F", "", "x"), Indicator = "", Operator = "=~", stringsAsFactors = FALSE)
w <- with_indicator_names(tbl, c("x", "y"))
ok(identical(w$Indicator, c("F", "", "x.1")), "with_indicator_names: empty latent has no indicator; clash with a variable gets a suffix")

# --- candidate_record: every record has the same fields
sdf <- data.frame(Dependent = c("a", "b"), Operator = "~", a = c(FALSE, FALSE), b = c(TRUE, FALSE), stringsAsFactors = FALSE)
rec0 <- candidate_record(sdf, "")
ok(identical(rec0$removed_str, "None (Baseline Model)") && identical(rec0$retained_str, "a ~ b") && !rec0$converged &&
     rec0$status == "[Non-converged]" && is.infinite(rec0$deficit), "candidate_record: non-converged defaults")
rec1 <- candidate_record(sdf, "a ~ x", fit = fit, aic = 1, converged = TRUE, proper = TRUE, identified = TRUE, status = "",
                         added_covs = NULL)
ok(identical(names(rec0), names(rec1)), "candidate_record: same fields for every record")
ok(rec1$aic == 1 && rec1$converged && identical(rec1$removed_str, "a ~ x") && identical(rec1$added_covs, character(0)),
   "candidate_record: overrides applied, NULL keeps the default")
cat("All shared-helper tests passed\n")
