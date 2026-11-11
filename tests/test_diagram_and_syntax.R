# Regression tests: diagram nodes, measurement syntax helper, defined parameters.
# Run: Rscript tests/test_diagram_and_syntax.R   (from the project root)
suppressMessages(library(lavaan))
exprs <- parse("app.R")
wanted <- c("semDiagram", "build_meas_lines", "row_checked", "%||%")
for (e in exprs) {
  if (is.call(e) && identical(e[[1]], as.name("<-")) && as.character(e[[2]]) %in% wanted) eval(e)
}
ok <- function(cond, msg) { cat(if (isTRUE(cond)) "PASS" else "FAIL", "-", msg, "\n"); if (!isTRUE(cond)) quit(status = 1) }

# 1. Defined parameters / constraints must not become diagram nodes
m <- "dem60 =~ y1 + y2 + y3
dem60 ~ a*x1
y5 ~ b*dem60
ab := a*b"
d <- PoliticalDemocracy; d$x1 <- d$x1 %||% 0
fit <- suppressWarnings(sem(m, data = transform(PoliticalDemocracy, x1 = y4)))
dot <- semDiagram(fit)
ok(!grepl("\"ab\"", dot, fixed = TRUE) && !grepl("\"a*b\"", dot, fixed = TRUE), "no fake nodes for := parameters")

# 2. Defined parameters stay in the unfiltered estimates
pe <- parameterEstimates(fit, standardized = FALSE, remove.def = FALSE)
ok(any(pe$op == ":="), "remove.def = FALSE keeps := rows")

# 3. build_meas_lines normalizes latent names and handles tables without indicator columns
meas <- data.frame(Latent = c("My Latent", "1st", ""), Indicator = "", Operator = "=~",
                   y1 = c(TRUE, FALSE, TRUE), y2 = c(TRUE, TRUE, TRUE), stringsAsFactors = FALSE)
ln <- build_meas_lines(meas)
ok(identical(ln, c("My.Latent =~ y1 + y2", "X1st =~ y2")), "latent names normalized, empty latent skipped")
ok(length(build_meas_lines(meas[, 1:3])) == 0, "3-column table returns character(0)")
ok(length(build_meas_lines(NULL)) == 0, "NULL table returns character(0)")
# 4. Zero coefficients are drawn in a neutral gray, not blue (positive) or red (negative)
m0 <- "dem60 =~ y1 + y2 + y3
y5 ~ dem60
y5 ~ 0*y4"
fit0 <- suppressWarnings(sem(m0, data = PoliticalDemocracy))
dot0 <- semDiagram(fit0)
ok(grepl("#7F7F7F", dot0, fixed = TRUE), "fixed-zero path is drawn in gray")
ok(grepl("#0000FF", dot0, fixed = TRUE), "positive paths stay blue")
zero_line <- grep("label=\"0.000\"", strsplit(dot0, "\n")[[1]], value = TRUE)
ok(length(zero_line) == 1 && grepl("fontcolor=\"gray50\"", zero_line, fixed = TRUE), "zero label is gray")
ok(!any(grepl("fontcolor=\"gray50\"", grep("label=\"0.000\"", strsplit(dot0, "\n")[[1]], value = TRUE, invert = TRUE), fixed = TRUE)),
   "nonzero labels keep the default color")
cat("All diagram/syntax tests done\n")
