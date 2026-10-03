# Test renv dependency detection on code with and without explicit library() vs pkg::func()

code1 <- "
# Test with pkg::func
test <- function() {
  regsem::cv_regsem()
}
"

code2 <- "
# Test without direct pkg call (e.g. getExportedValue or dynamic call)
test <- function() {
  if (requireNamespace('regsem', quietly = TRUE)) {
    f <- get('cv_regsem', asNamespace('regsem'))
  }
}
"

tmp1 <- tempfile(fileext = ".R")
tmp2 <- tempfile(fileext = ".R")
writeLines(code1, tmp1)
writeLines(code2, tmp2)

cat("Dependencies in code1 (pkg::func):\n")
print(renv::dependencies(tmp1)$Package)

cat("\nDependencies in code2 (dynamic get):\n")
print(renv::dependencies(tmp2)$Package)

unlink(c(tmp1, tmp2))
