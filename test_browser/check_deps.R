db <- installed.packages()
check_pkgs <- c('litedown', 'xfun', 'rmarkdown', 'knitr', 'regsem', 'Rsolnp')

for (p in check_pkgs) {
  if (p %in% rownames(db)) {
    cat(sprintf("[%s]\n  Depends: %s\n  Imports: %s\n\n", p, db[p, "Depends"], db[p, "Imports"]))
  } else {
    cat(sprintf("[%s] NOT INSTALLED LOCALLY\n\n", p))
  }
}

cat("=== WHO REQUIRES rmarkdown or knitr? ===\n")
for (p in rownames(db)) {
  deps <- paste(db[p, "Depends"], db[p, "Imports"])
  if (grepl("rmarkdown|knitr", deps)) {
    cat(sprintf("  %s depends on rmarkdown/knitr\n", p))
  }
}
