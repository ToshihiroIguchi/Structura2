# export_shinylive.R
# This script prepares a clean app source directory containing only the necessary production files
# and exports it to a static site in the 'site/' directory. This avoids stuffing test folders into app.json.

if (!requireNamespace("shinylive", quietly = TRUE)) {
  install.packages("shinylive", repos = c("https://posit-dev.r-universe.dev", "https://cloud.r-project.org"))
}

src_dir <- "app_source"
dest_dir <- "site"

# Clean up existing directories
if (dir.exists(src_dir)) unlink(src_dir, recursive = TRUE)
dir.create(src_dir)
dir.create(file.path(src_dir, "www"))

# Copy production files only
files_to_copy <- c("app.R", "help.md")
for (f in files_to_copy) {
  if (file.exists(f)) {
    file.copy(f, file.path(src_dir, f))
    cat(sprintf("Copied to source: %s\n", f))
  } else {
    cat(sprintf("WARNING: File not found: %s\n", f))
  }
}

# Copy www assets
www_files <- list.files("www", full.names = TRUE)
for (wf in www_files) {
  file.copy(wf, file.path(src_dir, "www", basename(wf)))
  cat(sprintf("Copied www asset: %s\n", basename(wf)))
}

cat("Prepared clean app source directory. Exporting via ShinyLive...\n")
shinylive::export(appdir = src_dir, destdir = dest_dir)

# Update the HTML title from 'Shiny App' to 'Structura2'
index_html <- file.path(dest_dir, "index.html")
if (file.exists(index_html)) {
  html_content <- readLines(index_html, warn = FALSE)
  html_content <- gsub("<title>Shiny App</title>", "<title>Structura2</title>", html_content, ignore.case = TRUE)
  writeLines(html_content, index_html)
  cat("Updated index.html title to 'Structura2'\n")
}

# Clean up temp directory disabled for debugging
# unlink(src_dir, recursive = TRUE)

cat("\nShinyLive export complete. The static site has been generated in the 'site/' directory.\n")
cat("To preview the app locally, run:\n")
cat("  python -m http.server 8000 --directory site\n")
cat("And navigate to: http://localhost:8000\n")
