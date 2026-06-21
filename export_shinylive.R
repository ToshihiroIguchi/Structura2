# export_shinylive.R
# This script prepares a clean app source directory containing only the necessary production files
# and exports it to a static site in the 'site/' directory. This avoids stuffing test folders into app.json.

if (!requireNamespace("shinylive", quietly = TRUE)) {
  install.packages("shinylive", repos = c("https://posit-dev.r-universe.dev", "https://cloud.r-project.org"))
}

# Pre-download @hpcc-js/wasm assets for offline use before export
assets_dir <- "www/hpcc-js"
dir.create(assets_dir, showWarnings = FALSE, recursive = TRUE)
js_path <- file.path(assets_dir, "graphviz.umd.js")
wasm_path <- file.path(assets_dir, "graphvizlib.wasm")

if (!file.exists(js_path)) {
  cat("Downloading graphviz.umd.js for production build...\n")
  tryCatch({
    download.file("https://cdn.jsdelivr.net/npm/@hpcc-js/wasm/dist/graphviz.umd.js", js_path, mode = "wb")
  }, error = function(e) {
    cat(sprintf("WARNING: Failed to download graphviz.umd.js: %s\n", e$message))
  })
}
if (!file.exists(wasm_path)) {
  cat("Downloading graphvizlib.wasm for production build...\n")
  tryCatch({
    download.file("https://cdn.jsdelivr.net/npm/@hpcc-js/wasm/dist/graphvizlib.wasm", wasm_path, mode = "wb")
  }, error = function(e) {
    cat(sprintf("WARNING: Failed to download graphvizlib.wasm: %s\n", e$message))
  })
}

dest_dir <- "site"
# Use a temporary directory outside the project root to bypass .gitignore rules
# which prevent renv::dependencies from scanning files inside gitignored directories.
src_dir <- file.path(tempdir(), "Structura2_app_source")

# Clean up existing directories
if (dir.exists(src_dir)) unlink(src_dir, recursive = TRUE)
dir.create(src_dir, recursive = TRUE)
dir.create(file.path(src_dir, "www"))

# Clean up destination directory to ensure no stale WASM package files from previous builds remain
if (dir.exists(dest_dir)) {
  unlink(dest_dir, recursive = TRUE)
  cat(sprintf("Cleaned up existing destination directory: %s\n", dest_dir))
}
dir.create(dest_dir)

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

# Copy www assets (recursively, including directories like hpcc-js)
www_files <- list.files("www", full.names = TRUE)
for (wf in www_files) {
  if (dir.exists(wf)) {
    dest_subdir <- file.path(src_dir, "www", basename(wf))
    dir.create(dest_subdir, showWarnings = FALSE, recursive = TRUE)
    file.copy(list.files(wf, full.names = TRUE), dest_subdir, recursive = TRUE, overwrite = TRUE)
  } else {
    file.copy(wf, file.path(src_dir, "www", basename(wf)), overwrite = TRUE)
  }
  cat(sprintf("Copied www asset: %s\n", basename(wf)))
}

cat("Prepared clean app source directory. Exporting via ShinyLive...\n")
shinylive::export(appdir = src_dir, destdir = dest_dir)

# Copy favicon.ico to the root of site directory
if (file.exists("www/favicon.ico")) {
  file.copy("www/favicon.ico", file.path(dest_dir, "favicon.ico"), overwrite = TRUE)
  cat("Copied favicon.ico to site root\n")
}

# Update the HTML title and inject favicon.ico link in index.html
index_html <- file.path(dest_dir, "index.html")
if (file.exists(index_html)) {
  html_content <- readLines(index_html, warn = FALSE)
  
  # Update title (using case-insensitive regex for title tag to be robust)
  html_content <- gsub("<title>.*?</title>", "<title>Structura2</title>", html_content, ignore.case = TRUE)
  
  # Inject favicon.ico before </head> (using more robust match for head closing tag)
  favicon_tag <- '    <link rel="icon" type="image/x-icon" href="./favicon.ico" />\n  </head>'
  html_content <- gsub("</head>", favicon_tag, html_content, ignore.case = TRUE)
  
  writeLines(html_content, index_html)
  cat("Updated index.html title to 'Structura2' and injected favicon link\n")
}

# Clean up temp directory
if (dir.exists(src_dir)) {
  unlink(src_dir, recursive = TRUE)
  cat("Cleaned up temporary source directory.\n")
}

cat("\nShinyLive export complete. The static site has been generated in the 'site/' directory.\n")
cat("To preview the app locally, run:\n")
cat("  python -m http.server 8000 --directory site\n")
cat("And navigate to: http://localhost:8000\n")
