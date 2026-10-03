# export_shinylive.R
# This script prepares a clean app source directory containing only the necessary production files
# and exports it to a static site in the 'site/' directory. This avoids stuffing test folders into app.json.

if (!requireNamespace("shinylive", quietly = TRUE)) {
  install.packages("shinylive", repos = c("https://posit-dev.r-universe.dev", "https://cloud.r-project.org"))
}

# Pre-download the @hpcc-js/wasm Graphviz bundle (wasm is embedded in the UMD file, so no separate
# .wasm file is needed). It is hosted at site/hpcc-js/ rather than inside app.json.
assets_dir <- "www/hpcc-js"
dir.create(assets_dir, showWarnings = FALSE, recursive = TRUE)
js_path <- file.path(assets_dir, "graphviz.umd.js")

if (!file.exists(js_path)) {
  cat("Downloading graphviz.umd.js for production build...\n")
  tryCatch({
    download.file("https://cdn.jsdelivr.net/npm/@hpcc-js/wasm/dist/graphviz.umd.js", js_path, mode = "wb")
  }, error = function(e) {
    cat(sprintf("WARNING: Failed to download graphviz.umd.js: %s\n", e$message))
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
  # hpcc-js is hosted directly under site/hpcc-js/ (see below) to keep app.json small and to
  # avoid serving it through the slow R/webR HTTP emulation.
  if (basename(wf) == "hpcc-js") next
  if (dir.exists(wf)) {
    dest_subdir <- file.path(src_dir, "www", basename(wf))
    dir.create(dest_subdir, showWarnings = FALSE, recursive = TRUE)
    file.copy(list.files(wf, full.names = TRUE), dest_subdir, recursive = TRUE, overwrite = TRUE)
  } else {
    file.copy(wf, file.path(src_dir, "www", basename(wf)), overwrite = TRUE)
  }
  cat(sprintf("Copied www asset: %s\n", basename(wf)))
}

# Build-time dependency hint: makes ShinyLive bundle regsem (and its dependencies) without
# app.R referencing it, so the browser does not mount it at startup. This file is removed from
# app.json right after export (see below) and the packages are fetched on first use instead.
deferred_hint_file <- "_build_deps_hint.R"
writeLines(c("# Build-only dependency hint (stripped from app.json after export)",
             "library(regsem)"), file.path(src_dir, deferred_hint_file))

cat("Prepared clean app source directory. Exporting via ShinyLive...\n")
shinylive::export(appdir = src_dir, destdir = dest_dir)

# ---- Defer optional packages -------------------------------------
# ShinyLive mounts every bundled package at startup, one by one. Packages not needed by the
# initial UI (regsem and its dependency closure) are removed from metadata.rds and listed in
# packages/deferred.txt; their .tgz files stay in the site and app.R loads them on demand.
tryCatch({
  # 1. Strip the build-only hint from app.json (it must not be scanned by the browser)
  app_json <- file.path(dest_dir, "app.json")
  bundle <- jsonlite::read_json(app_json, simplifyVector = FALSE)
  bundle <- Filter(function(f) !identical(f$name, deferred_hint_file), bundle)
  jsonlite::write_json(bundle, app_json, auto_unbox = TRUE, null = "null", digits = NA)

  # 2. Split metadata.rds into eager and deferred packages
  webr_dir <- file.path(dest_dir, "shinylive", "webr")
  meta_path <- file.path(webr_dir, "packages", "metadata.rds")
  meta <- readRDS(meta_path)
  pkg_names <- vapply(meta, function(x) as.character(x$name), character(1))
  names(meta) <- pkg_names

  direct_deps <- function(entry) {
    tmp <- tempfile()
    dir.create(tmp)
    on.exit(unlink(tmp, recursive = TRUE))
    desc_in_tgz <- paste0(entry$name, "/DESCRIPTION")
    utils::untar(file.path(webr_dir, entry$path), files = desc_in_tgz, exdir = tmp)
    dcf <- read.dcf(file.path(tmp, desc_in_tgz), fields = c("Depends", "Imports", "LinkingTo"))
    deps <- trimws(strsplit(paste(stats::na.omit(as.character(dcf)), collapse = ","), ",")[[1]])
    deps <- sub("\\s*\\(.*$", "", deps)
    intersect(deps[nzchar(deps)], pkg_names)
  }

  eager_roots <- intersect(c("shinyjs", "DT", "rhandsontable", "markdown", "lavaan"), pkg_names)
  eager <- eager_roots
  repeat {
    new_deps <- setdiff(unique(unlist(lapply(meta[eager], direct_deps))), eager)
    if (length(new_deps) == 0) break
    eager <- c(eager, new_deps)
  }
  deferred <- setdiff(pkg_names, eager)

  if (!("regsem" %in% deferred)) {
    stop("regsem is not in the deferred set; leaving metadata.rds untouched")
  }
  saveRDS(unname(meta[eager]), meta_path)
  writeLines(vapply(meta[deferred], function(x) as.character(x$path), character(1)),
             file.path(webr_dir, "packages", "deferred.txt"))
  cat(sprintf("Deferred %d packages (loaded on demand): %s\n", length(deferred),
              paste(deferred, collapse = ", ")))
}, error = function(e) {
  cat(sprintf("WARNING: package deferral skipped (%s). The site still works but starts slower.\n", e$message))
})

# Host the Graphviz bundle as a plain static file next to index.html
dir.create(file.path(dest_dir, "hpcc-js"), showWarnings = FALSE, recursive = TRUE)
if (file.exists(js_path)) {
  file.copy(js_path, file.path(dest_dir, "hpcc-js", "graphviz.umd.js"), overwrite = TRUE)
  cat("Copied graphviz.umd.js to site/hpcc-js/
")
} else {
  cat("WARNING: graphviz.umd.js not found; path diagrams will not render in the static site
")
}

# Copy favicon.ico to the root of site directory
if (file.exists("www/favicon.ico")) {
  file.copy("www/favicon.ico", file.path(dest_dir, "favicon.ico"), overwrite = TRUE)
  cat("Copied favicon.ico to site root\n")
}

# Update the HTML title and inject favicon.ico link & custom CSS in index.html
index_html <- file.path(dest_dir, "index.html")
if (file.exists(index_html)) {
  html_content <- readLines(index_html, warn = FALSE)
  
  # Update title (using case-insensitive regex for title tag to be robust)
  html_content <- gsub("<title>.*?</title>", "<title>Structura2</title>", html_content, ignore.case = TRUE)
  
  # Host-page loading overlay (real startup milestones): sources live in host/
  read_host_file <- function(f) paste(readLines(file.path("host", f), warn = FALSE), collapse = "\n")
  splash_css  <- read_host_file("splash.css")
  splash_html <- read_host_file("splash.html")
  splash_js   <- read_host_file("splash.js")

  # Inject favicon.ico, SW cache buster, and custom CSS overrides before </head>
  # (fixed = TRUE: the injected text contains backslashes and must be inserted verbatim)
  head_inject <- paste0(
    '    <link rel="icon" type="image/x-icon" href="./favicon.ico" />\n',
    '    <script>\n',
    '      if ("serviceWorker" in navigator) {\n',
    '        navigator.serviceWorker.getRegistrations().then(function(regs) {\n',
    '          for (let reg of regs) { reg.update(); }\n',
    '        });\n',
    '      }\n',
    '    </script>\n',
    '    <style>\n',
    '      body, html {\n',
    '        margin: 0; padding: 0; width: 100%; height: 100%;\n',
    '        background-color: #0f172a !important;\n',
    '        color: #f8fafc !important;\n',
    '        font-family: system-ui, -apple-system, BlinkMacSystemFont, "Segoe UI", Roboto, sans-serif;\n',
    '      }\n',
    '      #root {\n',
    '        width: 100%; height: 100%;\n',
    '        background-color: #0f172a !important;\n',
    '      }\n',
    splash_css, '\n',
    '    </style>\n',
    '  </head>'
  )
  html_content <- sub("</head>", head_inject, html_content, fixed = TRUE)

  # Inject the progress overlay right after <body> so its script runs before shinylive.js
  # creates the WebR worker (module scripts are deferred until parsing is complete).
  splash_overlay <- paste0(
    '  <body>\n', splash_html, '\n',
    '    <script>\n', splash_js, '\n    </script>'
  )
  html_content <- sub("<body>", splash_overlay, html_content, fixed = TRUE)

  writeLines(html_content, index_html)
  cat("Updated index.html with Structura2 dark theme loader and favicon\n")
}

# Clean up temp directory
if (dir.exists(src_dir)) {
  unlink(src_dir, recursive = TRUE)
  cat("Cleaned up temporary source directory.\n")
}

cat("\nShinyLive export complete. The static site has been generated in the 'site/' directory.\n")
cat("To preview the app locally, run:\n")
cat("  python -m http.server 8100 --directory site\n")
cat("And navigate to: http://localhost:8100\n")
