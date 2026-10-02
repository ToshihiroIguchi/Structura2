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

# Update the HTML title and inject favicon.ico link & custom CSS in index.html
index_html <- file.path(dest_dir, "index.html")
if (file.exists(index_html)) {
  html_content <- readLines(index_html, warn = FALSE)
  
  # Update title (using case-insensitive regex for title tag to be robust)
  html_content <- gsub("<title>.*?</title>", "<title>Structura2</title>", html_content, ignore.case = TRUE)
  
  # Inject favicon.ico, SW cache buster, and custom CSS overrides before </head>
  custom_css <- paste0(
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
    '      #structura-splash-overlay {\n',
    '        position: fixed; inset: 0; z-index: 999999;\n',
    '        display: flex; flex-direction: column; justify-content: center; align-items: center;\n',
    '        background: radial-gradient(circle at 50% 35%, #1e293b 0%, #0f172a 100%);\n',
    '        color: #f8fafc; pointer-events: auto;\n',
    '      }\n',
    '      .structura-splash-card {\n',
    '        position: relative; width: 90%; max-width: 440px;\n',
    '        background: rgba(30, 41, 59, 0.75);\n',
    '        border: 1px solid rgba(255, 255, 255, 0.12);\n',
    '        border-radius: 16px; padding: 36px 32px;\n',
    '        box-shadow: 0 20px 40px rgba(0, 0, 0, 0.5), 0 0 40px rgba(59, 130, 246, 0.12);\n',
    '        backdrop-filter: blur(16px); -webkit-backdrop-filter: blur(16px);\n',
    '        text-align: center;\n',
    '      }\n',
    '      .structura-splash-close {\n',
    '        position: absolute; top: 12px; right: 16px;\n',
    '        background: transparent; border: none; color: #94a3b8;\n',
    '        font-size: 18px; cursor: pointer; padding: 4px 8px;\n',
    '        border-radius: 6px; transition: color 0.2s;\n',
    '      }\n',
    '      .structura-splash-close:hover { color: #f8fafc; background: rgba(255, 255, 255, 0.08); }\n',
    '      .structura-title {\n',
    '        margin: 0 0 6px 0; font-size: 26px; font-weight: 600; letter-spacing: 1px;\n',
    '        background: linear-gradient(135deg, #ffffff 0%, #cbd5e1 100%);\n',
    '        -webkit-background-clip: text; -webkit-text-fill-color: transparent;\n',
    '      }\n',
    '      .structura-subtitle {\n',
    '        margin: 0 0 26px 0; font-size: 13px; color: #94a3b8; font-weight: 400;\n',
    '      }\n',
    '      .structura-progress-track {\n',
    '        position: relative; width: 100%; height: 6px;\n',
    '        background: rgba(255, 255, 255, 0.08); border-radius: 9999px;\n',
    '        overflow: hidden; margin-bottom: 16px;\n',
    '      }\n',
    '      .structura-progress-indeterminate {\n',
    '        position: absolute; top: 0; height: 100%;\n',
    '        background: linear-gradient(90deg, #3b82f6 0%, #60a5fa 50%, #3b82f6 100%);\n',
    '        border-radius: 9999px;\n',
    '        box-shadow: 0 0 12px rgba(59, 130, 246, 0.6);\n',
    '        animation: structura-shimmer 2.2s infinite ease-in-out;\n',
    '      }\n',
    '      @keyframes structura-shimmer {\n',
    '        0% { left: -35%; width: 35%; }\n',
    '        50% { left: 40%; width: 45%; }\n',
    '        100% { left: 100%; width: 35%; }\n',
    '      }\n',
    '      .structura-status-row {\n',
    '        display: flex; justify-content: space-between; align-items: center;\n',
    '        font-size: 12px; color: #94a3b8;\n',
    '      }\n',
    '    </style>\n',
    '  </head>'
  )
  html_content <- gsub("</head>", custom_css, html_content, ignore.case = TRUE)
  
  # Inject Indeterminate Progress Overlay inside <body>
  splash_overlay <- paste0(
    '  <body>\n',
    '    <div id="structura-splash-overlay">\n',
    '      <div class="structura-splash-card">\n',
    '        <button class="structura-splash-close" title="Dismiss overlay" onclick="window.__hideStructuraOverlay()">&#215;</button>\n',
    '        <h1 class="structura-title">Structura2</h1>\n',
    '        <p class="structura-subtitle">Structural Equation Modeling Engine</p>\n',
    '        <div class="structura-progress-track">\n',
    '          <div class="structura-progress-indeterminate"></div>\n',
    '        </div>\n',
    '        <div class="structura-status-row">\n',
    '          <span id="structura-splash-status">Starting WebR environment...</span>\n',
    '        </div>\n',
    '      </div>\n',
    '    </div>\n',
    '    <script>\n',
    '      (function() {\n',
    '        let dismissed = false;\n',
    '        function hideOverlay() {\n',
    '          if (dismissed) return;\n',
    '          dismissed = true;\n',
    '          const overlay = document.getElementById("structura-splash-overlay");\n',
    '          if (overlay) {\n',
    '            overlay.style.transition = "opacity 0.6s ease-out";\n',
    '            overlay.style.opacity = "0";\n',
    '            setTimeout(function() { overlay.remove(); }, 650);\n',
    '          }\n',
    '        }\n',
    '        window.__hideStructuraOverlay = hideOverlay;\n',
    '        \n',
    '        // Phase status text rotator\n',
    '        const startTime = Date.now();\n',
    '        const statusElem = document.getElementById("structura-splash-status");\n',
    '        const statusTimer = setInterval(function() {\n',
    '          if (dismissed || !statusElem) return;\n',
    '          const elapsed = (Date.now() - startTime) / 1000;\n',
    '          if (elapsed > 8) {\n',
    '            statusElem.innerText = "Preparing Structura2 UI...";\n',
    '          } else if (elapsed > 3.5) {\n',
    '            statusElem.innerText = "Loading SEM engine & libraries...";\n',
    '          }\n',
    '        }, 500);\n',
    '        \n',
    '        // 1. Listen for postMessage from Shiny server\n',
    '        window.addEventListener("message", function(e) {\n',
    '          if (e.data && (e.data.type === "structura-ready" || e.data === "structura-ready")) {\n',
    '            clearInterval(statusTimer);\n',
    '            if (statusElem) statusElem.innerText = "Ready!";\n',
    '            setTimeout(hideOverlay, 200);\n',
    '          }\n',
    '        });\n',
    '        \n',
    '        // 2. DOM fallback check for modal/app container inside iframe\n',
    '        const domCheck = setInterval(function() {\n',
    '          if (dismissed) { clearInterval(domCheck); return; }\n',
    '          const iframe = document.querySelector("iframe");\n',
    '          if (iframe && iframe.contentDocument) {\n',
    '            if (iframe.contentDocument.querySelector("#sample_ds, #structura-main-app, .modal-dialog")) {\n',
    '              clearInterval(domCheck);\n',
    '              clearInterval(statusTimer);\n',
    '              if (statusElem) statusElem.innerText = "Ready!";\n',
    '              setTimeout(hideOverlay, 200);\n',
    '            }\n',
    '          }\n',
    '        }, 200);\n',
    '        \n',
    '        // 3. Fallback safety timeout (35s)\n',
    '        setTimeout(function() {\n',
    '          clearInterval(statusTimer);\n',
    '          clearInterval(domCheck);\n',
    '          hideOverlay();\n',
    '        }, 35000);\n',
    '      })();\n',
    '    </script>'
  )
  html_content <- gsub("<body>", splash_overlay, html_content, fixed = TRUE)
  
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
