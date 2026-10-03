# Structura2 - Agent Guidelines

## Project Overview

**Structura2** is a personal tool for **Structural Equation Modeling (SEM)**, originally built with R/Shiny and being migrated to run as a **static site via ShinyLive** (WebAssembly R / WebR). It is developed and maintained by a single developer for personal use.

The application allows users to:
- Upload CSV data files with automatic encoding detection
- Define measurement and structural models via interactive table UI
- Fit SEM models using `lavaan`
- Visualize results through path diagrams (rendered via browser-side `@hpcc-js/wasm` based on `semDiagram` outputs), fit indices, and parameter tables

## Language Rules

> **All direct communication with the USER must be in Japanese.**
> **All code, comments, commit messages, documentation, and file contents must be written in English.**

This rule applies without exception.

- The official name of this application is **Structura2**. Always use **Structura2** in all titles, UI elements, files, and documentation (do not use "Structura").

## Repository Structure

```
Structura2/
├── app.R              # Main Shiny application (UI + Server with inlined semDiagram)
├── help.md            # User-facing help documentation
├── export_shinylive.R # Script to export the app as a static ShinyLive site
├── host/              # Loading overlay for the static site (splash.css/html/js), injected into index.html by export_shinylive.R
├── www/
│   ├── logo.png       # Application logo (small; shown at 40 px)
│   ├── hpcc-js/       # graphviz.umd.js (wasm embedded); hosted at site/hpcc-js/ in the static site, not in app.json
│   └── style.css      # Custom CSS overrides
├── test_webr/         # Minimal test app for WebR compatibility verification
├── AGENT.md           # This file: agent guidelines (all AI models)
├── GEMINI.md          # Gemini-specific agent guidelines
├── CLAUDE.md          # Claude-specific agent guidelines
└── README.md          # Project readme
```

## Key Dependencies

| Package | Source | Role |
|---------|--------|------|
| `shiny` | CRAN | Core web framework |
| `shinyjs` | CRAN | JavaScript interop (show/hide elements) |
| `DT` | CRAN | Interactive data tables |
| `rhandsontable` | CRAN | Editable spreadsheet-like tables (measurement/structural model) |
| `lavaan` | CRAN | SEM engine |
| `@hpcc-js/wasm` | CDN / Local | Browser-side WebAssembly Graphviz path diagram rendering |
| `markdown` | CRAN | Render help.md |
| (Browser JS) | Built-in | HTML5 FileReader and TextDecoder for auto encoding detection |
| `semDiagram` | Inlined (app.R) | SEM path diagram builder (outputs DOT format) |

## WebR / ShinyLive Constraints

When modifying this app, keep these WebR limitations in mind:

1. **No source compilation**: Only pre-compiled WASM binaries can be used. Packages must be available at `repo.r-wasm.org` or R-universe.
2. **Limited locale support**: `Sys.setlocale()` does not work. The environment is fixed to "C" locale.
3. **Browser memory constraints**: Large file uploads are limited by browser tab memory.
4. **No system binaries**: Graphviz `dot` engine is not required on the server since rendering is offloaded to the client browser via `@hpcc-js/wasm`.

## Error Handling Policy

**All user-facing operations must be wrapped in `tryCatch` to prevent app crashes.** This is especially critical for:

- CSV file upload and parsing
- Data transformation (log10, one-hot encoding, standardization)
- `lavaan` model fitting
- Path diagram rendering
- Any operation that depends on user-supplied data

When an error occurs, display a user-friendly message instead of crashing the application.

## Build Instructions

### Local Development (Standard R)
```r
shiny::runApp(".")
```

### Static Site Export (ShinyLive)
```r
source("export_shinylive.R")
```
This generates a `site/` directory with static HTML/JS/WASM assets.

Startup-time design of the static site (keep these in mind when changing `app.R` or the export script):

- **Progress overlay**: `host/splash.*` is injected into `site/index.html`. It shows real milestones: the WebR worker is hooked in the host page, and `app.R` reports stages (`report_startup_stage()`) through a `BroadcastChannel` named `structura-progress`; `structura-ready` / `structura-error` arrive via `postMessage`. Add new milestones in both `host/splash.js` (`MILESTONES`) and `app.R`.
- **Small `app.json`**: `graphviz.umd.js` is copied to `site/hpcc-js/` instead of being bundled (bundled files are served through the slow R/webR HTTP emulation). Keep `www/` assets small.
- **Deferred packages**: regsem and its dependencies are removed from `metadata.rds` after export and listed in `packages/deferred.txt`; `ensure_regsem_loaded()` fetches them on first use. ShinyLive scans `app.R` for literal package names (`library()`, `pkg::`, `requireNamespace("pkg")`, ...) and installs them at startup, so regsem must only be referenced through `regsem_pkg` (a dynamically built name). A build-only hint file (`_build_deps_hint.R`) makes the export bundle regsem and is stripped from `app.json`.

### Serving the Static Site Locally
```bash
python -m http.server 8100 --directory site
```
