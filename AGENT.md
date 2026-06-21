# Structura2 - Agent Guidelines

## Project Overview

**Structura2** is a personal tool for **Structural Equation Modeling (SEM)**, originally built with R/Shiny and being migrated to run as a **static site via ShinyLive** (WebAssembly R / WebR). It is developed and maintained by a single developer for personal use.

The application allows users to:
- Upload CSV data files with automatic encoding detection
- Define measurement and structural models via interactive table UI
- Fit SEM models using `lavaan`
- Visualize results through path diagrams (`DiagrammeR`/`semDiagram`), fit indices, and parameter tables

## Language Rules

> **All direct communication with the USER must be in Japanese.**
> **All code, comments, commit messages, documentation, and file contents must be written in English.**

This rule applies without exception.

- The official name of this application is **Structura2**. Always use **Structura2** in all titles, UI elements, files, and documentation (do not use "Structura").

## Repository Structure

```
Structura2/
├── app.R              # Main Shiny application (UI + Server)
├── global.R           # Global library loads and shared helpers
├── utils.R            # Inlined functions from readflex & semDiagram packages
├── help.md            # User-facing help documentation
├── export_shinylive.R # Script to export the app as a static ShinyLive site
├── www/
│   ├── logo.png       # Application logo
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
| `rhandsontable` | CRAN | Editable spreadsheet-like tables (measurement/structural model, correlation heatmap) |
| `lavaan` | CRAN | SEM engine |
| `DiagrammeR` | CRAN | Graphviz-based path diagram rendering |
| `markdown` | CRAN | Render help.md |
| `readflex` | GitHub (inlined) | CSV reader with auto encoding detection |
| `semDiagram` | GitHub (inlined) | SEM path diagram builder using DiagrammeR |

## WebR / ShinyLive Constraints

When modifying this app, keep these WebR limitations in mind:

1. **No source compilation**: Only pre-compiled WASM binaries can be used. Packages must be available at `repo.r-wasm.org` or R-universe.
2. **Limited locale support**: `Sys.setlocale()` does not work. The environment is fixed to "C" locale.
3. **Browser memory constraints**: Large file uploads are limited by browser tab memory.
4. **No system binaries**: Graphviz `dot` engine may not be available. `DiagrammeR::grViz()` functionality must be verified.

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

### Serving the Static Site Locally
```bash
python -m http.server 8000 --directory site
```
