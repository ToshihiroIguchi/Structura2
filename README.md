
# Structura2

**Structural Insights, Simplified.**
<br>
<img src="www/logo.png" width="12.5%" />

**Live Demo**: [https://toshihiroiguchi.github.io/Structura2/](https://toshihiroiguchi.github.io/Structura2/)

## Description

Structura2 is an interactive Shiny application for Structural Equation Modeling (SEM) in **R** ([r-project.org](https://www.r-project.org/?utm_source=chatgpt.com)), making it easy to upload data, specify models, and visualize results in a unified interface. It leverages the **lavaan** package for comprehensive latent variable analysis ([cran.r-project.org](https://cran.r-project.org/package%3Dlavaan?utm_source=chatgpt.com)).

## Features

* **Data Upload & Inspection**: Upload CSV files with automatic encoding handling via **readflex**.
* **Log-transform**: Apply common logarithm (log10) to positive numeric columns.
* **One-hot Encoding**: Convert categorical variables to dummy indicators for SEM compatibility.
* **Model Specification**: Define measurement (`Latent =~ Indicators`) and structural (`Dependent ~ Predictors`) models in interactive Handsontable grids powered by **rhandsontable** and **DT** ([cran.r-project.org](https://cran.r-project.org/package%3Dshiny?utm_source=chatgpt.com), [shiny.posit.co](https://shiny.posit.co/?utm_source=chatgpt.com)).
* **SEM Fitting**: Fit models using **lavaan** with support for mean structures and detailed fit measures.
* **Visualization**: Render path diagrams via **DiagrammeR**/**semDiagram**, and inspect correlation heatmaps using **rhandsontable**.
* **Comprehensive Reporting**: View fit indices (p-value, SRMR, RMSEA, AIC, BIC, GFI, AGFI, NFI, CFI), parameter tables, and formatted equations in real time.

## Launch Application

There are several ways to launch and run **Structura2**, depending on whether you want to run it online, locally via standard Shiny, or as a compiled static site.

### Option 1: Live Demo (No Setup Required)
Simply access the application online via GitHub Pages:
👉 **[Structura2 Live Demo](https://toshihiroiguchi.github.io/Structura2/)**

This version is compiled into WebAssembly using ShinyLive and runs entirely inside your web browser. You do not need to install R or any libraries.

### Option 2: Run Locally (Traditional Shiny App)
To run the traditional Shiny application locally, you need [R](https://www.r-project.org/) installed.

1. **Install Dependencies**: Open R or RStudio and run the following command to install the required packages:
   ```r
   install.packages(c("shiny", "shinyjs", "DT", "rhandsontable", "lavaan", "DiagrammeR", "markdown"))
   ```
2. **Run the App**: Set your working directory to the project folder and run:
   ```r
   shiny::runApp(".")
   ```
   The app will open in your default browser (usually at `http://127.0.0.1:xxxx`).

### Option 3: Compile and Serve Static Site Locally (ShinyLive WebAssembly)
You can compile the app to a static site and serve it using a local web server.

1. **Install ShinyLive**: In R, install the `shinylive` package:
   ```r
   install.packages("shinylive")
   ```
2. **Export the App**: Open your terminal (or command prompt) in the project root directory and run the export script:
   ```bash
   Rscript export_shinylive.R
   ```
   This will prepare a clean source structure and generate the static site inside the `site/` directory.
3. **Serve the Directory**: Run a local web server to serve the generated assets.
   - **Using Python 3**:
     ```bash
     python -m http.server 8000 --directory site
     ```
   - **Using Node.js (http-server)**:
     ```bash
     npx http-server site -p 8000
     ```
4. **Access the App**: Open your web browser and navigate to `http://localhost:8000`.


## Hosting the Shiny App Directly from GitHub (Traditional Shiny)

You can launch **Structura2** directly from its GitHub repository as a traditional Shiny application (not ShinyLive) and make it accessible across your LAN. This requires a local installation of R. 
This script automatically detects your host's IPv4 address and configures Shiny's host/port options so other devices on your local network can connect.

```r
# ── Packages ──────────────────────────────────────────────────
if (!requireNamespace("stringr", quietly = TRUE)) {
  install.packages("stringr")
}
library(stringr)
library(shiny)

# ── Function: Detect host IPv4 address ───────────────────────
get_ip <- function() {
  sysname <- Sys.info()[["sysname"]]
  
  if (sysname == "Windows") {
    # Run ipconfig and convert CP932 output to UTF-8
    raw   <- system("ipconfig", intern = TRUE)
    lines <- iconv(raw, from = "CP932", to = "UTF-8")
    
    # Grab the first line that contains the token "IPv4"
    ipv4_lines <- grep("IPv4", lines, value = TRUE, ignore.case = TRUE)
    line <- if (length(ipv4_lines) > 0) ipv4_lines[1] else ""
    
    # Fallback: use findstr if nothing was found
    if (!nzchar(line)) {
      line <- shell('ipconfig | findstr /i "IPv4"', intern = TRUE)[1]
    }
    
    # Final fallback: netsh provides a locale-independent label
    if (!nzchar(line)) {
      out  <- system("netsh interface ipv4 show ipaddresses", intern = TRUE)
      line <- grep("IP Address", out, value = TRUE)[1]
    }
    
    # Extract the IPv4 numeric pattern
    ip <- str_extract(line, "\\b(?:[0-9]{1,3}\\.){3}[0-9]{1,3}\\b")
    
  } else {
    # Linux / macOS: primary approach
    addr4 <- system("ip -4 addr", intern = TRUE)
    inet  <- addr4[grep("inet ", addr4)[1]]
    ip    <- sub(".*inet\\s+([0-9\\.]+)/.*", "\\1", inet)
    
    # Fallback: use routing information
    if (!nzchar(ip)) {
      rt <- system("ip route get 8.8.8.8", intern = TRUE)[1]
      ip <- str_extract(rt, "\\b(?:[0-9]{1,3}\\.){3}[0-9]{1,3}\\b")
    }
  }
  
  ip
}

# ── Configuration & launch ────────────────────────────────────
port    <- 8100
host_ip <- get_ip()
cat("Launching Structura2 on", host_ip, "port", port, "\n")

# runGitHub() cannot take host directly; set Shiny options
options(
  shiny.host = host_ip,
  shiny.port = port
)

# Launch Structura2 from GitHub
shiny::runGitHub(
  repo           = "Structura2",
  username       = "ToshihiroIguchi",
  ref            = "main",
  launch.browser = FALSE,
  port           = port
)
```


## Image
<br>
<img src="image.png"/>

## License

Released under the **MIT License** © 2025 Toshihiro Iguchi.

## Author

**Toshihiro Iguchi**
