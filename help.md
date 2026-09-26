<img src="www/logo.png" width="25%" style="margin-bottom: 15px;" />

# Structura2: User Manual & Reference Guide
### *Structural Insights, Simplified.*

---

## Table of Contents
1. [Introduction & Architecture](#1-introduction--architecture)
2. [Quick-Start Tutorial (5-Minute Walkthrough)](#2-quick-start-tutorial-5-minute-walkthrough)
3. [Data Management & Preprocessing](#3-data-management--preprocessing)
   - [Data Tab & Encodings](#31-data-tab--file-encodings)
   - [Built-In Demo Datasets](#32-built-in-demo-datasets)
   - [Analysis Settings](#33-analysis-settings-mode--missing-data)
   - [Data Transformation & Filtering](#34-data-transformation--selection)
4. [Model Specification & lavaan Syntax](#4-model-specification--lavaan-syntax)
   - [Measurement Model (`=~`)](#41-measurement-model)
   - [Structural Model (`~`) & Correlation Heatmap](#42-structural-model--correlation-heatmap)
   - [Suggested Paths via Modification Indices](#43-suggested-paths-via-modification-indices)
   - [Manual Equations (Advanced Syntax)](#44-manual-equations-advanced-syntax)
   - [Inspecting Compiled Syntax](#45-inspecting-compiled-syntax)
5. [Automated Model Optimization (Model Pruning)](#5-automated-model-optimization-model-pruning)
   - [Strategy & Criteria Configuration](#51-step-1-strategy--criteria-configuration)
   - [Variable Isolation Prevention & Path Locking](#52-variable-isolation-prevention--path-locking)
   - [Live Canvas Trajectory Monitoring](#53-live-canvas-trajectory-monitoring)
   - [Candidate Ranking Catalog & 1-Click Application](#54-step-2-candidate-ranking-catalog--application)
6. [Results Interpretation & Diagnostic Metrics](#6-results-interpretation--diagnostic-metrics)
   - [Goodness-of-Fit Indices](#61-goodness-of-fit-indices)
   - [Approximate Equations](#62-approximate-equations)
   - [Parameter Estimates Table](#63-parameter-estimates-table)
   - [Comprehensive Model Summary](#64-comprehensive-model-summary)
7. [Visualization, Graphics & Report Generation](#7-visualization-graphics--report-generation)
   - [Graphviz Layout Engines](#71-graphviz-layout-engines)
   - [Diagram Visual Conventions](#72-diagram-visual-conventions)
   - [Vector SVG & High-Resolution PNG Export](#73-vector-svg--high-resolution-png-export)
   - [Browser-Rendered A4 PDF Analysis Report](#74-browser-rendered-a4-pdf-analysis-report)
8. [Troubleshooting & Common Lavaan Errors](#8-troubleshooting--common-lavaan-errors)
9. [Methodological Best Practices & Citations](#9-methodological-best-practices--citations)

---

## 1. Introduction & Architecture

**Structura2** is an interactive, browser-based environment for **Structural Equation Modeling (SEM)**, confirmatory factor analysis (CFA), and path analysis built on **R** and the **lavaan** framework.

### Client-Side Execution & Total Data Privacy (WebAssembly / ShinyLive)
Structura2 is compiled and executed locally inside your web browser via **WebAssembly (WebR / ShinyLive)**:
* **Zero-Server Upload**: When you upload your dataset, all statistical computations, matrix inversions, and model estimations occur entirely within your browser's private memory sandbox.
* **Confidentiality Guaranteed**: No row, column, variable name, or parameter estimate is ever transmitted to an external server or cloud service.
* **Offline Capable**: Once loaded, Structura2 functions reliably without an active internet connection.

### Hardware & Browser Compatibility
* **Recommended Browsers**: Chromium-based browsers (Google Chrome, Microsoft Edge, Brave), Mozilla Firefox, or Apple Safari (v16.4+).
* **System Resources**: For datasets with $> 50$ columns or complex models involving simulated annealing optimization, a 64-bit modern browser with at least 4 GB of available RAM is recommended.

---

## 2. Quick-Start Tutorial (5-Minute Walkthrough)

Follow these steps to specify and estimate your first SEM model in under five minutes:

1. **Load a Sample Dataset**:
   - When Structura2 opens, the **Load Data** dialog appears. Select the demo dataset **`HolzingerSwineford1939`** and click outside the modal or proceed.
2. **Review Filtered Variables**:
   - Click the **Filtered** tab. The default **Analysis mode** is set to `Standardized (scaled)`.
   - In **Display columns**, uncheck demographic identifiers (`id`, `sex`, `school`, `grade`) and keep the cognitive test batteries (`x1` through `x9`).
3. **Define the Measurement Model**:
   - Switch to the **Model** tab.
   - In the **Measurement Model** table:
     - Row 1: Set `Latent` to `Visual` and check boxes for `x1`, `x2`, and `x3`.
     - Click **Add Row**. In Row 2, name the construct `Textual` and check `x4`, `x5`, and `x6`.
     - Click **Add Row**. In Row 3, name the construct `Speed` and check `x7`, `x8`, and `x9`.
4. **Specify Structural Regressions**:
   - In the **Structural Model** table, locate row `Speed`. Check `Visual` and `Textual` to evaluate how visual and textual factors predict cognitive speed.
5. **Estimate & Review**:
   - Click the green **Run / Update Model** button.
   - Within seconds, the vector **Path Diagram** renders in the right pane, while global fit measures appear in the **Diagnostics** sub-tab.
6. **Export Findings**:
   - Click **Save SVG** or **Save PNG** to save publication-grade diagrams, or click **Export PDF Report** to produce an A4 summary report ready for distribution.

---

## 3. Data Management & Preprocessing

### 3.1 Data Tab & File Encodings
The **Data** tab displays your uploaded raw data.
* **Universal Encoding Support**: When uploading your own CSV file via **Browse…**, Structura2 utilizes browser-level binary decoding (`TextDecoder`) with automatic encoding detection. It inspects byte patterns and seamlessly decodes:
  - `UTF-8`
  - `Shift-JIS` (Japanese Windows / Excel)
  - `GB18030` (Simplified Chinese)
  - `Big5` (Traditional Chinese)
  - `EUC-KR` (Korean)
  - `Windows-1252` (Western European Latin-1 fallback)
* **Table Exploration**: The table offers global text search, column-level search filters, sortable headers, and pagination. Floating-point numeric columns are formatted to 3 decimal places while integer codes (e.g., subject IDs) retain raw integrity.

### 3.2 Built-In Demo Datasets
When launching or resetting the application, you can explore five benchmark datasets from psychometrics and econometrics:
* **`HolzingerSwineford1939`**: Classic cognitive performance test scores of 301 students across 9 spatial, verbal, and speed tasks. Ideal for 3-factor CFA.
* **`PoliticalDemocracy`**: Bollen’s (1989) longitudinal industrialization and political democracy panel dataset (75 countries measured across 1960 and 1965).
* **`Demo.growth`**: Longitudinal data tracking a latent growth curve model with repeated measures over 4 time points.
* **`Demo.twolevel`**: Clustered data structure for testing two-level hierarchical latent constructs.
* **`FacialBurns`**: Clinical psychological assessment data examining trauma, body image, and adjustment variables.

### 3.3 Analysis Settings (Mode & Missing Data)
Located at the top of the **Filtered** tab, these settings govern the mathematical foundation of model fitting:

#### Analysis Mode
* **Standardized (scaled)** *(Default)*:
  Variables are automatically mean-centered and scaled to unit variance ($z$-scores) prior to estimation. Ideal for path models where direct comparison of effect magnitudes across different measurement units is desired.
* **Raw (unstandardized)**:
  Estimates parameters on the original metric of the variables and enables mean structures ($\sim 1$, intercepts, and latent means). When selected, an additional checkbox appears in the Model tab allowing you to toggle whether standardized coefficients should be displayed on path diagram edges.

#### Missing Data Handling
* **Listwise deletion (`listwise`)** *(Default)*:
  Excludes any observation that contains one or more missing (`NA`) values across analyzed variables. Simple and traditional, but can sacrifice statistical power and induce bias if data are not Missing Completely at Random (MCAR).
* **FIML (`ml`)**:
  Full Information Maximum Likelihood under Missing at Random (MAR) assumptions. Uses all available observed data points per participant without imputing or discarding rows. Automatically activates mean structure estimation.
* **FIML with exogenous covariates (`ml.x`)**:
  FIML estimation that also models the mean and variance of exogenous observed predictors ($x$), allowing cases with missing predictor values to remain in the analysis.
* **Two-stage ML (`two.stage`) & Robust Two-stage (`robust.two.stage`)**:
  Computes saturated model expectations in stage one followed by structured SEM estimation in stage two. The robust variant applies Huber-White sandwich corrections for standard errors under non-normal distributions.

### 3.4 Data Transformation & Selection
* **Log-Transform Columns (log10)**:
  Positive continuous variables exhibiting substantial positive skewness can be log-transformed. Only strictly positive numeric columns ($\min > 0$) are selectable. Transformed variables are automatically prefixed with `log_`.
* **Display Columns Selection**:
  Check or uncheck variables to isolate downstream modeling to relevant measures.
* **Zero-Variance Protection**:
  Variables with zero variance (constants or singular vectors) are automatically identified, excluded from the candidate pool, and summarized in an informative note to protect lavaan from non-invertible covariance matrices.

---

## 4. Model Specification & lavaan Syntax

### 4.1 Measurement Model
The Measurement Model defines how unobserved latent constructs are manifested by observed indicator variables ($= \sim$ operator).
* **Assigning Indicators**: Enter a construct name in the `Latent` column, then check the boxes corresponding to the desired indicator columns.
* **Adding Constructs**: Click **Add Row** to create as many latent factors as required.
* **Real-Time Latent Validation**:
  - Latent variable names must be valid R identifiers (no spaces or illegal punctuation).
  - Latent variable names cannot duplicate observed column names in your dataset.
  - Latent variable names must be unique.
  - If a naming conflict occurs, a clear red alert is displayed, and the **Run / Update Model** button is automatically disabled until resolved.

### 4.2 Structural Model & Correlation Heatmap
The Structural Model defines linear regressions ($\sim$ operator) among dependent variables (rows) and predictor variables (columns).
* **Matrix Interface**: Both observed indicators and defined latent constructs appear in the grid.
* **Bivariate Correlation Heatmap**:
  Cell background colors dynamically reflect the empirical correlation coefficient ($R^2$ strength) between variable pairs:
  - **White / Light Pastel**: Negligible or low linear correlation ($R^2 \approx 0$).
  - **Deeper Red**: Moderate to strong linear association ($R^2 \to 1.0$).
  This visual aid highlights potential regression candidates while warning against pairing variables that share near-perfect collinearity.
* **Diagonal Lockout**: Diagonal cells (self-predictions) are dimmed, locked, and non-editable.

### 4.3 Suggested Paths via Modification Indices
Structura2 incorporates automated **Modification Index (MI)** detection to recommend exploratory paths for model refinement:
* **Threshold**: Identifies unselected structural paths where the expected univariate $\chi^2$ drop exceeds **$3.84$** ($\alpha = 0.05$ critical threshold for $1$ degree of freedom).
* **Blue Border Highlighting**: When enabled via the **Highlight suggested paths** checkbox, potential paths display a prominent blue border.
* **Informative Tooltip**: Hovering your cursor over a highlighted cell displays:
  - **MI**: Expected decrease in model $\chi^2$ test statistic if this path were freely estimated.
  - **std.EPC**: Expected parameter change on the standardized scale.
* **Interactive Addition**: Clicking a suggested checkbox immediately incorporates it into your structural specification.

> **Caution**: Modification indices are purely data-driven. Adding paths solely to boost fit indices can lead to model overfitting and capitalize on sample-specific noise. Always substantiate every addition with plausible domain theory.

### 4.4 Manual Equations (Advanced Syntax)
Directly beneath the structural grid, the **Manual Equations** text box allows you to add custom lavaan formulas that cannot be expressed via checkboxes:
* **Residual Error Covariances**: `y1 ~~ y2` or `x1 ~~ x2`
* **Latent Covariances**: `Factor1 ~~ Factor2`
* **Equality Constraints**: Give parameters identical label prefixes, e.g.:
  ```lavaan
  visual =~ c(a, a)*x1 + x2 + x3
  ```
* **Indirect & Total Effects (`:=`)**:
  ```lavaan
  y ~ b*m
  m ~ a*x
  indirect := a*b
  total := c + (a*b)
  ```

### 4.5 Inspecting Compiled Syntax
The **lavaan Syntax** box provides live, read-only transparency. Every change made across the measurement table, structural matrix, or manual text box is instantly parsed into standardized lavaan code, ensuring total reproducibility.

---

## 5. Automated Model Optimization (Model Pruning)

When formulating an exploratory or complex structural model, theoretical specifications may include extraneous or non-significant links. The **Auto-Optimize Model** engine provides automated, constrained structural path pruning to locate the most parsimonious model that retains excellent empirical fit.

### 5.1 Step 1: Strategy & Criteria Configuration
Clicking the cyan **Auto-Optimize Model** button (active whenever structural paths are defined and fitted) opens the Step 1 configuration dialog:

#### Optimization Criterion
* **AIC (Akaike Information Criterion)**:
  Balancing model fit with a modest penalty for parameter count ($\text{penalty} = 2k$). Best for predictive modeling and balanced complexity.
* **BIC (Bayesian Information Criterion)**:
  Applies a heavier penalty scaling with sample size ($\text{penalty} = k \ln N$). Strongly favors sparse, highly parsimonious specifications.

#### Search Algorithm Strategy
* **Adaptive Auto-Switch** *(Recommended)*:
  Calculates the total combinatorial search space $2^M$, where $M$ is the number of unlocked structural paths. If $2^M \le 1024$ (configurable threshold), it executes an **Exhaustive Search** guaranteeing global optimality. If $2^M > 1024$, it automatically switches to a **Stepwise Search** for instant convergence without browser freezing.
* **Stepwise Search (Fast & Deterministic)**:
  Greedy backward elimination removing the single path at each iteration that yields the largest improvement in the chosen criterion, halting when no further reduction is possible.
* **Exhaustive Search (100% Exact All-Subset)**:
  Evaluates every possible permutation of present/absent paths. Recommended when $M \le 10$.
* **Simulated Annealing (SA - Fast Trajectory Search)**:
  Stochastic meta-heuristic capable of escaping local minima by probabilistically accepting temporary score degradations at higher temperatures ($T$). Hyperparameters (Iterations, Initial Temp, Cooling Rate) can be fine-tuned under *Advanced Algorithm Hyper-Parameters*.

### 5.2 Variable Isolation Prevention & Path Locking
* **Variable Isolation Prevention**:
  Pruning can inadvertently sever all connections to a variable, isolating it from the system.
  - **Dependent Variables**: When checked, the algorithm guarantees the variable retains **at least one incoming path** (in-degree $\ge 1$).
  - **Predictor Variables**: When checked, the algorithm guarantees the variable retains **at least one outgoing path** (out-degree $\ge 1$).
  - Quick **All** / **None** shortcuts enable one-click constraint management.
* **Path Locking Table**:
  An interactive table representing active structural links. Checking a path marks it as **Locked** (pinned), unconditionally excluding it from being pruned.

### 5.3 Live Canvas Trajectory Monitoring
During optimization, Structura2 launches an animated progress modal powered by an HTML5 Canvas line chart:
* **Dashed White Line**: Baseline model score ($AIC_0$ or $BIC_0$).
* **Gray Scatter Points**: Individual candidate evaluations.
* **Blue Solid Line**: Best score trajectory discovered so far.
* **Real-Time Step Counters**: Current step, percentage complete, and score delta ($\Delta$).
* **Stop & View Results**: Click at any time to immediately interrupt exploration and review the best candidate models evaluated up to that point.

### 5.4 Step 2: Candidate Ranking Catalog & Application
Upon completion, the Step 2 dialog presents an organized catalog of model candidates:
* **Candidate Ranking Table**:
  Lists models sorted from best to worst criterion score. Columns include:
  - `Rank` and `Status` (`[Optimal]`, `[Baseline]`, `[Improved]`, or `[Degraded Fit]`).
  - `Retained Paths`: Explicit listing of structural regressions maintained in that model.
  - `AIC`, `BIC`, and respective deltas ($\Delta AIC$, $\Delta BIC$).
  - Post-computed fit measures: **CFI**, **RMSEA**, and **SRMR**.
* **Degraded Fit Warning (`[Degraded Fit]`)**:
  If a candidate model achieves a low AIC/BIC purely through extreme parsimony while causing CFI to fall below $0.90$ or RMSEA/SRMR to exceed $0.08$, Structura2 flags it prominently to prevent adopting an ill-fitting specification.
* **Interactive Path Diagram Preview**:
  Clicking any candidate row or using the `<` and `>` arrow navigation buttons renders an instant vector path diagram preview of that candidate.
* **Apply Selected Model to UI**:
  Clicking this button transfers the candidate's exact structural configuration directly back to your main UI and immediately re-fits the model, updating all diagnostic tables and main path diagrams without manual re-entry.

---

## 6. Results Interpretation & Diagnostic Metrics

### 6.1 Goodness-of-Fit Indices
In the **Model** tab (**Diagnostics** sub-tab), Structura2 computes a comprehensive suite of fit indices. Metrics exceeding recommended thresholds are flagged in **red text**.

| Metric | Full Name | Standard Threshold | Statistical Meaning & Caveats |
| :--- | :--- | :--- | :--- |
| **p-value** | Model $\chi^2$ test $p$-value | $\ge 0.05$ | Tests the null hypothesis of exact fit ($\Sigma(\theta) = \Sigma$). In moderate-to-large samples ($N > 250$), trivial residuals routinely force $p < 0.05$. Never reject a model solely on a significant $\chi^2$. |
| **SRMR** | Standardized Root Mean Square Residual | $\le 0.08$ | Average standardized discrepancy between observed and model-implied covariances. Highly sensitive to misspecified factor covariances. |
| **RMSEA** | Root Mean Square Error of Approximation | $\le 0.06$ (close)<br>$\le 0.08$ (fair) | Parsimony-adjusted index measuring discrepancy per degree of freedom. Accounts for model complexity. Values $> 0.10$ indicate poor fit. |
| **CFI** | Comparative Fit Index | $\ge 0.90$ (acceptable)<br>$\ge 0.95$ (good) | Incremental fit comparing your model to an independence baseline null model. Robust across varying sample sizes. |
| **NFI** | Normed Fit Index | $\ge 0.90$ | Traditional baseline comparison index; may underestimate fit in smaller sample sizes ($N < 150$). |
| **GFI / AGFI** | Goodness-of-Fit / Adjusted GFI | $\ge 0.90$ | Measures proportion of variance/covariance jointly accounted for by the model. AGFI adjusts for degrees of freedom. Vulnerable to sample size shifts. |
| **AIC / BIC** | Information Criteria | Lower is better | Relative comparative measures. Does not indicate absolute fit, but allows ranking across both nested and non-nested alternative models. |

### 6.2 Approximate Equations
The **Equations** sub-tab displays algebraic representations of your measurement loadings and structural regressions with parameter estimates and intercepts:
```text
x1 = 1.000*Visual
x2 = 0.554*Visual + 0.125
Speed = 0.412*Visual + 0.380*Textual
```
*Note: Available when running in **Raw (unstandardized)** mode where intercepts are estimated.*

### 6.3 Parameter Estimates Table
Located in the **Details** tab, this interactive table displays detailed regression weights, factor loadings, variances, and covariances:
* `lhs`, `op`, `rhs`: Parameter equation definition (e.g., `visual =~ x1`).
* `est`: Unstandardized point estimate.
* `se`: Standard error of the estimate.
* `z`: Wald $z$-statistic ($z = \text{est} / \text{se}$).
* `pvalue`: Two-tailed asymptotic significance level ($p < .001$ formatting supported).
* `std.all`: Completely standardized solution (variance of both latent and observed variables standardized to $1.0$). Enabled by checking **Include Standardized (std.all)**.
* **Precision Control**: Use the **Decimals** selector to view `2`, `3`, `4`, or full floating-point precision (`All (Raw)`).
* **Exporting**: Click **Copy** to place the table onto your clipboard or **CSV** to save directly for statistical reports.

### 6.4 Comprehensive Model Summary
The **Model Summary** panel provides complete verbatim text output generated directly by `lavaan::summary()`. It includes optimizer convergence status, number of iterations, log-likelihood values, and degrees of freedom.

---

## 7. Visualization, Graphics & Report Generation

### 7.1 Graphviz Layout Engines
Structura2 renders path diagrams using browser-compiled WebAssembly Graphviz (`@hpcc-js/wasm`), eliminating the need for server-side graph installations. Select layouts from the **Diagram Settings** sub-tab:
* **Hierarchical Left → Right (`dot_LR`)** *(Default)*:
  Standard left-to-right DAG layout. Optimal for typical SEM pipelines (Predictors $\to$ Mediators $\to$ Outcomes).
* **Hierarchical Top → Bottom (`dot_TB`)**:
  Classic vertical flow, ideal for recursive path structures and latent growth curves.
* **Spring Model (`neato`)**:
  Force-directed layout based on Kamada-Kawai energy minimization. Excellent for cyclic graphs or non-hierarchical network structures.
* **Force-Directed Placement (`fdp`)**:
  Spring-electrical model (Fruchterman-Reingold heuristic) handling larger, highly connected path clusters.
* **Circular Layout (`circo`)**:
  Places nodes in a ring structure with internal arcs, highlighting cyclical feedbacks and shared correlations.
* **Radial Layout (`twopi`)**:
  Organizes nodes into concentric rings outward from a central construct.

### 7.2 Diagram Visual Conventions
Rendered diagrams strictly adhere to standard SEM graphical conventions:
* **Node Shapes**:
  - **Rectangles**: Manifest / observed variables.
  - **Ellipses**: Latent factors / unobserved constructs.
* **Edge Styling**:
  - **Single-Headed Arrows ($\to$)**: Directional regression paths or factor loadings.
  - **Double-Headed Curved Arcs ($\leftrightarrow$)**: Covariances or residual error correlations.
* **Color Schemes**:
  - **Blue Arrows**: Positive parameter estimates ($+ \beta$).
  - **Red Arrows**: Negative parameter estimates ($-\beta$).
* **Line Width & Opacity**:
  - Arrow stroke width is proportional to effect magnitude ($|\beta|$).
  - Statistically significant paths ($p < 0.05$) are rendered fully opaque; non-significant paths are rendered with lower opacity.
* **Header Annotations**:
  The top of the canvas displays sample size ($N$), estimation mode (Standardized vs. Raw), key fit metrics ($p$, SRMR, RMSEA, AIC, BIC, CFI), and collinearity diagnostics (Condition Number).

### 7.3 Vector SVG & High-Resolution PNG Export
Buttons located immediately above the path diagram provide instant image downloads:
* **Save SVG**:
  Exports clean vector graphics (`structura2_path_diagram.svg`). Can be scaled infinitely without pixelation, ideal for inclusion in LaTeX documents, Microsoft Word, Adobe Illustrator, or web pages.
* **Save PNG**:
  Exports a crisp, double-resolution ($2\times$ scale, 300 DPI equivalent) bitmap (`structura2_path_diagram.png`) with a clean white background, ready for journal submissions and slide presentations.

### 7.4 Browser-Rendered A4 PDF Analysis Report
Clicking the **Export PDF Report** button on the Model tab synthesizes your entire analysis session into a standardized A4 document:
* **Header**: Project title, timestamp, estimation mode, and missing data handler.
* **Section 1 (Model Fit Summary)**: Complete table of global fit statistics ($N$, $\chi^2$, $df$, $p$, CFI, TLI, RMSEA, SRMR, AIC, BIC).
* **Section 2 (Vector Path Diagram)**: Embedded vector diagram centered on the first page.
* **Section 3 (Parameter Estimates)**: Comprehensive table of all estimated paths, standard errors, $z$-values, and standardized coefficients.
* **Section 4 (lavaan Syntax)**: Complete code listing for full research transparency.
* **Browser Print Integration**: Automatically opens your browser's native print preview dialog with tailored print CSS styles (`@media print`) configured for clean page breaks. Simply choose "Save as PDF" to produce your publication report.

---

## 8. Troubleshooting & Common Lavaan Errors

When estimating structural equation models, mathematical anomalies in empirical covariance matrices can cause estimation warnings or failures. Structura2 catches these issues gracefully:

### 1. "Sample covariance matrix is not positive-definite"
* **Meaning**: The sample covariance matrix cannot be inverted because one or more eigenvalues are zero or negative.
* **Common Causes**:
  - Near-perfect collinearity ($r > 0.95$) between two observed variables.
  - Linear combination dependencies (e.g., including subscale items alongside their computed total score in the same model).
  - Sample size is smaller than the number of observed indicators ($N < p$).
* **Fix**: In the **Filtered** tab, remove redundant variables. Look at the correlation heatmap in the Structural Model to spot dark-red pairs.

### 2. "Model did not converge: Estimation algorithm could not find a stable solution"
* **Meaning**: The numerical optimization routine reached its iteration ceiling before finding a gradient minimum.
* **Common Causes**:
  - Vastly differing variable variances (e.g., mixing income in tens of thousands with age in tens).
  - Problematic starting values in complex reciprocal feedback loops.
* **Fix**: Switch **Analysis mode** to `Standardized (scaled)` in the **Filtered** tab. If running in Raw mode, apply a `log10` transformation to high-magnitude variables.

### 3. "Model identification problem / Insufficient degrees of freedom ($df < 0$)"
* **Meaning**: You are attempting to estimate more parameters (loadings, regressions, variances) than the unique elements available in the covariance matrix ($p(p+1)/2$).
* **Common Causes**:
  - Defining a single-indicator latent factor without fixing its error variance.
  - Saturated or over-parameterized feedback structures.
* **Fix**: Ensure latent factors have at least three indicators, or introduce equality constraints using the **Manual Equations** box.

### 4. "Latent variable names cannot be the same as observed variables"
* **Meaning**: In lavaan syntax, construct names must be distinct from manifest column headers.
* **Fix**: Rename the latent variable in the **Measurement Model** table (e.g., use `F_Math` instead of `Math`).

---

## 9. Methodological Best Practices & Citations

1. **Holistic Model Evaluation**:
   Never accept or reject a structural model based on a single metric. A model with an exemplary RMSEA may still suffer from nonsensical parameter estimates (Heywood cases, negative variances). Always verify that individual parameter signs and magnitudes align with substantive theory.
2. **Confirmatory vs. Exploratory Separation**:
   If you use **Auto-Optimize Model** or **Modification Indices** to refine your model, transparently acknowledge in your research report that modifications were exploratory and data-driven. Best practice recommends cross-validating the pruned structure on an independent holdout dataset.
3. **Missing Data Reporting**:
   Always report the proportion of missing data per variable and state the missingness mechanism assumed (MCAR under listwise deletion; MAR under FIML).

### Suggested Academic Citations
When reporting analyses conducted with Structura2, please cite:

* **Structura2**:
  > Iguchi, T. (2025-2026). *Structura2: Structural Insights, Simplified*. R/Shiny & WebAssembly Application. https://github.com/ToshihiroIguchi/Structura2
* **lavaan Engine**:
  > Rosseel, Y. (2012). lavaan: An R Package for Structural Equation Modeling. *Journal of Statistical Software*, 48(2), 1-36. https://doi.org/10.18637/jss.v048.i02
* **ShinyLive / WebR**:
  > Posit Software, PBC. (2024). *ShinyLive: Run Shiny Applications in the Browser*. https://posit-dev.github.io/r-shinylive/
* **Graphviz WebAssembly Rendering**:
  > HPCC Systems. (2024). *@hpcc-js/wasm: Graphviz WebAssembly library*. https://github.com/hpcc-js/hpcc-js-wasm
