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
   - [Saving & Restoring Models](#46-saving--restoring-models)
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
   - [Results ZIP Download](#75-results-zip-download)
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
   - Click the green **Run** button.
   - Within seconds, the vector **Path Diagram** renders in the right pane, while global fit measures appear in the **Diagnostics** sub-tab.
6. **Export Findings**:
   - Click **Save SVG** or **Save PNG** to save publication-grade diagrams, click **PDF** to produce an A4 summary report ready for distribution, or click **ZIP** to get every result file (and the model definition) in one archive.
7. **Keep Your Model**:
   - Every successful fit is autosaved in your browser. After a reload, load the same data again and choose **Restore it** (see [Saving & Restoring Models](#46-saving--restoring-models)).

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
  - If a naming conflict occurs, a clear red alert is displayed, and the **Run** button is automatically disabled until resolved.

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
* **Top N Candidates**: Unselected structural paths are ranked by their expected univariate $\chi^2$ drop (MI), and the top **N** are highlighted (**Number of highlighted paths**, default $5$). No threshold is applied, so judge each candidate by its theoretical plausibility.
* **Ranking and Cycles**: The strongest suggestion (rank #1) gets a thicker border; paths that would close a feedback loop with existing paths are drawn in orange.
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

### 4.6 Saving & Restoring Models
The **Saved Models** button (Model tab, next to **ZIP**) opens a dialog that keeps your model definition so you do not have to rebuild it after a reload:
* **Autosave**: after every *successful* fit, the model is saved automatically in this browser. When you load a dataset with matching columns, Structura2 offers **Restore it**; the link also stays available in the panel.
* **Named models**: type a name and click **Save**; choose a saved model and click **Load** or **Delete**. Up to 20 models are kept.
* **Export JSON / Import JSON**: writes the current model to a `.json` file (and reads one back). Use this as your durable backup and to move a model to another browser or computer.

What is saved: the measurement rows, the active structural paths, the manual equations, and the analysis settings (analysis mode, missing-data handling, log-transform and displayed columns, diagram options, number of highlighted paths). **No data values are ever saved** — only variable names — so the same dataset (or one with the same column names) must be loaded first.

Things to know:
* A restore **does not fit the model**: review the tables, then click **Run**.
* Variables or paths that do not exist in the current data are skipped, and the notification lists them. If the saved model was fitted on a different number of rows (for example, because the Data tab was filtered), you are told as well; the Data-tab row filter itself is not saved.
* Browser storage belongs to the site you are using. It can be wiped by the browser (private windows, "clear site data", Safari's inactivity cleanup), and sites that share a domain (for example several apps on one `github.io` address) also share it. When storage is unavailable the panel says so; **Export JSON** always works.

---

## 5. Automated Model Optimization (Model Pruning)

When formulating an exploratory or complex structural model, theoretical specifications may include extraneous or non-significant links. The **Optimize** engine provides automated, constrained structural path pruning to locate the most parsimonious model that retains excellent empirical fit.

### 5.1 Step 1: Strategy & Criteria Configuration
Clicking the cyan **Optimize** button (active whenever structural paths are defined and fitted) opens the Step 1 configuration dialog:

#### Optimization Criterion
* **AIC (Akaike Information Criterion)**:
  Balancing model fit with a modest penalty for parameter count ($\text{penalty} = 2k$). Best for predictive modeling and balanced complexity.
* **BIC (Bayesian Information Criterion)**:
  Applies a heavier penalty scaling with sample size ($\text{penalty} = k \ln N$). Strongly favors sparse, highly parsimonious specifications.

#### Search Algorithm Strategy
Structura2 provides a robust suite of optimization strategies spanning exact combinatorial search, modern regularized estimation, greedy pruning, and heuristic meta-heuristics:
* **Adaptive Auto-Switch** *(Recommended for General Use)*:
  Calculates the total combinatorial search space $2^M$, where $M$ is the number of unlocked structural paths. If $2^M \le 1024$ (configurable threshold), it executes an **Exhaustive Search** guaranteeing global optimality. If $2^M > 1024$, it automatically switches to a **Stepwise Search** for instant convergence without browser freezing.
* **Regularized SEM (Lasso / Elastic Net - Modern Industry Standard)**:
  Penalized maximum likelihood estimation (`regsem::cv_regsem`) applying $L_1$ (Lasso) or Elastic Net regularization to structural regression coefficients across a grid of shrinkage parameters ($\lambda$). The modern gold standard for continuous shrinkage and sparse model selection in SEM.
* **Stepwise Search (Fast & Deterministic)**:
  Greedy backward elimination removing the single path at each iteration that yields the largest improvement in the chosen criterion (AIC or BIC), halting when no further reduction is possible.
* **Exhaustive Search (100% Exact All-Subset)**:
  Evaluates every possible permutation ($2^M$) of present/absent paths. Recommended when $M \le 10$.
* **Simulated Annealing (SA - Fast Trajectory Search)**:
  Stochastic meta-heuristic capable of escaping local minima by probabilistically accepting temporary score degradations at higher temperatures ($T$). Hyperparameters (Iterations, Initial Temp, Random Seed) can be fine-tuned under *Advanced Algorithm Hyper-Parameters*; the cooling rate is derived from the iteration count and the seed makes runs reproducible.

### 5.2 Variable Isolation Prevention & Path Locking
* **How candidates are estimated**:
  Every candidate is the baseline with some structural paths removed and is estimated with exactly the same lavaan call and default rules as a model you build by hand. The same path diagram therefore always gives the same AIC/BIC, and applying a candidate reproduces the scores shown in the catalogue. Two lavaan defaults matter when a path is removed:
  - **A variable that loses every path** (as predictor and as dependent variable) is silently dropped by lavaan, so its likelihood is computed on different data and AIC/BIC could not be compared with the baseline. Structura2 therefore always keeps at least one path, in either direction, for every observed variable of your structural model.
  - **A removed path can be replaced by a covariance.** If a variable becomes exogenous, lavaan frees its covariances with the other exogenous variables; if two variables both stay dependent, lavaan frees their residual covariance (`m ~~ y`). The association is then still in the model and the parameter count does not drop. Such candidates are labelled `[Replaced]`, listed with the covariances lavaan added (column `Added Cov.`), and never ranked as `[Optimal]`. Consequently, a path between two dependent variables (e.g. `y2 ~ y1` when both have other predictors) cannot be pruned by this tool.
  - **A dependent variable that loses every incoming path** becomes exogenous, so lavaan replaces the paths with covariances and the candidate could never be ranked as optimal. Structura2 therefore always keeps **at least one incoming path** (in-degree $\ge 1$) for every dependent variable of your structural model.
* **Variable Isolation Prevention** (additional, optional constraint):
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
  - `Rank` and `Status` (`[Optimal]`, `[Baseline]`, `[Improved]`, `[Equivalent]`, `[Degraded Fit]`, `[Replaced]`, or `[Variable Dropped]`).
  - `Retained Paths`: Explicit listing of structural regressions maintained in that model.
  - `AIC`, `BIC`, and respective deltas ($\Delta AIC$, $\Delta BIC$), plus `Added Cov.` (covariances lavaan freed in place of a removed path).
  - Post-computed fit measures: **CFI**, **RMSEA**, and **SRMR**.
* **Degraded Fit Warning (`[Degraded Fit]`)**:
  If a candidate model achieves a low AIC/BIC purely through extreme parsimony while causing CFI to fall below $0.90$ or RMSEA/SRMR to exceed $0.08$, Structura2 flags it prominently to prevent adopting an ill-fitting specification.
* **Interactive Path Diagram Preview**:
  Clicking any candidate row or using the `<` and `>` arrow navigation buttons renders an instant vector path diagram preview of that candidate.
* **Apply Selected Model to UI**:
  Clicking this button transfers the candidate's exact structural configuration directly back to your main UI and immediately re-fits the model, updating all diagnostic tables and main path diagrams without manual re-entry.
  After the refit, Structura2 compares the refitted AIC/BIC with the catalogue values and warns if they differ. Applying a `[Replaced]` candidate shows a warning because the removed association is still part of the model.

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
* **Exporting**: Click **Copy** to place the table onto your clipboard or **CSV** to save directly for statistical reports. Both buttons export **every** row of the table (not only the visible page) and follow the current decimals setting; for full-precision values use **ZIP** (see [7.5](#75-results-zip-download)).

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
Clicking the **PDF** button on the Model tab synthesizes your entire analysis session into a standardized A4 document:
* **Header**: Project title, timestamp, estimation mode, and missing data handler.
* **Section 1 (Model Fit Summary)**: Complete table of global fit statistics ($N$, $\chi^2$, $df$, $p$, CFI, TLI, RMSEA, SRMR, AIC, BIC).
* **Section 2 (Vector Path Diagram)**: Embedded vector diagram centered on the first page.
* **Section 3 (Parameter Estimates)**: Comprehensive table of all estimated paths, standard errors, $z$-values, and standardized coefficients.
* **Section 4 (lavaan Syntax)**: Complete code listing for full research transparency.
* **Browser Print Integration**: Automatically opens your browser's native print preview dialog with tailored print CSS styles (`@media print`) configured for clean page breaks. Simply choose "Save as PDF" to produce your publication report.

### 7.5 Results ZIP Download
Clicking **ZIP** on the Model tab (after a successful fit) saves `structura2_results_<date>_<time>.zip` containing:

| File | Content |
|------|---------|
| `parameter_estimates.csv` | All parameter estimates at full precision, including `std.all` |
| `fit_measures.csv` | Every fit measure reported by lavaan |
| `variable_summary.csv` | Valid / missing counts, mean, SD, skewness, kurtosis |
| `reliability.csv` | Cronbach's $\alpha$, CR, AVE per latent construct (only if the model has latent variables) |
| `model_syntax.txt` | The lavaan syntax that was fitted |
| `model.json` | The model definition; import it via **Saved Models → Import JSON** to restore the model |
| `lavaan_summary.txt` | Verbatim `lavaan::summary()` output |
| `path_diagram.svg` / `.png` | The path diagram as currently displayed |
| `README.txt` | Data name, settings and lavaan version |

The files describe the model **as it was fitted** (data and settings captured when you pressed *Run*), even if you changed settings afterwards. CSV files are UTF-8 with a byte-order mark, so Excel displays non-ASCII (e.g. Japanese) variable names correctly.

---

## 8. Troubleshooting & Common Lavaan Errors

When estimating structural equation models, mathematical anomalies in empirical covariance matrices can cause estimation warnings or failures. Structura2 catches these issues gracefully:

### 1. "These variables are exactly linearly dependent: ..."
* **Meaning**: One variable is an exact copy or an exact sum of others, so the covariance matrix cannot be inverted. Structura2 names the variables involved before estimation starts.
* **Common Causes**:
  - A duplicated column, or a total score included together with the items it sums.
* **Fix**: Remove one of the listed variables in **Filtered > Display columns**.

### 2. "Too few rows: the model uses p variables but only N complete rows are available"
* **Meaning**: Fewer complete rows than variables ($N < p$), so the covariance matrix is singular.
* **Fix**: Use more data, remove variables from the model, or choose a missing-data method such as FIML.

### 3. "The model is not identified (df < 0)"
* **Meaning**: The model estimates more parameters than the $p(p+1)/2$ distinct variances and covariances in the data. Structura2 reports the shortfall and lists likely candidates (residual covariances, factors with few indicators).
* **Common Causes**:
  - A factor with only one or two indicators and no other factor to anchor it.
  - Too many residual covariances (`~~`) or feedback loops.
* **Fix**: Remove at least the reported number of free parameters, or add indicators. If standard errors cannot be computed even though $df \ge 0$, the model is also not identified; simplify it the same way.

### 4. "Results are shown but may be unreliable"
* **Meaning**: The model was estimated, but the solution should be treated with caution.
* **Typical messages**:
  - *Negative variance (Heywood case)*: usually too few indicators per factor or a small sample.
  - *Factors are indistinguishable*: two factors correlate at about 1; merge them into one factor.
  - *Highly correlated predictors* ($|r| \ge 0.95$): coefficients and standard errors become unstable. Combine the variables as indicators of one latent factor, or keep only one.
* **Note**: High correlation alone is not an error. Indicators of the same factor are expected to correlate.

### 5. "Model did not converge: Estimation algorithm could not find a stable solution"
* **Meaning**: The numerical optimization routine reached its iteration ceiling before finding a gradient minimum.
* **Common Causes**:
  - Vastly differing variable variances (e.g., mixing income in tens of thousands with age in tens).
  - Problematic starting values in complex reciprocal feedback loops.
* **Fix**: Switch **Analysis mode** to `Standardized (scaled)` in the **Filtered** tab. If running in Raw mode, apply a `log10` transformation to high-magnitude variables.

### 6. "Latent variable names cannot be the same as observed variables"
* **Meaning**: In lavaan syntax, construct names must be distinct from manifest column headers.
* **Fix**: Rename the latent variable in the **Measurement Model** table (e.g., use `F_Math` instead of `Math`).

---

## 9. Methodological Best Practices & Citations

1. **Holistic Model Evaluation**:
   Never accept or reject a structural model based on a single metric. A model with an exemplary RMSEA may still suffer from nonsensical parameter estimates (Heywood cases, negative variances). Always verify that individual parameter signs and magnitudes align with substantive theory.
2. **Confirmatory vs. Exploratory Separation**:
   If you use **Optimize** or **Modification Indices** to refine your model, transparently acknowledge in your research report that modifications were exploratory and data-driven. Best practice recommends cross-validating the pruned structure on an independent holdout dataset.
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
