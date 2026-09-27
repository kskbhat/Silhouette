# Silhouette 0.9.7

**Scoring a given partition**

- `Silhouette()`, `softSilhouette()`, `cerSilhouette()`, `dbSilhouette()` and `calSilhouette()`
  gain a `cluster` argument for scoring a partition produced elsewhere (for example, by
  hierarchical clustering). When `clust_fun` is used, `cluster` may name the label component of
  its output (e.g. `cluster = "cluster"` for `ppclust::fcm()`). The argument follows `...`, so
  it must be named.
- Labels now follow one rule: `cluster` if supplied, otherwise the largest membership in
  `prob_matrix`, otherwise the closest cluster in `prox_matrix`. The exception is the probability
  density silhouette (PDS, `prob_type = "pd"`), which assigns each observation to its largest
  density, as in its definition (Bhat Kapu and Kiruthika, 2024). The neighboring cluster is the
  closest other cluster in the proximity matrix used.

**Behavior changes**

- `calSilhouette()` scores the same partition in every row except the PDS rows (see above).
  Previously its crisp and median distance-based rows ignored `prob_matrix` and used
  nearest-center labels.
- `softSilhouette(prob_type = "pd")` assigns each observation to its largest density for every
  average, so the PDS lies in [0, 1] as defined. Previously fuzzy averaging used the largest
  membership instead, which gave negative widths.
- The exponent `a` of the fuzzy weights may now be 0 (every weight is then 1 and the fuzzy average
  equals the crisp average), as in Bhat Kapu and Kiruthika (2024); negative values are rejected.
- In `Silhouette()` with `prob_matrix`, the neighbor is now the closest other cluster in
  `prox_matrix` (it was the second largest membership).
- The fuzzy weights are now the gap between the two largest memberships, as defined by Campello
  and Hruschka (2006), for every function; they no longer depend on the labels.
- `extSilhouette()` no longer rounds `ext_sil_width` and `avg_sil_width` to four decimals; the new
  print method shows four decimals.

**Numerical robustness**

- `dbSilhouette()`: a zero neighboring membership (for example, an underflowed posterior
  probability) no longer makes every other width zero. Such observations get width 1, the other
  widths are scaled by the largest finite log ratio, and a warning reports how many there were.
  When the largest log ratio is below `sqrt(.Machine$double.eps)`, the widths are set to 0 with a
  warning instead of magnifying rounding error. Equal memberships give width 0.
- `softSilhouette(prob_type = "nlpp")`: zero probabilities (infinite `-log`) no longer give `NaN`
  widths or a neighbor equal to the assigned cluster.
- All width functions: two equal proximities, including two zero distances, give width 0 rather
  than `NaN`; infinite proximities give widths of 1 or -1.
- Input checks are shared by all functions: matrices must be numeric with at least two columns and
  no `NA`/`NaN` (`+Inf` is allowed in `prox_matrix`, `-Inf` is not); negative proximities are
  rejected (they would give widths outside [-1, 1]); membership rows must be non-negative and sum to 1 within `sqrt(.Machine$double.eps)`;
  `prob_matrix` must have the same dimensions as `prox_matrix`; `cluster` must hold column numbers.
- A fuzzy average whose weights are all zero now gives a warning (the average is `NaN`).

**Other improvements**

- `extSilhouette()` gains a `weights` argument: `"size"` (default, as before), `"equal"`, or a
  numeric vector of user weights. `dim_table` gains a `weight` column with each mode's share, and
  the result has `print()` and `format()` methods.
- Summaries requested with `print.summary = TRUE` (and `summary()`) are reported with `message()`
  only, so they can be suppressed with `suppressMessages()`.
- `cerSilhouette(print.summary = TRUE)` reported its method as "db"; it now reports "certainty".
- `plotSilhouette()` labeled the legend incorrectly when a cluster was empty.
- Error messages about `prob_matrix` no longer refer to `prob_type = 'pd'` when it was not used.
- New argument `clust_args` (a list) in every width function and `calSilhouette()`. Arguments
  passed through `...` are matched partially against the function's own arguments first, so
  `m = 3` meant for the fuzzifier of `ppclust::fcm()` was taken as `method`; pass such arguments
  as `clust_args = list(m = 3)`. `clust_fun` may also be a string of the form `"pkg::fun"`.
- `plotSilhouette(palette = )` keeps the summary legend (the palette used to replace it), a
  numeric `linetype` of 0 is accepted as documented, and objects from **cluster** or
  **factoextra** without silhouette information give a clear error.
- Negative proximities are now an error: they gave widths outside [-1, 1], which `summary()`
  and `plot()` then rejected with a misleading message. That message now points to
  `is.Silhouette(strict = TRUE)`.
- `extSilhouette()` warns when the modes differ in proximity type, method or averaging, and
  checks for empty objects before computing their averages.
- The line listing the object's attributes after `print.summary = TRUE` is gone.
- **dplyr** and **lifecycle** are no longer imported; `inst/CITATION` takes the version from
  the package metadata.

**Documentation**

- The help pages describe what each width measures, the labeling rule, tie handling (the first
  column wins), the row-sum tolerance, when widths can be negative, and why the `"medoid"`
  normalization keeps its name although it applies equally to centroids.
- `?calSilhouette` explains which rows are monotone transformations of each other, which coincide
  in special cases, and which are genuinely different, and lists its actual return columns.
- Corrected the fuzzy weight formula and the description of `nlpp` (non-negative, not
  non-positive).
- Roxygen markdown is enabled, so bold text and lists in the help pages render properly.
- `?plotSilhouette` cites `factoextra::fviz_silhouette()`, whose design the plot follows.
- The help pages use the names of Bhat Kapu and Kiruthika (2024): PACS, PPS, NLPPS and PDS
  (`softSilhouette()`), CeS (`cerSilhouette()`) and DBS (`dbSilhouette()`), and `?softSilhouette`
  gives the PDS in terms of component densities (for a mixture, gamma/pi-hat = f_k/f).
- The help pages describe how the number of clusters is read from these widths: the reverse
  elbow drop of the average for the probability-based widths and local maxima for the DBS, with
  the result depending on the algorithm, the width and the averaging.
- New tests cover zero distances and probabilities, ties, K = 2, singleton and empty clusters,
  `NA`/`Inf` input, similarity versus dissimilarity input, supplied labels, objects from other
  packages, and the exact identities between widths (PAC and medoid; fuzzy c-means memberships and
  distances; squared distances).

**Performance Optimizations**

- Fully vectorized core silhouette computation algorithms (`Silhouette`, `dbSilhouette`, `cerSilhouette`).
- Replaced slow row-wise `apply()` and `order()` sorting routines with highly optimized, C-implemented `max.col()` and matrix-based masking.
- Replaced row-wise `for` loops for calculating silhouette widths and weights with vectorized matrix-index extraction and element-wise arithmetic, yielding dramatic speedups for large datasets.

**Documentation**

- Replaced the single long vignette with four shorter ones:
  `vignette("Silhouette")` (getting started),
  `vignette("soft-clustering")` (widths from membership and posterior probabilities),
  `vignette("interpreting-widths")` (relation to the classical silhouette, ranges and valid comparisons) and
  `vignette("multiway")` (extended silhouette for block clustering, with a worked double k-means example).
- Added three articles to the package website: silhouette plots for results from this and other packages,
  a case study comparing soft clusterings of thyroid patients, and a comparison with other packages that
  includes measured timings and memory use. The scripts that produce the timings are in `data-raw/benchmarks`.
- The vignettes now state that the number of clusters is chosen where the silhouette average is largest,
  that the rows of `calSilhouette()` are on different scales and should not be compared with each other,
  and that simplified averages are not comparable with classical ones or with thresholds published for them.
- Vignettes no longer attach **proxy** (which masks `stats::dist()`), and they skip their code with a
  message when a suggested package is not installed.
- Removed the prebuilt vignette files from `inst/doc`.

# Silhouette 0.9.6 (October 2025)

**New Features**

- Introduced new functions: 

- **`getSilhouette()`**  
  Constructs a Silhouette class object directly from user-provided components without performing silhouette calculations. Allows users to build Silhouette objects when they already have cluster assignments, neighbor clusters, silhouette widths, and optional weights.

- **`is.Silhouette()`**  
  Tests whether an object is of class "Silhouette" with optional strict validation. Checks both class inheritance and expected structure including required columns, attributes, and value ranges.

- **`cerSilhouette()`**  
  Computes certainty silhouette widths for soft clustering using maximum posterior probabilities. Provides an alternative evaluation method for fuzzy clustering algorithms with support for crisp, fuzzy, and median averaging.

- **`dbSilhouette()`**  
  Computes density-based silhouette widths using log-ratios of posterior probabilities based on Menardi (2011) method. Offers normalized silhouette evaluation for soft clustering with enhanced discrimination between cluster assignments.

- **`calSilhouette()`**  
  Computes all available silhouette indices from the package functions and returns a comparative summary data frame. Automatically calculates crisp, fuzzy, and median silhouette values across different methods including proximity-based (medoid, pac), soft silhouette variations (pp_pac, pp_medoid, nlpp_pac, nlpp_medoid, pd_pac, pd_medoid), and probability-based methods (cer, db). Supports direct matrix input or clustering function output for streamlined comparative analysis.

**Improvements**

- Extended existing functions to support **median-based silhouette calculations** as an alternative to mean-based computations.
- Added new `average` attribute for existing Silhouette class supporting "crisp", "fuzzy", and "median" options.
- Enhanced clustering evaluation capabilities with density-based and certainty-based statistical measures.
- Updated documentation to reflect new median calculation options and soft clustering methods.
- Enhanced `calSilhouette()` to compute all combinations of `softSilhouette()` methods (pp, nlpp, pd) with both pac and medoid approaches, providing comprehensive comparative analysis across 11 different silhouette computation methods.

## Updated Dependencies

- **Imports**: `dplyr`, `ggplot2`, `ggpubr`, `lifecycle`, `methods`, `stats`  
- **Suggests**: `proxy`, `ppclust`, `blockcluster`, `cluster`, `factoextra`, `drclust`

# Silhouette 0.9.5 (August 2025)

- Improved documentation:
  - Updated `README.md` and `DESCRIPTION` fields.
  - Fixed an example in the vignettes for clarity.
- Added **lifecycle badges** to all exported functions.

# Silhouette 0.9.4 (July 2025)

**CRAN Suggested Corrections**

- Clarified the description of the **PAC (Probability of Alternative Cluster)** method in the documentation by citing the original authors (Author, Year).
- Removed usage of `installed.packages()` from the package code and replaced it with a more robust and CRAN-compliant approach, following suggested practices.
- All user-facing functions now include the argument `print.summary`, which controls whether summary details are printed. This argument behaves similarly to `if (verbose) cat(...)` and now defaults to `FALSE`.
- Messages using `message()` are included in example sections to **suggest installation of optional packages** (e.g., `ppclust`, `blockcluster`). These packages are **not called directly**, but their outputs are used in examples. This is done to guide users without violating CRAN's policy on requiring packages in examples.
- Removed the license file as suggested.

# Silhouette 0.9.3 (July 2025)

- Removed *Citation* file from **inst** folder and added *WORDLIST* in it.
- Modified Description and Title in *DESCRIPTION* file.

# Silhouette 0.9.2 (July 2025)

**Initial Stable Release**

The first stable release of the **Silhouette** package, offering tools to compute and visualize silhouette widths for evaluating clustering quality. It supports both **crisp** and **soft** clustering outputs, along with **multi-way** clustering (e.g., biclustering).

## Key Features

- **`Silhouette()`**  
  Computes silhouette widths using proximity matrices (dissimilarity or similarity). Supports `"medoid"` and `"pac"` normalization methods, and fuzzy silhouette calculation via membership probabilities.

- **`softSilhouette()`**  
  Tailored for soft clustering. Transforms membership probabilities into proximity measures (posterior probabilities, negative log-probabilities, or raw distributions) before silhouette computation.

- **`extSilhouette()`**  
  Extends silhouette analysis to multi-way clustering (e.g., biclustering or triclustering) by aggregating silhouette widths across modes, weighted by observation counts.

- **`plotSilhouette()`**  
  Creates detailed silhouette plots using **ggplot2**, with options for grayscale themes, cluster-wise summaries, and labeled observations. Compatible with outputs from `Silhouette`, `softSilhouette`, and popular packages like **cluster** and **factoextra**.

- **`summary.Silhouette()`**  
  Summarizes silhouette results, including average silhouette widths per cluster and overall summary statistics.

## Dependencies

- **Imports**: `dplyr`, `ggplot2`, `ggpubr`, `methods`  
- **Suggests**: `proxy`, `ppclust`, `blockcluster`, `cluster`, `factoextra`
