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
