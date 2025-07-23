# Silhouette 0.9.3 (July 2025)

- Removed Citation file from inst/ and added WORDLIST
- Modified description and title in DESCRIPTION file.

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
