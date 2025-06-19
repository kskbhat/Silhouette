Silhouette: Tools for Silhouette Cluster Diagnostics
The Silhouette package provides tools for silhouette-based diagnostics in clustering, including standard, soft, and multi-way (e.g., biclustering, triclustering) clustering. It computes silhouette widths to evaluate cluster quality, supports both crisp and fuzzy clustering, and includes visualization functions to assess cohesion and separation. Ideal for researchers and data scientists analyzing clustering results.
Installation
Install from CRAN (once accepted):
install.packages("Silhouette")

Or install the development version from GitHub:
# Install devtools if needed
if (!requireNamespace("devtools", quietly = TRUE)) {
  install.packages("devtools")
}
devtools::install_github("yourusername/Silhouette")

Usage
library(Silhouette)

# Example with synthetic probability matrix for soft clustering
set.seed(123)
prob_matrix <- matrix(runif(150 * 3), nrow = 150, ncol = 3)
prob_matrix <- prob_matrix / rowSums(prob_matrix)
out <- softSilhouette(prob_matrix = prob_matrix)
plot(out)

Features

Compute silhouette widths for standard (Silhouette), soft (softSilhouette), and multi-way clustering (extSilhouette).
Support for crisp and fuzzy clustering with methods like pac and medoid.
Visualize cluster quality with customizable silhouette plots (plot.Silhouette).
Compatible with clustering functions like ppclust::fcm and blockcluster::coclusterContinuous.

Release Notes
Version 0.9.2 (2025-06-19) - Resubmission to CRAN

Bugfixes:
Reduced example runtime in Silhouette function to under 10 seconds for CRAN compliance.
Corrected LICENSE file naming (renamed from LICENSE.md to LICENSE) and removed from .Rbuildignore.


Improvements:
Added conditional checks for suggested packages (ppclust, blockcluster) to ensure robust examples.
Enhanced documentation with clearer Rd files and additional examples.


Notes:
Addressed CRAN feedback from initial submission of version 0.9.1.
No vignettes added yet; planned for future releases.



Version 0.9.1 (2025-05-01) - Initial CRAN Submission

Initial release with core functions: Silhouette, softSilhouette, extSilhouette, and plot.Silhouette.
Support for silhouette diagnostics in standard, soft, and multi-way clustering.
Comprehensive documentation and examples using the iris dataset.
Dependencies: dplyr, ggplot2, ggpubr, methods.
Suggested packages: ppclust, blockcluster, knitr, rmarkdown, testthat.

Contributing
Contributions are welcome! Please submit issues or pull requests on the GitHub repository.
License
This package is licensed under the Apache License (>= 2.0). See the LICENSE file for details.
Contact
Maintainer: Shrikrishna Bhat K (skbhat.in@gmail.com)
