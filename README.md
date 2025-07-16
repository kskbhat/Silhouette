<!-- badges: start -->
[![R-CMD-check](https://github.com/kskbhat/Silhouette/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/kskbhat/Silhouette/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->


# Silhouette: Tools for Silhouette Cluster Diagnostics

The **Silhouette** package provides tools for silhouette-based diagnostics in clustering, including standard, soft, and multi-way (e.g., biclustering, triclustering) clustering. It computes silhouette widths to evaluate cluster quality, supports both crisp and fuzzy clustering, and includes visualisation functions to assess cohesion and separation. This package is ideal for researchers and data scientists analysing clustering results.

---

## Installation

**From CRAN** (once accepted):

```r
install.packages("Silhouette")
```

**Development version from GitHub**:

```r
# Install devtools if not already installed
if (!requireNamespace("devtools", quietly = TRUE)) {
  install.packages("devtools")
}

# Install Silhouette from GitHub
devtools::install_github("kskbhat/Silhouette")
```

---

## Usage

```r
library(Silhouette)

help(package = "Silhouette")
```

---

## Features

* Compute silhouette widths for:
  * Standard clustering (`Silhouette`)
  * Soft clustering (`softSilhouette`)
  * Multi-way clustering (`extSilhouette`)
* Support for **crisp** and **fuzzy** clustering with methods like `pac` and `medoid`
* Visualise cluster quality with customizable silhouette plots (`plot.Silhouette`)
* Compatible with any clustering functions that return distance or membership matrices.
---

## Release Notes

### Version 0.9.2 (2025-06-19) – Resubmission to CRAN

**Bugfixes:**

* Reduced example runtime in `Silhouette` function to under 10 seconds (for CRAN compliance)
* Renamed `LICENSE.md` to `LICENSE` and removed it from `.Rbuildignore`

**Improvements:**

* Added conditional checks for suggested packages (`ppclust`, `blockcluster`) to ensure robust examples
* Enhanced documentation with clearer `.Rd` files and additional examples

**Notes:**

* Addressed CRAN feedback from initial submission of version 0.9.1
* No vignettes included yet; planned for future release

### Version 0.9.1 (2025-05-01) – Initial CRAN Submission

* Initial release with core functions:
  * `Silhouette`, `softSilhouette`, `extSilhouette`, and `plot.Silhouette`
* Support for silhouette diagnostics in standard, soft, and multi-way clustering
* Comprehensive documentation and examples using the **iris** dataset
* **Dependencies:** `dplyr`, `ggplot2`, `ggpubr`, `methods`
* **Suggested packages:** `ppclust`, `blockcluster`, `knitr`, `rmarkdown`, `testthat`

---

## Contributing

Contributions are welcome! Please submit issues or pull requests on the [GitHub repository](https://github.com/kskbhat/Silhouette).

---

## License

This package is licensed under the **Apache License (≥ 2.0)**. See the [LICENSE](LICENSE) file for details.

---

## Contact

**Maintainer:** Shrikrishna Bhat K  
📧 Email: [skbhat.in@gmail.com](mailto:skbhat.in@gmail.com)
