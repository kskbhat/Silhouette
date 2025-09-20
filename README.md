# Silhouette <a href="https://kskbhat.github.io/Silhouette/"><img src="man/figures/logo.png" alt="Silhouette website" align="right" height="139"/></a>

<!-- badges: start -->
[![CRAN Status (ago)](https://www.r-pkg.org/badges/version/Silhouette)](https://cran.r-project.org/package=Silhouette)[![codecov](https://codecov.io/gh/kskbhat/Silhouette/branch/main/graph/badge.svg)](https://codecov.io/gh/kskbhat/Silhouette)

[![GitHub](https://img.shields.io/github/v/release/kskbhat/Silhouette?include_prereleases&label=GitHub%20Release)](https://github.com/kskbhat/Silhouette/releases)
[![R-CMD-check](https://github.com/kskbhat/Silhouette/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/kskbhat/Silhouette/actions/workflows/R-CMD-check.yaml) [![pages-build-deployment](https://github.com/kskbhat/Silhouette/actions/workflows/pages/pages-build-deployment/badge.svg)](https://github.com/kskbhat/Silhouette/actions/workflows/pages/pages-build-deployment)
<!--
[![CRAN downloads](https://cranlogs.r-pkg.org/badges/grand-total/Silhouette?color=blue)](https://r-pkg.org/pkg/Silhouette)[![CRAN monthly downloads](https://cranlogs.r-pkg.org/badges/last-month/Silhouette?color=green)](https://kskbhat.r-universe.dev/Silhouette)[![CRAN Task View: Cluster Analysis & Finite Mixture Models](https://img.shields.io/badge/CRAN%20Task%20View-Cluster-blue)](https://CRAN.R-project.org/view=Cluster)
[![DOI](https://img.shields.io/badge/DOI-10.32614%2FCRAN.package.Silhouette-blue.svg)](https://doi.org/10.32614/CRAN.package.Silhouette)
[![CRAN Status](https://www.r-pkg.org/badges/version/Silhouette)](https://CRAN.R-project.org/package=Silhouette)
[![CRAN Downloads](https://cranlogs.r-pkg.org/badges/grand-total/Silhouette)](https://cran.r-project.org/package=Silhouette)

[![R-hub](https://github.com/kskbhat/Silhouette/actions/workflows/rhub.yaml/badge.svg)](https://github.com/kskbhat/Silhouette/actions/workflows/rhub.yaml)
![CRAN downloads per day](https://cranlogs.r-pkg.org/badges/last-day/Silhouette)
-->

<!-- badges: end -->

An [R](https://www.r-project.org/) package for silhouette-based diagnostics in standard, soft, and multi-way clustering.

Quantifies clustering quality by measuring both **cohesion within clusters** and **separation between clusters**. Implements advanced silhouette width computations for diverse clustering structures, including: simplified silhouette by [Van der Laan et al. (2003)](https://doi.org/10.1080/0094965031000136012), Probability of Alternative Cluster normalization methods by [Raymaekers & Rousseeuw (2022)](https://doi.org/10.1080/10618600.2022.2050249), fuzzy clustering and silhouette diagnostics using membership probabilities by [Campello & Hruschka (2006)](https://doi.org/10.1016/j.fss.2006.07.006), [Menardi (20011)](https://doi.org/10.1007/s11222-010-9169-0) and [Bhat & Kiruthika (2024)](https://doi.org/10.1080/23737484.2024.2408534), and multi-way clustering extensions such as block and tensor clustering by [Schepers et al. (2008)](https://doi.org/10.1007/s00357-008-9005-9) and [Bhat & Kiruthika (2025)](https://doi.org/10.21203/rs.3.rs-6973596/v1). Provides tools for computation and visualization based on [Rousseeuw (1987)](https://doi.org/10.1016/0377-0427(87)90125-7) to support robust and reproducible cluster diagnostics across standard, soft, and multi-way clustering settings.

**Note:** This package does not use the classical [Rousseeuw (1987)](https://doi.org/10.1016/0377-0427(87)90125-7) calculation directly.

------------------------------------------------------------------------

## ✅ Why This Package?

-   **Unified & consistent**: Offers one coherent interface for crisp, soft, and multi-way clustering silhouette diagnostics.
-   **Flexible**: Works with distance matrices, clustering outputs, or soft membership probabilities.
-   **Advanced**: Implements newer normalization methods (PAC, db), handles soft clustering, and supports mode-wise silhouette aggregation.
-   **Visualization**: Plot functions produce clear, customizable silhouette plots compatible with many clustering outputs and existing silhouette outputs from [`factoextra`](https://doi.org/10.32614/CRAN.package.factoextra), [`cluster`](https://doi.org/10.32614/CRAN.package.cluster) and  [`drclust`](https://doi.org/10.32614/CRAN.package.drclust) R packages.
-   **Comparability**: Summaries and plots make it easy to compare clustering algorithms or tune the number of clusters.
-   **Interoperable**: All `Silhouette` class functions works with any clustering output that provides a proximity or membership probability matrix. Users can also supply a proximity matrix and a clustering function—*including S3 or S4 methods*—to let `Silhouette` class perform clustering and compute silhouettes internally in one step.

------------------------------------------------------------------------

## Installation

You can install the released version of `Silhouette` from GitHub using:

``` r
# Install devtools if needed
if (!requireNamespace("devtools", quietly = TRUE)) {
  install.packages("devtools")
}

# Install from GitHub
devtools::install_github("kskbhat/Silhouette")
```

From CRAN, install via:

``` r
install.packages("Silhouette")
```

------------------------------------------------------------------------

## Usage

Usage of the main functions is demonstrated in the package examples and documentation.

For an intro, see the vignette `Silhouette`, which is available as

``` r
vignette("Silhouette")
```

You can access the vignette from the [Get started](https://kskbhat.github.io/Silhouette/articles/Silhouette.html) tab in the top navigation bar of the package's [website](https://kskbhat.github.io/Silhouette/).

------------------------------------------------------------------------

## Report a Bug or Request a Feature

If you encounter a bug or have an idea for a new feature in the **Silhouette** package, please let us know by opening an issue on GitHub:

- [Create an issue on GitHub](https://github.com/kskbhat/Silhouette/issues)
- For bugs: include a minimal reproducible example, describe the expected vs. actual behavior, and mention your R and package versions
- For feature requests: clearly describe the proposed feature, its purpose, and how it would benefit users

Your feedback and suggestions are valuable and help improve the package.
