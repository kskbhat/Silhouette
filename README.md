# Silhouette <a href="https://kskbhat.github.io/Silhouette/"><img src="man/figures/logo.png" alt="Silhouette website" align="right" height="139"/></a>

<!-- badges: start -->

[![R-CMD-check](https://github.com/kskbhat/Silhouette/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/kskbhat/Silhouette/actions/workflows/R-CMD-check.yaml) [![Lifecycle: stable](https://img.shields.io/badge/lifecycle-stable-brightgreen.svg)](https://lifecycle.r-lib.org/articles/stages.html) <!--
[![CRAN Status](https://www.r-pkg.org/badges/version/Silhouette)](https://CRAN.R-project.org/package=Silhouette)
[![CRAN Downloads](https://cranlogs.r-pkg.org/badges/grand-total/Silhouette)](https://cran.r-project.org/package=Silhouette)
[![DOI](https://img.shields.io/badge/DOI-10.32614%2FCRAN.package.Silhouette-blue.svg)](https://doi.org/10.32614/CRAN.package.Silhouette)
[![R-hub](https://github.com/kskbhat/Silhouette/actions/workflows/rhub.yaml/badge.svg)](https://github.com/kskbhat/Silhouette/actions/workflows/rhub.yaml)
[![pages-build-deployment](https://github.com/kskbhat/Silhouette/actions/workflows/pages/pages-build-deployment/badge.svg)](https://github.com/kskbhat/Silhouette/actions/workflows/pages/pages-build-deployment)
-->

<!-- badges: end -->

An [R](https://www.r-project.org/) package for silhouette-based diagnostics in standard, soft, and multi-way clustering.

Quantifies clustering quality by measuring both **cohesion within clusters** and **separation between clusters**. Implements advanced silhouette width computations for diverse clustering structures, including: simplified silhouette by [Van der Laan et al. (2003)](https://doi.org/10.1080/0094965031000136012), Probability of Alternative Cluster normalization methods by [Raymaekers & Rousseeuw (2022)](https://doi.org/10.1080/10618600.2022.2050249), fuzzy clustering and silhouette diagnostics using membership probabilities by [Campello & Hruschka (2006)](https://doi.org/10.1016/j.fss.2006.07.006) and [Bhat & Kiruthika (2024)](https://doi.org/10.1080/23737484.2024.2408534), and multi-way clustering extensions such as block and tensor clustering by [Schepers et al. (2008)](https://doi.org/10.1007/s00357-008-9005-9) and [Bhat & Kiruthika (2025)](https://doi.org/10.21203/rs.3.rs-6973596/v1). Provides tools for computation and visualization based on [Rousseeuw (1987)](https://doi.org/10.1016/0377-0427(87)90125-7) to support robust and reproducible cluster diagnostics across standard, soft, and multi-way clustering settings.

**Note:** This package does not use the classical [Rousseeuw (1987)](https://doi.org/10.1016/0377-0427(87)90125-7) calculation directly.

------------------------------------------------------------------------

## ✅ Why This Package?

-   **Unified & consistent**: Offers one coherent interface for crisp, soft, and multi-way clustering silhouette diagnostics.\
-   **Flexible**: Works with distance matrices, clustering outputs, or soft membership probabilities.\
-   **Advanced**: Implements newer normalization methods (PAC), handles soft clustering, and supports mode-wise silhouette aggregation.\
-   **Visualization**: Plot functions produce clear, customizable silhouette plots compatible with many clustering outputs and existing silhouette outputs from [`factoextra`](https://doi.org/10.32614/CRAN.package.factoextra) and [`cluster`](https://doi.org/10.32614/CRAN.package.cluster) R packages.\
-   **Comparability**: Summaries and plots make it easy to compare clustering algorithms or tune the number of clusters.\
- **Interoperable**: `Silhouette()` and `softSilhouette()` works with any clustering output that provides a proximity or membership probability matrix. Users can also supply a proximity matrix and a clustering function—*including S3 or S4 methods*—to let `Silhouette()` or `softSilhouette()` perform clustering and compute silhouettes internally in one step.


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

For an intro, see the vignette A quick tour of `Silhouette`, which is available as

``` r
vignette("Silhouette")
```

You can access the vignette from the `User Guide` tab in the top navigation bar of the package's [website](https://kskbhat.github.io/Silhouette/).

------------------------------------------------------------------------

## References

Rousseeuw, P. J. (1987). Silhouettes: A graphical aid to the interpretation and validation of cluster analysis. *Journal of Computational and Applied Mathematics*, 20, 53–65. [doi:10.1016/0377-0427(87)90125-7](https://doi.org/10.1016/0377-0427(87)90125-7)

Van der Laan, M., Pollard, K., & Bryan, J. (2003). A new partitioning around medoids algorithm. *Journal of Statistical Computation and Simulation*, 73(8), 575–584. [doi:10.1080/0094965031000136012](https://doi.org/10.1080/0094965031000136012)

Campello, R. J., & Hruschka, E. R. (2006). A fuzzy extension of the silhouette width criterion for cluster analysis. *Fuzzy Sets and Systems*, 157(21), 2858–2875. [doi:10.1016/j.fss.2006.07.006](https://doi.org/10.1016/j.fss.2006.07.006)

Schepers, J., Ceulemans, E., & Van Mechelen, I. (2008). Selecting among multi-mode partitioning models of different complexities: A comparison of four model selection criteria. *Journal of Classification*, 25(1), 67–85. [doi:10.1007/s00357-008-9005-9](https://doi.org/10.1007/s00357-008-9005-9)

Kassambara, A., & Mundt, F. (2020). *factoextra: Extract and Visualize the Results of Multivariate Data Analyses*. R package version 1.0.7. [doi:10.32614/CRAN.package.factoextra](https://doi.org/10.32614/CRAN.package.factoextra)

Raymaekers, J., & Rousseeuw, P. J. (2022). Silhouettes and quasi residual plots for neural nets and tree-based classifiers. *Journal of Computational and Graphical Statistics*, 31(4), 1332–1343. [doi:10.1080/10618600.2022.2050249](https://doi.org/10.1080/10618600.2022.2050249)

Bhat Kapu, S., & Kiruthika. (2024). Some density-based silhouette diagnostics for soft clustering algorithms. *Communications in Statistics: Case Studies, Data Analysis and Applications*, 10(3–4), 221–238. [doi:10.1080/23737484.2024.2408534](https://doi.org/10.1080/23737484.2024.2408534)

Maechler, M., Rousseeuw, P., Struyf, A., Hubert, M., & Hornik, K. (2025). *cluster: Cluster Analysis Basics and Extensions*. R package version 2.1.8.1. [doi:10.32614/CRAN.package.cluster](https://doi.org/10.32614/CRAN.package.cluster)

Bhat Kapu, S., & Kiruthika, C. (2025). Block Probabilistic Distance Clustering: A Unified Framework and Evaluation. PREPRINT (Version 1) available at Research Square. [doi:10.21203/rs.3.rs-6973596/v1](https://doi.org/10.21203/rs.3.rs-6973596/v1)
