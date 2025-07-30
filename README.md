# Silhouette <a href="https://kskbhat.github.io/Silhouette/"><img src="man/figures/logo.png" align="right" height="139" alt="Silhouette website" /></a>

<!-- badges: start -->
[![CRAN Status](https://www.r-pkg.org/badges/version/Silhouette)](https://CRAN.R-project.org/package=Silhouette)
[![CRAN Downloads](https://cranlogs.r-pkg.org/badges/grand-total/Silhouette)](https://cran.r-project.org/package=Silhouette)
[![Lifecycle: stable](https://img.shields.io/badge/lifecycle-stable-brightgreen.svg)](https://lifecycle.r-lib.org/articles/stages.html)
[![R-CMD-check](https://github.com/kskbhat/Silhouette/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/kskbhat/Silhouette/actions/workflows/R-CMD-check.yaml)
[![R-hub](https://github.com/kskbhat/Silhouette/actions/workflows/rhub.yaml/badge.svg)](https://github.com/kskbhat/Silhouette/actions/workflows/rhub.yaml)
[![pages-build-deployment](https://github.com/kskbhat/Silhouette/actions/workflows/pages/pages-build-deployment/badge.svg)](https://github.com/kskbhat/Silhouette/actions/workflows/pages/pages-build-deployment)
<!-- badges: end -->


An [R](https://www.r-project.org/) package for silhouette-based diagnostics in standard, soft, and multi-way clustering.

Silhouette diagnostics assess clustering quality using cohesion and separation of clusters. This package implements silhouette widths for various clustering setups, including support for soft membership probabilities and multi-way clustering structures.

---

## Installation

You can install the released version of `Silhouette` from GitHub using:

```r
# Install devtools if needed
if (!requireNamespace("devtools", quietly = TRUE)) {
  install.packages("devtools")
}

# Install from GitHub
devtools::install_github("kskbhat/Silhouette")
```

Once accepted on CRAN, install via:

```r
install.packages("Silhouette")
```

---

## Usage

Usage of the main functions is demonstrated in the package examples and documentation.

For an intro, see the vignette A quick tour of `Silhouette`, which is available as

```r
vignette("Silhouette")
```

You can access the vignette from the `User Guide` tab in the top navigation bar of the package's [website](https://kskbhat.github.io/Silhouette/).

---

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
