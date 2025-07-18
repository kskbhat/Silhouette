<!-- badges: start -->
[![R-CMD-check](https://github.com/kskbhat/Silhouette/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/kskbhat/Silhouette/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

# Silhouette

An [R](https://www.r-project.org/) package for silhouette-based diagnostics in standard, soft, and multi-way clustering.

Silhouette diagnostics assess clustering quality using cohesion and separation of clusters. This package implements silhouette widths for various clustering setups, including support for soft membership probabilities and multi-way clustering structures.

---

## Installation

You can install the released version of **Silhouette** from GitHub using:

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

For an intro see the vignette A quick tour of `Silhouette`, which is available as

```r
vignette("Silhouette")
```

You can also access the vignette from the `Vignettes` tab in the top navigation bar of the package website.

For information on stable release updates, visit the `NEWS` section available in the top navigation bar.

---

## References

Rousseeuw, P. J. (1987). Silhouettes: A graphical aid to the interpretation and validation of cluster analysis. *Journal of Computational and Applied Mathematics*, 20, 53–65. [https://doi.org/10.1016/0377-0427(87)90125-7](https://doi.org/10.1016/0377-0427(87)90125-7)

Van der Laan, M., Pollard, K., & Bryan, J. (2003). A new partitioning around medoids algorithm. *Journal of Statistical Computation and Simulation*, 73(8), 575–584. [https://doi.org/10.1080/0094965031000136012](https://doi.org/10.1080/0094965031000136012)

Campello, R. J., & Hruschka, E. R. (2006). A fuzzy extension of the silhouette width criterion for cluster analysis. *Fuzzy Sets and Systems*, 157(21), 2858–2875. [https://doi.org/10.1016/j.fss.2006.07.006](https://doi.org/10.1016/j.fss.2006.07.006)

Schepers, J., Ceulemans, E., & Van Mechelen, I. (2008). Selecting among multi-mode partitioning models of different complexities: A comparison of four model selection criteria. *Journal of Classification*, 25(1), 67–85. [https://doi.org/10.1007/s00357-008-9005-9](https://doi.org/10.1007/s00357-008-9005-9)

Kassambara, A., & Mundt, F. (2020). *factoextra: Extract and Visualize the Results of Multivariate Data Analyses*. R package version 1.0.7. [https://CRAN.R-project.org/package=factoextra](https://CRAN.R-project.org/package=factoextra)

Raymaekers, J., & Rousseeuw, P. J. (2022). Silhouettes and quasi residual plots for neural nets and tree-based classifiers. *Journal of Computational and Graphical Statistics*, 31(4), 1332–1343. [https://doi.org/10.1080/10618600.2022.2050249](https://doi.org/10.1080/10618600.2022.2050249)

Bhat Kapu, S., & Kiruthika. (2024). Some density-based silhouette diagnostics for soft clustering algorithms. *Communications in Statistics: Case Studies, Data Analysis and Applications*, 10(3–4), 221–238. [https://doi.org/10.1080/23737484.2024.2408534](https://doi.org/10.1080/23737484.2024.2408534)

Maechler, M., Rousseeuw, P., Struyf, A., Hubert, M., & Hornik, K. (2025). *cluster: Cluster Analysis Basics and Extensions*. R package version 2.1.8.1. [https://CRAN.R-project.org/package=cluster](https://CRAN.R-project.org/package=cluster)
