# Calculate Silhouette Width for Soft Clustering Algorithms

Computes silhouette widths for soft clustering results by interpreting
cluster membership probabilities (or their transformations) as proximity
measures. Although originally designed for evaluating clustering quality
within a method, this adaptation allows heuristic comparison across soft
clustering algorithms using average silhouette widths.

## Usage

``` r
softSilhouette(
  prob_matrix,
  prob_type = c("pp", "nlpp", "pd"),
  method = c("pac", "medoid"),
  average = c("crisp", "fuzzy", "median"),
  a = 2,
  sort = FALSE,
  print.summary = FALSE,
  clust_fun = NULL,
  ...
)
```

## Arguments

- prob_matrix:

  A numeric matrix where rows represent observations and columns
  represent cluster membership probabilities (or transformed
  probabilities, depending on `prob_type`). If `clust_fun` is provided,
  `prob_matrix` should be the name of the matrix component as a string
  (e.g., `"u"` for [`fcm`](https://rdrr.io/pkg/ppclust/man/fcm.html)).

- prob_type:

  Character string specifying the type transformation of membership
  matrix considered as proximity matrix in `prob_matrix`. Options are:

  `"pp"`

  :   Posterior probabilities \\\[\gamma\_{ik}\]\_{n \times K}\\
      (non-negative, typically summing to 1 per row), treated as
      similarities

  `"nlpp"`

  :   Negative log of posterior probabilities \\\[-\ln\gamma\_{ik}\]\_{n
      \times K}\\ (non-positive), treated as dissimilarities.

  `"pd"`

  :   Probability distribution \\\[\gamma\_{ik}/\pi\_{k}\]\_{n \times
      K}\\ (normalized posterior probabilities relative to cluster
      proportions \\\pi\_{k}\\), treated as similarities.

  Defaults to `"pp"`.

- method:

  Character string specifying the silhouette calculation method. Options
  are `"pac"` (Probability of Alternative Cluster) or `"medoid"`.
  Defaults to `"pac"`.

- average:

  Character string specifying the method for computing the average
  silhouette width. Options are:

  - `"crisp"` – unweighted (simple) average.

  - `"fuzzy"` – weighted average based on membership probability
    differences.

  - `"median"` – median silhouette width across observations.

  Defaults to `"crisp"`.

- a:

  Numeric value controlling the fuzzifier or weight scaling in fuzzy
  silhouette averaging. Higher values increase the emphasis on strong
  membership differences. Must be positive. Defaults to `2`.

- sort:

  Logical; if `TRUE`, sorts the output `widths` data frame by cluster
  and descending silhouette width. Defaults to `FALSE`.

- print.summary:

  Logical; if `TRUE`, prints a summary table of average silhouette
  widths and sizes for each cluster. Defaults to `FALSE`.

- clust_fun:

  Optional S3 or S4 function object or function as character string
  specifying a clustering function that produces the proximity measure
  matrix. For example, [`fcm`](https://rdrr.io/pkg/ppclust/man/fcm.html)
  or `"fcm"`. If provided, `prox_matrix` must be the name of the matrix
  component in the clustering output (e.g., `"d"` for
  [`fcm`](https://rdrr.io/pkg/ppclust/man/fcm.html) when
  `proximity_type = "dissimilarity"`). Defaults to `NULL`.

- ...:

  Additional arguments passed to `clust_fun`, such as `x,centers` for
  [`fcm`](https://rdrr.io/pkg/ppclust/man/fcm.html).

## Value

A data frame of class `"Silhouette"` containing cluster assignments,
nearest neighbor clusters, silhouette widths for each observation, and
weights (for fuzzy clustering). The object includes the following
attributes:

- proximity_type:

  The proximity type used (`"similarity"` or `"dissimilarity"`).

- method:

  The silhouette calculation method used (`"medoid"` or `"pac"`).

- average:

  Character — the averaging method: `"crisp"`, `"fuzzy"`, or `"median"`.

## Details

Although the silhouette method was originally developed for evaluating
clustering structure within a single result, this implementation allows
leveraging cluster membership probabilities from soft clustering methods
to construct proximity-based silhouettes. These silhouette widths can be
compared heuristically across different algorithms to assess clustering
quality.

See
[doi:10.1080/23737484.2024.2408534](https://doi.org/10.1080/23737484.2024.2408534)
for more details.

\#' If \`average = "crisp"\`, the \*\*crisp silhouette index\*\* is
calculated as (\\CS\\) is: \$\$ CS = \frac{1}{n} \sum\_{i=1}^{n} S(x_i)
\$\$ summarizing overall clustering quality.

If \`average = "fuzzy"\` and \`prob_matrix\` is provided, denoted as
\\\Gamma = \[\gamma\_{ik}\]\_{n \times K}\\, with \\\gamma\_{ik}\\
representing the probability of observation \\i\\ belonging to cluster
\\k\\, the \*\*fuzzy silhouette index\*\* (\\FS\\) is calculated as:
\$\$ FS = \frac{\sum\_{i=1}^{n} w_i S(x_i) }{\sum\_{i=1}^{n} w_i} \$\$
where \\w_i = \sum\_{i=1}^{n} \left( \gamma\_{ik} - \max\_{k' \neq k}
\gamma\_{ik'} \right)^{\alpha}\\ is \`weight\` and \\\alpha\\ (the \`a\`
argument) controls the emphasis on confident assignments.

If \`average = "median"\` then median Silhouette is Calculated

## References

Raymaekers, J., & Rousseeuw, P. J. (2022). Silhouettes and quasi
residual plots for neural nets and tree-based classifiers. *Journal of
Computational and Graphical Statistics*, 31(4), 1332–1343.
[doi:10.1080/10618600.2022.2050249](https://doi.org/10.1080/10618600.2022.2050249)

Bhat Kapu, S., & Kiruthika. (2024). Some density-based silhouette
diagnostics for soft clustering algorithms. Communications in
Statistics: Case Studies, Data Analysis and Applications, 10(3-4),
221-238.
[doi:10.1080/23737484.2024.2408534](https://doi.org/10.1080/23737484.2024.2408534)

## See also

[`Silhouette`](https://kskbhat.github.io/Silhouette/reference/Silhouette.md),
[`dbSilhouette`](https://kskbhat.github.io/Silhouette/reference/dbSilhouette.md),
[`cerSilhouette`](https://kskbhat.github.io/Silhouette/reference/cerSilhouette.md),
[`getSilhouette`](https://kskbhat.github.io/Silhouette/reference/getSilhouette.md),
[`is.Silhouette`](https://kskbhat.github.io/Silhouette/reference/is.Silhouette.md),
[`plotSilhouette`](https://kskbhat.github.io/Silhouette/reference/plotSilhouette.md)

## Examples

``` r
# \donttest{
# Compare two soft clustering algorithms using softSilhouett
# Example: FCM vs. FCM2 on iris data, using average silhouette width as a criterion
data(iris)
if (requireNamespace("ppclust", quietly = TRUE)) {
  fcm_result <- ppclust::fcm(iris[, 1:4], 3)
  out_fcm <- softSilhouette(prob_matrix = fcm_result$u,print.summary = TRUE)
  plot(out_fcm)
  sfcm <- summary(out_fcm, print.summary = FALSE)
} else {
  message("Install 'ppclust' to run this example: install.packages('ppclust')")
}
#> -----------------------------------------------
#> Average crisp similarity pac silhouette: 0.7541
#> -----------------------------------------------
#> 
#>   cluster size avg.sil.width
#> 1       1   40        0.7005
#> 2       2   50        0.9507
#> 3       3   60        0.6261
#> 
#> Available attributes: names, class, row.names, proximity_type, method, average
if (requireNamespace("ppclust", quietly = TRUE)) {
  fcm2_result <- ppclust::fcm2(iris[, 1:4], 3)
  out_fcm2 <- softSilhouette(prob_matrix = fcm2_result$u,print.summary = TRUE)
  plot(out_fcm2)
  sfcm2 <- summary(out_fcm2, print.summary = FALSE)
} else {
  message("Install 'ppclust' to run this example: install.packages('ppclust')")
}
#> -----------------------------------------------
#> Average crisp similarity pac silhouette: 0.4113
#> -----------------------------------------------
#> 
#>   cluster size avg.sil.width
#> 1       1   25        0.3623
#> 2       2   60        0.2666
#> 3       3   65        0.5636
#> 
#> Available attributes: names, class, row.names, proximity_type, method, average
# Compare average silhouette widths of fcm and fcm2
if (requireNamespace("ppclust", quietly = TRUE)) {
  cat("FCM average silhouette width:", sfcm$avg.width, "\n",
  "FCM2 average silhouette width:", sfcm2$avg.width, "\n")
}
#> FCM average silhouette width: 0.7541271 
#>  FCM2 average silhouette width: 0.411275 
# }
```
