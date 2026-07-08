# Density-Based Silhouette Width (DBS) for Soft Clustering

Computes silhouette widths based on Menardi (2011) density-based method
using log-ratios of posterior probabilities.

## Usage

``` r
dbSilhouette(
  prob_matrix,
  average = c("median", "crisp", "fuzzy"),
  a = 2,
  sort = FALSE,
  print.summary = FALSE,
  clust_fun = NULL,
  ...
)
```

## Arguments

- prob_matrix:

  A numeric matrix of posterior probabilities where rows represent
  observations and columns represent clusters. Must sum to 1 by row. If
  `clust_fun` is provided, `prob_matrix` must be a string giving the
  name of the matrix component (e.g., "u").

- average:

  Character string specifying the method for computing the average
  silhouette width. Options are:

  - `"crisp"` – unweighted (simple) average.

  - `"fuzzy"` – weighted average based on membership probability
    differences.

  - `"median"` – median silhouette width across observations.

  Defaults to `"median"`.

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
  specifying a clustering function that produces the proximity matrix.
  For example, [`fcm`](https://rdrr.io/pkg/ppclust/man/fcm.html) or
  `"fcm"`. If provided, `prob_matrix` must be a string giving the name
  of the matrix component (e.g., "u"). Defaults to `NULL`.

- ...:

  Additional arguments passed to `clust_fun`, such as `x, centers` for
  [`fcm`](https://rdrr.io/pkg/ppclust/man/fcm.html).

## Value

A data frame of class `"Silhouette"` containing cluster assignments,
nearest neighbor clusters, silhouette widths for each observation, and
weights (for fuzzy clustering). The object includes the following
attributes:

- proximity_type:

  The proximity type used i.e., `"similarity"`.

- method:

  The silhouette calculation method used i.e., `"db"`.

- average:

  Character — the averaging method: `"crisp"`, `"fuzzy"`, or `"median"`.

## Details

Let the posterior probability matrix or cluster membership matrix as
\$\$\Gamma = \[\gamma\_{ik}\]\_{n \times K},\$\$

The \*\*density-based silhouette width\*\* for observation \\i\\ is:
\$\$ \mathrm{DBS}\_i = \frac{\log\left( \frac{\gamma\_{ik}}{\max\_{k'
\neq k} \gamma\_{ik'}} \right)} {\max\_{j=1,\dots,n} \left\| \log\left(
\frac{\gamma\_{jk}}{\max\_{k' \neq k} \gamma\_{jk'}} \right) \right\|}
\$\$

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

Menardi, G. (2011). Density-based silhouette diagnostics for clustering
methods. *Statistics and Computing*, 21(3), 295–308.
[doi:10.1007/s11222-010-9169-0](https://doi.org/10.1007/s11222-010-9169-0)

## See also

[`Silhouette`](https://kskbhat.github.io/Silhouette/reference/Silhouette.md),
[`softSilhouette`](https://kskbhat.github.io/Silhouette/reference/softSilhouette.md),
[`cerSilhouette`](https://kskbhat.github.io/Silhouette/reference/cerSilhouette.md),
[`getSilhouette`](https://kskbhat.github.io/Silhouette/reference/getSilhouette.md),
[`is.Silhouette`](https://kskbhat.github.io/Silhouette/reference/is.Silhouette.md),
[`plotSilhouette`](https://kskbhat.github.io/Silhouette/reference/plotSilhouette.md)

## Examples

``` r
# \donttest{
# Compare two soft clustering algorithms using dbSilhouette
# Example: FCM vs. FCM2 on iris data, using average silhouette width as a criterion
data(iris)
if (requireNamespace("ppclust", quietly = TRUE)) {
  fcm_result <- ppclust::fcm(iris[, 1:4], 3)
  out_fcm <- dbSilhouette(prob_matrix = fcm_result$u, print.summary = TRUE)
  plot(out_fcm)
  sfcm <- summary(out_fcm, print.summary = FALSE)
} else {
  message("Install 'ppclust' to run this example: install.packages('ppclust')")
}
#> ---------------------------------------
#> Median similarity db silhouette: 0.3098
#> ---------------------------------------
#> 
#>   cluster size avg.sil.width
#> 1       1   40        0.2456
#> 2       2   50        0.5074
#> 3       3   60        0.2225
#> 
#> Available attributes: names, class, row.names, proximity_type, method, average
if (requireNamespace("ppclust", quietly = TRUE)) {
  fcm2_result <- ppclust::fcm2(iris[, 1:4], 3)
  out_fcm2 <- dbSilhouette(prob_matrix = fcm2_result$u, print.summary = TRUE)
  plot(out_fcm2)
  sfcm2 <- summary(out_fcm2, print.summary = FALSE)
} else {
  message("Install 'ppclust' to run this example: install.packages('ppclust')")
}
#> ---------------------------------------
#> Median similarity db silhouette: 0.2648
#> ---------------------------------------
#> 
#>   cluster size avg.sil.width
#> 1       1   91        0.4541
#> 2       2    9        0.0998
#> 3       3   50        0.1997
#> 
#> Available attributes: names, class, row.names, proximity_type, method, average
# Compare average silhouette widths of fcm and fcm2
if (requireNamespace("ppclust", quietly = TRUE)) {
  cat("FCM average silhouette width:", sfcm$avg.width, "\n",
  "FCM2 average silhouette width:", sfcm2$avg.width, "\n")
}
#> FCM average silhouette width: 0.3097745 
#>  FCM2 average silhouette width: 0.2648375 
# }
```
