# Certainty Silhouette Width (Cer) for Soft Clustering

Computes silhouette widths using maximum of posterior probabilities as
Silhouette.

## Usage

``` r
cerSilhouette(
  prob_matrix,
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

  The silhouette calculation method used i.e., `"certainty"`.

- average:

  Character — the averaging method: `"crisp"`, `"fuzzy"`, or `"median"`.

## Details

Let the posterior probability matrix or cluster membership matrix as
\$\$\Gamma = \[\gamma\_{ik}\]\_{n \times K},\$\$ The \*\*certainty
silhouette width\*\* for observation \\i\\ is: \$\$ \mathrm{Cer}\_i =
\max\_{k=1,\dots,K} \gamma\_{ik} \$\$

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

Bhat Kapu, S., & Kiruthika. (2024). Some density-based silhouette
diagnostics for soft clustering algorithms. Communications in
Statistics: Case Studies, Data Analysis and Applications, 10(3-4),
221-238.
[doi:10.1080/23737484.2024.2408534](https://doi.org/10.1080/23737484.2024.2408534)

## See also

[`Silhouette`](https://kskbhat.github.io/Silhouette/reference/Silhouette.md),
[`softSilhouette`](https://kskbhat.github.io/Silhouette/reference/softSilhouette.md),
[`dbSilhouette`](https://kskbhat.github.io/Silhouette/reference/dbSilhouette.md),
[`getSilhouette`](https://kskbhat.github.io/Silhouette/reference/getSilhouette.md),
[`is.Silhouette`](https://kskbhat.github.io/Silhouette/reference/is.Silhouette.md),
[`plotSilhouette`](https://kskbhat.github.io/Silhouette/reference/plotSilhouette.md)

## Examples

``` r
# \donttest{
# Compare two soft clustering algorithms using cerSilhouette
# Example: FCM vs. FCM2 on iris data, using average silhouette width as a criterion
data(iris)
if (requireNamespace("ppclust", quietly = TRUE)) {
  fcm_result <- ppclust::fcm(iris[, 1:4], 3)
  out_fcm <- cerSilhouette(prob_matrix = fcm_result$u, print.summary = TRUE)
  plot(out_fcm)
  sfcm <- summary(out_fcm, print.summary = FALSE)
} else {
  message("Install 'ppclust' to run this example: install.packages('ppclust')")
}
#> ----------------------------------------------
#> Average crisp similarity db silhouette: 0.8572
#> ----------------------------------------------
#> 
#>   cluster size avg.sil.width
#> 1       1   50        0.9645
#> 2       2   40        0.8351
#> 3       3   60        0.7826
#> 
#> Available attributes: names, class, row.names, proximity_type, method, average
if (requireNamespace("ppclust", quietly = TRUE)) {
  fcm2_result <- ppclust::fcm2(iris[, 1:4], 3)
  out_fcm2 <- cerSilhouette(prob_matrix = fcm2_result$u, print.summary = TRUE)
  plot(out_fcm2)
  sfcm2 <- summary(out_fcm2, print.summary = FALSE)
} else {
  message("Install 'ppclust' to run this example: install.packages('ppclust')")
}
#> ----------------------------------------------
#> Average crisp similarity db silhouette: 0.6236
#> ----------------------------------------------
#> 
#>   cluster size avg.sil.width
#> 1       1   25        0.5974
#> 2       2   65        0.7135
#> 3       3   60        0.5371
#> 
#> Available attributes: names, class, row.names, proximity_type, method, average
# Compare average silhouette widths of fcm and fcm2
if (requireNamespace("ppclust", quietly = TRUE)) {
  cat("FCM average silhouette width:", sfcm$avg.width, "\n",
  "FCM2 average silhouette width:", sfcm2$avg.width, "\n")
}
#> FCM average silhouette width: 0.8572481 
#>  FCM2 average silhouette width: 0.6235972 
# }
```
