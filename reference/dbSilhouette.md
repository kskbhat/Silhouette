# Density-Based Silhouette Width (DBS) for Soft Clustering

Computes the density-based silhouette (DBS) of Menardi (2011), using
log-ratios of posterior probabilities.

## Usage

``` r
dbSilhouette(
  prob_matrix,
  average = c("median", "crisp", "fuzzy"),
  a = 2,
  sort = FALSE,
  print.summary = FALSE,
  clust_fun = NULL,
  ...,
  cluster = NULL,
  clust_args = list()
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

  - `"fuzzy"` – weighted average; the weight of an observation is the
    gap between its two largest memberships raised to the power `a`.

  - `"median"` – median silhouette width across observations.

  Defaults to `"median"`.

- a:

  Non-negative number, the exponent \\\alpha\\ of the weights in fuzzy
  silhouette averaging. Higher values increase the emphasis on strong
  membership differences; `a = 0` gives every observation the same
  weight, so the fuzzy average equals the crisp average. Defaults to
  `2`.

- sort:

  Logical; if `TRUE`, sorts the output `widths` data frame by cluster
  and descending silhouette width. Defaults to `FALSE`.

- print.summary:

  Logical; if `TRUE`, reports a summary table of average silhouette
  widths and sizes for each cluster through
  [`message`](https://rdrr.io/r/base/message.html). Defaults to `FALSE`
  for
  [`Silhouette()`](https://kskbhat.github.io/Silhouette/reference/Silhouette.md)
  and `TRUE` for [`summary()`](https://rdrr.io/r/base/summary.html).

- clust_fun:

  Optional S3 or S4 function object or function as character string
  specifying a clustering function that produces the membership matrix.
  For example, [`fcm`](https://rdrr.io/pkg/ppclust/man/fcm.html) or
  `"fcm"`. If provided, `prob_matrix` must be a string giving the name
  of the matrix component (e.g., "u"). Defaults to `NULL`.

- ...:

  Additional arguments passed to `clust_fun`, such as `x, centers` for
  [`fcm`](https://rdrr.io/pkg/ppclust/man/fcm.html).

- cluster:

  Optional integer vector of length \\n\\ giving the cluster of each
  observation as a column number of `prob_matrix`. If `NULL` (the
  default), each observation is assigned to the cluster with its largest
  membership (for `"pp"` and `"nlpp"`) or its largest density (for
  `"pd"`); see *Cluster labels*. If `clust_fun` is provided, `cluster`
  may be the name of the label component of its output. Must be named in
  the call because it follows `...`.

- clust_args:

  A list of further arguments for `clust_fun`; use it for arguments
  whose names could be partially matched to this function's own (for
  example `clust_args = list(m = 3)` for the fuzzifier of
  [`fcm`](https://rdrr.io/pkg/ppclust/man/fcm.html)). Defaults to
  [`list()`](https://rdrr.io/r/base/list.html).

## Value

A data frame of class `"Silhouette"` with one row per observation and
columns `cluster`, `neighbor`, `weight` (only when `average = "fuzzy"`)
and `sil_width`. The object includes the following attributes:

- proximity_type:

  The proximity type used i.e., `"similarity"`.

- method:

  The silhouette calculation method used i.e., `"db"`.

- average:

  Character — the averaging method: `"crisp"`, `"fuzzy"`, or `"median"`.

## Details

The DBS compares the largest membership with the second largest on the
log scale, scaled by the largest such log ratio in the sample. Its
numerator is that of the NLPPS of Bhat Kapu and Kiruthika (2024), with a
different denominator. Following Menardi (2011), the median is the
default summary, and the number of clusters is read from the local
maxima of the average over \\k\\.

Let the posterior probability matrix or cluster membership matrix be
\\\Gamma = \[\gamma\_{ik}\]\_{n \times K}\\, let \\c_i\\ be the cluster
of observation \\i\\ and \\\nu_i\\ the cluster with its largest
membership other than \\c_i\\. With the log ratio \$\$R_i = \log
\frac{\gamma\_{ic_i}}{\gamma\_{i\nu_i}},\$\$ the **density-based
silhouette width** is \$\$ \mathrm{DBS}\_i =
\frac{R_i}{\max\_{j=1,\dots,n} \|R_j\|}. \$\$ With the default labels
(largest membership) \\R_i \ge 0\\, so the widths lie in \\\[0, 1\]\\
and the most confidently assigned observation has width 1. With labels
supplied through `cluster`, \\R_i\\ can be negative and the widths lie
in \\\[-1, 1\]\\.

Because of the scaling, the average width depends on how extreme the
most confident observation is: a single observation with a very large
log ratio shrinks every other width towards zero. Compare the average
only across fits with similar extremes, and report it together with the
mode-wise or cluster-wise summaries.

## Numerical safeguards

The following rules keep the widths finite and meaningful:

- **Zero probabilities.** When the neighboring membership is exactly
  zero (for example, after a posterior probability underflows), \\R_i =
  +\infty\\. Such observations receive width 1 (and width -1 when the
  assigned membership is zero, which can happen only with supplied
  labels), and a warning reports how many there are. The remaining
  widths are scaled by the largest *finite* \\\|R_j\|\\. (In version
  0.9.6 and earlier, the infinite maximum turned every finite width into
  0.)

- **Equal memberships.** When the two memberships are equal, \\R_i = 0\\
  and the width is 0.

- **Degenerate fits.** When the largest finite \\\|R_j\|\\ is below
  `sqrt(.Machine$double.eps)` (about \\1.5 \times 10^{-8}\\), the
  memberships are numerically indistinguishable and scaling would only
  magnify rounding error. The finite widths are then set to 0 with a
  warning.

Ties are resolved in favor of the first column; see
[`Silhouette`](https://kskbhat.github.io/Silhouette/reference/Silhouette.md).

## Averages

If `average = "crisp"`, the **crisp silhouette index** (\\CS\\) is the
mean width: \$\$ CS = \frac{1}{n} \sum\_{i=1}^{n} S(x_i) \$\$
summarizing overall clustering quality.

If `average = "fuzzy"` and `prob_matrix` is provided, denoted as
\\\Gamma = \[\gamma\_{ik}\]\_{n \times K}\\, with \\\gamma\_{ik}\\
representing the probability of observation \\i\\ belonging to cluster
\\k\\, the **fuzzy silhouette index** (\\FS\\) of Campello and Hruschka
(2006) is: \$\$ FS = \frac{\sum\_{i=1}^{n} w_i S(x_i) }{\sum\_{i=1}^{n}
w_i}, \qquad w_i = ( \gamma\_{i(1)} - \gamma\_{i(2)} )^{\alpha}, \$\$
where \\\gamma\_{i(1)}\\ and \\\gamma\_{i(2)}\\ are the largest and
second largest memberships of observation \\i\\, and \\\alpha\\ (the `a`
argument) controls the emphasis on confident assignments (\\\alpha \ge
0\\; \\\alpha = 0\\ gives the crisp average). The weight \\w_i\\ is
stored in the `weight` column and does not depend on the cluster labels;
it equals Equation (8) of Bhat Kapu and Kiruthika (2024) when each
observation is assigned to its largest membership. If all weights are
zero, the fuzzy average is undefined (`NaN`) and a warning is given.

If `average = "median"`, the median width is reported.

Missing widths are removed before averaging.

## References

Menardi, G. (2011). Density-based silhouette diagnostics for clustering
methods. *Statistics and Computing*, 21(3), 295–308.
[doi:10.1007/s11222-010-9169-0](https://doi.org/10.1007/s11222-010-9169-0)

Bhat Kapu, S., & Kiruthika. (2024). Some density-based silhouette
diagnostics for soft clustering algorithms. Communications in
Statistics: Case Studies, Data Analysis and Applications, 10(3-4),
221-238.
[doi:10.1080/23737484.2024.2408534](https://doi.org/10.1080/23737484.2024.2408534)

## See also

[`Silhouette`](https://kskbhat.github.io/Silhouette/reference/Silhouette.md),
[`softSilhouette`](https://kskbhat.github.io/Silhouette/reference/softSilhouette.md),
[`cerSilhouette`](https://kskbhat.github.io/Silhouette/reference/cerSilhouette.md),
[`calSilhouette`](https://kskbhat.github.io/Silhouette/reference/calSilhouette.md),
[`getSilhouette`](https://kskbhat.github.io/Silhouette/reference/getSilhouette.md),
[`is.Silhouette`](https://kskbhat.github.io/Silhouette/reference/is.Silhouette.md),
[`plotSilhouette`](https://kskbhat.github.io/Silhouette/reference/plotSilhouette.md)

## Examples

``` r
prob <- matrix(c(0.90, 0.10,
                 0.60, 0.40,
                 0.20, 0.80,
                 0.45, 0.55), ncol = 2, byrow = TRUE)
dbSilhouette(prob)
#>   cluster neighbor  sil_width
#> 1       1        2 1.00000000
#> 2       1        2 0.18453512
#> 3       2        1 0.63092975
#> 4       2        1 0.09132917

# A zero neighboring probability gives width 1 and a warning
prob0 <- rbind(prob, c(1, 0))
dbSilhouette(prob0)
#> Warning: 1 observation(s) have a zero membership in the neighboring (or assigned) cluster, so their log ratio is infinite; their widths are set to 1 (or -1) and the other widths are scaled by the largest finite log ratio.
#>   cluster neighbor  sil_width
#> 1       1        2 1.00000000
#> 2       1        2 0.18453512
#> 3       2        1 0.63092975
#> 4       2        1 0.09132917
#> 5       1        2 1.00000000

# \donttest{
# Compare two soft clustering algorithms on the iris data
data(iris)
if (requireNamespace("ppclust", quietly = TRUE)) {
  fcm_result <- ppclust::fcm(iris[, 1:4], 3)
  out_fcm <- dbSilhouette(prob_matrix = fcm_result$u, print.summary = TRUE)
  plot(out_fcm)
  fcm2_result <- ppclust::fcm2(iris[, 1:4], 3)
  out_fcm2 <- dbSilhouette(prob_matrix = fcm2_result$u)
  c(fcm = summary(out_fcm, print.summary = FALSE)$avg.width,
    fcm2 = summary(out_fcm2, print.summary = FALSE)$avg.width)
} else {
  message("Install 'ppclust' to run this example: install.packages('ppclust')")
}
#> ---------------------------------------
#> Median similarity db silhouette: 0.3098
#> ---------------------------------------
#> 
#>  cluster size avg.sil.width
#>        1   50        0.5074
#>        2   60        0.2225
#>        3   40        0.2456
#>       fcm      fcm2 
#> 0.3097745 0.2607283 
# }
```
