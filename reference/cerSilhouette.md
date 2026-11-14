# Certainty Silhouette Width (CeS) for Soft Clustering

Computes the certainty silhouette (CeS) of Bhat Kapu and Kiruthika
(2024) for each observation: the membership probability of its assigned
cluster (by default, its largest membership).

## Usage

``` r
cerSilhouette(
  prob_matrix,
  average = c("crisp", "fuzzy", "median"),
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

  Defaults to `"crisp"`.

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

  The silhouette calculation method used i.e., `"certainty"`.

- average:

  Character — the averaging method: `"crisp"`, `"fuzzy"`, or `"median"`.

## Details

The certainty silhouette is the counterpart of the uncertainty used in
model-based clustering: values near 1 indicate high certainty that an
observation belongs to its cluster, and values near \\1/K\\ ambiguity.
It is bounded below by \\1/K\\, so its level falls as \\K\\ grows; Bhat
Kapu and Kiruthika (2024) read the number of clusters from the reverse
elbow drop of its average over \\k\\ (see
[`softSilhouette`](https://kskbhat.github.io/Silhouette/reference/softSilhouette.md)).

Let the posterior probability matrix or cluster membership matrix be
\\\Gamma = \[\gamma\_{ik}\]\_{n \times K}\\. The **certainty silhouette
width** for observation \\i\\ assigned to cluster \\c_i\\ is: \$\$
\mathrm{Cer}\_i = \gamma\_{ic_i}, \$\$ which equals \\\max\_{k}
\gamma\_{ik} \in \[1/K, 1\]\\ with the default labels. With labels
supplied through `cluster`, the width lies in \\\[0, 1\]\\. The
`neighbor` column gives the cluster with the largest membership other
than \\c_i\\.

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

Bhat Kapu, S., & Kiruthika. (2024). Some density-based silhouette
diagnostics for soft clustering algorithms. Communications in
Statistics: Case Studies, Data Analysis and Applications, 10(3-4),
221-238.
[doi:10.1080/23737484.2024.2408534](https://doi.org/10.1080/23737484.2024.2408534)

## See also

[`Silhouette`](https://kskbhat.github.io/Silhouette/reference/Silhouette.md),
[`softSilhouette`](https://kskbhat.github.io/Silhouette/reference/softSilhouette.md),
[`dbSilhouette`](https://kskbhat.github.io/Silhouette/reference/dbSilhouette.md),
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
cerSilhouette(prob)
#>   cluster neighbor sil_width
#> 1       1        2      0.90
#> 2       1        2      0.60
#> 3       2        1      0.80
#> 4       2        1      0.55

# \donttest{
# Compare two soft clustering algorithms on the iris data
data(iris)
if (requireNamespace("ppclust", quietly = TRUE)) {
  fcm_result <- ppclust::fcm(iris[, 1:4], 3)
  out_fcm <- cerSilhouette(prob_matrix = fcm_result$u, print.summary = TRUE)
  plot(out_fcm)
  fcm2_result <- ppclust::fcm2(iris[, 1:4], 3)
  out_fcm2 <- cerSilhouette(prob_matrix = fcm2_result$u)
  c(fcm = summary(out_fcm, print.summary = FALSE)$avg.width,
    fcm2 = summary(out_fcm2, print.summary = FALSE)$avg.width)
} else {
  message("Install 'ppclust' to run this example: install.packages('ppclust')")
}
#> -----------------------------------------------------
#> Average crisp similarity certainty silhouette: 0.8572
#> -----------------------------------------------------
#> 
#>  cluster size avg.sil.width
#>        1   50        0.9645
#>        2   40        0.8351
#>        3   60        0.7826
#>       fcm      fcm2 
#> 0.8572481 0.6235972 
# }
```
