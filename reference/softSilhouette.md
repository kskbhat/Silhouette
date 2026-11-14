# Calculate Silhouette Width for Soft Clustering Algorithms

Computes silhouette widths for soft clustering results by interpreting
cluster membership probabilities (or their transformations) as proximity
measures (Bhat Kapu and Kiruthika, 2024). Although originally designed
for evaluating clustering quality within a method, this adaptation
allows heuristic comparison across soft clustering algorithms using
average silhouette widths.

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
  ...,
  cluster = NULL,
  clust_args = list()
)
```

## Arguments

- prob_matrix:

  A numeric matrix where rows represent observations and columns
  represent cluster membership probabilities. Rows must sum to 1. If
  `clust_fun` is provided, `prob_matrix` should be the name of the
  matrix component as a string (e.g., `"u"` for
  [`fcm`](https://rdrr.io/pkg/ppclust/man/fcm.html)).

- prob_type:

  Character string specifying the type transformation of membership
  matrix considered as proximity matrix in `prob_matrix`. Options are:

  `"pp"`

  :   Posterior probabilities \\\[\gamma\_{ik}\]\_{n \times K}\\
      (non-negative, summing to 1 per row), treated as similarities
      (PPS, PACS).

  `"nlpp"`

  :   Negative log of posterior probabilities \\\[-\ln\gamma\_{ik}\]\_{n
      \times K}\\ (non-negative; \\+\infty\\ for a zero probability),
      treated as dissimilarities (NLPPS).

  `"pd"`

  :   Probability density: the component densities \\f_k(x_i)\\, treated
      as similarities (PDS). They are computed from the memberships by
      Bayes' theorem as \\\[\gamma\_{ik}/\hat\pi\_{k}\]\_{n \times K}\\,
      with \\\hat\pi\_{k} = n^{-1}\sum_i \gamma\_{ik}\\; for a mixture
      fitted by EM this equals \\f_k(x_i)/f(x_i)\\ (see Details), and
      for other soft clustering algorithms the same ratio is used.

  Defaults to `"pp"`.

- method:

  Character string specifying the normalization of the silhouette width.
  Options are `"pac"` (Probability of Alternative Cluster) or
  `"medoid"`; see
  [`Silhouette`](https://kskbhat.github.io/Silhouette/reference/Silhouette.md).
  Defaults to `"pac"`.

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
  specifying a clustering function that produces the proximity measure
  matrix. For example, [`fcm`](https://rdrr.io/pkg/ppclust/man/fcm.html)
  or `"fcm"`. If provided, `prox_matrix` must be the name of the matrix
  component in the clustering output (e.g., `"d"` for
  [`fcm`](https://rdrr.io/pkg/ppclust/man/fcm.html) when
  `proximity_type = "dissimilarity"`). Defaults to `NULL`.

- ...:

  Additional arguments passed to `clust_fun`, such as `x,centers` for
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

  A list of further arguments for `clust_fun`. Arguments given through
  `...` are matched first against this function's own arguments, and R
  matches names partially: `m = 3` meant for the fuzzifier of
  [`fcm`](https://rdrr.io/pkg/ppclust/man/fcm.html) would be taken as
  `method`. Pass such arguments here instead, e.g.
  `clust_args = list(m = 3)`. Defaults to
  [`list()`](https://rdrr.io/r/base/list.html).

## Value

A data frame of class `"Silhouette"` with one row per observation and
columns `cluster`, `neighbor`, `weight` (only when `average = "fuzzy"`)
and `sil_width`. The object includes the following attributes:

- proximity_type:

  The proximity type used (`"similarity"` or `"dissimilarity"`).

- method:

  The silhouette calculation method used (`"medoid"` or `"pac"`).

- average:

  Character — the averaging method: `"crisp"`, `"fuzzy"`, or `"median"`.

## Details

With the names used by Bhat Kapu and Kiruthika (2024):
`prob_type = "pp"` with `method = "pac"` is the PACS (the silhouette
based on the probability of the alternative class of Raymaekers and
Rousseeuw, 2022, applied to posterior probabilities), `"pp"` with
`"medoid"` the PPS (posterior probability silhouette), `"nlpp"` with
`"medoid"` the NLPPS (negative log of posterior probability silhouette),
and `"pd"` with `"medoid"` the PDS (probability density silhouette). The
certainty silhouette (CeS) and the density-based silhouette (DBS) are
computed by
[`cerSilhouette`](https://kskbhat.github.io/Silhouette/reference/cerSilhouette.md)
and
[`dbSilhouette`](https://kskbhat.github.io/Silhouette/reference/dbSilhouette.md).

The membership matrix is transformed according to `prob_type` and the
widths are computed as in
[`Silhouette`](https://kskbhat.github.io/Silhouette/reference/Silhouette.md).
The PPS and PACS treat the posterior probabilities as a similarity
proximity measure, the NLPPS turns them into a dissimilarity through the
negative logarithm, and the PDS treats the component densities as a
similarity (Bhat Kapu and Kiruthika, 2024). With `method = "pac"`, the
`"pp"` width is \\(\gamma\_{ic_i} - \gamma\_{i\nu_i})/(\gamma\_{ic_i} +
\gamma\_{i\nu_i})\\; with `"medoid"` it is \\1 -
\gamma\_{i\nu_i}/\gamma\_{ic_i}\\.

For a finite mixture \\f(x) = \sum_k \pi_k f_k(x)\\ fitted by EM,
\\\gamma\_{ik} = \pi_k f_k(x_i)/f(x_i)\\ and, at convergence,
\\\hat\pi_k\\ is the fitted mixing proportion, so
\\\gamma\_{ik}/\hat\pi_k = f_k(x_i)/f(x_i)\\ (up to the convergence
tolerance). The PDS width of Equation (22) of Bhat Kapu and Kiruthika
(2024), \\(f\_{c_i}(x_i) - \max\_{k \neq c_i} f_k(x_i)) /
\max(f\_{c_i}(x_i), \max\_{k \neq c_i} f_k(x_i))\\, is therefore
computed from the memberships alone, and can be used with any soft
clustering algorithm.

A zero probability gives an infinite `"nlpp"` dissimilarity. The width
is then 1 if only the neighboring membership is zero, -1 if only the
assigned membership is zero, and 0 if both are zero.

## Choosing the number of clusters

The average of these widths over \\k = 2, 3, \ldots\\ typically starts
near 1 and decreases. Bhat Kapu and Kiruthika (2024) read the number of
clusters from the reverse elbow drop of this curve, the value of \\k\\
after which the average drops sharply, rather than from its maximum.
Which \\k\\ is suggested depends on the algorithm that produced the
memberships, on the width and on the averaging, so report all three with
the result.

## Cluster labels

The labels are `cluster` when supplied. Otherwise, for `"pp"` and
`"nlpp"`, each observation is assigned to the cluster with its largest
membership, the partition a soft clustering algorithm reports, and the
widths are non-negative. For `"pd"`, each observation is assigned to the
cluster with its largest density \\\gamma\_{ik}/\hat\pi_k\\, as in the
definition of the PDS, so the PDS widths lie in \\\[0, 1\]\\ for every
average. The largest density and the largest membership coincide when
the clusters have equal estimated weights; otherwise the PDS can place
an observation in a smaller cluster than its largest membership does.
The neighboring cluster is the closest other cluster in the transformed
matrix.

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

Raymaekers, J., & Rousseeuw, P. J. (2022). Silhouettes and quasi
residual plots for neural nets and tree-based classifiers. *Journal of
Computational and Graphical Statistics*, 31(4), 1332–1343.
[doi:10.1080/10618600.2022.2050249](https://doi.org/10.1080/10618600.2022.2050249)

Campello, R. J., & Hruschka, E. R. (2006). A fuzzy extension of the
silhouette width criterion for cluster analysis. *Fuzzy Sets and
Systems*, 157(21), 2858–2875.
[doi:10.1016/j.fss.2006.07.006](https://doi.org/10.1016/j.fss.2006.07.006)

Bhat Kapu, S., & Kiruthika. (2024). Some density-based silhouette
diagnostics for soft clustering algorithms. Communications in
Statistics: Case Studies, Data Analysis and Applications, 10(3-4),
221-238.
[doi:10.1080/23737484.2024.2408534](https://doi.org/10.1080/23737484.2024.2408534)

## See also

[`Silhouette`](https://kskbhat.github.io/Silhouette/reference/Silhouette.md),
[`dbSilhouette`](https://kskbhat.github.io/Silhouette/reference/dbSilhouette.md),
[`cerSilhouette`](https://kskbhat.github.io/Silhouette/reference/cerSilhouette.md),
[`calSilhouette`](https://kskbhat.github.io/Silhouette/reference/calSilhouette.md),
[`getSilhouette`](https://kskbhat.github.io/Silhouette/reference/getSilhouette.md),
[`is.Silhouette`](https://kskbhat.github.io/Silhouette/reference/is.Silhouette.md),
[`plotSilhouette`](https://kskbhat.github.io/Silhouette/reference/plotSilhouette.md)

## Examples

``` r
# A small membership matrix
prob <- matrix(c(0.90, 0.10,
                 0.60, 0.40,
                 0.20, 0.80,
                 0.45, 0.55), ncol = 2, byrow = TRUE)
softSilhouette(prob)
#>   cluster neighbor sil_width
#> 1       1        2       0.8
#> 2       1        2       0.2
#> 3       2        1       0.6
#> 4       2        1       0.1
softSilhouette(prob, prob_type = "nlpp", average = "fuzzy")
#>   cluster neighbor weight sil_width
#> 1       1        2   0.64 0.9124893
#> 2       1        2   0.04 0.2841150
#> 3       2        1   0.36 0.7564708
#> 4       2        1   0.01 0.1437114

# \donttest{
# Compare two soft clustering algorithms on the iris data
data(iris)
if (requireNamespace("ppclust", quietly = TRUE)) {
  fcm_result <- ppclust::fcm(iris[, 1:4], 3)
  out_fcm <- softSilhouette(prob_matrix = fcm_result$u, print.summary = TRUE)
  plot(out_fcm)
  fcm2_result <- ppclust::fcm2(iris[, 1:4], 3)
  out_fcm2 <- softSilhouette(prob_matrix = fcm2_result$u)
  c(fcm = summary(out_fcm, print.summary = FALSE)$avg.width,
    fcm2 = summary(out_fcm2, print.summary = FALSE)$avg.width)
} else {
  message("Install 'ppclust' to run this example: install.packages('ppclust')")
}
#> -----------------------------------------------
#> Average crisp similarity pac silhouette: 0.7541
#> -----------------------------------------------
#> 
#>  cluster size avg.sil.width
#>        1   50        0.9507
#>        2   40        0.7005
#>        3   60        0.6261
#>       fcm      fcm2 
#> 0.7541271 0.4112750 
# }
```
