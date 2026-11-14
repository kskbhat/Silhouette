# Calculate All Available Silhouette Summaries

Computes every silhouette width available in the package for one
clustering and returns a data frame comparing the crisp, fuzzy, and
median averages of each.

## Usage

``` r
calSilhouette(
  prox_matrix = NULL,
  proximity_type = c("dissimilarity", "similarity"),
  prob_matrix = NULL,
  a = 2,
  print.summary = FALSE,
  clust_fun = NULL,
  ...,
  cluster = NULL,
  clust_args = list()
)
```

## Arguments

- prox_matrix:

  A numeric matrix where rows represent observations and columns
  represent proximity measures (e.g., distances or similarities) to
  clusters. Typically, this is a membership or dissimilarity matrix from
  clustering results. If `clust_fun` is provided, `prox_matrix` should
  be the name of the matrix component as a string (e.g., if
  `clust_fun = `[`fcm`](https://rdrr.io/pkg/ppclust/man/fcm.html) from
  ppclust package the `prox_matrix = "d"`). Defaults to `NULL`.

- proximity_type:

  Character string specifying the type of proximity measure in
  `prox_matrix`. Options are `"similarity"` (higher values indicate
  closer proximity) or `"dissimilarity"` (lower values indicate closer
  proximity). Defaults to `"dissimilarity"`.

- prob_matrix:

  A numeric matrix of cluster membership probabilities, where rows
  represent observations and columns represent clusters in the same
  order as `prox_matrix`. If `clust_fun` is provided, `prob_matrix` can
  be given as the name of the matrix component (e.g., `"u"` for the
  [`fcm`](https://rdrr.io/pkg/ppclust/man/fcm.html) function). Defaults
  to `NULL`.

- a:

  Non-negative number, the exponent \\\alpha\\ of the weights in fuzzy
  silhouette averaging. Higher values increase the emphasis on strong
  membership differences; `a = 0` gives every observation the same
  weight, so the fuzzy average equals the crisp average. Defaults to
  `2`.

- print.summary:

  Logical; if `TRUE`, reports the summary table through
  [`message`](https://rdrr.io/r/base/message.html). Defaults to `FALSE`.

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
  observation as a column number (\\1, \ldots, K\\). If `clust_fun` is
  provided, `cluster` may be the name of the label component of its
  output. Must be named in the call because it follows `...`. See
  *Cluster labels*.

- clust_args:

  A list of further arguments for `clust_fun`. Arguments given through
  `...` are matched first against this function's own arguments, and R
  matches names partially: `m = 3` meant for the fuzzifier of
  [`fcm`](https://rdrr.io/pkg/ppclust/man/fcm.html) would be taken as
  `method`. Pass such arguments here instead, e.g.
  `clust_args = list(m = 3)`. Defaults to
  [`list()`](https://rdrr.io/r/base/list.html).

## Value

A data frame with one row per method and the following columns:

- Method:

  Character vector of method names, as listed in Details.

- Crisp:

  Crisp (unweighted mean) average silhouette width.

- Fuzzy:

  Fuzzy (membership-weighted) average silhouette width; present only
  when `prob_matrix` is supplied.

- Median:

  Median silhouette width.

## Details

The rows are not independent pieces of evidence: several are exact or
monotone transformations of one another (see *Relations between the
rows*). The distance-based rows compare distances to the cluster
representatives; the probability-based rows are the silhouettes of Bhat
Kapu and Kiruthika (2024), which treat the posterior probabilities or
the component densities as proximities, and the density-based silhouette
of Menardi (2011).

The methods included depend on the available input matrices:

**If `prox_matrix` is available:**

- `medoid` - Medoid-based silhouette using
  [`Silhouette`](https://kskbhat.github.io/Silhouette/reference/Silhouette.md)

- `pac` - PAC-based silhouette using
  [`Silhouette`](https://kskbhat.github.io/Silhouette/reference/Silhouette.md)

**If `prob_matrix` is available:**

- `pp_pac` - PACS: posterior probabilities with the PAC normalization
  ([`softSilhouette`](https://kskbhat.github.io/Silhouette/reference/softSilhouette.md))

- `pp_medoid` - PPS: posterior probability silhouette
  ([`softSilhouette`](https://kskbhat.github.io/Silhouette/reference/softSilhouette.md))

- `nlpp_pac` - Negative log posterior probabilities with the PAC
  normalization
  ([`softSilhouette`](https://kskbhat.github.io/Silhouette/reference/softSilhouette.md))

- `nlpp_medoid` - NLPPS: negative log of posterior probability
  silhouette
  ([`softSilhouette`](https://kskbhat.github.io/Silhouette/reference/softSilhouette.md))

- `pd_pac` - Probability densities with the PAC normalization
  ([`softSilhouette`](https://kskbhat.github.io/Silhouette/reference/softSilhouette.md))

- `pd_medoid` - PDS: probability density silhouette
  ([`softSilhouette`](https://kskbhat.github.io/Silhouette/reference/softSilhouette.md))

- `cer` - CeS: certainty silhouette
  ([`cerSilhouette`](https://kskbhat.github.io/Silhouette/reference/cerSilhouette.md))

- `db` - DBS: density-based silhouette of Menardi (2011)
  ([`dbSilhouette`](https://kskbhat.github.io/Silhouette/reference/dbSilhouette.md))

At least one of `prox_matrix` or `prob_matrix` must be provided. Each
width is computed once and summarized by all available averages; the
fuzzy average needs `prob_matrix`.

If a width cannot be computed, its row is `NA` and a warning gives the
reason. Warnings from the individual functions are collected and each
distinct warning is reported once.

## Cluster labels

When `cluster` is supplied, every row scores that partition. Otherwise
every row except the PDS rows (`pd_pac`, `pd_medoid`) uses the largest
membership in `prob_matrix`, or the closest cluster in `prox_matrix`
when there are no memberships. The PDS rows assign each observation to
its largest density, as in the definition of the PDS (see
[`softSilhouette`](https://kskbhat.github.io/Silhouette/reference/softSilhouette.md));
this agrees with the largest membership when the clusters have equal
estimated weights. (In version 0.9.6 and earlier, the crisp and median
distance-based rows used the closest cluster in `prox_matrix` even when
`prob_matrix` was supplied.)

## Relations between the rows

For the same labels, the rows fall into the following groups.

- Monotone transformations of each other (same ranking of
  observations).:

  Each `pac` width is \\s/(2 - \|s\|)\\ of the corresponding `medoid`
  width \\s\\: `medoid`/`pac`, `pp_medoid`/`pp_pac`,
  `nlpp_medoid`/`nlpp_pac` and `pd_medoid`/`pd_pac`. With the default
  labels, `pp_medoid`, `pp_pac` and `db` are all increasing functions of
  the ratio of the two largest memberships, so they too rank
  observations identically. The averages still differ, and may peak at
  different numbers of clusters.

- Equal in special cases.:

  `pd` (PDS) equals `pp` (PPS) when all estimated cluster weights are
  equal. For fuzzy c-means with fuzzifier \\m\\, `pp_medoid` and
  `pp_pac` equal `medoid` and `pac` computed from the dissimilarities
  \\d\_{ik}^{1/(m-1)}\\, where \\d\_{ik}\\ are squared Euclidean
  distances to the prototypes (for \\m = 2\\, the `d` component of
  [`fcm`](https://rdrr.io/pkg/ppclust/man/fcm.html)).

- Genuinely different.:

  The distance-based rows (in general), `nlpp` (which also depends on
  the size of the neighboring membership), and `cer` (the largest
  membership alone, bounded below by \\1/K\\).

## Choosing the number of clusters

Computing `calSilhouette()` over \\k = 2, 3, \ldots\\ gives one curve
per row. The distance-based rows are usually read at their maximum. Bhat
Kapu and Kiruthika (2024) read the probability-based rows from the
reverse elbow drop of the curve, and the DBS from its local maxima
(Menardi, 2011). The suggested number of clusters depends on the
algorithm that produced the memberships, on the width and on the
averaging.

## References

Bhat Kapu, S., & Kiruthika. (2024). Some density-based silhouette
diagnostics for soft clustering algorithms. Communications in
Statistics: Case Studies, Data Analysis and Applications, 10(3-4),
221-238.
[doi:10.1080/23737484.2024.2408534](https://doi.org/10.1080/23737484.2024.2408534)

Menardi, G. (2011). Density-based silhouette diagnostics for clustering
methods. *Statistics and Computing*, 21(3), 295–308.
[doi:10.1007/s11222-010-9169-0](https://doi.org/10.1007/s11222-010-9169-0)

## See also

[`Silhouette`](https://kskbhat.github.io/Silhouette/reference/Silhouette.md),
[`softSilhouette`](https://kskbhat.github.io/Silhouette/reference/softSilhouette.md),
[`dbSilhouette`](https://kskbhat.github.io/Silhouette/reference/dbSilhouette.md),
[`cerSilhouette`](https://kskbhat.github.io/Silhouette/reference/cerSilhouette.md)

## Examples

``` r
if (requireNamespace("ppclust", quietly = TRUE)) {
  # Example with FCM clustering
  library(ppclust)
  data(iris)
  fcm_result <- fcm(iris[, -5], centers = 3)

  # Using matrices directly
  summary_result <- calSilhouette(
    prox_matrix = fcm_result$d,
    prob_matrix = fcm_result$u,
    proximity_type = "dissimilarity",
    print.summary = TRUE
  )
}
#> Summary of All Silhouette Methods
#> ==========================================
#>       Method     Crisp     Fuzzy    Median
#>       medoid 0.8288288 0.9297577 0.9179945
#>          pac 0.7541271 0.8799106 0.8484197
#>       pp_pac 0.7541271 0.8799106 0.8484197
#>    pp_medoid 0.8288288 0.9297577 0.9179945
#>     nlpp_pac 0.8185224 0.9304926 0.9261387
#>  nlpp_medoid 0.8749545 0.9604788 0.9616532
#>       pd_pac 0.7469894 0.8784604 0.8562152
#>    pd_medoid 0.8196600 0.9279118 0.9225377
#>          cer 0.8572481 0.9248327 0.9041529
#>           db 0.3303585 0.4206051 0.3097745

if (requireNamespace("ppclust", quietly = TRUE)) {
  # Using clustering function
  summary_result2 <- calSilhouette(
    prox_matrix = "d",
    prob_matrix = "u",
    proximity_type = "dissimilarity",
    clust_fun = ppclust::fcm,
    x = iris[, -5],
    centers = 3,
    print.summary = TRUE
  )
}
#> Summary of All Silhouette Methods
#> ==========================================
#>       Method     Crisp     Fuzzy    Median
#>       medoid 0.8288288 0.9297577 0.9179945
#>          pac 0.7541271 0.8799106 0.8484197
#>       pp_pac 0.7541271 0.8799106 0.8484197
#>    pp_medoid 0.8288288 0.9297577 0.9179945
#>     nlpp_pac 0.8185224 0.9304926 0.9261387
#>  nlpp_medoid 0.8749545 0.9604788 0.9616532
#>       pd_pac 0.7469894 0.8784604 0.8562152
#>    pd_medoid 0.8196600 0.9279118 0.9225377
#>          cer 0.8572481 0.9248327 0.9041529
#>           db 0.3303585 0.4206051 0.3097745
```
