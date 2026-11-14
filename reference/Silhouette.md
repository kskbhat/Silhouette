# Calculate Silhouette Widths, Summary, and Plot for Clustering Results

Computes the simplified silhouette width of each observation from an \\n
\times K\\ matrix of proximities between observations and clusters (for
example, distances to cluster centers). The width compares the proximity
of an observation to its own cluster with its proximity to the closest
other cluster. Widths lie in \\\[-1, 1\]\\; values near 1 indicate a
clear assignment and negative values indicate that another cluster is
closer.

## Usage

``` r
Silhouette(
  prox_matrix,
  proximity_type = c("dissimilarity", "similarity"),
  method = c("medoid", "pac"),
  average = c("crisp", "fuzzy", "median"),
  prob_matrix = NULL,
  a = 2,
  sort = FALSE,
  print.summary = FALSE,
  clust_fun = NULL,
  ...,
  cluster = NULL,
  clust_args = list()
)

# S3 method for class 'Silhouette'
plot(
  x,
  label = FALSE,
  summary.legend = TRUE,
  grayscale = FALSE,
  linetype = c("dashed", "solid", "dotted", "dotdash", "longdash", "twodash"),
  ...
)

# S3 method for class 'Silhouette'
summary(object, print.summary = TRUE, ...)
```

## Arguments

- prox_matrix:

  A numeric matrix where rows represent observations and columns
  represent proximity measures (e.g., distances or similarities) to
  clusters. Typically, this is a membership or dissimilarity matrix from
  clustering results. If `clust_fun` is provided, `prox_matrix` should
  be the name of the matrix component as a string (e.g., if
  `clust_fun = `[`fcm`](https://rdrr.io/pkg/ppclust/man/fcm.html) from
  ppclust package the `prox_matrix = "d"`).

- proximity_type:

  Character string specifying the type of proximity measure in
  `prox_matrix`. Options are `"similarity"` (higher values indicate
  closer proximity) or `"dissimilarity"` (lower values indicate closer
  proximity). Defaults to `"dissimilarity"`.

- method:

  Character string specifying the normalization of the silhouette width.
  Options are `"medoid"` (divide by the larger of the two proximities)
  or `"pac"` (Probability of Alternative Cluster; divide by their sum).
  See Details. Defaults to `"medoid"`.

- average:

  Character string specifying the method for computing the average
  silhouette width. Options are:

  - `"crisp"` – unweighted (simple) average.

  - `"fuzzy"` – weighted average; the weight of an observation is the
    gap between its two largest memberships raised to the power `a`.

  - `"median"` – median silhouette width across observations.

  Defaults to `"crisp"`.

- prob_matrix:

  A numeric matrix of cluster membership probabilities, where rows
  represent observations and columns represent clusters in the same
  order as `prox_matrix`. If `clust_fun` is provided, `prob_matrix` can
  be given as the name of the matrix component (e.g., `"u"` for the
  [`fcm`](https://rdrr.io/pkg/ppclust/man/fcm.html) function). When
  given, and `cluster` is not, each observation is assigned to the
  cluster with its largest membership, and the weights for fuzzy
  averaging are computed. When `NULL` and `average = "fuzzy"`, the
  function falls back to computing the `"crisp"` silhouette with a
  warning, since fuzzy silhouette widths require membership
  probabilities. Defaults to `NULL`.

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
  for `Silhouette()` and `TRUE` for
  [`summary()`](https://rdrr.io/r/base/summary.html).

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
  observation as a column number of `prox_matrix` (\\1, \ldots, K\\).
  Use it to score a partition produced elsewhere, for example by
  hierarchical clustering or by a clustering algorithm whose labels
  differ from the nearest representative. If `clust_fun` is provided,
  `cluster` may be the name of the label component of its output (e.g.,
  `"cluster"` for [`kmeans`](https://rdrr.io/r/stats/kmeans.html)). Must
  be named in the call because it follows `...`. Defaults to `NULL`.

- clust_args:

  A list of further arguments for `clust_fun`. Arguments given through
  `...` are matched first against this function's own arguments, and R
  matches names partially: `m = 3` meant for the fuzzifier of
  [`fcm`](https://rdrr.io/pkg/ppclust/man/fcm.html) would be taken as
  `method`. Pass such arguments here instead, e.g.
  `clust_args = list(m = 3)`. Defaults to
  [`list()`](https://rdrr.io/r/base/list.html).

- x:

  An object of class `"Silhouette"`, typically the output of the
  `Silhouette`,
  [`softSilhouette`](https://kskbhat.github.io/Silhouette/reference/softSilhouette.md),
  [`dbSilhouette`](https://kskbhat.github.io/Silhouette/reference/dbSilhouette.md)
  and
  [`cerSilhouette`](https://kskbhat.github.io/Silhouette/reference/cerSilhouette.md)
  function. Also supports objects classes
  [`eclust`](https://rpkgs.datanovia.com/factoextra/reference/eclust.html),
  [`hcut`](https://rpkgs.datanovia.com/factoextra/reference/hcut.html),
  [`pam`](https://rdrr.io/pkg/cluster/man/pam.html),
  [`clara`](https://rdrr.io/pkg/cluster/man/clara.html),
  [`fanny`](https://rdrr.io/pkg/cluster/man/fanny.html),
  [`silhouette`](https://rdrr.io/pkg/cluster/man/silhouette.html), or
  `silhouette` from cluster, factoextra, drclust packages. For these
  classes, explicitly call
  [`plotSilhouette()`](https://kskbhat.github.io/Silhouette/reference/plotSilhouette.md)
  to generate the plot.

- label:

  Logical; if `TRUE`, the x-axis is labeled with observation row indices
  from the input data and titled "Row Index". Defaults to `FALSE`.

- summary.legend:

  Logical; if `TRUE`, prints a summary of average silhouette widths and
  sizes for each cluster in legend ("Cluster (Size): Width"). If
  `FALSE`, the legend shows only cluster numbers. Defaults to `TRUE`.

- grayscale:

  Logical; if `TRUE`, the plot uses a grayscale color palette for
  clusters. If `FALSE`, uses the default or specified color palette.
  Defaults to `FALSE`.

- linetype:

  Character or numeric value specifying the type of line to be used for
  the horizontal reference line indicating the average silhouette width.
  Accepts standard ggplot2 linetype values, such as:

  - Character: `"solid"`, `"dashed"`, `"dotted"`, `"dotdash"`,
    `"longdash"`, `"twodash"`.

  - Numeric: a single integer from 0 to 6 corresponding to ggplot2 line
    patterns.

  Defaults to `"dashed"`.

- object:

  An object of class `"Silhouette"`, typically the output of the
  `Silhouette`,
  [`softSilhouette`](https://kskbhat.github.io/Silhouette/reference/softSilhouette.md),
  [`dbSilhouette`](https://kskbhat.github.io/Silhouette/reference/dbSilhouette.md),
  and
  [`cerSilhouette`](https://kskbhat.github.io/Silhouette/reference/cerSilhouette.md)
  function.

## Value

A data frame of class `"Silhouette"` with one row per observation and
columns `cluster`, `neighbor`, `weight` (only when `prob_matrix` is
supplied) and `sil_width`. The object includes the following attributes:

- proximity_type:

  The proximity type used (`"similarity"` or `"dissimilarity"`).

- method:

  The silhouette calculation method used (`"medoid"` or `"pac"`).

- average:

  Character — the averaging method: `"crisp"`, `"fuzzy"`, or `"median"`.

Further, `summary` returns (invisibly) a list containing:

- `clus.avg.widths`: A named numeric vector of average silhouette widths
  per cluster.

- `avg.width`: The overall average silhouette width.

- `sil.sum`: A data frame with columns `cluster`, `size`, and
  `avg.sil.width` summarizing cluster sizes and average silhouette
  widths.

## Details

`Silhouette()` is not the same as
[`silhouette`](https://rdrr.io/pkg/cluster/man/silhouette.html) from the
cluster package: that function needs all \\n(n-1)/2\\ pairwise
dissimilarities, whereas `Silhouette()` needs only the \\n \times K\\
proximities between observations and cluster representatives (centroids,
medoids or prototypes). The two give different, although usually
similar, values.

The `Silhouette` function implements the Simplified Silhouette method
introduced by Van der Laan, Pollard, & Bryan (2003), which adapts and
generalizes the classic silhouette method of Rousseeuw (1987).

Clustering quality is evaluated using a proximity matrix, denoted as
\\\Delta = \[\delta\_{ik}\]\_{n \times K}\\ for dissimilarity measures
or \\\Delta' = \[\delta'\_{ik}\]\_{n \times K}\\ for similarity
measures. Here, \\i = 1, \ldots, n\\ indexes observations, and \\k = 1,
\ldots, K\\ indexes clusters. \\\delta\_{ik}\\ represents the
dissimilarity (e.g., distance) between observation \\i\\ and cluster
\\k\\, while \\\delta'\_{ik}\\ represents similarity values.

Let \\c_i\\ be the cluster of observation \\i\\ (see *Cluster labels*
below) and \\\nu_i\\ its neighboring cluster, the closest cluster other
than \\c_i\\. The silhouette width \\S(x_i)\\ for observation \\i\\
depends on the proximity type:

For **dissimilarity** measures: \$\$ S(x_i) = \frac{ \delta\_{i\nu_i} -
\delta\_{ic_i} }{ N(x_i) } \$\$ For **similarity** measures: \$\$ S(x_i)
= \frac{ \delta'\_{ic_i} - \delta'\_{i\nu_i} }{ N(x_i) } \$\$ where
\\N(x_i)\\ is a normalizing factor defined by the method.

**Choice of method:** The normalizer \\N(x_i)\\ is selected according to
the `method` argument:

- For `medoid` (Van der Laan et al., 2003):

  - Dissimilarity: \\\max(\delta\_{ic_i}, \delta\_{i\nu_i})\\

  - Similarity: \\\max(\delta'\_{ic_i}, \delta'\_{i\nu_i})\\

- For `pac` (Raymaekers & Rousseeuw, 2022):

  - Dissimilarity: \\\delta\_{ic_i} + \delta\_{i\nu_i}\\

  - Similarity: \\\delta'\_{ic_i} + \delta'\_{i\nu_i}\\

The names describe the normalization, not the algorithm. The name
`"medoid"` is kept for compatibility with earlier versions and with Van
der Laan et al. (2003), who introduced this width for partitioning
around medoids; it divides by the larger of the two proximities and
applies equally to centroids (as in k-means), prototypes, or any other
cluster representative. Likewise, `"pac"` does not require a classifier.
For the same labels and non-negative proximities, the PAC width equals
\\s/(2 - \|s\|)\\, where \\s\\ is the medoid width, so the two rank
observations identically and differ only in scale. These methods can
also be applied to proximities derived from **classification
algorithms**, to assess the separation of groups formed by a classifier.

Two equal proximities give a width of 0 (including two zero distances).
An infinite proximity to the neighboring cluster gives a width of 1 and
an infinite proximity to the assigned cluster gives -1.

## Cluster labels

The cluster \\c_i\\ of each observation is taken, in order of
precedence, from

1.  `cluster`, when supplied;

2.  the largest membership in `prob_matrix`, when supplied;

3.  the closest cluster in `prox_matrix`.

The neighboring cluster \\\nu_i\\ is always the closest cluster other
than \\c_i\\ in the proximity matrix. When the labels come from the
proximity matrix itself (case 3), every width is non-negative, so a
poorly assigned observation shows up only as a width near 0. When the
labels come from elsewhere (cases 1 and 2), an observation that is
closer to another cluster than to its own receives a negative width. To
assess the partition produced by a clustering algorithm, supply that
partition through `cluster` or `prob_matrix`.

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

## Input checks and ties

`prox_matrix` must be a numeric matrix with at least two columns and no
missing values; `+Inf` is allowed (for example, \\-\log 0\\) but `-Inf`
is not. Negative proximities are rejected, because the widths would then
no longer lie in \\\[-1, 1\]\\; transform them first (for example,
\\(1 + r)/2\\ for correlations). `prob_matrix` must be non-negative with
rows summing to 1 within a tolerance of `sqrt(.Machine$double.eps)`
(about \\1.5 \times 10^{-8}\\); rows are not renormalized.

The closest cluster, the neighboring cluster and the largest membership
are found with
[`max.col`](https://rdrr.io/r/base/maxCol.html)`(..., ties.method = "first")`.
Exactly tied proximities or memberships are therefore resolved in favor
of the column that comes first, so with exact ties the labels (though
not the widths from the proximity matrix itself, which are then 0)
depend on the order of the columns. Supply `cluster` to control the
assignment of tied observations.

## References

Rousseeuw, P. J. (1987). Silhouettes: A graphical aid to the
interpretation and validation of cluster analysis. *Journal of
Computational and Applied Mathematics*, 20, 53–65.
[doi:10.1016/0377-0427(87)90125-7](https://doi.org/10.1016/0377-0427%2887%2990125-7)

Van der Laan, M., Pollard, K., & Bryan, J. (2003). A new partitioning
around medoids algorithm. *Journal of Statistical Computation and
Simulation*, 73(8), 575–584.
[doi:10.1080/0094965031000136012](https://doi.org/10.1080/0094965031000136012)

Campello, R. J., & Hruschka, E. R. (2006). A fuzzy extension of the
silhouette width criterion for cluster analysis. *Fuzzy Sets and
Systems*, 157(21), 2858–2875.
[doi:10.1016/j.fss.2006.07.006](https://doi.org/10.1016/j.fss.2006.07.006)

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

[`softSilhouette`](https://kskbhat.github.io/Silhouette/reference/softSilhouette.md),
[`dbSilhouette`](https://kskbhat.github.io/Silhouette/reference/dbSilhouette.md),
[`cerSilhouette`](https://kskbhat.github.io/Silhouette/reference/cerSilhouette.md),
[`calSilhouette`](https://kskbhat.github.io/Silhouette/reference/calSilhouette.md),
[`getSilhouette`](https://kskbhat.github.io/Silhouette/reference/getSilhouette.md),
[`is.Silhouette`](https://kskbhat.github.io/Silhouette/reference/is.Silhouette.md),
[`plotSilhouette`](https://kskbhat.github.io/Silhouette/reference/plotSilhouette.md),
[`silhouette`](https://rdrr.io/pkg/cluster/man/silhouette.html)

## Examples

``` r
# Simplified silhouette of a k-means partition of the iris data
data(iris)
out <- kmeans(iris[, -5], 3)
if (requireNamespace("proxy", quietly = TRUE)) {
  d <- proxy::dist(iris[, -5], out$centers)
  silh_out <- Silhouette(d, print.summary = TRUE)
  plot(silh_out)

  # Score a partition produced elsewhere: average-linkage clustering,
  # with the distances to the centroids of its groups
  hc <- cutree(hclust(dist(iris[, -5]), "average"), 3)
  centers <- apply(iris[, -5], 2, function(v) tapply(v, hc, mean))
  silh_hc <- Silhouette(proxy::dist(iris[, -5], centers), cluster = hc)
  summary(silh_hc)
} else {
  message("Install 'proxy': install.packages('proxy')")
}
#> -----------------------------------------------------
#> Average crisp dissimilarity medoid silhouette: 0.6664
#> -----------------------------------------------------
#> 
#>  cluster size avg.sil.width
#>        1   50        0.8592
#>        2   62        0.5546
#>        3   38        0.5950
#> -----------------------------------------------------
#> Average crisp dissimilarity medoid silhouette: 0.6662
#> -----------------------------------------------------
#> 
#>  cluster size avg.sil.width
#>        1   50        0.8601
#>        2   64        0.5484
#>        3   36        0.6064
# \donttest{
# Scree plot for optimal clusters (2 to 7)
if (requireNamespace("ppclust", quietly = TRUE)) {
  library(ppclust)
  avg_sil_width <- rep(NA,7)
  for (k in 2:7) {
    out <- Silhouette(
      prox_matrix = "d",
      proximity_type = "dissimilarity",
      prob_matrix = "u",
      clust_fun = ppclust::fcm,
      x = iris[, 1:4],
      centers = k,
      average = "fuzzy"
    )
    # Compute average silhouette width from widths
    avg_sil_width[k] <- summary(out, print.summary = FALSE)$avg.width
  }
  plot(avg_sil_width,
    type = "o",
    ylab = "Overall Silhouette Width",
    xlab = "Number of Clusters",
    main = "Scree Plot"
  )
} else {
  message("Install 'ppclust': install.packages('ppclust')")
}

# }
```
