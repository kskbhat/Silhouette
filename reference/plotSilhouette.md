# Plot Silhouette Analysis Results

Creates a silhouette plot for visualizing the silhouette widths of
clustering results, with bars colored by cluster and an optional summary
of cluster statistics in legend.

## Usage

``` r
plotSilhouette(
  x,
  label = FALSE,
  summary.legend = TRUE,
  grayscale = FALSE,
  linetype = c("dashed", "solid", "dotted", "dotdash", "longdash", "twodash"),
  ...
)
```

## Arguments

- x:

  An object of class `"Silhouette"`, typically the output of the
  [`Silhouette`](https://kskbhat.github.io/Silhouette/reference/Silhouette.md),
  [`softSilhouette`](https://kskbhat.github.io/Silhouette/reference/softSilhouette.md),
  [`dbSilhouette`](https://kskbhat.github.io/Silhouette/reference/dbSilhouette.md)
  and
  [`cerSilhouette`](https://kskbhat.github.io/Silhouette/reference/cerSilhouette.md)
  function. Also supports objects classes
  [`eclust`](https://rdrr.io/pkg/factoextra/man/eclust.html),
  [`hcut`](https://rdrr.io/pkg/factoextra/man/hcut.html),
  [`pam`](https://rdrr.io/pkg/cluster/man/pam.html),
  [`clara`](https://rdrr.io/pkg/cluster/man/clara.html),
  [`fanny`](https://rdrr.io/pkg/cluster/man/fanny.html),
  [`silhouette`](https://rdrr.io/pkg/cluster/man/silhouette.html), or
  `silhouette` from cluster, factoextra, drclust packages. For these
  classes, explicitly call `plotSilhouette()` to generate the plot.

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

  - Character: \`"solid"\`, \`"dashed"\`, \`"dotted"\`, \`"dotdash"\`,
    \`"longdash"\`, \`"twodash"\`.

  - Numeric: integers from 0 to 6 corresponding to ggplot2 line
    patterns.

  Defaults to \`"dashed"\`.

- ...:

  Additional arguments passed to
  [`ggpar`](https://rpkgs.datanovia.com/ggpubr/reference/ggpar.html) for
  customizing the plot (e.g., `palette`, `legend`, `xlab`, `ylab`,
  `subtitle`, `title`).

## Value

A `ggplot2` object representing the Silhouette plot.

## Details

The Silhouette plot displays the silhouette width (`sil_width`) for each
observation, grouped by cluster, with bars sorted by cluster and
descending silhouette width. The `summary.legend` option adds cluster
sizes and average silhouette widths to the legend.

This function replica of S3 method for objects of class `"Silhouette"`,
typically produced by the
[`Silhouette`](https://kskbhat.github.io/Silhouette/reference/Silhouette.md),
[`softSilhouette`](https://kskbhat.github.io/Silhouette/reference/softSilhouette.md),
,
[`dbSilhouette`](https://kskbhat.github.io/Silhouette/reference/dbSilhouette.md)
or ,
[`cerSilhouette`](https://kskbhat.github.io/Silhouette/reference/cerSilhouette.md)
functions in this package. It also supports objects of the following
classes, with silhouette information extracted from their respective
component:

- `"eclust"`: Produced by
  [`eclust`](https://rdrr.io/pkg/factoextra/man/eclust.html) from the
  factoextra package.

- `"hcut"`: Produced by
  [`hcut`](https://rdrr.io/pkg/factoextra/man/hcut.html) from the
  factoextra package.

- `"pam"`: Produced by [`pam`](https://rdrr.io/pkg/cluster/man/pam.html)
  from the cluster package.

- `"clara"`: Produced by
  [`clara`](https://rdrr.io/pkg/cluster/man/clara.html) from the cluster
  package.

- `"fanny"`: Produced by
  [`fanny`](https://rdrr.io/pkg/cluster/man/fanny.html) from the cluster
  package.

- `"silhouette"`: Produced by
  [`silhouette`](https://rdrr.io/pkg/cluster/man/silhouette.html) from
  the cluster package or `silhouette` from the drclust package.

For these classes (`"eclust"`, `"hcut"`, `"pam"`, `"clara"`, `"fanny"`,
`"silhouette"`), users should explicitly call `plotSilhouette()` (e.g.,
`plotSilhouette(pam_result)`) to ensure the correct method is used, as
the generic [`plot()`](https://rdrr.io/r/graphics/plot.default.html) may
not dispatch to this function for these objects.

## References

Rousseeuw, P. J. (1987). Silhouettes: A graphical aid to the
interpretation and validation of cluster analysis. *Journal of
Computational and Applied Mathematics*, 20, 53–65.
[doi:10.1016/0377-0427(87)90125-7](https://doi.org/10.1016/0377-0427%2887%2990125-7)

## See also

[`Silhouette`](https://kskbhat.github.io/Silhouette/reference/Silhouette.md),
[`softSilhouette`](https://kskbhat.github.io/Silhouette/reference/softSilhouette.md),
[`dbSilhouette`](https://kskbhat.github.io/Silhouette/reference/dbSilhouette.md),
[`cerSilhouette`](https://kskbhat.github.io/Silhouette/reference/cerSilhouette.md),
[`getSilhouette`](https://kskbhat.github.io/Silhouette/reference/getSilhouette.md),
[`is.Silhouette`](https://kskbhat.github.io/Silhouette/reference/is.Silhouette.md)

## Examples

``` r
# \donttest{
data(iris)

# Crisp Silhouette with k-means
out <- kmeans(iris[, -5], 3)
if (requireNamespace("proxy", quietly = TRUE)) {
  library(proxy)
  dist <- dist(iris[, -5], out$centers)
  plot(Silhouette(dist))
}


#' # Fuzzy Silhouette with ppclust::fcm
if (requireNamespace("ppclust", quietly = TRUE)) {
  library(ppclust)
  out_fuzzy <- Silhouette(
    prox_matrix = "d",
    proximity_type = "dissimilarity",
    prob_matrix = "u",
    clust_fun = ppclust::fcm,
    x = iris[, 1:4],
    centers = 3,
    sort = TRUE
  )
  plot(out_fuzzy, summary.legend = FALSE, grayscale = TRUE)
} else {
  message("Install 'ppclust': install.packages('ppclust')")
}


# Silhouette plot for pam clustering
if (requireNamespace("cluster", quietly = TRUE)) {
  library(cluster)
  pam_result <- pam(iris[, 1:4], k = 3)
  plotSilhouette(pam_result)
}


# Silhouette plot for clara clustering
if (requireNamespace("cluster", quietly = TRUE)) {
  clara_result <- clara(iris[, 1:4], k = 3)
  plotSilhouette(clara_result)
}


# Silhouette plot for fanny clustering
if (requireNamespace("cluster", quietly = TRUE)) {
  fanny_result <- fanny(iris[, 1:4], k = 3)
  plotSilhouette(fanny_result)
}


# Example using base silhouette() object
if (requireNamespace("cluster", quietly = TRUE)) {
  sil <- silhouette(pam_result)
  plotSilhouette(sil)
}


# Silhouette plot for eclust clustering
if (requireNamespace("factoextra", quietly = TRUE)) {
  library(factoextra)
  eclust_result <- eclust(iris[, 1:4], "kmeans", k = 3, graph = FALSE)
  plotSilhouette(eclust_result)
}
#> Loading required package: ggplot2
#> Welcome to factoextra!
#> Want to learn more? See two factoextra-related books at https://www.datanovia.com/en/product/practical-guide-to-principal-component-methods-in-r/


# Silhouette plot for hcut clustering
if (requireNamespace("factoextra", quietly = TRUE)) {
  hcut_result <- hcut(iris[, 1:4], k = 3)
  plotSilhouette(hcut_result)
}


# Silhouette plot for hcut clustering
if (requireNamespace("drclust", quietly = TRUE)) {
  library(drclust)
  iris_mat <- as.matrix(iris[,-5])
  drclust_out <- dpcakm(iris_mat, 20, 3)
  d <- silhouette(iris_mat, drclust_out)
  plotSilhouette(d$cl.silhouette)
}
#> 
#> Attaching package: ‘drclust’
#> The following object is masked from ‘package:cluster’:
#> 
#>     silhouette
#>    cluster size ave.sil.width
#> 1        1   20          0.17
#> 2        2   16          0.36
#> 3        3   12         -0.03
#> 4        4   12          0.11
#> 5        5   11          0.21
#> 6        6   10          0.14
#> 7        7    9          0.23
#> 8        8    8          0.40
#> 9        9    8          0.16
#> 10      10    7          0.15
#> 11      11    6          0.27
#> 12      12    5          0.18
#> 13      13    5          0.09
#> 14      14    5         -0.14
#> 15      15    4          0.26
#> 16      16    4          0.19
#> 17      17    3         -0.35
#> 18      18    3          0.29
#> 19      19    1          0.00
#> 20      20    1          0.00

# }
```
