# Calculate Extended Silhouette Width for Multi-Way Clustering

**\[experimental\]**

Computes an extended silhouette width for multi-way clustering (e.g.,
biclustering, triclustering, or n-mode tensor clustering) by combining
silhouette widths from a list of Silhouette objects, each representing
one mode of clustering. The extended silhouette width is a weighted
average of the average silhouette widths of the modes. By default each
mode is weighted by its number of objects; equal or user-specified
weights are also available. The output is an object of class
`extSilhouette`.

## Usage

``` r
extSilhouette(
  sil_list,
  dim_names = NULL,
  print.summary = FALSE,
  weights = c("size", "equal")
)

# S3 method for class 'extSilhouette'
format(x, digits = 4, ...)

# S3 method for class 'extSilhouette'
print(x, digits = 4, ...)
```

## Arguments

- sil_list:

  A list of objects of class `"Silhouette"`, typically the output of
  [`Silhouette`](https://kskbhat.github.io/Silhouette/reference/Silhouette.md)
  or
  [`softSilhouette`](https://kskbhat.github.io/Silhouette/reference/softSilhouette.md),
  where each object represents the silhouette analysis for one mode of
  multi-way clustering (e.g., rows, columns, or other dimensions in
  biclustering or tensor clustering).

- dim_names:

  An optional character vector of dimension names (e.g.,
  `c("Rows", "Columns")`). If `NULL`, defaults to `"Mode 1"`,
  `"Mode 2"`, etc.

- print.summary:

  Logical; if `TRUE`, reports the extended silhouette width and the
  dimension table through
  [`message`](https://rdrr.io/r/base/message.html). Default is `FALSE`.

- weights:

  How the modes are weighted: `"size"` (the default) weights each mode
  by its number of objects, `"equal"` gives every mode the same weight,
  and a non-negative numeric vector with one entry per mode gives
  user-specified weights (they are rescaled to sum to 1).

- x:

  An object of class `"extSilhouette"`.

- digits:

  Number of decimal places shown.

- ...:

  Further arguments, currently ignored.

## Value

A list of class `"extSilhouette"` with the following components:

- ext_sil_width:

  A numeric scalar, the extended silhouette width.

- dim_table:

  A data frame with columns `dimension` (e.g., "Mode 1", "Mode 2"),
  `n_obs` (number of objects), `weight` (the share of the total weight
  given to the mode) and `avg_sil_width` (average silhouette width of
  the mode).

- weights:

  The weighting used: `"size"`, `"equal"` or `"user"`.

Values are not rounded; the print method shows four decimals.

## Details

The extended silhouette width is computed as: \$\$ ExS = \frac{
\sum\_{i} \omega_i \bar{S}\_i }{ \sum\_{i} \omega_i } \$\$ where
\\\bar{S}\_i\\ is the average silhouette width of mode \\i\\ (from
`summary(x)$avg.width`, so it follows the averaging rule of that object)
and \\\omega_i\\ is its weight. With `weights = "size"`, \\\omega_i =
n_i\\, the number of objects in mode \\i\\ (`nrow(x)`), as in Schepers
et al. (2008); ExS is then the average width over all objects of all
modes, each object counting once. With `weights = "equal"`, \\\omega_i =
1\\, so ExS is the mean of the mode-wise averages.

Size weighting treats every clustered object alike, but when the modes
differ greatly in size the largest mode dominates: with 200 rows and 30
columns, the rows receive about 87\\ of the weight. Equal weighting
gives every mode the same say regardless of its size. Because the choice
can change which model is selected, report the mode-wise averages in
`dim_table` together with ExS.

Each `Silhouette` object in `sil_list` must contain at least one
observation and have a finite average width. For consistency, compute
all `Silhouette` objects with the same function, `method` and averaging
rule.

## References

Schepers, J., Ceulemans, E., & Van Mechelen, I. (2008). Selecting among
multi-mode partitioning models of different complexities: A comparison
of four model selection criteria. *Journal of Classification*, 25(1),
67–85.
[doi:10.1007/s00357-008-9005-9](https://doi.org/10.1007/s00357-008-9005-9)

Bhat Kapu, S., & Kiruthika, C. (2025). Block Probabilistic Distance
Clustering: A Unified Framework and Evaluation. PREPRINT (Version 1)
available at Research Square.
[doi:10.21203/rs.3.rs-6973596/v1](https://doi.org/10.21203/rs.3.rs-6973596/v1)

## See also

[`Silhouette`](https://kskbhat.github.io/Silhouette/reference/Silhouette.md),
[`softSilhouette`](https://kskbhat.github.io/Silhouette/reference/softSilhouette.md),
[`dbSilhouette`](https://kskbhat.github.io/Silhouette/reference/dbSilhouette.md),
[`cerSilhouette`](https://kskbhat.github.io/Silhouette/reference/cerSilhouette.md),
[`getSilhouette`](https://kskbhat.github.io/Silhouette/reference/getSilhouette.md),
[`is.Silhouette`](https://kskbhat.github.io/Silhouette/reference/is.Silhouette.md)

## Examples

``` r
# Two modes: 150 objects in 3 clusters and 4 variables in 2 clusters
set.seed(123)
prob_mode1 <- matrix(runif(150 * 3), nrow = 150, ncol = 3)
prob_mode1 <- prob_mode1 / rowSums(prob_mode1)
prob_mode2 <- matrix(runif(4 * 2), nrow = 4, ncol = 2)
prob_mode2 <- prob_mode2 / rowSums(prob_mode2)

sil_mode1 <- softSilhouette(prob_matrix = prob_mode1, method = "pac")
sil_mode2 <- softSilhouette(prob_matrix = prob_mode2, method = "pac")

# Size-weighted (default) and equally weighted extended silhouette
extSilhouette(list(sil_mode1, sil_mode2), dim_names = c("Rows", "Columns"))
#> ------------------------------------------
#> Extended silhouette (size weights): 0.2202
#> ------------------------------------------
#> Dimension summary:
#>  dimension n_obs weight avg_sil_width
#>       Rows   150  0.974        0.2113
#>    Columns     4  0.026        0.5530
extSilhouette(list(sil_mode1, sil_mode2), dim_names = c("Rows", "Columns"),
              weights = "equal")
#> -------------------------------------------
#> Extended silhouette (equal weights): 0.3822
#> -------------------------------------------
#> Dimension summary:
#>  dimension n_obs weight avg_sil_width
#>       Rows   150    0.5        0.2113
#>    Columns     4    0.5        0.5530
```
