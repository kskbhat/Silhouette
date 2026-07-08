# Calculate Extended Silhouette Width for Multi-Way Clustering

\`r lifecycle::badge('experimental')\`

Computes an extended silhouette width for multi-way clustering (e.g.,
biclustering, triclustering, or n-mode tensor clustering) by combining
silhouette widths from a list of Silhouette objects, each representing
one mode of clustering. The extended silhouette width is the weighted
average of the average silhouette widths from each mode, weighted by the
number of observations in each mode's silhouette analysis. The output is
an object of class `extSilhouette`.

## Usage

``` r
extSilhouette(sil_list, dim_names = NULL, print.summary = FALSE)
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

  Logical; if `TRUE`, prints a summary of the extended silhouette width
  and dimension table. Default is `FALSE`.

## Value

A list of class `"extSilhouette"` with the following components:

- ext_sil_width:

  A numeric scalar representing the extended silhouette width.

- dim_table:

  A data frame with columns `dimension` (e.g., "Mode 1", "Mode 2"),
  `n_obs` (number of observations), and `avg_sil_width` (average
  silhouette width for each mode).

## Details

The extended silhouette width is computed as: \$\$ ExS = \frac{ \sum
(n_i \cdot w_i) }{ \sum n_i } \$\$ where \\n_i\\ is the number of
observations in mode \\i\\ (derived from `nrow(x$widths)`), and \\w_i\\
is the average silhouette width for that mode (from `x$avg.width`). Each
`Silhouette` object in `sil_list` must contain a non-empty `widths` data
frame and a numeric `avg.width` value. Modes with zero observations
(\\n_i = 0\\) are not allowed, as they would result in an undefined
weighted average. For consistency make sure all `Silhouette` objects
derived from same \`method\` and arguments.

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
# Example using mock two-mode cluster membership matrices
# Mode 1: 150 observations, 3 clusters
set.seed(123)
prob_mode1 <- matrix(runif(150 * 3), nrow = 150, ncol = 3)
prob_mode1 <- prob_mode1 / rowSums(prob_mode1)

# Mode 2: 4 variables, 2 clusters
prob_mode2 <- matrix(runif(4 * 2), nrow = 4, ncol = 2)
prob_mode2 <- prob_mode2 / rowSums(prob_mode2)

sil_mode1 <- softSilhouette(prob_matrix = prob_mode1, method = "pac")
sil_mode2 <- softSilhouette(prob_matrix = prob_mode2, method = "pac")

# Compute Extended silhouette
ext_sil <- extSilhouette(list(sil_mode1, sil_mode2), print.summary = TRUE)
#> ---------------------------
#> Extended silhouette: 0.2202 
#> ---------------------------
#> Dimension Summary:
#>   dimension n_obs avg_sil_width
#> 1    Mode 1   150        0.2113
#> 2    Mode 2     4        0.5530
#> 
#> Available components:
#> [1] "ext_sil_width" "dim_table"    
```
