# Create Silhouette Object from User Components

Constructs a Silhouette class object directly from user-provided
components without performing silhouette calculations. This function
allows users to build a Silhouette object when they already have the
necessary components.

## Usage

``` r
getSilhouette(
  cluster,
  neighbor,
  sil_width,
  weight = NULL,
  proximity_type = c("dissimilarity", "similarity"),
  method = NA,
  average = c("crisp", "fuzzy", "median")
)
```

## Arguments

- cluster:

  Numeric or integer vector of cluster assignments for each observation

- neighbor:

  Numeric or integer vector of nearest neighbor cluster assignments for
  each observation

- sil_width:

  Numeric vector of silhouette widths for each observation (must be
  between -1 and +1)

- weight:

  Numeric vector of weights for each observation (must be between 0 and
  1, only used when average = "fuzzy")

- proximity_type:

  Character; the proximity type used. Options: "similarity" or
  "dissimilarity"

- method:

  Character; the silhouette calculation method used (default: NULL, can
  be any custom name)

- average:

  Character; the averaging method. Options: "crisp", "fuzzy", or
  "median"

## Value

A data frame of class `"Silhouette"` containing cluster assignments,
nearest neighbor clusters, silhouette widths for each observation, and
weights (for fuzzy clustering). The object includes the following
attributes:

- proximity_type:

  The proximity type used (`"similarity"` or `"dissimilarity"`).

- method:

  The silhouette calculation method used (`"medoid"` or `"pac"`).

- average:

  Character — the averaging method: `"crisp"`, `"fuzzy"`, or `"median"`.

## See also

[`Silhouette`](https://kskbhat.github.io/Silhouette/reference/Silhouette.md),
[`softSilhouette`](https://kskbhat.github.io/Silhouette/reference/softSilhouette.md),
[`dbSilhouette`](https://kskbhat.github.io/Silhouette/reference/dbSilhouette.md),
[`cerSilhouette`](https://kskbhat.github.io/Silhouette/reference/cerSilhouette.md),
[`is.Silhouette`](https://kskbhat.github.io/Silhouette/reference/is.Silhouette.md),
[`plotSilhouette`](https://kskbhat.github.io/Silhouette/reference/plotSilhouette.md)

## Examples

``` r
# Create a simple crisp Silhouette object (3 columns)
cluster_assignments <- c(1, 1, 2, 2, 3, 3)
neighbor_clusters <- c(2, 2, 1, 1, 1, 1)
silhouette_widths <- c(0.8, 0.7, 0.6, 0.9, 0.5, 0.4)

sil_obj <- getSilhouette(
  cluster = cluster_assignments,
  neighbor = neighbor_clusters,
  sil_width = silhouette_widths,
  proximity_type = "dissimilarity",
  method = "medoid",
  average = "crisp"
)
sil_obj
#>   cluster neighbor sil_width
#> 1       1        2       0.8
#> 2       1        2       0.7
#> 3       2        1       0.6
#> 4       2        1       0.9
#> 5       3        1       0.5
#> 6       3        1       0.4

# Create a fuzzy Silhouette object with weights (4 columns)
weights <- c(0.9, 0.8, 0.7, 0.95, 0.6, 0.5)

sil_fuzzy <- getSilhouette(
  cluster = cluster_assignments,
  neighbor = neighbor_clusters,
  sil_width = silhouette_widths,
  weight = weights,
  proximity_type = "similarity",
  method = "pac",
  average = "fuzzy"
)
sil_fuzzy
#>   cluster neighbor sil_width weight
#> 1       1        2       0.8   0.90
#> 2       1        2       0.7   0.80
#> 3       2        1       0.6   0.70
#> 4       2        1       0.9   0.95
#> 5       3        1       0.5   0.60
#> 6       3        1       0.4   0.50

# Custom method name
sil_custom <- getSilhouette(
  cluster = cluster_assignments,
  neighbor = neighbor_clusters,
  sil_width = silhouette_widths,
  proximity_type = "dissimilarity",
  method = "my_custom_method",
  average = "crisp"
)
sil_custom
#>   cluster neighbor sil_width
#> 1       1        2       0.8
#> 2       1        2       0.7
#> 3       2        1       0.6
#> 4       2        1       0.9
#> 5       3        1       0.5
#> 6       3        1       0.4
```
