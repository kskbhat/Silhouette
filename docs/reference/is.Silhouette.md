# Check if Object is of Class Silhouette

Tests whether an object is of class "Silhouette". This function checks
both the class inheritance and the expected structure of a Silhouette
object.

## Usage

``` r
is.Silhouette(x, strict = FALSE)
```

## Arguments

- x:

  An object to test

- strict:

  Logical; if TRUE, performs additional structural validation beyond
  just class checking (default: FALSE)

## Value

Logical; TRUE if the object is of class "Silhouette", FALSE otherwise

## Details

When `strict = FALSE`, the function only checks if the object inherits
from the "Silhouette" class.

When `strict = TRUE`, the function additionally validates:

- Object is a data frame

- Has required columns: cluster, neighbor, sil_width

- Has required attributes: proximity_type, method, average

- Column types are appropriate (integer for cluster/neighbor, numeric
  for sil_width)

The Silhouette object attributes are validated as follows:

- `proximity_type`: Must be one of "dissimilarity" or "similarity"

- `average`: Must be one of "crisp", "fuzzy", or "median"

- `method`: Can be NULL or any string

## See also

[`Silhouette`](https://kskbhat.github.io/Silhouette/reference/Silhouette.md),
[`softSilhouette`](https://kskbhat.github.io/Silhouette/reference/softSilhouette.md),
[`dbSilhouette`](https://kskbhat.github.io/Silhouette/reference/dbSilhouette.md),
[`cerSilhouette`](https://kskbhat.github.io/Silhouette/reference/cerSilhouette.md),
[`getSilhouette`](https://kskbhat.github.io/Silhouette/reference/getSilhouette.md),
[`plotSilhouette`](https://kskbhat.github.io/Silhouette/reference/plotSilhouette.md)

## Examples

``` r
# Create a Silhouette object
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

# Test if object is Silhouette
is.Silhouette(sil_obj)          # TRUE
#> [1] TRUE
is.Silhouette(sil_obj, strict = TRUE)  # TRUE
#> [1] TRUE

# Test with non-Silhouette objects
is.Silhouette(data.frame(a = 1, b = 2))  # FALSE
#> [1] FALSE
is.Silhouette(matrix(1:10, ncol = 2))    # FALSE
#> [1] FALSE
is.Silhouette(list(a = 1, b = 2))        # FALSE
#> [1] FALSE
is.Silhouette(NULL)                       # FALSE
#> [1] FALSE
```
