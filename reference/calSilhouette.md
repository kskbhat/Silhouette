# Compute Calculate of All Possible Silhouette Methods

Computes all possible silhouette indices from available functions in the
package and returns a summary data frame comparing crisp, fuzzy, and
median silhouette values across different methods.

## Usage

``` r
calSilhouette(
  prox_matrix = NULL,
  proximity_type = c("dissimilarity", "similarity"),
  prob_matrix = NULL,
  a = 2,
  print.summary = FALSE,
  clust_fun = NULL,
  ...
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
  ppclust package the `prox_matrix = "d"`).

- proximity_type:

  Character string specifying the type of proximity measure in
  `prox_matrix`. Options are `"similarity"` (higher values indicate
  closer proximity) or `"dissimilarity"` (lower values indicate closer
  proximity). Defaults to `"dissimilarity"`.

- prob_matrix:

  A numeric matrix of cluster membership probabilities, where rows
  represent observations and columns represent clusters (depending on
  `prob_type`). If `clust_fun` is provided, `prob_matrix` can be given
  as the name of the matrix component (e.g., `"u"` for the
  [`fcm`](https://rdrr.io/pkg/ppclust/man/fcm.html) function). Defaults
  to `NULL`.

- a:

  Numeric value controlling the fuzzifier or weight scaling in fuzzy
  silhouette averaging. Higher values increase the emphasis on strong
  membership differences. Must be positive. Defaults to `2`.

- print.summary:

  Logical; if `TRUE`, prints a summary table of average silhouette
  widths and sizes for each cluster. Defaults to `FALSE`.

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

## Value

A data frame with the following columns:

- Method:

  Character vector of method names

- Crisp_Silhouette:

  Numeric vector of crisp (unweighted) average silhouette values

- Fuzzy_Silhouette:

  Numeric vector of fuzzy (weighted) average silhouette values (NA if
  `prob_matrix` is not available for the method)

- Median_Silhouette:

  Numeric vector of median silhouette values

## Details

This function computes all available silhouette methods from the package
and returns a comparative summary. The methods included depend on the
available input matrices:

\*\*If `prox_matrix` is available:\*\*

- `medoid` - Medoid-based silhouette using
  [`Silhouette`](https://kskbhat.github.io/Silhouette/reference/Silhouette.md)

- `pac` - PAC-based silhouette using
  [`Silhouette`](https://kskbhat.github.io/Silhouette/reference/Silhouette.md)

\*\*If `prob_matrix` is available:\*\*

- `pp_pac` - Posterior probabilities silhouette with PAC method using
  [`softSilhouette`](https://kskbhat.github.io/Silhouette/reference/softSilhouette.md)

- `pp_medoid` - Posterior probabilities silhouette with Medoid method
  using
  [`softSilhouette`](https://kskbhat.github.io/Silhouette/reference/softSilhouette.md)

- `nlpp_pac` - Negative log posterior probabilities silhouette with PAC
  method using
  [`softSilhouette`](https://kskbhat.github.io/Silhouette/reference/softSilhouette.md)

- `nlpp_medoid` - Negative log posterior probabilities silhouette with
  Medoid method using
  [`softSilhouette`](https://kskbhat.github.io/Silhouette/reference/softSilhouette.md)

- `pd_pac` - Probability distribution silhouette with PAC method using
  [`softSilhouette`](https://kskbhat.github.io/Silhouette/reference/softSilhouette.md)

- `pd_medoid` - Probability distribution silhouette with Medoid method
  using
  [`softSilhouette`](https://kskbhat.github.io/Silhouette/reference/softSilhouette.md)

- `cer` - Certainty-based silhouette using
  [`cerSilhouette`](https://kskbhat.github.io/Silhouette/reference/cerSilhouette.md)

- `db` - Density-based silhouette using
  [`dbSilhouette`](https://kskbhat.github.io/Silhouette/reference/dbSilhouette.md)

At least one of `prox_matrix` or `prob_matrix` must be provided.

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
#> 
#> Summary of All Silhouette Methods
#> ========================================== 
#>       Method     Crisp     Fuzzy    Median
#>       medoid 0.8288288 0.9297577 0.9179945
#>          pac 0.7541271 0.8799106 0.8484197
#>       pp_pac 0.7541271 0.8799106 0.8484197
#>    pp_medoid 0.8288288 0.9297577 0.9179945
#>     nlpp_pac 0.8185224 0.9304926 0.9261387
#>  nlpp_medoid 0.8749545 0.9604788 0.9616532
#>       pd_pac 0.7469894 0.8786105 0.8562152
#>    pd_medoid 0.8196600 0.9280044 0.9225377
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
#> 
#> Summary of All Silhouette Methods
#> ========================================== 
#>       Method     Crisp     Fuzzy    Median
#>       medoid 0.8288288 0.9297577 0.9179945
#>          pac 0.7541271 0.8799106 0.8484197
#>       pp_pac 0.7541271 0.8799106 0.8484197
#>    pp_medoid 0.8288288 0.9297577 0.9179945
#>     nlpp_pac 0.8185224 0.9304926 0.9261387
#>  nlpp_medoid 0.8749545 0.9604788 0.9616532
#>       pd_pac 0.7469894 0.8786105 0.8562152
#>    pd_medoid 0.8196600 0.9280044 0.9225377
#>          cer 0.8572481 0.9248327 0.9041529
#>           db 0.3303585 0.4206051 0.3097745
```
