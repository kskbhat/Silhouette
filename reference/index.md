# Package index

## Widths from distances or similarities

Simplified silhouette widths from an n x K matrix of proximities between
observations and clusters, with medoid or PAC normalization.

- [`Silhouette()`](https://kskbhat.github.io/Silhouette/reference/Silhouette.md)
  [`plot(`*`<Silhouette>`*`)`](https://kskbhat.github.io/Silhouette/reference/Silhouette.md)
  [`summary(`*`<Silhouette>`*`)`](https://kskbhat.github.io/Silhouette/reference/Silhouette.md)
  : Calculate Silhouette Widths, Summary, and Plot for Clustering
  Results

## Widths from membership probabilities

Silhouette widths for soft and model-based clustering, computed from
membership or posterior probabilities.

- [`softSilhouette()`](https://kskbhat.github.io/Silhouette/reference/softSilhouette.md)
  : Calculate Silhouette Width for Soft Clustering Algorithms
- [`cerSilhouette()`](https://kskbhat.github.io/Silhouette/reference/cerSilhouette.md)
  : Certainty Silhouette Width (CeS) for Soft Clustering
- [`dbSilhouette()`](https://kskbhat.github.io/Silhouette/reference/dbSilhouette.md)
  : Density-Based Silhouette Width (DBS) for Soft Clustering

## All widths at once

- [`calSilhouette()`](https://kskbhat.github.io/Silhouette/reference/calSilhouette.md)
  : Calculate All Available Silhouette Summaries

## Block and multi-way clustering

- [`extSilhouette()`](https://kskbhat.github.io/Silhouette/reference/extSilhouette.md)
  [`format(`*`<extSilhouette>`*`)`](https://kskbhat.github.io/Silhouette/reference/extSilhouette.md)
  [`print(`*`<extSilhouette>`*`)`](https://kskbhat.github.io/Silhouette/reference/extSilhouette.md)
  **\[experimental\]** : Calculate Extended Silhouette Width for
  Multi-Way Clustering

## Silhouette objects and plots

- [`plotSilhouette()`](https://kskbhat.github.io/Silhouette/reference/plotSilhouette.md)
  : Plot Silhouette Analysis Results
- [`getSilhouette()`](https://kskbhat.github.io/Silhouette/reference/getSilhouette.md)
  : Create Silhouette Object from User Components
- [`is.Silhouette()`](https://kskbhat.github.io/Silhouette/reference/is.Silhouette.md)
  : Check if Object is of Class Silhouette
