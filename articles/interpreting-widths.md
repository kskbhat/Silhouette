# Interpreting silhouette widths

Silhouette widths look alike but mean different things depending on how
they were computed. This vignette explains how the widths in
**Silhouette** relate to the classical silhouette, what ranges they
take, and which comparisons between them are valid.

``` r

library(Silhouette)
X <- as.matrix(iris[, 1:4])
set.seed(1)
km <- kmeans(X, centers = 3, nstart = 20)
P <- as.matrix(proxy::dist(X, km$centers))   # distances to the 3 centers
```

## Classical and simplified widths

The classical width of Rousseeuw (1987) compares the *average*
dissimilarity of observation \\i\\ to the other members of its cluster,
\\a_i\\, with the smallest average dissimilarity to the members of
another cluster, \\b_i\\: \\ s_i = \frac{b_i - a_i}{\max(a_i, b_i)}. \\
It needs all \\n(n-1)/2\\ pairwise dissimilarities. The simplified
silhouette of Van der Laan et al. (2003) replaces the two averages by
the dissimilarities between \\x_i\\ and a representative of each
cluster, such as its centroid or medoid, so it needs only an \\n \times
K\\ matrix. That is what
[`Silhouette()`](https://kskbhat.github.io/Silhouette/reference/Silhouette.md)
computes.

The two are related but not equal:

``` r

s_simple <- Silhouette(P)$sil_width
s_classic <- cluster::silhouette(km$cluster, dist(X))[, "sil_width"]
c(correlation = cor(s_simple, s_classic),
  mean_simplified = mean(s_simple),
  mean_simplified_pac = mean(Silhouette(P, method = "pac")$sil_width),
  mean_classical = mean(s_classic))
#>         correlation     mean_simplified mean_simplified_pac      mean_classical 
#>           0.9538537           0.6663856           0.5375927           0.5528190
```

The simplified widths rank observations much as the classical ones do,
but the simplified average (medoid normalization) is higher, because the
distance to a centroid is smaller than the average distance to all
members of a cluster. The PAC average is usually closer to the classical
level. Thresholds published for the classical average, such as “above
0.5 is reasonable structure”, therefore do not transfer to the
simplified average. If you need a fast approximation of the classical
width itself for a crisp partition, `approxSilhouette()` in the
Bioconductor package **bluster** (Lun 2023) is designed for that; the
strengths of **Silhouette** are soft and model-based clusterings,
arbitrary cluster-level proximities, and multi-way partitions.

## Three properties of the simplified widths

Write \\a_i\\ and \\b_i\\ for the proximities to the assigned and
neighboring clusters, and \\r_i = a_i / b_i\\ for dissimilarities.

**1. With labels from the same matrix, the widths are not negative.**
Without `cluster` or `prob_matrix`,
[`Silhouette()`](https://kskbhat.github.io/Silhouette/reference/Silhouette.md)
assigns each observation to its nearest cluster, so \\a_i \le b_i\\ and
the medoid width \\1 - r_i\\ lies in \\\[0, 1\]\\. Unlike the classical
silhouette, where a negative width flags an observation that is closer
on average to another cluster, poorly placed observations show up here
as widths near zero. Negative widths appear only when the partition
comes from elsewhere (next section).

``` r

min(s_simple)
#> [1] 0.02224533
```

**2. PAC is a monotone shrinkage of the medoid width.** The PAC width is
\\(1 - r_i)/(1 + r_i) = s_i / (2 - s_i)\\, and \\s_i / (2 - \|s_i\|)\\
in general. It orders observations exactly as the medoid width does and
is never larger in absolute value (a medoid width of 0.5 becomes 1/3),
so the choice between them changes the level of the average, not which
observations look poorly clustered.

**3. Squared distances inflate the widths.** Replacing every distance by
its square replaces \\r_i\\ by \\r_i^2\\, and the width by \\1 - r_i^2 =
s_i(2 - s_i)\\. Some functions, such as
[`ppclust::fcm()`](https://rdrr.io/pkg/ppclust/man/fcm.html), return
squared distances, so check the scale of the matrix you pass.

``` r

all.equal(Silhouette(P^2)$sil_width, s_simple * (2 - s_simple))
#> [1] TRUE
```

## Where the cluster labels come from

A silhouette scores a partition, so it matters which partition is
scored. Every width function takes its labels, in order of precedence,
from

1.  the `cluster` argument, when supplied;
2.  the largest membership in `prob_matrix`, when supplied;
3.  the closest cluster in the proximity matrix.

For k-means at convergence, the nearest center is the assigned cluster,
but for other methods it need not be. Without `cluster`, scoring an
average-linkage partition against its own centroids would silently
reassign some observations to their nearest centroid; with `cluster`,
the partition is kept and those observations receive negative widths:

``` r

hc <- cutree(hclust(dist(X), method = "average"), k = 3)
centers <- apply(X, 2, function(v) tapply(v, hc, mean))
P_hc <- proxy::dist(X, centers)
c(reassigned_without_cluster = sum(Silhouette(P_hc)$cluster != hc),
  negative_with_cluster = sum(Silhouette(P_hc, cluster = hc)$sil_width < 0))
#> reassigned_without_cluster      negative_with_cluster 
#>                          2                          2
```

If a membership matrix is supplied as `prob_matrix`, the labels come
from the memberships. An observation whose most probable cluster is not
its nearest center then gets a negative width, which is informative: it
marks disagreement between the fitted model and the geometry. For a
Gaussian mixture:

``` r

suppressPackageStartupMessages(library(mclust))
gm <- Mclust(X, G = 3, verbose = FALSE)
P_gmm <- as.matrix(proxy::dist(X, t(gm$parameters$mean)))
sum(Silhouette(P_gmm, prob_matrix = gm$z)$sil_width < 0)
#> [1] 11
```

## Ranges of the widths

| Width | Function and argument | Range |
|:---|:---|:---|
| medoid | `Silhouette(method = "medoid")` | \\\[0, 1\]\\ with labels from the same matrix |
| PAC | `Silhouette(method = "pac")` | \\\[0, 1\]\\ with labels from the same matrix |
| PACS, PPS (posterior probabilities) | `softSilhouette(prob_type = "pp")` | \\\[0, 1\]\\ |
| NLPPS (negative log posteriors) | `softSilhouette(prob_type = "nlpp")` | \\\[0, 1\]\\ |
| PDS (probability densities) | `softSilhouette(prob_type = "pd")` | \\\[0, 1\]\\ |
| CeS (certainty) | [`cerSilhouette()`](https://kskbhat.github.io/Silhouette/reference/cerSilhouette.md) | \\\[1/K, 1\]\\ |
| DBS (density-based) | [`dbSilhouette()`](https://kskbhat.github.io/Silhouette/reference/dbSilhouette.md) | \\\[0, 1\]\\, and the maximum in each sample is always 1 |

The ranges assume the default labels. With labels supplied through
`cluster` (or, for
[`Silhouette()`](https://kskbhat.github.io/Silhouette/reference/Silhouette.md),
through `prob_matrix`), every width except the certainty width can be
negative and ranges over \\\[-1, 1\]\\; the certainty width ranges over
\\\[0, 1\]\\.

Exactly tied proximities or memberships are resolved in favor of the
column that comes first. With ties, the labels (though not the widths,
which are then 0) depend on the order of the columns; supply `cluster`
to control them.

## Which comparisons are valid

- **The same width across partitions of the same data.** Average widths
  of one kind (same function, method, transformation and averaging) can
  be compared across algorithms or numbers of clusters, keeping in mind
  that each width favors a particular kind of cluster.
- **Not one width against another.** The rows of
  [`calSilhouette()`](https://kskbhat.github.io/Silhouette/reference/calSilhouette.md)
  are on different scales. Choosing “the best method” by the largest
  value compares scales, not clusterings. Nor are the rows independent
  evidence: each PAC row is a monotone transformation of the matching
  medoid row, the PPS, PACS and DBS rank observations identically, the
  PDS equals the PPS when the clusters have equal weights, and for fuzzy
  c-means the PPS and distance-based widths coincide (see
  [`?calSilhouette`](https://kskbhat.github.io/Silhouette/reference/calSilhouette.md)).
- **Not simplified against classical.** See the first section.
- **Fuzzy averages only at the same `a`.** The weights
  \\(\gamma\_{i(1)} - \gamma\_{i(2)})^a\\, built from the two largest
  memberships, change with `a`, and so does the average.
- **Certainty and density-based widths with care.** The certainty width
  cannot fall below \\1/K\\, so its level falls as \\K\\ grows. The
  density-based width is scaled by the most confident observation in the
  sample, so its average depends on that one observation.
- **Choosing \\K\\ needs a stated reading.** For the distance-based
  widths, the number of clusters is usually read where the average is
  largest. For the widths computed from memberships (PACS, PPS, NLPPS,
  CeS, PDS), whose average typically starts near 1 and falls, Bhat Kapu
  and Kiruthika (2024) read it from the reverse elbow drop, and the DBS
  is read from its local maxima (Menardi 2011). The result depends on
  the algorithm, the width and the averaging; see
  [`vignette("soft-clustering", package = "Silhouette")`](https://kskbhat.github.io/Silhouette/articles/soft-clustering.md).

## Session information

``` r

sessionInfo()
#> R version 4.6.2 (2026-10-29)
#> Platform: x86_64-pc-linux-gnu
#> Running under: Ubuntu 26.04.1 LTS
#> 
#> Matrix products: default
#> BLAS:   /usr/lib/x86_64-linux-gnu/openblas-pthread/libblas.so.3 
#> LAPACK: /usr/lib/x86_64-linux-gnu/openblas-pthread/libopenblasp-r0.3.32.so;  LAPACK version 3.12.0
#> 
#> locale:
#>  [1] LC_CTYPE=C.UTF-8       LC_NUMERIC=C           LC_TIME=C.UTF-8       
#>  [4] LC_COLLATE=C.UTF-8     LC_MONETARY=C.UTF-8    LC_MESSAGES=C.UTF-8   
#>  [7] LC_PAPER=C.UTF-8       LC_NAME=C              LC_ADDRESS=C          
#> [10] LC_TELEPHONE=C         LC_MEASUREMENT=C.UTF-8 LC_IDENTIFICATION=C   
#> 
#> time zone: UTC
#> tzcode source: system (glibc)
#> 
#> attached base packages:
#> [1] stats     graphics  grDevices utils     datasets  methods   base     
#> 
#> other attached packages:
#> [1] mclust_6.1.3     Silhouette_0.9.7
#> 
#> loaded via a namespace (and not attached):
#>  [1] sass_0.4.10        generics_0.1.4     tidyr_1.3.2        rstatix_1.1.0     
#>  [5] digest_0.6.39      magrittr_2.0.5     evaluate_1.0.5     grid_4.6.2        
#>  [9] RColorBrewer_1.1-3 fastmap_1.2.0      jsonlite_2.0.0     backports_1.5.1   
#> [13] Formula_1.2-6      purrr_1.2.2        scales_1.4.0       textshaping_1.0.5 
#> [17] jquerylib_0.1.4    abind_1.4-8        cli_3.6.6          rlang_1.3.0       
#> [21] cachem_1.1.0       yaml_2.3.12        otel_0.3.0         tools_4.6.2       
#> [25] ggsignif_0.6.4     dplyr_1.2.1        ggplot2_4.0.4      ggpubr_1.0.0      
#> [29] broom_1.0.13       vctrs_0.7.3        R6_2.6.1           proxy_0.4-29      
#> [33] lifecycle_1.0.5    fs_2.1.0           car_3.1-5          htmlwidgets_1.6.4 
#> [37] ragg_1.5.2         cluster_2.1.8.3    pkgconfig_2.0.3    desc_1.4.3        
#> [41] pkgdown_2.2.1      pillar_1.11.1      bslib_0.12.0       gtable_0.3.6      
#> [45] glue_1.8.1         systemfonts_1.3.2  xfun_0.62          tibble_3.3.1      
#> [49] tidyselect_1.2.1   knitr_1.52         farver_2.1.2       htmltools_0.5.9   
#> [53] rmarkdown_2.32     carData_3.0-6      compiler_4.6.2     S7_0.2.2
```

## References

Bhat Kapu, Shrikrishna, and C. Kiruthika. 2024. “Some Density-Based
Silhouette Diagnostics for Soft Clustering Algorithms.” *Communications
in Statistics: Case Studies, Data Analysis and Applications* 10 (3-4):
221–38. <https://doi.org/10.1080/23737484.2024.2408534>.

Lun, Aaron. 2023. *bluster: Clustering Algorithms for Bioconductor*.
<https://doi.org/10.18129/B9.bioc.bluster>.

Menardi, Giovanna. 2011. “Density-Based Silhouette Diagnostics for
Clustering Methods.” *Statistics and Computing* 21 (3): 295–308.
<https://doi.org/10.1007/s11222-010-9169-0>.

Rousseeuw, Peter J. 1987. “Silhouettes: A Graphical Aid to the
Interpretation and Validation of Cluster Analysis.” *Journal of
Computational and Applied Mathematics* 20: 53–65.
<https://doi.org/10.1016/0377-0427(87)90125-7>.

Van der Laan, Mark, Katherine Pollard, and Jennifer Bryan. 2003. “A New
Partitioning Around Medoids Algorithm.” *Journal of Statistical
Computation and Simulation* 73 (8): 575–84.
<https://doi.org/10.1080/0094965031000136012>.
