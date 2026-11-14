# Silhouette: Proximity Measure Based Diagnostics for Standard, Soft, and Multi-Way Clustering

Quantifies clustering quality by measuring both cohesion within clusters
and separation between clusters. Silhouette widths are computed from the
n by K matrices that clustering algorithms return (distances or
similarities to cluster prototypes, fuzzy memberships, or posterior
probabilities) instead of all pairwise dissimilarities, for the
partition an algorithm produced or one supplied by the user. Implements
the simplified silhouette of Van der Laan et al. (2003)
[doi:10.1080/0094965031000136012](https://doi.org/10.1080/0094965031000136012)
, the probability of alternative cluster normalization of Raymaekers and
Rousseeuw (2022)
[doi:10.1080/10618600.2022.2050249](https://doi.org/10.1080/10618600.2022.2050249)
, the fuzzy silhouette average of Campello and Hruschka (2006)
[doi:10.1016/j.fss.2006.07.006](https://doi.org/10.1016/j.fss.2006.07.006)
, the density-based silhouette of Menardi (2011)
[doi:10.1007/s11222-010-9169-0](https://doi.org/10.1007/s11222-010-9169-0)
, the soft silhouettes of Bhat Kapu and Kiruthika (2024)
[doi:10.1080/23737484.2024.2408534](https://doi.org/10.1080/23737484.2024.2408534)
, and the extended silhouette for multi-way clustering of Schepers et
al. (2008)
[doi:10.1007/s00357-008-9005-9](https://doi.org/10.1007/s00357-008-9005-9)
and Bhat Kapu and Kiruthika (2025)
[doi:10.21203/rs.3.rs-6973596/v1](https://doi.org/10.21203/rs.3.rs-6973596/v1)
. Provides summaries and 'ggplot2' silhouette plots in the style of
Rousseeuw (1987)
[doi:10.1016/0377-0427(87)90125-7](https://doi.org/10.1016/0377-0427%2887%2990125-7)
.

## See also

Useful links:

- <https://kskbhat.github.io/Silhouette/>

- Report bugs at <https://github.com/kskbhat/Silhouette/issues>

## Author

**Maintainer**: Shrikrishna Bhat K <skbhat.in@gmail.com>
([ORCID](https://orcid.org/0009-0000-6180-5783)) \[copyright holder\]

Authors:

- Shrikrishna Bhat K <skbhat.in@gmail.com>
  ([ORCID](https://orcid.org/0009-0000-6180-5783)) \[copyright holder\]

- Kiruthika C ([ORCID](https://orcid.org/0000-0001-9655-702X))
