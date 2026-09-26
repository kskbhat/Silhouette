# Agreement between the simplified silhouette (n x K proximities) and the
# classical silhouette of Rousseeuw (1987) (n x n dissimilarities).
# For each simulated data set, k-means is fitted for k = 2, ..., 8 and the
# average silhouette width is computed four ways (classical, simplified
# medoid and PAC, and bluster::approxSilhouette). We record (i) the
# correlation of individual widths at the true K, (ii) the difference in
# average widths, and (iii) the number of clusters each index selects.

suppressPackageStartupMessages({
  library(Silhouette)
  library(cluster)
  library(MixSim)
  library(bluster)
})

settings <- expand.grid(BarOmega = c(0.001, 0.01, 0.05), p = c(2, 5),
                        K = 4, n = 500)
reps <- 50
kgrid <- 2:8
out <- list()

for (s in seq_len(nrow(settings))) {
  st <- settings[s, ]
  for (r in seq_len(reps)) {
    seed <- 1000 * s + r
    set.seed(seed)
    Q <- MixSim(BarOmega = st$BarOmega, K = st$K, p = st$p)
    A <- simdataset(n = st$n, Pi = Q$Pi, Mu = Q$Mu, S = Q$S)
    X <- A$X
    D <- dist(X)
    asw <- matrix(NA, length(kgrid), 4,
                  dimnames = list(kgrid, c("rousseeuw", "medoid", "pac", "bluster")))
    cor_true <- mad_true <- NA
    for (j in seq_along(kgrid)) {
      k <- kgrid[j]
      set.seed(seed + k)
      km <- kmeans(X, k, nstart = 10)
      P <- proxy::dist(X, km$centers)
      s_r <- cluster::silhouette(km$cluster, D)
      s_m <- Silhouette(P, method = "medoid")
      s_p <- Silhouette(P, method = "pac")
      s_b <- approxSilhouette(X, km$cluster)$width
      asw[j, ] <- c(mean(s_r[, "sil_width"]), mean(s_m$sil_width),
                    mean(s_p$sil_width), mean(s_b))
      if (k == st$K) {
        # Silhouette() re-derives cluster labels as the nearest centre,
        # which coincides with the k-means labels at convergence
        agree <- mean(s_m$cluster == km$cluster)
        cor_true <- cor(s_r[, "sil_width"], s_m$sil_width)
        mad_true <- mean(abs(s_r[, "sil_width"] - s_m$sil_width))
        cor_bl <- cor(s_r[, "sil_width"], s_b)
        lab_agree <- agree
      }
    }
    sel <- kgrid[apply(asw, 2, which.max)]
    out[[length(out) + 1]] <- data.frame(
      BarOmega = st$BarOmega, p = st$p, K = st$K, n = st$n, rep = r,
      cor_widths = cor_true, cor_bluster = cor_bl, mean_abs_diff = mad_true,
      label_agreement = lab_agree,
      asw_rousseeuw = asw[as.character(st$K), "rousseeuw"],
      asw_medoid = asw[as.character(st$K), "medoid"],
      asw_pac = asw[as.character(st$K), "pac"],
      asw_bluster = asw[as.character(st$K), "bluster"],
      k_rousseeuw = sel[1], k_medoid = sel[2], k_pac = sel[3], k_bluster = sel[4])
  }
  cat("done setting", s, "\n")
}
res <- do.call(rbind, out)
saveRDS(res, "agreement_results.rds")
