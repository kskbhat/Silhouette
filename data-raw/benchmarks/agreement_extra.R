# Additional settings for the agreement study (see agreement.R), added in
# response to review: unequal cluster sizes, higher dimension, and outliers.
# Everything else (n = 500, K = 4, k-means for k = 2..8, 50 data sets per
# setting, the four average widths) is as in agreement.R.
#
# Run from this folder:  Rscript agreement_extra.R
# Output: agreement_extra.rds

suppressPackageStartupMessages({
  library(Silhouette)
  library(cluster)
  library(MixSim)
  library(bluster)
})

settings <- data.frame(
  setting  = c("unequal", "p10", "outliers"),
  BarOmega = 0.01,
  p        = c(2, 10, 2),
  PiLow    = c(0.05, 1, 1),     # smallest mixing proportion (1 = equal)
  n_out    = c(0, 0, 25),       # outliers added by simdataset()
  K = 4, n = 500)
reps <- 50
kgrid <- 2:8
out <- list()

for (s in seq_len(nrow(settings))) {
  st <- settings[s, ]
  for (r in seq_len(reps)) {
    seed <- 90000 + 1000 * s + r
    set.seed(seed)
    Q <- MixSim(BarOmega = st$BarOmega, K = st$K, p = st$p, PiLow = st$PiLow)
    A <- simdataset(n = st$n - st$n_out, Pi = Q$Pi, Mu = Q$Mu, S = Q$S, n.out = st$n_out)
    X <- A$X
    D <- dist(X)
    asw <- matrix(NA, length(kgrid), 4,
                  dimnames = list(kgrid, c("rousseeuw", "medoid", "pac", "bluster")))
    for (j in seq_along(kgrid)) {
      k <- kgrid[j]
      set.seed(seed + k)
      km <- kmeans(X, k, nstart = 10)
      P <- proxy::dist(X, km$centers)
      s_r <- cluster::silhouette(km$cluster, D)
      s_m <- Silhouette(P, cluster = km$cluster)
      s_p <- Silhouette(P, method = "pac", cluster = km$cluster)
      s_b <- approxSilhouette(X, km$cluster)$width
      asw[j, ] <- c(mean(s_r[, "sil_width"]), mean(s_m$sil_width),
                    mean(s_p$sil_width), mean(s_b))
      if (k == st$K) {
        cor_true <- cor(s_r[, "sil_width"], s_m$sil_width)
        cor_bl <- cor(s_r[, "sil_width"], s_b)
      }
    }
    sel <- kgrid[apply(asw, 2, which.max)]
    out[[length(out) + 1]] <- data.frame(
      setting = st$setting, BarOmega = st$BarOmega, p = st$p, PiLow = st$PiLow,
      n_out = st$n_out, K = st$K, n = nrow(X), rep = r,
      cor_widths = cor_true, cor_bluster = cor_bl,
      asw_rousseeuw = asw[as.character(st$K), "rousseeuw"],
      asw_medoid = asw[as.character(st$K), "medoid"],
      asw_pac = asw[as.character(st$K), "pac"],
      asw_bluster = asw[as.character(st$K), "bluster"],
      k_rousseeuw = sel[1], k_medoid = sel[2], k_pac = sel[3], k_bluster = sel[4])
  }
  message("done setting ", st$setting)
}
res <- do.call(rbind, out)
attr(res, "session") <- sessionInfo()
saveRDS(res, "agreement_extra.rds")
