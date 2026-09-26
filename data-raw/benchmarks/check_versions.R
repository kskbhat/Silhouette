# Check that Silhouette 0.9.6 and 0.9.7 give identical widths on the benchmark
# data (crisp medoid widths, fuzzy weights and nlpp widths).
# Usage: Rscript check_versions.R <lib_for_Silhouette> <outfile>
args <- commandArgs(trailingOnly = TRUE)
if (args[1] != "default") .libPaths(c(args[1], .libPaths()))
suppressPackageStartupMessages(library(Silhouette))
source("bench_data.R")
out <- lapply(c(1000, 1e4, 1e5), function(n) {
  dat <- make_data(n, seed = n)
  P <- proxy::dist(dat$X, dat$centers)
  crisp <- Silhouette(P)
  fuzzy <- Silhouette(P, prob_matrix = dat$U, average = "fuzzy")
  nlpp <- softSilhouette(dat$U, prob_type = "nlpp")
  list(n = n, crisp = crisp$sil_width, crisp_cl = crisp$cluster, crisp_nb = crisp$neighbor,
       fuzzy = fuzzy$sil_width, weight = fuzzy$weight, nlpp = nlpp$sil_width)
})
attr(out, "version") <- as.character(packageVersion("Silhouette"))
saveRDS(out, args[2])
