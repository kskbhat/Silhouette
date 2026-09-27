library(testthat)
library(Silhouette)

# Edge cases, input validation and the exact identities of the propositions in
# the accompanying article. Exact identities are checked to numerical tolerance.

sil_of <- function(x) x$sil_width

## 1. Zero distances and zero probabilities --------------------------------
test_that("zero distances give finite widths", {
  prox <- rbind(c(0, 0, 1),   # tie at zero: width 0
                c(0, 2, 3),   # on its center: width 1
                c(1, 0, 4))
  s <- Silhouette(prox)
  expect_equal(s$sil_width, c(0, 1, 1))
  expect_equal(Silhouette(prox, method = "pac")$sil_width, c(0, 1, 1))
  expect_true(all(s$cluster != s$neighbor))
})

test_that("zero probabilities are handled by every transformation", {
  prob <- rbind(c(1, 0, 0),
                c(0.7, 0.3, 0),
                c(0, 0.4, 0.6),
                c(0.2, 0.5, 0.3))
  for (pt in c("pp", "nlpp", "pd")) {
    for (m in c("pac", "medoid")) {
      s <- softSilhouette(prob, prob_type = pt, method = m)
      expect_false(anyNA(s$sil_width), info = paste(pt, m))
      expect_true(all(s$cluster != s$neighbor), info = paste(pt, m))
      expect_true(is.Silhouette(s, strict = TRUE), info = paste(pt, m))
    }
  }
  # -log(0) = Inf for the neighbor gives width 1 (previously NaN)
  expect_equal(softSilhouette(prob, prob_type = "nlpp")$sil_width[1], 1)
  expect_equal(softSilhouette(prob, prob_type = "nlpp", method = "medoid")$sil_width[1], 1)
  expect_equal(cerSilhouette(prob)$sil_width, c(1, 0.7, 0.6, 0.5))
})

test_that("dbSilhouette() does not collapse when a neighboring probability is zero", {
  prob <- rbind(c(1, 0), c(0.9, 0.1), c(0.6, 0.4), c(0.2, 0.8))
  expect_warning(s <- dbSilhouette(prob), "1 observation\\(s\\) have a zero membership")
  R <- log(c(0.9 / 0.1, 0.6 / 0.4, 0.8 / 0.2))
  expect_equal(s$sil_width, c(1, R / max(R)))
  expect_true(all(s$sil_width[-1] > 0))
})

test_that("dbSilhouette() sets widths to zero for numerically degenerate memberships", {
  prob <- cbind(0.5 + c(1e-12, -1e-12, 2e-12), 0.5 - c(1e-12, -1e-12, 2e-12))
  expect_warning(s <- dbSilhouette(prob, average = "crisp"), "numerically")
  expect_equal(s$sil_width, c(0, 0, 0))
  # exactly equal memberships: widths 0, no warning
  expect_silent(s2 <- dbSilhouette(matrix(0.5, 3, 2)))
  expect_equal(s2$sil_width, c(0, 0, 0))
})

## 2. Ties -------------------------------------------------------------------
test_that("exact ties go to the first column", {
  prox <- rbind(c(1, 1, 2), c(2, 1, 1))
  s <- Silhouette(prox)
  expect_equal(s$cluster, c(1L, 2L))
  expect_equal(s$neighbor, c(2L, 3L))
  expect_equal(s$sil_width, c(0, 0))
  prob <- rbind(c(0.4, 0.4, 0.2))
  expect_equal(softSilhouette(prob)$cluster, 1L)
  expect_equal(softSilhouette(prob)$neighbor, 2L)
})

test_that("all-zero fuzzy weights give NaN with a warning", {
  prob <- matrix(0.5, 4, 2)
  s <- softSilhouette(prob, average = "fuzzy")
  expect_equal(s$weight, rep(0, 4))
  expect_warning(avg <- summary(s, print.summary = FALSE)$avg.width, "All fuzzy weights are zero")
  expect_true(is.nan(avg))
})

## 3. K = 2, singleton and empty clusters -------------------------------------
test_that("K = 2 and singleton clusters work", {
  prox <- rbind(c(1, 3), c(2, 5), c(4, 1))
  s <- Silhouette(prox)
  expect_equal(s$cluster, c(1L, 1L, 2L))
  expect_equal(s$neighbor, c(2L, 2L, 1L))
  sm <- summary(s, print.summary = FALSE)
  expect_equal(sm$sil.sum$size, c(2L, 1L))
  expect_equal(unname(sm$clus.avg.widths[["2"]]), 0.75)
})

test_that("empty clusters are dropped from summaries and plots", {
  prox <- rbind(c(1, 2, 3), c(1, 3, 2), c(3, 2, 1), c(2, 3, 1))
  s <- Silhouette(prox, cluster = c(1, 1, 3, 3))
  sm <- summary(s, print.summary = FALSE)
  expect_equal(sm$sil.sum$cluster, c("1", "3"))
  expect_equal(sm$sil.sum$size, c(2L, 2L))
  p <- plotSilhouette(s)
  expect_s3_class(p, "ggplot")
  labels <- p$scales$get_scales("fill")$labels
  expect_equal(labels, c("C1 (2): 0.5", "C3 (2): 0.5"))
})

## 4. Missing and infinite values, input checks ------------------------------------
test_that("missing and -Inf proximities are rejected, +Inf is allowed", {
  prox <- rbind(c(1, 2), c(2, 1))
  bad <- prox; bad[1, 1] <- NA
  expect_error(Silhouette(bad), "missing values")
  bad[1, 1] <- NaN
  expect_error(Silhouette(bad), "missing values")
  bad[1, 1] <- -Inf
  expect_error(Silhouette(bad), "-Inf")
  inf <- prox; inf[1, 2] <- Inf
  expect_equal(Silhouette(inf)$sil_width[1], 1)
  inf[1, ] <- Inf
  expect_equal(Silhouette(inf)$sil_width[1], 0)
})

test_that("negative proximities are rejected", {
  expect_error(Silhouette(rbind(c(-1, 2), c(2, 1))), "must not contain negative values")
})

test_that("membership matrices are checked with a documented tolerance", {
  prob <- rbind(c(0.3, 0.7), c(0.6, 0.4))
  expect_silent(softSilhouette(prob + c(1e-10, 0)))
  expect_error(softSilhouette(prob + c(1e-6, 0)), "must sum to 1")
  expect_error(softSilhouette(rbind(c(-0.1, 1.1), c(0.5, 0.5))), "negative")
  expect_error(softSilhouette(rbind(c(NA, 1), c(0.5, 0.5))), "missing values")
  expect_error(Silhouette(matrix(1:4, 2), prob_matrix = matrix(0.25, 2, 4)), "same dimensions")
})

## 5. Similarity versus dissimilarity ---------------------------------------------
test_that("medoid widths are invariant to taking reciprocals", {
  set.seed(1)
  d <- matrix(runif(60, 0.1, 5), ncol = 3)
  s_d <- Silhouette(d)
  s_s <- Silhouette(1 / d, proximity_type = "similarity")
  expect_equal(s_s$cluster, s_d$cluster)
  expect_equal(s_s$neighbor, s_d$neighbor)
  expect_equal(s_s$sil_width, s_d$sil_width)
})

## 6. User-supplied labels ---------------------------------------------------------
test_that("cluster = keeps the supplied partition and gives negative widths", {
  prox <- rbind(c(1, 4, 5), c(3, 2, 6), c(5, 4, 1))
  s <- Silhouette(prox, cluster = c(1, 1, 3))
  expect_equal(s$cluster, c(1L, 1L, 3L))
  expect_equal(s$neighbor, c(2L, 2L, 2L))
  expect_equal(s$sil_width, c((4 - 1) / 4, (2 - 3) / 3, (4 - 1) / 4))
  expect_lt(s$sil_width[2], 0)
  # labels win over prob_matrix
  prob <- rbind(c(0.1, 0.8, 0.1), c(0.8, 0.1, 0.1), c(0.1, 0.1, 0.8))
  expect_equal(Silhouette(prox, prob_matrix = prob, cluster = c(1, 1, 3))$cluster, c(1L, 1L, 3L))
  expect_equal(Silhouette(prox, prob_matrix = prob)$cluster, c(2L, 1L, 3L))
})

test_that("cluster = is validated", {
  prox <- rbind(c(1, 2), c(2, 1))
  expect_error(Silhouette(prox, cluster = factor(c(1, 2))), "not a factor")
  expect_error(Silhouette(prox, cluster = c(1, 3)), "between 1 and 2")
  expect_error(Silhouette(prox, cluster = 1), "one entry per row")
  expect_error(Silhouette(prox, cluster = c(1, 1.5)), "whole numbers")
  expect_error(Silhouette(prox, cluster = c(1, NA)), "whole numbers")
})

test_that("cluster = can name a component of the clust_fun output", {
  set.seed(2)
  km_d <- function(x, k) {
    km <- kmeans(x, k, nstart = 5)
    km$d <- as.matrix(proxy::dist(x, km$centers))
    km
  }
  skip_if_not_installed("proxy")
  s <- Silhouette("d", clust_fun = km_d, x = iris[, -5], k = 3, cluster = "cluster")
  expect_s3_class(s, "Silhouette")
  expect_true(all(s$sil_width >= 0))
})

test_that("cluster = works for every width and in calSilhouette()", {
  set.seed(3)
  prob <- prop.table(matrix(runif(30), ncol = 3), 1)
  lab <- sample(1:3, 10, replace = TRUE)
  for (s in list(softSilhouette(prob, cluster = lab),
                 softSilhouette(prob, prob_type = "nlpp", cluster = lab),
                 cerSilhouette(prob, cluster = lab))) {
    expect_equal(s$cluster, lab)
  }
  expect_equal(cerSilhouette(prob, cluster = lab)$sil_width, prob[cbind(1:10, lab)])
  db <- suppressWarnings(dbSilhouette(prob, cluster = lab))
  expect_true(all(db$sil_width >= -1 & db$sil_width <= 1))
  expect_true(any(db$sil_width < 0))
  prox <- matrix(runif(30), ncol = 3)
  cs <- calSilhouette(prox_matrix = prox, prob_matrix = prob, cluster = lab)
  expect_equal(cs$Crisp[cs$Method == "medoid"],
               mean(Silhouette(prox, cluster = lab)$sil_width))
})

## 7. Consistent labels ------------------------------------------------------------
test_that("the PDS assigns by the largest density for every average", {
  set.seed(4)
  prob <- prop.table(matrix(rexp(300), ncol = 3) %*% diag(c(4, 1, 1)), 1)
  fits <- lapply(c("crisp", "fuzzy", "median"), function(av)
    softSilhouette(prob, prob_type = "pd", average = av))
  labs <- lapply(fits, `[[`, "cluster")
  expect_identical(labs[[1]], labs[[2]])
  expect_identical(labs[[1]], labs[[3]])
  dens <- sweep(prob, 2, colMeans(prob), "/")
  expect_identical(labs[[1]], max.col(dens, ties.method = "first"))
  expect_true(any(labs[[1]] != max.col(prob, ties.method = "first")))
  for (f in fits) expect_true(all(f$sil_width >= 0 & f$sil_width <= 1))
})

test_that("calSilhouette() scores one partition in every row", {
  set.seed(5)
  prox <- matrix(runif(90), ncol = 3)
  prob <- prop.table(matrix(runif(90), ncol = 3), 1)
  cs <- calSilhouette(prox_matrix = prox, prob_matrix = prob)
  s_med <- Silhouette(prox, prob_matrix = prob)
  expect_equal(s_med$cluster, max.col(prob, ties.method = "first"))
  expect_equal(cs$Crisp[cs$Method == "medoid"], mean(s_med$sil_width))
  expect_equal(cs$Median[cs$Method == "medoid"], median(s_med$sil_width))
  expect_equal(cs$Fuzzy[cs$Method == "medoid"],
               sum(s_med$weight * s_med$sil_width) / sum(s_med$weight))
  expect_equal(cs$Crisp[cs$Method == "pd_pac"],
               mean(softSilhouette(prob, prob_type = "pd")$sil_width))
  expect_equal(cs$Median[cs$Method == "db"], median(dbSilhouette(prob)$sil_width))
  expect_equal(cs$Method, c("medoid", "pac", "pp_pac", "pp_medoid", "nlpp_pac",
                            "nlpp_medoid", "pd_pac", "pd_medoid", "cer", "db"))
})

test_that("fuzzy weights are the gap between the two largest memberships", {
  prob <- rbind(c(0.6, 0.3, 0.1), c(0.2, 0.5, 0.3))
  prox <- rbind(c(1, 2, 3), c(3, 1, 2))
  s <- Silhouette(prox, prob_matrix = prob, a = 1, cluster = c(3, 3))
  expect_equal(s$weight, c(0.3, 0.2))
  expect_equal(softSilhouette(prob, average = "fuzzy", a = 2)$weight, c(0.3, 0.2)^2)
})

## 8. Identities (Propositions 1 and 2 of the article) --------------------------------
test_that("PAC width is s / (2 - |s|) of the medoid width", {
  set.seed(6)
  d <- matrix(runif(150, 0.1, 3), ncol = 3)
  s <- sil_of(Silhouette(d))
  expect_equal(sil_of(Silhouette(d, method = "pac")), s / (2 - s))
  lab <- sample(1:3, 50, replace = TRUE)
  s2 <- sil_of(Silhouette(d, cluster = lab))
  expect_equal(sil_of(Silhouette(d, method = "pac", cluster = lab)), s2 / (2 - abs(s2)))
  # squared dissimilarities: s(2 - s)
  expect_equal(sil_of(Silhouette(d^2)), s * (2 - s))
})

test_that("fuzzy c-means memberships and distances give the same widths", {
  skip_if_not_installed("ppclust")
  for (m in c(2, 3)) {
    set.seed(7)
    fc <- ppclust::fcm(iris[, -5], centers = 3, m = m)
    t_ik <- fc$d^(1 / (m - 1))   # fc$d holds squared Euclidean distances
    for (meth in c("medoid", "pac")) {
      expect_equal(sil_of(softSilhouette(fc$u, method = meth)),
                   sil_of(Silhouette(t_ik, method = meth)),
                   tolerance = 1e-6, info = paste(m, meth))
    }
  }
})

test_that("pp and db widths rank observations identically; pd equals pp for equal weights", {
  set.seed(8)
  prob <- prop.table(matrix(runif(120), ncol = 3), 1)
  r <- function(x) rank(x, ties.method = "first")
  pp <- sil_of(softSilhouette(prob, method = "medoid"))
  expect_equal(r(sil_of(softSilhouette(prob, method = "pac"))), r(pp))
  expect_equal(r(sil_of(dbSilhouette(prob))), r(pp))
  bal <- rbind(prob, prob[, c(2, 3, 1)], prob[, c(3, 1, 2)])   # equal column means
  expect_equal(sil_of(softSilhouette(bal, prob_type = "pd")), sil_of(softSilhouette(bal)))
})

## 9. Objects from other packages ------------------------------------------------------
test_that("inputs from other packages are accepted", {
  skip_if_not_installed("proxy")
  skip_if_not_installed("mclust")
  km <- kmeans(iris[, -5], 3, nstart = 5)
  d <- proxy::dist(iris[, -5], km$centers)      # a "crossdist" object
  expect_s3_class(Silhouette(d, cluster = km$cluster), "Silhouette")
  suppressPackageStartupMessages(library(mclust))  # Mclust() needs mclustBIC() on the search path
  gm <- mclust::Mclust(iris[, -5], G = 3, verbose = FALSE)
  expect_s3_class(suppressWarnings(dbSilhouette(gm$z)), "Silhouette")
  expect_s3_class(softSilhouette(gm$z, cluster = gm$classification), "Silhouette")
})

## 10. Extended silhouette weights -------------------------------------------------------
test_that("extSilhouette() supports size, equal and user weights", {
  s1 <- getSilhouette(cluster = c(1, 1, 2, 2), neighbor = c(2, 2, 1, 1),
                      sil_width = c(0.2, 0.4, 0.6, 0.8))
  s2 <- getSilhouette(cluster = c(1, 2), neighbor = c(2, 1), sil_width = c(0.1, 0.3))
  e <- extSilhouette(list(s1, s2))
  expect_equal(e$ext_sil_width, (4 * 0.5 + 2 * 0.2) / 6)
  expect_equal(e$dim_table$weight, c(4, 2) / 6)
  expect_equal(extSilhouette(list(s1, s2), weights = "equal")$ext_sil_width, 0.35)
  expect_equal(extSilhouette(list(s1, s2), weights = c(1, 3))$ext_sil_width, 0.25 * 0.5 + 0.75 * 0.2)
  expect_equal(extSilhouette(list(s1, s2), weights = c(1, 3))$weights, "user")
  expect_error(extSilhouette(list(s1, s2), weights = c(1, -1)), "non-negative")
  expect_error(extSilhouette(list(s1, s2), weights = 1), "one entry per element")
  expect_error(extSilhouette(list(s1, s2), weights = "other"), "should be one of")
})

test_that("with MAP labels, negative PDS widths mean a higher neighboring density", {
  skip_if_not_installed("mclust")
  suppressPackageStartupMessages(library(mclust))
  set.seed(4)
  z <- c(rnorm(270, 0), rnorm(30, 2.5))
  g <- mclust::Mclust(z, G = 2, modelNames = "E", verbose = FALSE)
  # posteriors at the fitted parameters (the EM fixed point)
  post <- mclust::estep(data = z, modelName = "E", parameters = g$parameters)$z
  sd_k <- sqrt(g$parameters$variance$sigmasq)
  f <- sapply(1:2, function(k) dnorm(z, g$parameters$mean[k], sd_k))
  # gamma / pi equals f_k / f (Bayes' theorem), so pd compares component densities
  fx <- as.vector(f %*% g$parameters$pro)
  expect_equal(unname(sweep(post, 2, g$parameters$pro, "/")), f / fx, tolerance = 1e-8)
  # default PDS labels are the largest densities, so the widths are non-negative
  expect_true(all(softSilhouette(post, prob_type = "pd")$sil_width >= 0))
  # scoring the MAP partition instead gives negative widths exactly where the
  # neighboring component has the higher density
  pd <- softSilhouette(post, prob_type = "pd", cluster = max.col(post))
  i <- seq_along(z)
  nb_higher <- f[cbind(i, pd$neighbor)] > f[cbind(i, pd$cluster)]
  expect_true(any(pd$sil_width < 0))
  expect_equal(pd$sil_width < 0, nb_higher)
})

test_that("a = 0 gives the crisp average", {
  set.seed(9)
  prob <- prop.table(matrix(runif(60), ncol = 3), 1)
  s0 <- softSilhouette(prob, average = "fuzzy", a = 0)
  expect_equal(s0$weight, rep(1, 20))
  expect_equal(summary(s0, print.summary = FALSE)$avg.width, mean(s0$sil_width))
  expect_error(softSilhouette(prob, a = -0.5), "non-negative")
})
