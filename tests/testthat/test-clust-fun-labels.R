# A deterministic stand-in for a clustering function: memberships, distances and labels.
toy_fit <- function(n) {
  u <- cbind(seq(0.9, 0.1, length.out = n), seq(0.1, 0.9, length.out = n))
  list(u = u, d = 1 - u, lab = rep(c(2L, 1L), length.out = n))
}

test_that("clust_fun can supply the labels by component name", {
  out <- toy_fit(8)
  expect_equal(
    softSilhouette("u", clust_fun = toy_fit, n = 8, cluster = "lab"),
    softSilhouette(out$u, cluster = out$lab))
  expect_equal(
    cerSilhouette("u", clust_fun = toy_fit, n = 8, cluster = "lab"),
    cerSilhouette(out$u, cluster = out$lab))
  expect_equal(
    dbSilhouette("u", clust_fun = toy_fit, n = 8, cluster = "lab"),
    dbSilhouette(out$u, cluster = out$lab))
  expect_equal(
    calSilhouette(prox_matrix = "d", prob_matrix = "u", clust_fun = toy_fit, n = 8,
                  cluster = "lab"),
    calSilhouette(prox_matrix = out$d, prob_matrix = out$u, cluster = out$lab))
  expect_error(softSilhouette("u", clust_fun = toy_fit, n = 8, cluster = "nope"),
               "Component 'nope' not found")
})

test_that("calSilhouette() checks that the two matrices match", {
  expect_error(
    calSilhouette(prox_matrix = matrix(c(1, 2, 3, 4, 5, 6), 3, 2),
                  prob_matrix = matrix(1 / 3, 3, 3)),
    "same dimensions")
})

test_that("calSilhouette() reports warnings raised while computing a width", {
  prob0 <- rbind(c(1, 0), c(0.6, 0.4), c(0.3, 0.7))
  w <- testthat::capture_warnings(res <- calSilhouette(prob_matrix = prob0))
  expect_true(any(grepl("infinite", w)))
  expect_equal(res$Crisp[res$Method == "db"],
               mean(suppressWarnings(dbSilhouette(prob0, average = "crisp"))$sil_width))
})

test_that("calSilhouette() reports warnings raised while averaging", {
  tied <- matrix(0.5, 4, 2)
  w <- testthat::capture_warnings(res <- calSilhouette(prob_matrix = tied))
  expect_true(any(grepl("^pp_pac: All fuzzy weights are zero", w)))
  expect_true(all(is.nan(res$Fuzzy)))
  expect_true(all(res$Crisp[res$Method != "cer"] == 0))
})

test_that("clust_args passes arguments that would be partially matched", {
  skip_if_not_installed("ppclust")
  X <- as.matrix(iris[, 1:4])
  set.seed(1)
  s1 <- softSilhouette("u", clust_fun = ppclust::fcm, x = X, centers = 3, nstart = 1,
                       clust_args = list(m = 3))
  set.seed(1)
  fit <- ppclust::fcm(X, centers = 3, nstart = 1, m = 3)
  expect_equal(s1, softSilhouette(fit$u))
  # m in ... is matched to `method`, which is why clust_args exists
  expect_error(softSilhouette("u", clust_fun = ppclust::fcm, x = X, centers = 3, m = 3))
  expect_error(softSilhouette("u", clust_fun = toy_fit, clust_args = "n = 8"),
               "clust_args must be a list")
})

test_that("clust_fun may be given as 'pkg::fun'", {
  skip_if_not_installed("ppclust")
  X <- as.matrix(iris[, 1:4])
  set.seed(2)
  s1 <- cerSilhouette("u", clust_fun = "ppclust::fcm", x = X, centers = 2)
  set.seed(2)
  expect_equal(s1, cerSilhouette(ppclust::fcm(X, centers = 2)$u))
  expect_error(cerSilhouette("u", clust_fun = "ppclust::nofun", x = X), "not found")
  expect_error(cerSilhouette("u", clust_fun = "notapkg123::fcm", x = X), "not found")
})

test_that("widths match hand calculations", {
  # medoid and PAC widths from distances
  d <- rbind(c(1, 3, 4), c(2, 1, 5), c(6, 2, 3))
  s <- Silhouette(d)
  expect_equal(s$cluster, c(1L, 2L, 2L))
  expect_equal(s$neighbor, c(2L, 1L, 3L))
  expect_equal(s$sil_width, c(1 - 1 / 3, 1 - 1 / 2, 1 - 2 / 3))
  expect_equal(Silhouette(d, method = "pac")$sil_width, c(2 / 4, 1 / 3, 1 / 5))
  # PPS, CeS and DBS from memberships
  p <- rbind(c(0.7, 0.2, 0.1), c(0.1, 0.5, 0.4))
  expect_equal(softSilhouette(p, method = "medoid")$sil_width, c(1 - 0.2 / 0.7, 1 - 0.4 / 0.5))
  expect_equal(cerSilhouette(p)$sil_width, c(0.7, 0.5))
  r <- c(log(0.7 / 0.2), log(0.5 / 0.4))
  expect_equal(dbSilhouette(p)$sil_width, r / max(r))
  # fuzzy average with a = 1
  w <- c(0.5, 0.1)
  expect_equal(summary(cerSilhouette(p, average = "fuzzy", a = 1), print.summary = FALSE)$avg.width,
               sum(w * c(0.7, 0.5)) / sum(w))
})
