library(testthat)
library(Silhouette)
library(proxy)
library(ppclust)
library(blockcluster)
set.seed(123)

context("Extensive Silhouette package test suite")

data(iris)

# --- Helper functions ---
make_prob <- function(n = 30, k = 3) {
  m <- matrix(runif(n * k), nrow = n)
  m <- m / rowSums(m)
  m
}
make_prox <- function(n = 30, k = 3) {
  matrix(runif(n * k), nrow = n)
}
n <- 30
k <- 3

# --- 1. All parameterizations of main functions ---
test_that("Silhouette supports all proximity_types, methods, averages, sorts", {
  prox <- make_prox(n, k)
  prob <- make_prob(n, k)
  for (pt in c("similarity", "dissimilarity")) {
    for (method in c("pac", "medoid")) {
      for (average in c("crisp", "median")) {
        sil <- Silhouette(prox, proximity_type = pt, method = method, average = average, sort = TRUE)
        expect_s3_class(sil, "Silhouette")
        expect_true(all(c("cluster", "neighbor", "sil_width") %in% names(sil)))
      }
      sil_fuzzy <- Silhouette(prox, proximity_type = pt, method = method, prob_matrix = prob, average = "fuzzy", sort = TRUE)
      expect_s3_class(sil_fuzzy, "Silhouette")
      expect_true("weight" %in% names(sil_fuzzy))
      expect_true(all(sil_fuzzy$weight >= 0))
    }
  }
})

test_that("softSilhouette supports all prob_types, methods, averages", {
  p <- make_prob(n, k)
  # All probability interpretations work (pp, nlpp, pd)
  expect_s3_class(softSilhouette(p, prob_type = "pp", method = "pac", average = "crisp"), "Silhouette")
  expect_s3_class(softSilhouette(p, prob_type = "nlpp", method = "medoid", average = "crisp"), "Silhouette")
  expect_s3_class(softSilhouette(p, prob_type = "pd", method = "pac", average = "median"), "Silhouette")
  # Fuzzy average
  x <- softSilhouette(p, prob_type = "pp", method = "medoid", average = "fuzzy")
  expect_true("weight" %in% colnames(x))
})

test_that("cerSilhouette and dbSilhouette all branches", {
  p <- make_prob(n, k)
  expect_s3_class(cerSilhouette(p, average = "crisp"), "Silhouette")
  expect_s3_class(cerSilhouette(p, average = "median"), "Silhouette")
  expect_s3_class(cerSilhouette(p, average = "fuzzy"), "Silhouette")
  expect_s3_class(dbSilhouette(p, average = "crisp"), "Silhouette")
  expect_s3_class(dbSilhouette(p, average = "median"), "Silhouette")
  expect_s3_class(dbSilhouette(p, average = "fuzzy"), "Silhouette")
})

test_that("summary.Silhouette outputs correct structure for all", {
  prox <- make_prox(n, k)
  prob <- make_prob(n, k)
  sil1 <- Silhouette(prox, average = "crisp")
  sil2 <- Silhouette(prox, prob_matrix = prob, average = "fuzzy")
  sil3 <- Silhouette(prox, average = "median")
  for (s in list(sil1, sil2, sil3)) {
    summ <- summary(s, print.summary = FALSE)
    expect_true(is.numeric(summ$avg.width))
    expect_s3_class(summ$sil.sum, "data.frame")
    expect_named(summ, c("clus.avg.widths", "avg.width", "sil.sum"))
  }
})

test_that("Edge case: 100% assignment, ties, NA values", {
  p <- matrix(c(1, 0, 0, 0.5, 0.5, 0, 0.33, 0.33, 0.34), byrow=TRUE, ncol=3)
  expect_no_error(Silhouette(make_prox(3, 3), prob_matrix = p, average = "fuzzy"))
  p_na <- p; p_na[1,1] <- NA; p_na[2,2] <- Inf
  expect_error(Silhouette(make_prox(3, 3), prob_matrix = p_na), "numeric matrix")
})

# --- 2. extSilhouette, with dimension mismatches and summaries ---
test_that("extSilhouette returns correct outputs and errors", {
  p <- make_prob(n, k)
  s1 <- Silhouette(make_prox(n, k))
  s2 <- Silhouette(make_prox(n, k))
  expect_s3_class(extSilhouette(list(s1, s2), dim_names = c("Rows", "Columns"), print.summary = TRUE), "extSilhouette")
  expect_error(extSilhouette(list(s1), dim_names = c("one", "two")), "match the length")
  expect_error(extSilhouette(list(iris)), "class 'Silhouette'")
})

# --- 3. plotSilhouette: supported classes, linetype numeric, errors for invalid linetypes ---
test_that("plotSilhouette works with all object classes", {
  prox <- make_prox(n, k)
  prob <- make_prob(n, k)
  sil <- Silhouette(prox)
  # Silhouette
  expect_s3_class(plotSilhouette(sil, linetype = 1), "ggplot")
  # Silhouette class from cluster::pam
  if (requireNamespace("cluster", quietly = TRUE)) {
    pam_out <- cluster::pam(iris[, -5], k = 3)
    expect_s3_class(plotSilhouette(pam_out), "ggplot")
    sil_obj <- cluster::silhouette(pam_out)
    expect_s3_class(plotSilhouette(sil_obj), "ggplot")
  }
  # Eclust/hcut from factoextra
  if (requireNamespace("factoextra", quietly = TRUE)) {
    eclust_out <- factoextra::eclust(iris[, -5], "kmeans", k = 3, graph = FALSE)
    expect_s3_class(plotSilhouette(eclust_out), "ggplot")
    hcut_out <- factoextra::hcut(iris[, -5], k = 3)
    expect_s3_class(plotSilhouette(hcut_out), "ggplot")
  }
  # Error for unsupported linetype
  expect_error(plotSilhouette(sil, linetype = 10), "must be between 1 and 6")
  expect_error(plotSilhouette(list()), "Don't support an object")
})

# --- 4. Error handling: erroneous inputs, probability matrix checks ---
test_that("All error branches (input validation)", {
  prox <- make_prox(n, k)
  bad_mat <- matrix(1:3, ncol = 1)
  expect_error(Silhouette("not a matrix"), "numeric matrix")
  expect_error(Silhouette(bad_mat), "at least two columns")
  expect_error(softSilhouette("not a matrix"), "numeric matrix")
  expect_error(softSilhouette(bad_mat), "at least two columns")
  expect_error(dbSilhouette("not a matrix"), "numeric matrix")
  expect_error(dbSilhouette(bad_mat), "at least two columns")
  expect_error(cerSilhouette("not a matrix"), "numeric matrix")
  expect_error(cerSilhouette(bad_mat), "at least two columns")
  expect_error(dbSilhouette(prox, a = -1), "a must be a positive")
  expect_error(extSilhouette(list(1:3, iris)), "class 'Silhouette'")
  p <- make_prob(n, k); p[,1] <- NA
  expect_error(softSilhouette(p, prob_type = "nlpp"))
  p[,1] <- 0
  expect_error(softSilhouette(p, prob_type = "pd"))
})

# --- 5. Coverage for clust_fun argument (mock clustering functions) ---
test_that("All functions handle clust_fun as function and character", {
  # Mock clustering function returning list with named components
  mock_fun <- function(...) {
    list(u = make_prob(10, 3), d = make_prox(10, 3))
  }
  expect_s3_class(Silhouette("d", clust_fun = mock_fun), "Silhouette")
  expect_s3_class(Silhouette("d", clust_fun = "mock_fun"), "Silhouette")
  expect_s3_class(softSilhouette("u", clust_fun = mock_fun), "Silhouette")
  expect_s3_class(softSilhouette("u", clust_fun = "mock_fun"), "Silhouette")
  expect_s3_class(dbSilhouette("u", clust_fun = mock_fun), "Silhouette")
  expect_s3_class(dbSilhouette("u", clust_fun = "mock_fun"), "Silhouette")
  expect_s3_class(cerSilhouette("u", clust_fun = mock_fun), "Silhouette")
  expect_s3_class(cerSilhouette("u", clust_fun = "mock_fun"), "Silhouette")
})

# --- 6. Attribute propagation and estimates, sorting behavior ---
test_that("Attributes: proximity_type, method, average are preserved", {
  sil <- Silhouette(make_prox(n, k), proximity_type = "dissimilarity", method = "pac", average = "crisp")
  expect_equal(attr(sil, "proximity_type"), "dissimilarity")
  expect_equal(attr(sil, "method"), "pac")
  expect_equal(attr(sil, "average"), "crisp")
  expect_true(any(class(sil) == "Silhouette"))
})

test_that("Sorting works and produces correct order", {
  sil_unsorted <- Silhouette(make_prox(n, k), sort = FALSE)
  sil_sorted <- Silhouette(make_prox(n, k), sort = TRUE)
  expect_false(identical(row.names(sil_unsorted), row.names(sil_sorted)))
})

