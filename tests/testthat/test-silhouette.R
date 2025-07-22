# tests/testthat/test-silhouette.R

library(testthat)
library(Silhouette)
library(proxy)
library(ppclust)
library(blockcluster)

set.seed(123) # Global seed for reproducibility

## 1. Crisp Silhouette: Basic functionality and summary output
test_that("Silhouette() computes correct output for crisp clustering", {
  data(iris)
  km <- kmeans(iris[, -5], centers = 3)
  dist_mat <- proxy::dist(iris[, -5], km$centers)
  sil <- Silhouette(dist_mat, method = "medoid", print.summary = FALSE)
  expect_s3_class(sil, "Silhouette")
  expect_true(all(c("cluster", "neighbor", "sil_width") %in% colnames(sil)))

  summ <- summary(sil, print.summary = FALSE)
  expect_named(summ, c("clus.avg.widths", "avg.width", "sil.sum"))
  expect_true(is.numeric(summ$avg.width))
})

## 2. Fuzzy Silhouette: With proximity and membership probabilities
test_that("Silhouette() computes correct fuzzy silhouette and weights", {
  data(iris)
  fcm_out <- ppclust::fcm(iris[, -5], centers = 3)
  sil_fuzzy <- Silhouette(
    prox_matrix = fcm_out$d,
    prob_matrix = fcm_out$u,
    method = "pac",
    print.summary = FALSE
  )
  expect_s3_class(sil_fuzzy, "Silhouette")
  expect_true("weight" %in% colnames(sil_fuzzy))
  expect_true(all(sil_fuzzy$weight >= 0))
  summ <- summary(sil_fuzzy, print.summary = FALSE)
  expect_true(is.numeric(summ$avg.width))
})

## 3. Fuzzy Silhouette: Standalone softSilhouette()
test_that("softSilhouette() works for basic fuzzy input", {
  data(iris)
  fcm_out <- ppclust::fcm(iris[, -5], centers = 3)
  soft_sil <- softSilhouette(
    prob_matrix = fcm_out$u,
    prob_type = "pp",
    method = "pac",
    average = "fuzzy",
    print.summary = FALSE
  )
  expect_s3_class(soft_sil, "Silhouette")
  expect_true(all(soft_sil$sil_width >= -1 & soft_sil$sil_width <= 1))
})

## 4. Extended Silhouette: extSilhouette() with Silhouette objects
test_that("extSilhouette() returns the correct structure", {
  data(iris)
  fcm_out <- ppclust::fcm(iris[, -5], centers = 3)
  sil_rows <- softSilhouette(prob_matrix = fcm_out$u, method = "pac", print.summary = FALSE)
  sil_cols <- softSilhouette(prob_matrix = fcm_out$u, method = "pac", print.summary = FALSE)
  ext_sil <- extSilhouette(list(sil_rows, sil_cols), dim_names = c("Rows", "Columns"), print.summary = FALSE)
  expect_true(is.list(ext_sil))
  expect_true(all(c("ext_sil_width", "dim_table") %in% names(ext_sil)))
  expect_true(is.numeric(ext_sil$ext_sil_width))
  expect_s3_class(ext_sil$dim_table, "data.frame")
  expect_equal(nrow(ext_sil$dim_table), 2)
})

## 5. plotSilhouette: Returns ggplot output
test_that("plotSilhouette() returns a ggplot object for various inputs", {
  data(iris)
  km <- kmeans(iris[, -5], centers = 3)
  dist_mat <- proxy::dist(iris[, -5], km$centers)
  sil_obj <- Silhouette(dist_mat, print.summary = FALSE)
  p1 <- plotSilhouette(sil_obj, grayscale = TRUE)
  expect_s3_class(p1, "ggplot")

  # Test for cluster::pam if installed
  if (requireNamespace("cluster", quietly = TRUE)) {
    pam_out <- cluster::pam(iris[, -5], k = 3)
    expect_s3_class(plotSilhouette(pam_out), "ggplot")
  }

  # Test for factoextra::eclust if installed
  if (requireNamespace("factoextra", quietly = TRUE)) {
    eclust_out <- factoextra::eclust(iris[, -5], "kmeans", k = 3, graph = FALSE)
    expect_s3_class(plotSilhouette(eclust_out), "ggplot")
  }
})

## 6. summary.Silhouette: Output format
test_that("summary.Silhouette() returns named elements and data frame", {
  data(iris)
  km <- kmeans(iris[, -5], centers = 3)
  dist_mat <- proxy::dist(iris[, -5], km$centers)
  sil <- Silhouette(dist_mat, print.summary = FALSE)
  summ <- summary(sil, print.summary = FALSE)
  expect_true(is.double(summ$clus.avg.widths))
  expect_true(is.double(summ$avg.width))
  expect_s3_class(summ$sil.sum, "data.frame")
  expect_true(all(c("cluster", "size", "avg.sil.width") %in% names(summ$sil.sum)))
})

## 7. Robust error handling: Invalid and edge-case inputs
test_that("Functions error appropriately on bad input", {
  expect_error(Silhouette("not a matrix"), "numeric matrix")
  expect_error(Silhouette(matrix(1:5, ncol=1)), "at least two columns")
  expect_error(softSilhouette("not a matrix"), "numeric matrix")
  expect_error(softSilhouette(matrix(1:3, ncol=1)), "at least two columns")
  expect_error(extSilhouette(list(1:3, mtcars)), "class 'Silhouette'")
  expect_error(plotSilhouette(mtcars), "Don't support an object of class")
})
