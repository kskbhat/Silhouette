library(testthat)
library(Silhouette)
library(ppclust)

set.seed(123) # Global seed for reproducibility

## 1. Basic functionality tests
test_that("plotSilhouette() works with Silhouette objects", {
  data(iris)
  fcm_out <- ppclust::fcm(iris[, -5], centers = 3)
  sil <- softSilhouette(prob_matrix = fcm_out$u, print.summary = FALSE)

  # Test basic plot
  p <- plotSilhouette(sil)
  expect_s3_class(p, "ggplot")

  # Test with label = TRUE
  p <- plotSilhouette(sil, label = TRUE)
  expect_s3_class(p, "ggplot")

  # Test with summary.legend = FALSE
  p <- plotSilhouette(sil, summary.legend = FALSE)
  expect_s3_class(p, "ggplot")

  # Test with grayscale = TRUE
  p <- plotSilhouette(sil, grayscale = TRUE)
  expect_s3_class(p, "ggplot")
})

test_that("plotSilhouette() works with different linetypes", {
  data(iris)
  fcm_out <- ppclust::fcm(iris[, -5], centers = 3)
  sil <- softSilhouette(prob_matrix = fcm_out$u, print.summary = FALSE)

  # Test with different linetypes
  p <- plotSilhouette(sil, linetype = "solid")
  expect_s3_class(p, "ggplot")

  p <- plotSilhouette(sil, linetype = "dotted")
  expect_s3_class(p, "ggplot")

  p <- plotSilhouette(sil, linetype = 1)
  expect_s3_class(p, "ggplot")

  p <- plotSilhouette(sil, linetype = 3)
  expect_s3_class(p, "ggplot")
})

## 2. Error handling tests
test_that("plotSilhouette() errors with invalid Silhouette object", {
  # Test with non-dataframe Silhouette object
  invalid_sil <- structure(list(), class = c("Silhouette", "list"))
  expect_error(plotSilhouette(invalid_sil), "Don't support an object of class Silhouette list")

  # Test with too few columns
  small_sil <- structure(data.frame(a = 1:3, b = 4:6), class = c("Silhouette", "data.frame"))
  expect_error(plotSilhouette(small_sil), "Don't support an object of class Silhouette data.frame")
})

test_that("plotSilhouette() errors with invalid linetype", {
  data(iris)
  fcm_out <- ppclust::fcm(iris[, -5], centers = 3)
  sil <- softSilhouette(prob_matrix = fcm_out$u, print.summary = FALSE)

  # Test with invalid numeric linetype
  expect_error(plotSilhouette(sil, linetype = 7), "must be between 1 and 6")

  # Test with invalid character linetype (this should be handled by match.arg)
  # Note: This might not error because match.arg will select the first valid option
})

test_that("plotSilhouette() errors with unsupported object class", {
  expect_error(plotSilhouette(mtcars), "Don't support an object of class")
})

## 3. Integration tests with other clustering methods
test_that("plotSilhouette() works with cluster package objects", {
  data(iris)

  # Test if cluster package is available
  if (requireNamespace("cluster", quietly = TRUE)) {
    # Test with pam
    pam_result <- cluster::pam(iris[, 1:4], k = 3)
    p <- plotSilhouette(pam_result)
    expect_s3_class(p, "ggplot")

    # Test with clara
    clara_result <- cluster::clara(iris[, 1:4], k = 3)
    p <- plotSilhouette(clara_result)
    expect_s3_class(p, "ggplot")

    # Test with fanny
    fanny_result <- cluster::fanny(iris[, 1:4], k = 3)
    p <- plotSilhouette(fanny_result)
    expect_s3_class(p, "ggplot")

    # Test with silhouette object
    sil_result <- cluster::silhouette(pam_result)
    p <- plotSilhouette(sil_result)
    expect_s3_class(p, "ggplot")
  } else {
    skip("cluster package not available")
  }
})

test_that("plotSilhouette() works with factoextra package objects", {
  data(iris)

  # Test if factoextra package is available
  if (requireNamespace("factoextra", quietly = TRUE)) {
    # Test with eclust
    eclust_result <- factoextra::eclust(iris[, 1:4], "kmeans", k = 3, graph = FALSE)
    p <- plotSilhouette(eclust_result)
    expect_s3_class(p, "ggplot")

    # Test with hcut
    hcut_result <- factoextra::hcut(iris[, 1:4], k = 3)
    p <- plotSilhouette(hcut_result)
    expect_s3_class(p, "ggplot")
  } else {
    skip("factoextra package not available")
  }
})

## 4. Edge case tests
test_that("plot.Silhouette() method works", {
  data(iris)
  fcm_out <- ppclust::fcm(iris[, -5], centers = 3)
  sil <- softSilhouette(prob_matrix = fcm_out$u, print.summary = FALSE)

  # Test the S3 method directly
  p <- plot(sil)
  expect_s3_class(p, "ggplot")
})

test_that("plotSilhouette() works with additional ggplot parameters", {
  data(iris)
  fcm_out <- ppclust::fcm(iris[, -5], centers = 3)
  sil <- softSilhouette(prob_matrix = fcm_out$u, print.summary = FALSE)

  # Test with additional parameters
  p <- plotSilhouette(sil, title = "Custom Title", xlab = "Custom X Label")
  expect_s3_class(p, "ggplot")
})
