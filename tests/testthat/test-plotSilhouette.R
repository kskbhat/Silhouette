library(testthat)
library(Silhouette)
skip_if_not_installed("ppclust")
skip_if_not_installed("cluster")
skip_if_not_installed("factoextra")

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
  expect_error(plotSilhouette(sil, linetype = 7), "between 0 and 6")

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

test_that("plotSilhouette() works with fuzzy and median Silhouette objects", {
  data(iris)
  fcm_out <- ppclust::fcm(iris[, -5], centers = 3)
  
  # Fuzzy silhouette plot
  sil_fuzzy <- softSilhouette(prob_matrix = fcm_out$u, average = "fuzzy", print.summary = FALSE)
  p_fuzzy <- plotSilhouette(sil_fuzzy)
  expect_s3_class(p_fuzzy, "ggplot")
  
  # Median silhouette plot
  sil_median <- softSilhouette(prob_matrix = fcm_out$u, average = "median", print.summary = FALSE)
  p_median <- plotSilhouette(sil_median)
  expect_s3_class(p_median, "ggplot")
})

test_that("plotSilhouette() checks for invalid 'silhouette' class objects", {
  # 1. Test when silhouette object is not a matrix or data frame
  invalid_sil_obj <- structure(list(a = 1, b = 2), class = "silhouette")
  expect_error(
    plotSilhouette(invalid_sil_obj),
    "silhouette object must be a matrix or data.frame"
  )
  
  # 2. Test when silhouette object has fewer than 3 columns
  small_sil_obj <- structure(matrix(1:4, ncol = 2), class = "silhouette")
  expect_error(
    plotSilhouette(small_sil_obj),
    "silhouette object must have at least 3 columns"
  )
})

test_that("a palette keeps the summary legend", {
  d <- rbind(c(1, 3), c(2, 1), c(1, 4), c(5, 2))
  s <- Silhouette(d)
  p <- plotSilhouette(s, palette = c("red", "blue"))
  fill <- p$scales$get_scales("fill")
  expect_equal(fill$name, "Cluster (Size): Width")
  expect_match(fill$labels, "^C[12] \\(2\\): ")
  expect_equal(unname(fill$palette(2)), c("#FF0000", "#0000FF"))
  p2 <- plotSilhouette(s, palette = "jco", summary.legend = FALSE)
  expect_equal(p2$scales$get_scales("fill")$name, "Cluster")
})

test_that("the reference line is the average that was requested", {
  p <- rbind(c(0.9, 0.1), c(0.6, 0.4), c(0.3, 0.7), c(0.2, 0.8))
  for (av in c("crisp", "fuzzy", "median")) {
    s <- cerSilhouette(p, average = av)
    g <- plotSilhouette(s)
    hline <- g$layers[[2]]$data$yintercept
    if (is.null(hline)) hline <- g$layers[[2]]$aes_params$yintercept
    expect_equal(hline, summary(s, print.summary = FALSE)$avg.width)
  }
})

test_that("objects without silhouette information give a clear error", {
  obj <- structure(list(silinfo = NULL), class = "pam")
  expect_error(plotSilhouette(obj), "no silhouette information")
})

test_that("numeric linetype accepts 0 to 6 only", {
  s <- Silhouette(rbind(c(1, 3), c(2, 1)))
  expect_s3_class(plotSilhouette(s, linetype = 0), "ggplot")
  expect_error(plotSilhouette(s, linetype = 7), "single integer between 0 and 6")
  expect_error(plotSilhouette(s, linetype = c(1, 2)), "single integer between 0 and 6")
})
