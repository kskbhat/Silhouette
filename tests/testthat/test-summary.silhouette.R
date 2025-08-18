library(testthat)
library(Silhouette)
library(ppclust)

set.seed(123) # Global seed for reproducibility

## 1. Basic functionality tests
test_that("summary.Silhouette() works with basic parameters", {
  data(iris)
  fcm_out <- ppclust::fcm(iris[, -5], centers = 3)
  sil <- softSilhouette(prob_matrix = fcm_out$u, print.summary = FALSE)

  # Test summary with print.summary = TRUE (default)
  result <- summary(sil, print.summary = FALSE)

  expect_true(is.list(result))
  expect_true(all(c("clus.avg.widths", "avg.width", "sil.sum") %in% names(result)))
  expect_true(is.numeric(result$avg.width))
  expect_s3_class(result$sil.sum, "data.frame")
  expect_true(all(c("cluster", "size", "avg.sil.width") %in% names(result$sil.sum)))
})

test_that("summary.Silhouette() works with different average methods", {
  data(iris)
  fcm_out <- ppclust::fcm(iris[, -5], centers = 3)

  # Test with crisp average
  sil_crisp <- softSilhouette(prob_matrix = fcm_out$u, average = "crisp", print.summary = FALSE)
  result_crisp <- summary(sil_crisp, print.summary = FALSE)

  expect_true(is.list(result_crisp))
  expect_true(is.numeric(result_crisp$avg.width))

  # Test with fuzzy average
  sil_fuzzy <- softSilhouette(prob_matrix = fcm_out$u, average = "fuzzy", print.summary = FALSE)
  result_fuzzy <- summary(sil_fuzzy, print.summary = FALSE)

  expect_true(is.list(result_fuzzy))
  expect_true(is.numeric(result_fuzzy$avg.width))

  # Test with median average
  sil_median <- softSilhouette(prob_matrix = fcm_out$u, average = "median", print.summary = FALSE)
  result_median <- summary(sil_median, print.summary = FALSE)

  expect_true(is.list(result_median))
  expect_true(is.numeric(result_median$avg.width))
})

## 2. Error handling tests
# test_that("summary.Silhouette() errors with invalid object", {
#   invalid_sil <- structure(list(), class = "Silhouette")
#   expect_error(summary(invalid_sil), "object must be of class 'Silhouette'")
# })

## 3. Output validation tests
test_that("summary.Silhouette() returns correct structure", {
  data(iris)
  fcm_out <- ppclust::fcm(iris[, -5], centers = 3)
  sil <- softSilhouette(prob_matrix = fcm_out$u, print.summary = FALSE)

  result <- summary(sil, print.summary = FALSE)

  # Check clus.avg.widths
  expect_true(is.numeric(result$clus.avg.widths))
  expect_true(length(result$clus.avg.widths) == 3) # 3 clusters
  expect_true(!is.null(names(result$clus.avg.widths)))

  # Check avg.width
  expect_true(is.numeric(result$avg.width))
  expect_true(length(result$avg.width) == 1)
  expect_true(result$avg.width >= -1 & result$avg.width <= 1)

  # Check sil.sum
  expect_s3_class(result$sil.sum, "data.frame")
  expect_true(nrow(result$sil.sum) == 3) # 3 clusters
  expect_true(all(c("cluster", "size", "avg.sil.width") %in% names(result$sil.sum)))
  expect_true(is.character(result$sil.sum$cluster))
  expect_true(is.numeric(result$sil.sum$size))
  expect_true(is.numeric(result$sil.sum$avg.sil.width))
})

test_that("summary.Silhouette() works with different proximity types and methods", {
  data(iris)
  fcm_out <- ppclust::fcm(iris[, -5], centers = 3)

  # Test with similarity and pac
  sil1 <- softSilhouette(prob_matrix = fcm_out$u, prob_type = "pp", method = "pac", print.summary = FALSE)
  result1 <- summary(sil1, print.summary = FALSE)

  expect_true(is.list(result1))
  expect_true(is.numeric(result1$avg.width))

  # Test with dissimilarity and medoid
  sil2 <- softSilhouette(prob_matrix = fcm_out$u, prob_type = "nlpp", method = "medoid", print.summary = FALSE)
  result2 <- summary(sil2, print.summary = FALSE)

  expect_true(is.list(result2))
  expect_true(is.numeric(result2$avg.width))
})

## 4. Edge case tests
# test_that("summary.Silhouette() works with single cluster", {
#   # This test might be challenging to set up, so we'll skip it for now
#   # as it would require a special case in the Silhouette function
#   skip("Single cluster case not implemented")
# })

test_that("summary.Silhouette() works with large number of clusters", {
  data(iris)
  # Use a larger number of centers if possible
  fcm_out <- ppclust::fcm(iris[, -5], centers = 5)
  sil <- softSilhouette(prob_matrix = fcm_out$u, print.summary = FALSE)

  result <- summary(sil, print.summary = FALSE)

  expect_true(is.list(result))
  expect_true(length(result$clus.avg.widths) == 5)
  expect_true(nrow(result$sil.sum) == 5)
})

test_that("summary.Silhouette() returns invisible result", {
  data(iris)
  fcm_out <- ppclust::fcm(iris[, -5], centers = 3)
  sil <- softSilhouette(prob_matrix = fcm_out$u, print.summary = FALSE)

  # Capture the result to check if it's invisible
  result <- capture.output(summary(sil, print.summary = FALSE))
  expect_true(length(result) == 0) # Should be empty because result is invisible
})
