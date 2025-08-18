library(testthat)
library(Silhouette)
library(ppclust)

set.seed(123) # Global seed for reproducibility

## 1. Basic functionality tests
test_that("cerSilhouette() works with basic parameters", {
  data(iris)
  fcm_out <- ppclust::fcm(iris[, -5], centers = 3)
  result <- cerSilhouette(
    prob_matrix = fcm_out$u,
    average = "crisp",
    print.summary = FALSE
  )
  expect_s3_class(result, "Silhouette")
  expect_true(all(c("cluster", "neighbor", "sil_width") %in% colnames(result)))
  expect_true(all(result$sil_width >= 0 & result$sil_width <= 1)) # Certainty silhouette should be between 0 and 1
})

test_that("cerSilhouette() works with average = 'fuzzy'", {
  data(iris)
  fcm_out <- ppclust::fcm(iris[, -5], centers = 3)
  result <- cerSilhouette(
    prob_matrix = fcm_out$u,
    average = "fuzzy",
    print.summary = FALSE
  )
  expect_s3_class(result, "Silhouette")
  expect_true("weight" %in% colnames(result))
})

test_that("cerSilhouette() works with average = 'median'", {
  data(iris)
  fcm_out <- ppclust::fcm(iris[, -5], centers = 3)
  result <- cerSilhouette(
    prob_matrix = fcm_out$u,
    average = "median",
    print.summary = FALSE
  )
  expect_s3_class(result, "Silhouette")
})

test_that("cerSilhouette() works with sort = TRUE", {
  data(iris)
  fcm_out <- ppclust::fcm(iris[, -5], centers = 3)
  result <- cerSilhouette(
    prob_matrix = fcm_out$u,
    average = "crisp",
    sort = TRUE,
    print.summary = FALSE
  )
  expect_s3_class(result, "Silhouette")
  # Check if sorted by cluster and descending sil_width
  clusters <- result$cluster
  widths <- result$sil_width
  is_sorted <- all(diff(widths[clusters == clusters[1]]) <= 0) || 
    all(diff(widths[clusters == clusters[2]]) <= 0) || 
    all(diff(widths[clusters == clusters[3]]) <= 0)
  expect_true(is.logical(is_sorted))
})

## 2. clust_fun integration tests
test_that("cerSilhouette() works with clust_fun parameter", {
  data(iris)
  result <- cerSilhouette(
    prob_matrix = "u",
    average = "crisp",
    clust_fun = ppclust::fcm,
    x = iris[, -5],
    centers = 3,
    print.summary = FALSE
  )
  expect_s3_class(result, "Silhouette")
})

test_that("cerSilhouette() works with clust_fun as character string", {
  data(iris)
  result <- cerSilhouette(
    prob_matrix = "u",
    average = "crisp",
    clust_fun = "fcm",
    x = iris[, -5],
    centers = 3,
    print.summary = FALSE
  )
  expect_s3_class(result, "Silhouette")
})

## 3. Error handling tests
test_that("cerSilhouette() errors with invalid prob_matrix", {
  expect_error(cerSilhouette("not a matrix"), "numeric matrix")
  expect_error(cerSilhouette(matrix(1:3, ncol = 1)), "at least two columns")
})

test_that("cerSilhouette() errors with invalid a parameter", {
  data(iris)
  fcm_out <- ppclust::fcm(iris[, -5], centers = 3)
  expect_error(cerSilhouette(fcm_out$u, a = -1), "positive numeric value")
  expect_error(cerSilhouette(fcm_out$u, a = "invalid"), "positive numeric value")
})

test_that("cerSilhouette() errors with invalid average", {
  data(iris)
  fcm_out <- ppclust::fcm(iris[, -5], centers = 3)
  expect_error(cerSilhouette(fcm_out$u, average = "invalid"), "should be one of")
})

test_that("cerSilhouette() errors with rows not summing to 1", {
  data(iris)
  fcm_out <- ppclust::fcm(iris[, -5], centers = 3)
  # Don't normalize rows for this test
  expect_error(cerSilhouette(fcm_out$u * 2), "must sum to 1")
})

test_that("cerSilhouette() errors with clust_fun but invalid prob_matrix", {
  expect_error(cerSilhouette(matrix(1:4, ncol = 2), clust_fun = "fcm"), "single string")
})

test_that("cerSilhouette() errors with clust_fun but invalid function", {
  data(iris)
  expect_error(cerSilhouette("u", clust_fun = 123), "function object or a string")
  expect_error(cerSilhouette("u", clust_fun = "nonexistent_function"), "not found")
})

test_that("cerSilhouette() errors when component not found in clust_fun output", {
  data(iris)
  expect_error(cerSilhouette("nonexistent_component", clust_fun = "fcm", x = iris[, -5], centers = 3), "not found")
})

## 4. Attribute validation tests
test_that("cerSilhouette() sets correct attributes", {
  data(iris)
  fcm_out <- ppclust::fcm(iris[, -5], centers = 3)
  result <- cerSilhouette(
    prob_matrix = fcm_out$u,
    average = "crisp",
    print.summary = FALSE
  )
  expect_equal(attr(result, "proximity_type"), "similarity")
  expect_equal(attr(result, "method"), "certainty")
  expect_equal(attr(result, "average"), "crisp")
})

## 5. Edge case tests
test_that("cerSilhouette() works with a = 1", {
  data(iris)
  fcm_out <- ppclust::fcm(iris[, -5], centers = 3)
  result <- cerSilhouette(
    prob_matrix = fcm_out$u,
    average = "fuzzy",
    a = 1,
    print.summary = FALSE
  )
  expect_s3_class(result, "Silhouette")
})

test_that("cerSilhouette() works with a = 10", {
  data(iris)
  fcm_out <- ppclust::fcm(iris[, -5], centers = 3)
  result <- cerSilhouette(
    prob_matrix = fcm_out$u,
    average = "fuzzy",
    a = 10,
    print.summary = FALSE
  )
  expect_s3_class(result, "Silhouette")
})
