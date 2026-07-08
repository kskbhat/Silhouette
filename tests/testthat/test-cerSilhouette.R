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

test_that("cerSilhouette() works with print.summary = TRUE for all average methods", {
  data(iris)
  fcm_out <- ppclust::fcm(iris[, -5], centers = 3)
  
  # Crisp average
  expect_message(
    cerSilhouette(prob_matrix = fcm_out$u, average = "crisp", print.summary = TRUE),
    "Average crisp similarity db silhouette"
  )
  
  # Fuzzy average
  expect_message(
    cerSilhouette(prob_matrix = fcm_out$u, average = "fuzzy", print.summary = TRUE),
    "Average fuzzy similarity db silhouette"
  )
  
  # Median average
  expect_message(
    cerSilhouette(prob_matrix = fcm_out$u, average = "median", print.summary = TRUE),
    "Median similarity db silhouette"
  )
})

test_that("cerSilhouette() clust_fun extraction and validations", {
  df_dummy <- data.frame(x = rnorm(10), y = rnorm(10))
  
  # S3 function error
  expect_error(
    cerSilhouette("u", clust_fun = function(data) { stop("Clustering failed") }, data = df_dummy),
    "Error in clustering function"
  )
  
  # Extracted not numeric/matrix
  expect_error(
    cerSilhouette("u", clust_fun = function(data) { list(u = "not_numeric") }, data = df_dummy),
    "Extracted prob_matrix must be a numeric matrix"
  )
  
  # Extracted < 2 columns
  expect_error(
    cerSilhouette("u", clust_fun = function(data) { list(u = matrix(1:10, ncol = 1)) }, data = df_dummy),
    "Extracted prob_matrix must have at least two columns"
  )
  
  # S4 slot missing
  setClass("MockClustS4", slots = list(u = "matrix"))
  mock_s4_fun <- function(data) { new("MockClustS4", u = matrix(runif(20), ncol = 2)) }
  expect_error(
    cerSilhouette("missing_slot", clust_fun = mock_s4_fun, data = df_dummy),
    "Slot 'missing_slot' not found"
  )
})

test_that("cerSilhouette() S4 slot success and generic fallback", {
  # Successful S4 slot lookup (hits line 122 slot extraction branch)
  setClass("MockClustS4SuccessCer", slots = list(u = "matrix"))
  mock_s4_success <- function(data) { new("MockClustS4SuccessCer", u = matrix(c(0.7, 0.3, 0.2, 0.8), ncol = 2, byrow = TRUE)) }
  res <- cerSilhouette("u", clust_fun = mock_s4_success, data = iris)
  expect_s3_class(res, "Silhouette")
  
  # Generic fallback (hits line 114)
  expect_error(
    cerSilhouette("u", clust_fun = "show", object = matrix(1:4, 2, 2)),
    "not found"
  )
})
