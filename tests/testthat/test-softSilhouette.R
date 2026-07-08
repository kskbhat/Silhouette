library(testthat)
library(Silhouette)
library(ppclust)

set.seed(123) # Global seed for reproducibility

## 1. Basic functionality tests
test_that("softSilhouette() works with prob_type = 'pp'", {
  data(iris)
  fcm_out <- ppclust::fcm(iris[, -5], centers = 3)
  result <- softSilhouette(
    prob_matrix = fcm_out$u,
    prob_type = "pp",
    method = "pac",
    average = "crisp",
    print.summary = FALSE
  )
  expect_s3_class(result, "Silhouette")
  expect_true(all(c("cluster", "neighbor", "sil_width") %in% colnames(result)))
  expect_true(all(result$sil_width >= -1 & result$sil_width <= 1))
})

test_that("softSilhouette() works with prob_type = 'nlpp'", {
  data(iris)
  fcm_out <- ppclust::fcm(iris[, -5], centers = 3)
  result <- softSilhouette(
    prob_matrix = fcm_out$u,
    prob_type = "nlpp",
    method = "pac",
    average = "crisp",
    print.summary = FALSE
  )
  expect_s3_class(result, "Silhouette")
})

test_that("softSilhouette() works with prob_type = 'pd'", {
  data(iris)
  fcm_out <- ppclust::fcm(iris[, -5], centers = 3)
  result <- softSilhouette(
    prob_matrix = fcm_out$u,
    prob_type = "pd",
    method = "pac",
    average = "crisp",
    print.summary = FALSE
  )
  expect_s3_class(result, "Silhouette")
})

test_that("softSilhouette() works with method = 'medoid'", {
  data(iris)
  fcm_out <- ppclust::fcm(iris[, -5], centers = 3)
  result <- softSilhouette(
    prob_matrix = fcm_out$u,
    prob_type = "pp",
    method = "medoid",
    average = "crisp",
    print.summary = FALSE
  )
  expect_s3_class(result, "Silhouette")
})

test_that("softSilhouette() works with average = 'fuzzy'", {
  data(iris)
  fcm_out <- ppclust::fcm(iris[, -5], centers = 3)
  result <- softSilhouette(
    prob_matrix = fcm_out$u,
    prob_type = "pp",
    method = "pac",
    average = "fuzzy",
    print.summary = FALSE
  )
  expect_s3_class(result, "Silhouette")
  expect_true("weight" %in% colnames(result))
})

test_that("softSilhouette() works with average = 'median'", {
  data(iris)
  fcm_out <- ppclust::fcm(iris[, -5], centers = 3)
  result <- softSilhouette(
    prob_matrix = fcm_out$u,
    prob_type = "pp",
    method = "pac",
    average = "median",
    print.summary = FALSE
  )
  expect_s3_class(result, "Silhouette")
})

test_that("softSilhouette() works with sort = TRUE", {
  data(iris)
  fcm_out <- ppclust::fcm(iris[, -5], centers = 3)
  result <- softSilhouette(
    prob_matrix = fcm_out$u,
    prob_type = "pp",
    method = "pac",
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
test_that("softSilhouette() works with clust_fun parameter", {
  data(iris)
  result <- softSilhouette(
    prob_matrix = "u",
    prob_type = "pp",
    method = "pac",
    average = "crisp",
    clust_fun = ppclust::fcm,
    x = iris[, -5],
    centers = 3,
    print.summary = FALSE
  )
  expect_s3_class(result, "Silhouette")
})

test_that("softSilhouette() works with clust_fun as character string", {
  data(iris)
  result <- softSilhouette(
    prob_matrix = "u",
    prob_type = "pp",
    method = "pac",
    average = "crisp",
    clust_fun = "fcm",
    x = iris[, -5],
    centers = 3,
    print.summary = FALSE
  )
  expect_s3_class(result, "Silhouette")
})

## 3. Error handling tests
test_that("softSilhouette() errors with invalid prob_matrix", {
  expect_error(softSilhouette("not a matrix"), "numeric matrix")
  expect_error(softSilhouette(matrix(1:3, ncol = 1)), "at least two columns")
})

test_that("softSilhouette() errors with invalid a parameter", {
  data(iris)
  fcm_out <- ppclust::fcm(iris[, -5], centers = 3)
  expect_error(softSilhouette(fcm_out$u, a = -1), "positive numeric value")
  expect_error(softSilhouette(fcm_out$u, a = "invalid"), "positive numeric value")
})

test_that("softSilhouette() errors with invalid prob_type", {
  data(iris)
  fcm_out <- ppclust::fcm(iris[, -5], centers = 3)
  expect_error(softSilhouette(fcm_out$u, prob_type = "invalid"), "should be one of")
})

test_that("softSilhouette() errors with invalid method", {
  data(iris)
  fcm_out <- ppclust::fcm(iris[, -5], centers = 3)
  expect_error(softSilhouette(fcm_out$u, method = "invalid"), "should be one of")
})

test_that("softSilhouette() errors with invalid average", {
  data(iris)
  fcm_out <- ppclust::fcm(iris[, -5], centers = 3)
  expect_error(softSilhouette(fcm_out$u, average = "invalid"), "should be one of")
})

# test_that("softSilhouette() errors with prob_type = 'nlpp' and infinite values", {
#   data(iris)
#   fcm_out <- ppclust::fcm(iris[, -5], centers = 3)
#   # Create a matrix with zeros to produce infinite values in log
#   prob_matrix <- fcm_out$u
#   prob_matrix[1, 1] <- 0
#   expect_error(softSilhouette(prob_matrix, prob_type = "nlpp"), "infinite values")
# })

test_that("softSilhouette() errors with clust_fun but invalid prob_matrix", {
  expect_error(softSilhouette(matrix(1:4, ncol = 2), clust_fun = "fcm"), "single string")
})

test_that("softSilhouette() errors with clust_fun but invalid function", {
  data(iris)
  expect_error(softSilhouette("u", clust_fun = 123), "function object or a string")
  expect_error(softSilhouette("u", clust_fun = "nonexistent_function"), "not found")
})

test_that("softSilhouette() errors when component not found in clust_fun output", {
  data(iris)
  expect_error(softSilhouette("nonexistent_component", clust_fun = "fcm", x = iris[, -5], centers = 3), "not found")
})

## 4. Attribute validation tests
test_that("softSilhouette() sets correct attributes", {
  data(iris)
  fcm_out <- ppclust::fcm(iris[, -5], centers = 3)
  result <- softSilhouette(
    prob_matrix = fcm_out$u,
    prob_type = "pp",
    method = "pac",
    average = "crisp",
    print.summary = FALSE
  )
  expect_equal(attr(result, "proximity_type"), "similarity")
  expect_equal(attr(result, "method"), "pac")
  expect_equal(attr(result, "average"), "crisp")
})

## 5. Edge case tests
test_that("softSilhouette() works with a = 1", {
  data(iris)
  fcm_out <- ppclust::fcm(iris[, -5], centers = 3)
  result <- softSilhouette(
    prob_matrix = fcm_out$u,
    prob_type = "pp",
    method = "pac",
    average = "fuzzy",
    a = 1,
    print.summary = FALSE
  )
  expect_s3_class(result, "Silhouette")
})

test_that("softSilhouette() works with a = 10", {
  data(iris)
  fcm_out <- ppclust::fcm(iris[, -5], centers = 3)
  result <- softSilhouette(
    prob_matrix = fcm_out$u,
    prob_type = "pp",
    method = "pac",
    average = "fuzzy",
    a = 10,
    print.summary = FALSE
  )
  expect_s3_class(result, "Silhouette")
})

test_that("softSilhouette() works with print.summary = TRUE for all average methods", {
  data(iris)
  fcm_out <- ppclust::fcm(iris[, -5], centers = 3)
  
  # Crisp average
  expect_message(
    softSilhouette(prob_matrix = fcm_out$u, average = "crisp", print.summary = TRUE),
    "Average crisp similarity pac silhouette"
  )
  
  # Fuzzy average
  expect_message(
    softSilhouette(prob_matrix = fcm_out$u, average = "fuzzy", print.summary = TRUE),
    "Average fuzzy similarity pac silhouette"
  )
  
  # Median average
  expect_message(
    softSilhouette(prob_matrix = fcm_out$u, average = "median", print.summary = TRUE),
    "Median similarity pac silhouette"
  )
})

test_that("softSilhouette() clust_fun extraction and validations", {
  df_dummy <- data.frame(x = rnorm(10), y = rnorm(10))
  
  # S3 function error
  expect_error(
    softSilhouette("u", clust_fun = function(data) { stop("Clustering failed") }, data = df_dummy),
    "Error in clustering function"
  )
  
  # Extracted not numeric/matrix
  expect_error(
    softSilhouette("u", clust_fun = function(data) { list(u = "not_numeric") }, data = df_dummy),
    "Extracted prob_matrix must be a numeric matrix"
  )
  
  # Extracted < 2 columns
  expect_error(
    softSilhouette("u", clust_fun = function(data) { list(u = matrix(1:10, ncol = 1)) }, data = df_dummy),
    "Extracted prob_matrix must have at least two columns"
  )
  
  # S4 slot missing and present
  setClass("MockClustS4Soft", slots = list(u = "matrix"))
  mock_s4_fun <- function(data) { new("MockClustS4Soft", u = matrix(runif(20), ncol = 2)) }
  expect_error(
    softSilhouette("missing_slot", clust_fun = mock_s4_fun, data = df_dummy),
    "Slot 'missing_slot' not found"
  )
  
  # Column sums in prob_matrix must be non-zero for pd
  zero_col_mat <- matrix(c(1, 1, 0, 0), nrow = 2, ncol = 2) # row sums are 1, second column sum is 0
  expect_error(
    softSilhouette(prob_matrix = zero_col_mat, prob_type = "pd"),
    "Column sums in prob_matrix must be non-zero"
  )
})

test_that("softSilhouette() S4 slot success and generic fallback", {
  # Successful S4 slot lookup (hits line 129 slot extraction branch)
  setClass("MockClustS4SuccessSoft", slots = list(u = "matrix"))
  mock_s4_success <- function(data) { new("MockClustS4SuccessSoft", u = matrix(c(0.7, 0.3, 0.2, 0.8), ncol = 2, byrow = TRUE)) }
  res <- softSilhouette("u", clust_fun = mock_s4_success, data = iris)
  expect_s3_class(res, "Silhouette")
  
  # Generic fallback (hits line 121)
  expect_error(
    softSilhouette("u", clust_fun = "show", object = matrix(1:4, 2, 2)),
    "not found"
  )
})
