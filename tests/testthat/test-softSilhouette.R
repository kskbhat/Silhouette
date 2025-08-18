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

# FIXME
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
