library(testthat)
library(Silhouette)
library(ppclust)
library(proxy)

set.seed(123) # Global seed for reproducibility

## 1. Basic functionality tests
test_that("Silhouette() works with basic parameters", {
  data(iris)
  km <- kmeans(iris[, -5], centers = 3)
  dist_mat <- proxy::dist(iris[, -5], km$centers)
  result <- Silhouette(dist_mat, print.summary = FALSE)
  
  expect_s3_class(result, "Silhouette")
  expect_true(all(c("cluster", "neighbor", "sil_width") %in% colnames(result)))
  expect_true(all(result$sil_width >= -1 & result$sil_width <= 1))
})

test_that("Silhouette() works with proximity_type = 'similarity'", {
  data(iris)
  km <- kmeans(iris[, -5], centers = 3)
  dist_mat <- proxy::dist(iris[, -5], km$centers)
  # Convert to similarity matrix
  sim_mat <- 1 / (1 + dist_mat)
  result <- Silhouette(sim_mat, proximity_type = "similarity", print.summary = FALSE)
  
  expect_s3_class(result, "Silhouette")
})

test_that("Silhouette() works with method = 'pac'", {
  data(iris)
  km <- kmeans(iris[, -5], centers = 3)
  dist_mat <- proxy::dist(iris[, -5], km$centers)
  result <- Silhouette(dist_mat, method = "pac", print.summary = FALSE)
  
  expect_s3_class(result, "Silhouette")
})

test_that("Silhouette() works with method = 'medoid'", {
  data(iris)
  km <- kmeans(iris[, -5], centers = 3)
  dist_mat <- proxy::dist(iris[, -5], km$centers)
  result <- Silhouette(dist_mat, method = "medoid", print.summary = FALSE)
  
  expect_s3_class(result, "Silhouette")
})

test_that("Silhouette() works with average = 'fuzzy'", {
  data(iris)
  km <- kmeans(iris[, -5], centers = 3)
  dist_mat <- proxy::dist(iris[, -5], km$centers)
  
  # Create a dummy probability matrix for testing
  prob_mat <- matrix(runif(nrow(iris) * 3), nrow = nrow(iris), ncol = 3)
  prob_mat <- prob_mat / rowSums(prob_mat)  # Normalize to sum to 1
  
  result <- Silhouette(
    prox_matrix = dist_mat,
    prob_matrix = prob_mat,
    average = "fuzzy",
    print.summary = FALSE
  )
  
  expect_s3_class(result, "Silhouette")
  expect_true("weight" %in% colnames(result))
})

test_that("Silhouette() works with average = 'median'", {
  data(iris)
  km <- kmeans(iris[, -5], centers = 3)
  dist_mat <- proxy::dist(iris[, -5], km$centers)
  result <- Silhouette(dist_mat, average = "median", print.summary = FALSE)
  
  expect_s3_class(result, "Silhouette")
})

test_that("Silhouette() works with sort = TRUE", {
  data(iris)
  km <- kmeans(iris[, -5], centers = 3)
  dist_mat <- proxy::dist(iris[, -5], km$centers)
  result <- Silhouette(dist_mat, sort = TRUE, print.summary = FALSE)
  
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
test_that("Silhouette() works with clust_fun parameter", {
  data(iris)
  result <- Silhouette(
    prox_matrix = "d",
    proximity_type = "dissimilarity",
    prob_matrix = "u",
    clust_fun = ppclust::fcm,
    x = iris[, -5],
    centers = 3,
    print.summary = FALSE
  )
  
  expect_s3_class(result, "Silhouette")
})

test_that("Silhouette() works with clust_fun as character string", {
  data(iris)
  result <- Silhouette(
    prox_matrix = "d",
    proximity_type = "dissimilarity",
    prob_matrix = "u",
    clust_fun = "fcm",
    x = iris[, -5],
    centers = 3,
    print.summary = FALSE
  )
  
  expect_s3_class(result, "Silhouette")
})

## 3. Error handling tests
test_that("Silhouette() errors with invalid prox_matrix", {
  expect_error(Silhouette("not a matrix"), "numeric matrix")
  expect_error(Silhouette(matrix(1:3, ncol = 1)), "at least two columns")
})

test_that("Silhouette() errors with invalid prob_matrix", {
  data(iris)
  km <- kmeans(iris[, -5], centers = 3)
  dist_mat <- proxy::dist(iris[, -5], km$centers)
  
  expect_error(Silhouette(dist_mat, prob_matrix = "not a matrix"), "numeric matrix")
  expect_error(Silhouette(dist_mat, prob_matrix = matrix(1:3, ncol = 1)), "at least two columns")
})

test_that("Silhouette() errors with invalid a parameter", {
  data(iris)
  km <- kmeans(iris[, -5], centers = 3)
  dist_mat <- proxy::dist(iris[, -5], km$centers)
  
  expect_error(Silhouette(dist_mat, a = -1), "positive numeric value")
  expect_error(Silhouette(dist_mat, a = "invalid"), "positive numeric value")
})

test_that("Silhouette() errors with invalid proximity_type", {
  data(iris)
  km <- kmeans(iris[, -5], centers = 3)
  dist_mat <- proxy::dist(iris[, -5], km$centers)
  
  expect_error(Silhouette(dist_mat, proximity_type = "invalid"), "should be one of")
})

test_that("Silhouette() errors with invalid method", {
  data(iris)
  km <- kmeans(iris[, -5], centers = 3)
  dist_mat <- proxy::dist(iris[, -5], km$centers)
  
  expect_error(Silhouette(dist_mat, method = "invalid"), "should be one of")
})

test_that("Silhouette() errors with invalid average", {
  data(iris)
  km <- kmeans(iris[, -5], centers = 3)
  dist_mat <- proxy::dist(iris[, -5], km$centers)
  
  expect_error(Silhouette(dist_mat, average = "invalid"), "should be one of")
})

test_that("Silhouette() errors with prob_matrix rows not summing to 1", {
  data(iris)
  km <- kmeans(iris[, -5], centers = 3)
  dist_mat <- proxy::dist(iris[, -5], km$centers)
  
  # Create a probability matrix that doesn't sum to 1
  prob_mat <- matrix(runif(nrow(iris) * 3), nrow = nrow(iris), ncol = 3)
  
  expect_error(Silhouette(dist_mat, prob_matrix = prob_mat), "must sum to 1")
})

test_that("Silhouette() errors with clust_fun but invalid prox_matrix", {
  expect_error(Silhouette(matrix(1:4, ncol = 2), clust_fun = "fcm"), "single string")
})

test_that("Silhouette() errors with clust_fun but invalid function", {
  data(iris)
  expect_error(Silhouette("d", clust_fun = 123), "function object or a string")
  expect_error(Silhouette("d", clust_fun = "nonexistent_function"), "not found")
})

test_that("Silhouette() errors when component not found in clust_fun output", {
  data(iris)
  expect_error(Silhouette("nonexistent_component", clust_fun = "fcm", x = iris[, -5], centers = 3), "not found")
})

## 4. Attribute validation tests
test_that("Silhouette() sets correct attributes", {
  data(iris)
  km <- kmeans(iris[, -5], centers = 3)
  dist_mat <- proxy::dist(iris[, -5], km$centers)
  result <- Silhouette(dist_mat, print.summary = FALSE)
  
  expect_equal(attr(result, "proximity_type"), "dissimilarity")
  expect_equal(attr(result, "method"), "medoid")
  expect_equal(attr(result, "average"), "crisp")
})

## 5. Edge case tests
test_that("Silhouette() works with a = 1", {
  data(iris)
  km <- kmeans(iris[, -5], centers = 3)
  dist_mat <- proxy::dist(iris[, -5], km$centers)
  
  # Create a dummy probability matrix for testing
  prob_mat <- matrix(runif(nrow(iris) * 3), nrow = nrow(iris), ncol = 3)
  prob_mat <- prob_mat / rowSums(prob_mat)  # Normalize to sum to 1
  
  result <- Silhouette(
    prox_matrix = dist_mat,
    prob_matrix = prob_mat,
    average = "fuzzy",
    a = 1,
    print.summary = FALSE
  )
  
  expect_s3_class(result, "Silhouette")
})

test_that("Silhouette() works with a = 10", {
  data(iris)
  km <- kmeans(iris[, -5], centers = 3)
  dist_mat <- proxy::dist(iris[, -5], km$centers)
  
  # Create a dummy probability matrix for testing
  prob_mat <- matrix(runif(nrow(iris) * 3), nrow = nrow(iris), ncol = 3)
  prob_mat <- prob_mat / rowSums(prob_mat)  # Normalize to sum to 1
  
  result <- Silhouette(
    prox_matrix = dist_mat,
    prob_matrix = prob_mat,
    average = "fuzzy",
    a = 10,
    print.summary = FALSE
  )
  
  expect_s3_class(result, "Silhouette")
})

test_that("Silhouette() falls back to crisp when average = 'fuzzy' but no prob_matrix", {
  data(iris)
  km <- kmeans(iris[, -5], centers = 3)
  dist_mat <- proxy::dist(iris[, -5], km$centers)
  
  # Capture warning
  expect_warning(
    result <- Silhouette(dist_mat, average = "fuzzy", print.summary = FALSE),
    "average = 'fuzzy' requires prob_matrix; falling back to 'crisp'"
  )
  
  expect_s3_class(result, "Silhouette")
  expect_equal(attr(result, "average"), "crisp")
})
