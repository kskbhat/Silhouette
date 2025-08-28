library(testthat)
library(Silhouette)

set.seed(123) # Global seed for reproducibility

## Basic functionality tests for summarySilhouette
test_that("summarySilhouette() works with prox_matrix only", {
  # Create a simple distance matrix for testing
  set.seed(123)
  data(iris)
  
  # Create a simple clustering and distance matrix
  km_result <- kmeans(iris[, -5], centers = 3)
  centers <- km_result$centers
  
  # Calculate distance matrix (observations x clusters)
  prox_matrix <- matrix(0, nrow = nrow(iris), ncol = 3)
  for (i in 1:nrow(iris)) {
    for (j in 1:3) {
      prox_matrix[i, j] <- sqrt(sum((iris[i, -5] - centers[j, ])^2))
    }
  }
  
  result <- summarySilhouette(
    prox_matrix = prox_matrix,
    proximity_type = "dissimilarity",
    print.summary = FALSE
  )
  
  expect_s3_class(result, "data.frame")
  expect_true(all(c("Method", "Crisp_Silhouette", "Fuzzy_Silhouette", "Median_Silhouette") %in% colnames(result)))
  expect_true("medoid" %in% result$Method)
  expect_true("pac" %in% result$Method)
  expect_true(all(!is.na(result$Crisp_Silhouette)))
  expect_true(all(is.na(result$Fuzzy_Silhouette))) # Should be NA without prob_matrix
  expect_true(all(!is.na(result$Median_Silhouette)))
})

test_that("summarySilhouette() works with prob_matrix only", {
  # Create a simple probability matrix for testing
  set.seed(123)
  prob_matrix <- matrix(runif(150), nrow = 50, ncol = 3)
  # Normalize to sum to 1 per row
  prob_matrix <- prob_matrix / rowSums(prob_matrix)
  
  result <- summarySilhouette(
    prob_matrix = prob_matrix,
    print.summary = FALSE
  )
  
  expect_s3_class(result, "data.frame")
  expect_true(all(c("Method", "Crisp_Silhouette", "Fuzzy_Silhouette", "Median_Silhouette") %in% colnames(result)))
  expect_true("pp" %in% result$Method)
  expect_true("nlpp" %in% result$Method)
  expect_true("pd" %in% result$Method)
  expect_true("cer" %in% result$Method)
  expect_true("db" %in% result$Method)
  expect_true(all(!is.na(result$Crisp_Silhouette)))
  expect_true(all(!is.na(result$Fuzzy_Silhouette)))
  expect_true(all(!is.na(result$Median_Silhouette)))
})

test_that("summarySilhouette() works with both prox_matrix and prob_matrix", {
  # Create both matrices for testing
  set.seed(123)
  data(iris)
  
  # Create a simple clustering and distance matrix
  km_result <- kmeans(iris[, -5], centers = 3)
  centers <- km_result$centers
  
  # Calculate distance matrix (observations x clusters)
  prox_matrix <- matrix(0, nrow = nrow(iris), ncol = 3)
  for (i in 1:nrow(iris)) {
    for (j in 1:3) {
      prox_matrix[i, j] <- sqrt(sum((iris[i, -5] - centers[j, ])^2))
    }
  }
  
  # Create probability matrix
  prob_matrix <- matrix(runif(450), nrow = 150, ncol = 3)
  prob_matrix <- prob_matrix / rowSums(prob_matrix)
  
  result <- summarySilhouette(
    prox_matrix = prox_matrix,
    prob_matrix = prob_matrix,
    proximity_type = "dissimilarity",
    print.summary = FALSE
  )
  
  expect_s3_class(result, "data.frame")
  expect_true(all(c("Method", "Crisp_Silhouette", "Fuzzy_Silhouette", "Median_Silhouette") %in% colnames(result)))
  # Should have all methods
  expected_methods <- c("medoid", "pac", "pp", "nlpp", "pd", "cer", "db")
  expect_true(all(expected_methods %in% result$Method))
  expect_true(all(!is.na(result$Crisp_Silhouette)))
  expect_true(all(!is.na(result$Median_Silhouette)))
  # Fuzzy silhouette should be available for all methods when prob_matrix is provided
  expect_true(all(!is.na(result$Fuzzy_Silhouette)))
})

test_that("summarySilhouette() throws error when no matrices provided", {
  expect_error(
    summarySilhouette(),
    "At least one of 'prox_matrix' or 'prob_matrix' must be provided."
  )
})

test_that("summarySilhouette() validates prox_matrix", {
  expect_error(
    summarySilhouette(prox_matrix = "invalid"),
    "prox_matrix must be a numeric matrix."
  )
  
  expect_error(
    summarySilhouette(prox_matrix = matrix(1, nrow = 5, ncol = 1)),
    "prox_matrix must have at least two columns"
  )
})

test_that("summarySilhouette() validates prob_matrix", {
  expect_error(
    summarySilhouette(prob_matrix = "invalid"),
    "prob_matrix must be a numeric matrix."
  )
  
  expect_error(
    summarySilhouette(prob_matrix = matrix(1, nrow = 5, ncol = 1)),
    "prob_matrix must have at least two columns"
  )
})

test_that("summarySilhouette() returns correct structure", {
  # Create simple test data
  set.seed(123)
  prob_matrix <- matrix(runif(60), nrow = 20, ncol = 3)
  prob_matrix <- prob_matrix / rowSums(prob_matrix)
  
  result <- summarySilhouette(
    prob_matrix = prob_matrix,
    print.summary = FALSE
  )
  
  expect_equal(ncol(result), 4)
  expect_true(is.character(result$Method))
  expect_true(is.numeric(result$Crisp_Silhouette))
  expect_true(is.numeric(result$Fuzzy_Silhouette))
  expect_true(is.numeric(result$Median_Silhouette))
  expect_equal(nrow(result), 5) # Should have 5 prob-based methods
})