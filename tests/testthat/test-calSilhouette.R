library(testthat)
library(Silhouette)

test_that("calSilhouette() errors on empty/invalid inputs", {
  # At least one of prox_matrix or prob_matrix must be provided
  expect_error(calSilhouette(prox_matrix = NULL, prob_matrix = NULL),
               "At least one of 'prox_matrix' or 'prob_matrix' must be provided.")
  
  # Invalid proximity_type
  expect_error(calSilhouette(prox_matrix = matrix(1:4, 2, 2), proximity_type = "invalid"),
               "should be one of")
  
  # Invalid prox_matrix type
  expect_error(calSilhouette(prox_matrix = "not_a_matrix"),
               "prox_matrix must be a numeric matrix.")
  
  # prox_matrix with < 2 columns
  expect_error(calSilhouette(prox_matrix = matrix(c(1, 2), ncol = 1)),
               "prox_matrix must have at least two columns")

  # Invalid prob_matrix type
  expect_error(calSilhouette(prob_matrix = "not_a_matrix"),
               "prob_matrix must be a numeric matrix.")
  
  # prob_matrix with < 2 columns
  expect_error(calSilhouette(prob_matrix = matrix(c(1, 2), ncol = 1)),
               "prob_matrix must have at least two columns")
})

test_that("calSilhouette() works with basic matrices directly", {
  set.seed(123)
  prox <- matrix(runif(30, 0, 1), ncol = 3)
  prob <- matrix(runif(30, 0, 1), ncol = 3)
  prob <- prob / rowSums(prob)
  
  # 1. Proximity matrix only
  res_prox <- calSilhouette(prox_matrix = prox, proximity_type = "dissimilarity")
  expect_s3_class(res_prox, "data.frame")
  expect_true(all(c("Method", "Crisp", "Median") %in% colnames(res_prox)))
  expect_false("Fuzzy" %in% colnames(res_prox))
  
  # 2. Probability matrix only
  res_prob <- calSilhouette(prob_matrix = prob)
  expect_s3_class(res_prob, "data.frame")
  expect_true(all(c("Method", "Crisp", "Fuzzy", "Median") %in% colnames(res_prob)))
  
  # 3. Both matrices
  res_both <- calSilhouette(prox_matrix = prox, prob_matrix = prob, print.summary = TRUE)
  expect_s3_class(res_both, "data.frame")
  expect_true(all(c("Method", "Crisp", "Fuzzy", "Median") %in% colnames(res_both)))
})

test_that("calSilhouette() works with clust_fun argument", {
  # Mock clustering function returning S3 list
  mock_clust_s3 <- function(data, k) {
    list(
      d = matrix(runif(nrow(data) * k, 0, 1), ncol = k),
      u = matrix(runif(nrow(data) * k, 0, 1), ncol = k)
    )
  }
  
  df_dummy <- data.frame(x = rnorm(10), y = rnorm(10))
  
  # Valid function call
  res <- calSilhouette(
    prox_matrix = "d",
    prob_matrix = "u",
    clust_fun = mock_clust_s3,
    data = df_dummy,
    k = 2
  )
  expect_s3_class(res, "data.frame")
  
  # Character name of function in parent environment
  res_char <- calSilhouette(
    prox_matrix = "d",
    prob_matrix = "u",
    clust_fun = "mock_clust_s3",
    data = df_dummy,
    k = 2
  )
  expect_s3_class(res_char, "data.frame")
  
  # Invalid clust_fun type
  expect_error(calSilhouette(prox_matrix = "d", clust_fun = 123),
               "clust_fun must be a function object or a string naming a function.")
  
  # Function not found
  expect_error(calSilhouette(prox_matrix = "d", clust_fun = "non_existent_function_123"),
               "not found")
  
  # Error in clustering function execution
  expect_error(calSilhouette(prox_matrix = "d", clust_fun = mock_clust_s3, data = df_dummy),
               "Error in clustering function")
  
  # Invalid matrix component names (non-character / non-length 1)
  expect_error(calSilhouette(prox_matrix = c("d", "extra"), clust_fun = mock_clust_s3, data = df_dummy, k = 2),
               "must be a single string naming a matrix component")
  expect_error(calSilhouette(prob_matrix = list("u"), clust_fun = mock_clust_s3, data = df_dummy, k = 2),
               "must be a single string naming a matrix component")
  
  # Component not found in S3
  expect_error(calSilhouette(prox_matrix = "missing_component", clust_fun = mock_clust_s3, data = df_dummy, k = 2),
               "Component 'missing_component' not found")
  expect_error(calSilhouette(prob_matrix = "missing_component", clust_fun = mock_clust_s3, data = df_dummy, k = 2),
               "Component 'missing_component' not found")
})

test_that("calSilhouette() works with S4 clustering outputs", {
  # Define custom S4 class for mock clustering
  setClass("MockClustS4",
    slots = list(
      d = "matrix",
      u = "matrix"
    )
  )
  
  mock_clust_s4 <- function(data, k) {
    new("MockClustS4",
      d = matrix(runif(nrow(data) * k, 0, 1), ncol = k),
      u = matrix(runif(nrow(data) * k, 0, 1), ncol = k)
    )
  }
  
  df_dummy <- data.frame(x = rnorm(10), y = rnorm(10))
  
  res <- calSilhouette(
    prox_matrix = "d",
    prob_matrix = "u",
    clust_fun = mock_clust_s4,
    data = df_dummy,
    k = 2
  )
  expect_s3_class(res, "data.frame")
  
  # Component/Slot not found in S4
  expect_error(calSilhouette(prox_matrix = "missing_slot", clust_fun = mock_clust_s4, data = df_dummy, k = 2),
               "Slot 'missing_slot' not found")
  expect_error(calSilhouette(prob_matrix = "missing_slot", clust_fun = mock_clust_s4, data = df_dummy, k = 2),
               "Slot 'missing_slot' not found")
})

test_that("calSilhouette() error handling, fallbacks, and safe_compute paths", {
  # Save original functions
  orig_db <- Silhouette::dbSilhouette
  orig_cer <- Silhouette::cerSilhouette
  
  # 1. Mock dbSilhouette to return non-Silhouette object (hits line 147)
  assignInNamespace("dbSilhouette", function(...) "not_a_silhouette", ns = "Silhouette")
  
  data(iris)
  fcm_out <- ppclust::fcm(iris[, -5], centers = 3)
  res <- calSilhouette(prob_matrix = fcm_out$u, print.summary = FALSE)
  expect_true(!"db" %in% colnames(res))
  
  # 2. Mock dbSilhouette to throw an error with print.summary = TRUE (hits line 151)
  assignInNamespace("dbSilhouette", function(...) stop("Mock DB error"), ns = "Silhouette")
  expect_warning(
    calSilhouette(prob_matrix = fcm_out$u, print.summary = TRUE),
    "Error computing silhouette: Mock DB error"
  )
  
  # Restore original functions
  assignInNamespace("dbSilhouette", orig_db, ns = "Silhouette")
  assignInNamespace("cerSilhouette", orig_cer, ns = "Silhouette")
})

test_that("calSilhouette() S4 generic fallback", {
  # Using "show" S4 generic
  expect_error(
    calSilhouette("missing_slot", clust_fun = "show", object = matrix(1:4, 2, 2)),
    "not found"
  )
})
