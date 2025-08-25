library(testthat)
library(Silhouette)

set.seed(123) # Global seed for reproducibility

# Create test data
cluster_assignments <- c(1, 1, 2, 2, 3, 3)
neighbor_clusters <- c(2, 2, 1, 1, 1, 1)
silhouette_widths <- c(0.8, 0.7, 0.6, 0.9, 0.5, 0.4)
weights <- c(0.9, 0.8, 0.7, 0.95, 0.6, 0.5)

## 1. Basic functionality tests
test_that("getSilhouette() works with basic parameters for crisp silhouette", {
  result <- getSilhouette(
    cluster = cluster_assignments,
    neighbor = neighbor_clusters,
    sil_width = silhouette_widths,
    proximity_type = "dissimilarity",
    method = "medoid",
    average = "crisp"
  )

  expect_s3_class(result, "Silhouette")
  expect_true(all(c("cluster", "neighbor", "sil_width") %in% colnames(result)))
  expect_true(all(result$sil_width >= -1 & result$sil_width <= 1))
  expect_false("weight" %in% colnames(result))
})

test_that("getSilhouette() works with fuzzy silhouette", {
  result <- getSilhouette(
    cluster = cluster_assignments,
    neighbor = neighbor_clusters,
    sil_width = silhouette_widths,
    weight = weights,
    proximity_type = "similarity",
    method = "pac",
    average = "fuzzy"
  )

  expect_s3_class(result, "Silhouette")
  expect_true(all(c("cluster", "neighbor", "sil_width", "weight") %in% colnames(result)))
  expect_true(all(result$sil_width >= -1 & result$sil_width <= 1))
  expect_true(all(result$weight >= 0 & result$weight <= 1))
})

test_that("getSilhouette() works with median averaging", {
  result <- getSilhouette(
    cluster = cluster_assignments,
    neighbor = neighbor_clusters,
    sil_width = silhouette_widths,
    proximity_type = "dissimilarity",
    method = "custom_method",
    average = "median"
  )

  expect_s3_class(result, "Silhouette")
  expect_equal(attr(result, "average"), "median")
})

test_that("getSilhouette() works with NULL method", {
  result <- getSilhouette(
    cluster = cluster_assignments,
    neighbor = neighbor_clusters,
    sil_width = silhouette_widths,
    proximity_type = "dissimilarity",
    method = NULL,
    average = "crisp"
  )

  expect_s3_class(result, "Silhouette")
  expect_null(attr(result, "method"))
})

test_that("getSilhouette() works with custom method string", {
  result <- getSilhouette(
    cluster = cluster_assignments,
    neighbor = neighbor_clusters,
    sil_width = silhouette_widths,
    proximity_type = "dissimilarity",
    method = "my_custom_method",
    average = "crisp"
  )

  expect_s3_class(result, "Silhouette")
  expect_equal(attr(result, "method"), "my_custom_method")
})

## 2. Parameter validation tests
test_that("getSilhouette() validates proximity_type parameter", {
  expect_error(
    getSilhouette(
      cluster = cluster_assignments,
      neighbor = neighbor_clusters,
      sil_width = silhouette_widths,
      proximity_type = "invalid",
      method = "medoid",
      average = "crisp"
    ),
    "should be one of"
  )
})

test_that("getSilhouette() validates average parameter", {
  expect_error(
    getSilhouette(
      cluster = cluster_assignments,
      neighbor = neighbor_clusters,
      sil_width = silhouette_widths,
      proximity_type = "dissimilarity",
      method = "medoid",
      average = "invalid"
    ),
    "should be one of"
  )
})

test_that("getSilhouette() validates input lengths", {
  expect_error(
    getSilhouette(
      cluster = cluster_assignments,
      neighbor = neighbor_clusters[-1],  # Wrong length
      sil_width = silhouette_widths,
      proximity_type = "dissimilarity",
      method = "medoid",
      average = "crisp"
    ),
    "same length"
  )

  expect_error(
    getSilhouette(
      cluster = cluster_assignments,
      neighbor = neighbor_clusters,
      sil_width = silhouette_widths[-1],  # Wrong length
      proximity_type = "dissimilarity",
      method = "medoid",
      average = "crisp"
    ),
    "same length"
  )
})

test_that("getSilhouette() validates input types", {
  expect_error(
    getSilhouette(
      cluster = as.character(cluster_assignments),  # Not numeric
      neighbor = neighbor_clusters,
      sil_width = silhouette_widths,
      proximity_type = "dissimilarity",
      method = "medoid",
      average = "crisp"
    ),
    "must be numeric or integer"
  )

  expect_error(
    getSilhouette(
      cluster = cluster_assignments,
      neighbor = as.character(neighbor_clusters),  # Not numeric
      sil_width = silhouette_widths,
      proximity_type = "dissimilarity",
      method = "medoid",
      average = "crisp"
    ),
    "must be numeric or integer"
  )

  expect_error(
    getSilhouette(
      cluster = cluster_assignments,
      neighbor = neighbor_clusters,
      sil_width = as.character(silhouette_widths),  # Not numeric
      proximity_type = "dissimilarity",
      method = "medoid",
      average = "crisp"
    ),
    "must be numeric"
  )
})

# test_that("getSilhouette() validates sil_width range", {
#   expect_error(
#     getSilhouette(
#       cluster = cluster_assignments,
#       neighbor = neighbor_clusters,
#       sil_width = c(0.8, 0.7, 0.6, 0.9, 0.5, 1.5),  # Value > 1
#       proximity_type = "dissimilarity",
#       method = "medoid",
#       average = "crisp"
#     ),
#     "'sil_width' values must be between -1 and +1"
#   )
#
#   expect_error(
#     getSilhouette(
#       cluster = cluster_assignments,
#       neighbor = neighbor_clusters,
#       sil_width = c(0.8, 0.7, 0.6, 0.9, 0.5, -1.5),  # Value < -1
#       proximity_type = "dissimilarity",
#       method = "medoid",
#       average = "crisp"
#     ),
#     "'sil_width' values must be between -1 and +1"
#   )
# })

test_that("getSilhouette() validates weight range when provided", {
  expect_error(
    getSilhouette(
      cluster = cluster_assignments,
      neighbor = neighbor_clusters,
      sil_width = silhouette_widths,
      weight = c(0.9, 0.8, 0.7, 0.95, 0.6, 1.5),  # Value > 1
      proximity_type = "dissimilarity",
      method = "medoid",
      average = "fuzzy"
    ),
    "between 0 and 1"
  )

  expect_error(
    getSilhouette(
      cluster = cluster_assignments,
      neighbor = neighbor_clusters,
      sil_width = silhouette_widths,
      weight = c(0.9, 0.8, 0.7, 0.95, 0.6, -0.5),  # Value < 0
      proximity_type = "dissimilarity",
      method = "medoid",
      average = "fuzzy"
    ),
    "between 0 and 1"
  )
})

test_that("getSilhouette() validates cluster != neighbor", {
  expect_error(
    getSilhouette(
      cluster = c(1, 1, 2, 2, 3, 3),
      neighbor = c(1, 2, 1, 1, 1, 1),  # First element same as cluster
      sil_width = silhouette_widths,
      proximity_type = "dissimilarity",
      method = "medoid",
      average = "crisp"
    ),
    "should not be equal"
  )
})

## 3. Edge case tests
test_that("getSilhouette() converts fuzzy to crisp when no weight provided", {
  expect_warning(
    result <- getSilhouette(
      cluster = cluster_assignments,
      neighbor = neighbor_clusters,
      sil_width = silhouette_widths,
      proximity_type = "dissimilarity",
      method = "medoid",
      average = "fuzzy"  # But no weight provided
    ),
    "Converting to 'crisp' averaging"
  )

  expect_s3_class(result, "Silhouette")
  expect_equal(attr(result, "average"), "crisp")
  expect_false("weight" %in% colnames(result))
})

test_that("getSilhouette() converts to fuzzy when weight provided but average != 'fuzzy'", {
  expect_warning(
    result <- getSilhouette(
      cluster = cluster_assignments,
      neighbor = neighbor_clusters,
      sil_width = silhouette_widths,
      weight = weights,  # Weight provided
      proximity_type = "dissimilarity",
      method = "medoid",
      average = "crisp"  # But weight provided
    ),
    "Converting to 'fuzzy' averaging"
  )

  expect_s3_class(result, "Silhouette")
  expect_equal(attr(result, "average"), "fuzzy")
  expect_true("weight" %in% colnames(result))
})

test_that("getSilhouette() handles integer and numeric inputs correctly", {
  result <- getSilhouette(
    cluster = as.integer(cluster_assignments),
    neighbor = as.numeric(neighbor_clusters),
    sil_width = silhouette_widths,
    proximity_type = "dissimilarity",
    method = "medoid",
    average = "crisp"
  )

  expect_s3_class(result, "Silhouette")
  expect_type(result$cluster, "integer")
  expect_type(result$neighbor, "integer")
  expect_type(result$sil_width, "double")
})
