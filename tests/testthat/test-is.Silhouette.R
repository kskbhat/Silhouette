library(testthat)
library(Silhouette)

set.seed(123) # Global seed for reproducibility

# Create test data
cluster_assignments <- c(1, 1, 2, 2, 3, 3)
neighbor_clusters <- c(2, 2, 1, 1, 1, 1)
silhouette_widths <- c(0.8, 0.7, 0.6, 0.9, 0.5, 0.4)
weights <- c(0.9, 0.8, 0.7, 0.95, 0.6, 0.5)

## 1. Basic functionality tests
test_that("is.Silhouette() correctly identifies Silhouette objects", {
  # Create a valid Silhouette object
  sil_obj <- getSilhouette(
    cluster = cluster_assignments,
    neighbor = neighbor_clusters,
    sil_width = silhouette_widths,
    proximity_type = "dissimilarity",
    method = "medoid",
    average = "crisp"
  )

  # Test with strict = FALSE (default)
  expect_true(is.Silhouette(sil_obj))
  expect_true(is.Silhouette(sil_obj, strict = FALSE))

  # Test with strict = TRUE
  expect_true(is.Silhouette(sil_obj, strict = TRUE))
})

test_that("is.Silhouette() correctly identifies non-Silhouette objects", {
  # Test with various non-Silhouette objects
  expect_false(is.Silhouette(data.frame(a = 1, b = 2)))
  expect_false(is.Silhouette(matrix(1:10, ncol = 2)))
  expect_false(is.Silhouette(list(a = 1, b = 2)))
  expect_false(is.Silhouette(NULL))
  expect_false(is.Silhouette("not an object"))
  expect_false(is.Silhouette(123))
})

test_that("is.Silhouette() works with fuzzy Silhouette objects", {
  # Create a valid fuzzy Silhouette object
  sil_fuzzy <- getSilhouette(
    cluster = cluster_assignments,
    neighbor = neighbor_clusters,
    sil_width = silhouette_widths,
    weight = weights,
    proximity_type = "similarity",
    method = "pac",
    average = "fuzzy"
  )

  # Test with strict = FALSE (default)
  expect_true(is.Silhouette(sil_fuzzy))
  expect_true(is.Silhouette(sil_fuzzy, strict = FALSE))

  # Test with strict = TRUE
  expect_true(is.Silhouette(sil_fuzzy, strict = TRUE))
})

test_that("is.Silhouette() works with median Silhouette objects", {
  # Create a valid median Silhouette object
  sil_median <- getSilhouette(
    cluster = cluster_assignments,
    neighbor = neighbor_clusters,
    sil_width = silhouette_widths,
    proximity_type = "dissimilarity",
    method = "custom_method",
    average = "median"
  )

  # Test with strict = FALSE (default)
  expect_true(is.Silhouette(sil_median))
  expect_true(is.Silhouette(sil_median, strict = FALSE))

  # Test with strict = TRUE
  expect_true(is.Silhouette(sil_median, strict = TRUE))
})

## 2. Strict validation tests
test_that("is.Silhouette() with strict = TRUE validates data frame structure", {
  # Create a non-data frame object with Silhouette class
  not_df <- structure(list(), class = "Silhouette")
  expect_false(is.Silhouette(not_df, strict = TRUE))
})

test_that("is.Silhouette() with strict = TRUE validates required columns", {
  # Create an object missing required columns
  missing_cols <- structure(
    data.frame(other_col = 1:5),
    class = c("Silhouette", "data.frame"),
    proximity_type = "dissimilarity",
    method = "medoid",
    average = "crisp"
  )
  expect_false(is.Silhouette(missing_cols, strict = TRUE))
})

test_that("is.Silhouette() with strict = TRUE validates required attributes", {
  # Create an object missing required attributes
  missing_attrs <- structure(
    data.frame(cluster = 1:3, neighbor = c(2, 1, 1), sil_width = c(0.5, 0.6, 0.7)),
    class = c("Silhouette", "data.frame")
    # Missing proximity_type, method, average attributes
  )
  expect_false(is.Silhouette(missing_attrs, strict = TRUE))
})

test_that("is.Silhouette() with strict = TRUE validates column types", {
  # Create an object with incorrect column types
  wrong_types <- structure(
    data.frame(
      cluster = as.character(1:3),  # Should be numeric/integer
      neighbor = c(2, 1, 1),
      sil_width = c(0.5, 0.6, 0.7)
    ),
    class = c("Silhouette", "data.frame"),
    proximity_type = "dissimilarity",
    method = "medoid",
    average = "crisp"
  )
  expect_false(is.Silhouette(wrong_types, strict = TRUE))

  # Test with wrong neighbor type
  wrong_neighbor_type <- structure(
    data.frame(
      cluster = c(1, 2, 3),
      neighbor = as.character(c(2, 1, 1)),  # Should be numeric/integer
      sil_width = c(0.5, 0.6, 0.7)
    ),
    class = c("Silhouette", "data.frame"),
    proximity_type = "dissimilarity",
    method = "medoid",
    average = "crisp"
  )
  expect_false(is.Silhouette(wrong_neighbor_type, strict = TRUE))

  # Test with wrong sil_width type
  wrong_width_type <- structure(
    data.frame(
      cluster = c(1, 2, 3),
      neighbor = c(2, 1, 1),
      sil_width = as.character(c(0.5, 0.6, 0.7))  # Should be numeric
    ),
    class = c("Silhouette", "data.frame"),
    proximity_type = "dissimilarity",
    method = "medoid",
    average = "crisp"
  )
  expect_false(is.Silhouette(wrong_width_type, strict = TRUE))
})

test_that("is.Silhouette() with strict = TRUE validates weight column for fuzzy averaging", {
  # Create a fuzzy object missing weight column
  missing_weight <- structure(
    data.frame(cluster = 1:3, neighbor = c(2, 1, 1), sil_width = c(0.5, 0.6, 0.7)),
    class = c("Silhouette", "data.frame"),
    proximity_type = "dissimilarity",
    method = "medoid",
    average = "fuzzy"  # But no weight column
  )
  expect_false(is.Silhouette(missing_weight, strict = TRUE))

  # Create a fuzzy object with wrong weight type
  wrong_weight_type <- structure(
    data.frame(
      cluster = c(1, 2, 3),
      neighbor = c(2, 1, 1),
      sil_width = c(0.5, 0.6, 0.7),
      weight = as.character(c(0.9, 0.8, 0.7))  # Should be numeric
    ),
    class = c("Silhouette", "data.frame"),
    proximity_type = "dissimilarity",
    method = "medoid",
    average = "fuzzy"
  )
  expect_false(is.Silhouette(wrong_weight_type, strict = TRUE))
})

## 3. Attribute validation tests
test_that("is.Silhouette() with strict = TRUE validates proximity_type attribute", {
  # Create an object with invalid proximity_type
  invalid_proximity <- structure(
    data.frame(cluster = 1:3, neighbor = c(2, 1, 1), sil_width = c(0.5, 0.6, 0.7)),
    class = c("Silhouette", "data.frame"),
    proximity_type = "invalid",  # Should be "similarity" or "dissimilarity"
    method = "medoid",
    average = "crisp"
  )
  expect_false(is.Silhouette(invalid_proximity, strict = TRUE))

  # Test with valid proximity_type values
  valid_dissimilarity <- structure(
    data.frame(cluster = 1:3, neighbor = c(2, 1, 1), sil_width = c(0.5, 0.6, 0.7)),
    class = c("Silhouette", "data.frame"),
    proximity_type = "dissimilarity",
    method = "medoid",
    average = "crisp"
  )
  expect_true(is.Silhouette(valid_dissimilarity, strict = TRUE))

  valid_similarity <- structure(
    data.frame(cluster = 1:3, neighbor = c(2, 1, 1), sil_width = c(0.5, 0.6, 0.7)),
    class = c("Silhouette", "data.frame"),
    proximity_type = "similarity",
    method = "medoid",
    average = "crisp"
  )
  expect_true(is.Silhouette(valid_similarity, strict = TRUE))
})

test_that("is.Silhouette() with strict = TRUE validates average attribute", {
  # Create an object with invalid average
  invalid_average <- structure(
    data.frame(cluster = 1:3, neighbor = c(2, 1, 1), sil_width = c(0.5, 0.6, 0.7)),
    class = c("Silhouette", "data.frame"),
    proximity_type = "dissimilarity",
    method = "medoid",
    average = "invalid"  # Should be "crisp", "fuzzy", or "median"
  )
  expect_false(is.Silhouette(invalid_average, strict = TRUE))

  # Test with valid average values
  valid_crisp <- structure(
    data.frame(cluster = 1:3, neighbor = c(2, 1, 1), sil_width = c(0.5, 0.6, 0.7)),
    class = c("Silhouette", "data.frame"),
    proximity_type = "dissimilarity",
    method = "medoid",
    average = "crisp"
  )
  expect_true(is.Silhouette(valid_crisp, strict = TRUE))

  valid_fuzzy <- structure(
    data.frame(
      cluster = c(1, 2, 3),
      neighbor = c(2, 1, 1),
      sil_width = c(0.5, 0.6, 0.7),
      weight = c(0.9, 0.8, 0.7)
    ),
    class = c("Silhouette", "data.frame"),
    proximity_type = "dissimilarity",
    method = "medoid",
    average = "fuzzy"
  )
  expect_true(is.Silhouette(valid_fuzzy, strict = TRUE))

  valid_median <- structure(
    data.frame(cluster = 1:3, neighbor = c(2, 1, 1), sil_width = c(0.5, 0.6, 0.7)),
    class = c("Silhouette", "data.frame"),
    proximity_type = "dissimilarity",
    method = "medoid",
    average = "median"
  )
  expect_true(is.Silhouette(valid_median, strict = TRUE))
})

test_that("is.Silhouette() with strict = TRUE validates method attribute", {
  # Test with NULL method (valid)
  null_method <- structure(
    data.frame(cluster = 1:3, neighbor = c(2, 1, 1), sil_width = c(0.5, 0.6, 0.7)),
    class = c("Silhouette", "data.frame"),
    proximity_type = "dissimilarity",
    method = NA,
    average = "crisp"
  )
  expect_true(is.Silhouette(null_method, strict = TRUE))

  # Test with string method (valid)
  string_method <- structure(
    data.frame(cluster = 1:3, neighbor = c(2, 1, 1), sil_width = c(0.5, 0.6, 0.7)),
    class = c("Silhouette", "data.frame"),
    proximity_type = "dissimilarity",
    method = "custom_method",
    average = "crisp"
  )
  expect_true(is.Silhouette(string_method, strict = TRUE))
})

## 4. Value range validation tests
test_that("is.Silhouette() with strict = TRUE validates sil_width range", {
  # Create an object with sil_width out of range
  invalid_width_high <- structure(
    data.frame(cluster = 1:3, neighbor = c(2, 1, 1), sil_width = c(0.5, 0.6, 1.5)),  # > 1
    class = c("Silhouette", "data.frame"),
    proximity_type = "dissimilarity",
    method = "medoid",
    average = "crisp"
  )
  expect_false(is.Silhouette(invalid_width_high, strict = TRUE))

  invalid_width_low <- structure(
    data.frame(cluster = 1:3, neighbor = c(2, 1, 1), sil_width = c(0.5, 0.6, -1.5)),  # < -1
    class = c("Silhouette", "data.frame"),
    proximity_type = "dissimilarity",
    method = "medoid",
    average = "crisp"
  )
  expect_false(is.Silhouette(invalid_width_low, strict = TRUE))
})

test_that("is.Silhouette() with strict = TRUE validates weight range", {
  # Create a fuzzy object with weight out of range
  invalid_weight_high <- structure(
    data.frame(
      cluster = c(1, 2, 3),
      neighbor = c(2, 1, 1),
      sil_width = c(0.5, 0.6, 0.7),
      weight = c(0.9, 0.8, 1.5)  # > 1
    ),
    class = c("Silhouette", "data.frame"),
    proximity_type = "dissimilarity",
    method = "medoid",
    average = "fuzzy"
  )
  expect_false(is.Silhouette(invalid_weight_high, strict = TRUE))

  invalid_weight_low <- structure(
    data.frame(
      cluster = c(1, 2, 3),
      neighbor = c(2, 1, 1),
      sil_width = c(0.5, 0.6, 0.7),
      weight = c(0.9, 0.8, -0.5)  # < 0
    ),
    class = c("Silhouette", "data.frame"),
    proximity_type = "dissimilarity",
    method = "medoid",
    average = "fuzzy"
  )
  expect_false(is.Silhouette(invalid_weight_low, strict = TRUE))
})

test_that("is.Silhouette() with strict = TRUE validates cluster != neighbor", {
  # Create an object with cluster == neighbor
  equal_cluster_neighbor <- structure(
    data.frame(cluster = c(1, 2, 3), neighbor = c(1, 2, 3)),  # Equal values
    class = c("Silhouette", "data.frame"),
    proximity_type = "dissimilarity",
    method = "medoid",
    average = "crisp"
  )
  # Add sil_width column to make it a complete object
  equal_cluster_neighbor$sil_width <- c(0.5, 0.6, 0.7)
  expect_false(is.Silhouette(equal_cluster_neighbor, strict = TRUE))
})

## 5. Edge case tests
test_that("is.Silhouette() handles edge cases gracefully", {
  # Test with empty data frame
  empty_sil <- structure(
    data.frame(cluster = integer(0), neighbor = integer(0), sil_width = numeric(0)),
    class = c("Silhouette", "data.frame"),
    proximity_type = "dissimilarity",
    method = "medoid",
    average = "crisp"
  )
  expect_true(is.Silhouette(empty_sil, strict = TRUE))

  # Test with NA values
  na_sil <- structure(
    data.frame(
      cluster = c(1, 2, 3),
      neighbor = c(2, 1, 1),
      sil_width = c(0.5, 0.6, NA)
    ),
    class = c("Silhouette", "data.frame"),
    proximity_type = "dissimilarity",
    method = "medoid",
    average = "crisp"
  )
  expect_true(is.Silhouette(na_sil, strict = TRUE))
})
