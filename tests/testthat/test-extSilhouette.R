library(testthat)
library(Silhouette)
library(ppclust)

set.seed(123) # Global seed for reproducibility

## 1. Basic functionality tests
test_that("extSilhouette() works with basic parameters", {
  data(iris)
  fcm_out1 <- ppclust::fcm(iris[, -5], centers = 3)
  fcm_out2 <- ppclust::fcm(iris[, -5], centers = 2)

  sil1 <- softSilhouette(prob_matrix = fcm_out1$u, print.summary = FALSE)
  sil2 <- softSilhouette(prob_matrix = fcm_out2$u, print.summary = FALSE)

  result <- extSilhouette(list(sil1, sil2), print.summary = FALSE)

  expect_s3_class(result, "extSilhouette")
  expect_true(is.list(result))
  expect_true(all(c("ext_sil_width", "dim_table") %in% names(result)))
  expect_true(is.numeric(result$ext_sil_width))
  expect_s3_class(result$dim_table, "data.frame")
  expect_equal(nrow(result$dim_table), 2)
})

test_that("extSilhouette() works with custom dim_names", {
  data(iris)
  fcm_out1 <- ppclust::fcm(iris[, -5], centers = 3)
  fcm_out2 <- ppclust::fcm(iris[, -5], centers = 2)

  sil1 <- softSilhouette(prob_matrix = fcm_out1$u, print.summary = FALSE)
  sil2 <- softSilhouette(prob_matrix = fcm_out2$u, print.summary = FALSE)

  result <- extSilhouette(
    list(sil1, sil2),
    dim_names = c("Clusters1", "Clusters2"),
    print.summary = FALSE
  )

  expect_s3_class(result, "extSilhouette")
  expect_equal(result$dim_table$dimension[1], "Clusters1")
  expect_equal(result$dim_table$dimension[2], "Clusters2")
})

## 2. Error handling tests
test_that("extSilhouette() errors with non-Silhouette objects", {
  expect_error(extSilhouette(list(1:3, 4:6)), "class 'Silhouette'")
})

test_that("extSilhouette() errors with mismatched dim_names length", {
  data(iris)
  fcm_out1 <- ppclust::fcm(iris[, -5], centers = 3)
  fcm_out2 <- ppclust::fcm(iris[, -5], centers = 2)

  sil1 <- softSilhouette(prob_matrix = fcm_out1$u, print.summary = FALSE)
  sil2 <- softSilhouette(prob_matrix = fcm_out2$u, print.summary = FALSE)

  expect_error(
    extSilhouette(list(sil1, sil2), dim_names = c("OnlyOneName")),
    "Length of dim_names must match the length of sil_list"
  )
})

test_that("extSilhouette() errors with non-finite average silhouette widths", {
  # Create a Silhouette object and then modify it to have NaN average width
  na_sil <- getSilhouette(
    cluster = c(1, 1),
    neighbor = c(2, 2),
    sil_width = c(0.5, 0.5),
    proximity_type = "dissimilarity",
    method = "medoid",
    average = "crisp"
  )
  na_sil$sil_width <- c(NaN, NaN)
  expect_error(
    extSilhouette(list(na_sil)),
    "non-finite average silhouette widths"
  )
})

test_that("extSilhouette() errors with zero observations", {
  # Define a subclass and a custom summary method that returns a finite average width
  # for an empty data frame to bypass the summary.Silhouette NaN calculation
  summary.EmptySilMock <- function(object, ...) {
    list(avg.width = 0.5)
  }
  registerS3method("summary", "EmptySilMock", summary.EmptySilMock)
  
  empty_sil <- structure(
    data.frame(cluster = integer(0), neighbor = integer(0), sil_width = numeric(0)),
    class = c("EmptySilMock", "Silhouette", "data.frame"),
    proximity_type = "dissimilarity",
    method = "medoid",
    average = "crisp"
  )
  
  expect_error(
    extSilhouette(list(empty_sil)),
    "No observations found in any Silhouette object"
  )
})

test_that("extSilhouette() works with print.summary = TRUE", {
  data(iris)
  fcm_out1 <- ppclust::fcm(iris[, -5], centers = 3)
  fcm_out2 <- ppclust::fcm(iris[, -5], centers = 2)
  sil1 <- softSilhouette(prob_matrix = fcm_out1$u, print.summary = FALSE)
  sil2 <- softSilhouette(prob_matrix = fcm_out2$u, print.summary = FALSE)
  
  # Capture output and check if it printed
  output <- capture.output(extSilhouette(list(sil1, sil2), print.summary = TRUE))
  expect_true(length(output) > 0)
  expect_true(any(grepl("Extended silhouette", output)))
})

## 3. Edge case tests
# test_that("extSilhouette() works with single Silhouette object", {
#   data(iris)
#   fcm_out <- ppclust::fcm(iris[, -5], centers = 3)
#   sil <- softSilhouette(prob_matrix = fcm_out$u, print.summary = FALSE)
#
#   result <- extSilhouette(list(sil), print.summary = FALSE)
#
#   expect_s3_class(result, "extSilhouette")
#   expect_equal(nrow(result$dim_table), 1)
#   expect_equal(result$ext_sil_width, summary(sil, print.summary = FALSE)$avg.width)
# })

test_that("extSilhouette() works with three Silhouette objects", {
  data(iris)
  fcm_out1 <- ppclust::fcm(iris[, -5], centers = 3)
  fcm_out2 <- ppclust::fcm(iris[, -5], centers = 2)
  fcm_out3 <- ppclust::fcm(iris[, -5], centers = 4)

  sil1 <- softSilhouette(prob_matrix = fcm_out1$u, print.summary = FALSE)
  sil2 <- softSilhouette(prob_matrix = fcm_out2$u, print.summary = FALSE)
  sil3 <- softSilhouette(prob_matrix = fcm_out3$u, print.summary = FALSE)

  result <- extSilhouette(list(sil1, sil2, sil3), print.summary = FALSE)

  expect_s3_class(result, "extSilhouette")
  expect_equal(nrow(result$dim_table), 3)
})

## 4. Attribute validation tests
test_that("extSilhouette() sets correct structure", {
  data(iris)
  fcm_out1 <- ppclust::fcm(iris[, -5], centers = 3)
  fcm_out2 <- ppclust::fcm(iris[, -5], centers = 2)

  sil1 <- softSilhouette(prob_matrix = fcm_out1$u, print.summary = FALSE)
  sil2 <- softSilhouette(prob_matrix = fcm_out2$u, print.summary = FALSE)

  result <- extSilhouette(list(sil1, sil2), print.summary = FALSE)

  expect_true(is.list(result))
  expect_true(is.numeric(result$ext_sil_width))
  expect_true(is.data.frame(result$dim_table))
  expect_true(all(c("dimension", "n_obs", "avg_sil_width") %in% colnames(result$dim_table)))
})
