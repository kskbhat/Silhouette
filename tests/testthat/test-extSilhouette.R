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


# test_that("extSilhouette() errors with zero observations", {
#   # Create empty Silhouette objects
#   empty_sil <- structure(data.frame(), class = c("Silhouette", "data.frame"))
#   expect_error(extSilhouette(list(empty_sil, empty_sil)), "No observations found")
# })

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
