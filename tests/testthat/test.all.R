# tests/testthat/test-all.R
library(testthat)
library(ggplot2)

context("Full coverage tests for silhouette package")

make_prob <- function(n=5, k=3) {
  m <- matrix(runif(n*k), nrow=n)
  m <- m / rowSums(m)
  m
}

make_prox <- function(n=5, k=3) {
  matrix(runif(n*k), nrow=n)
}

test_that("Silhouette works for all branches", {
  prob <- make_prob()
  prox <- make_prox()

  # Crisp similarity pac
  s <- Silhouette(prox_matrix = prox,
                  proximity_type = "similarity",
                  method = "pac",
                  average = "crisp",
                  prob_matrix = NULL,
                  sort = TRUE)
  expect_s3_class(s, "Silhouette")
  expect_true("sil_width" %in% names(s))

  # Crisp similarity medoid
  Silhouette(prox_matrix = prox,
             proximity_type = "similarity",
             method = "medoid")

  # Crisp dissimilarity pac
  Silhouette(prox_matrix = prox,
             proximity_type = "dissimilarity",
             method = "pac")

  # Crisp dissimilarity medoid
  Silhouette(prox_matrix = prox,
             proximity_type = "dissimilarity",
             method = "medoid")

  # Fuzzy average with prob_matrix
  s <- Silhouette(prox_matrix = prox,
                  proximity_type = "similarity",
                  prob_matrix = prob,
                  average = "fuzzy",
                  a = 2)
  expect_true("weight" %in% names(s))

  # Median
  Silhouette(prox_matrix = prox,
             proximity_type = "similarity",
             average = "median")

  # average fuzzy without prob_matrix triggers warning
  expect_warning(Silhouette(prox_matrix = prox,
                            proximity_type = "similarity",
                            average = "fuzzy"))

  # Error cases
  expect_error(Silhouette(prox_matrix = 1:5))
  expect_error(Silhouette(prox_matrix = matrix(1, nrow=2, ncol=1)))
  expect_error(Silhouette(prox_matrix = prox, a = 0))
})

test_that("softSilhouette covers prob_type branches", {
  p <- make_prob()

  # pp, nlpp, pd all work
  softSilhouette(prob_matrix = p,
                 prob_type = "pp", method="pac")
  softSilhouette(prob_matrix = p,
                 prob_type = "nlpp", method="medoid")
  softSilhouette(prob_matrix = p,
                 prob_type = "pd")

  # fuzzy works
  softSilhouette(prob_matrix = p,
                 prob_type = "pp", average = "fuzzy")

  # pd zero column sums fails
  p2 <- p; p2[,1] <- 0
  expect_error(softSilhouette(prob_matrix = p2, prob_type = "pd"))

  # nlpp with zero -> Inf error
  p3 <- p; p3[1,1] <- 0
  expect_error(softSilhouette(prob_matrix = p3, prob_type = "nlpp"))
})

test_that("cerSilhouette runs all averages", {
  p <- make_prob()
  cerSilhouette(prob_matrix = p, average = "crisp")
  cerSilhouette(prob_matrix = p, average = "fuzzy")
  cerSilhouette(prob_matrix = p, average = "median")

  expect_error(cerSilhouette(prob_matrix = 1:5))
  expect_error(cerSilhouette(prob_matrix = matrix(1, nrow=2, ncol=1)))
  expect_error(cerSilhouette(prob_matrix = p, a = -1))
  p_bad <- p; p_bad[1,1] <- 2; p_bad[1,2] <- -1
  expect_error(cerSilhouette(prob_matrix = p_bad))
})

test_that("dbSilhouette works and errors", {
  p <- make_prob()
  dbSilhouette(prob_matrix = p, average = "median")
  dbSilhouette(prob_matrix = p, average = "crisp")
  dbSilhouette(prob_matrix = p, average = "fuzzy")

  expect_error(dbSilhouette(prob_matrix = 1:5))
  expect_error(dbSilhouette(prob_matrix = matrix(1, nrow=2, ncol=1)))
  expect_error(dbSilhouette(prob_matrix = p, a = -1))
})

test_that("summary.Silhouette works for all averages", {
  p <- make_prob()
  prox <- make_prox()
  s1 <- Silhouette(prox, proximity_type="similarity", average="crisp")
  s2 <- Silhouette(prox, proximity_type="similarity", prob_matrix=p, average="fuzzy")
  s3 <- Silhouette(prox, proximity_type="similarity", average="median")

  summary(s1, print.summary = TRUE)
  summary(s2, print.summary = TRUE)
  summary(s3, print.summary = TRUE)
  expect_error(summary("not a silhouette"))
})

test_that("extSilhouette works and errors", {
  prox <- make_prox(); prob <- make_prob()
  s1 <- Silhouette(prox, proximity_type="similarity")
  s2 <- Silhouette(prox, proximity_type="similarity")
  e <- extSilhouette(list(s1, s2), print.summary = TRUE)
  expect_s3_class(e, "extSilhouette")

  # Custom dim names
  extSilhouette(list(s1, s2), dim_names = c("Mode1","Mode2"))

  # Error cases
  expect_error(extSilhouette(list(mtcars))) # not Silhouette
  expect_error(extSilhouette(list(s1), dim_names = c("one","two")))
})

test_that("plotSilhouette works for all inputs", {
  prox <- make_prox(); prob <- make_prob()
  s <- Silhouette(prox, proximity_type="similarity")

  # Standard Silhouette
  p <- plotSilhouette(s)
  expect_s3_class(p, "ggplot")

  # With label TRUE, grayscale
  plotSilhouette(s, label = TRUE, grayscale=TRUE)

  # summary.legend = FALSE
  plotSilhouette(s, summary.legend = FALSE)

  # numeric linetype
  plotSilhouette(s, linetype = 2)

  # Unsupported numeric linetype
  expect_error(plotSilhouette(s, linetype = 9))

  # silhouette class
  sil_df <- data.frame(cluster=1:3, neighbor=2:4, sil_width=runif(3))
  class(sil_df) <- "silhouette"
  plotSilhouette(sil_df)

  # fake pam/eclust class
  fake <- list(silinfo=list(widths=data.frame(cluster=1:3, neighbor=2:4, sil_width=runif(3)),
                            clus.avg.widths=c("1"=0.5,"2"=0.6),
                            avg.width=0.55))
  class(fake) <- "pam"
  plotSilhouette(fake)

  # invalid object
  expect_error(plotSilhouette(list()))
})

