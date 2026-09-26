# Shared data generator for benchmark.R and probe.R.
# Five-dimensional Gaussian mixture with K = 5 well-separated components; the
# partition is the k-means solution started from the true means, and the
# memberships have the fuzzy c-means form (m = 2) computed from the distances
# to the k-means centres.

K <- 5
p <- 5

make_data <- function(n, seed) {
  set.seed(seed)
  mu <- matrix(rnorm(K * p, sd = 4), K, p)
  id <- sample.int(K, n, replace = TRUE)
  X <- mu[id, ] + matrix(rnorm(n * p), n, p)
  km <- kmeans(X, centers = mu, iter.max = 50)
  d2 <- as.matrix(proxy::dist(X, km$centers))^2
  U <- (1 / d2) / rowSums(1 / d2)
  list(X = X, cl = km$cluster, centers = km$centers, U = U)
}
