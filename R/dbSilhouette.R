#' Density-Based Silhouette Width (DBS) for Soft Clustering
#'
#' Computes silhouette widths based on Menardi (2011) density-based method using log-ratios of posterior probabilities.
#'
#' @param prob_matrix A numeric matrix of posterior probabilities where rows represent observations and columns represent clusters. Must sum to 1 by row. If \code{clust_fun} is provided, \code{prob_matrix} must be a string giving the name of the matrix component (e.g., "u").
#' @param a Numeric value controlling the fuzzifier or weight scaling in fuzzy silhouette averaging. Higher values increase the emphasis on strong membership differences. Must be positive. Defaults to \code{2}.
#' @param average Character string specifying the method for computing the average silhouette width.
#'   Options are:
#'   \itemize{
#'     \item \code{"crisp"} – unweighted (simple) average.
#'     \item \code{"fuzzy"} – weighted average based on membership probability differences.
#'     \item \code{"median"} – median silhouette width across observations.
#'   }
#'   Defaults to \code{"median"}.
#' @param sort Logical; if \code{TRUE}, sorts the output \code{widths} data frame by cluster and descending silhouette width. Defaults to \code{FALSE}.
#' @param print.summary Logical; if \code{TRUE}, prints a summary table of average silhouette widths and sizes for each cluster. Defaults to \code{FALSE}.
#' @param clust_fun Optional S3 or S4 function object or function as character string specifying a clustering function that produces the proximity matrix. For example, \code{\link[ppclust]{fcm}} or \code{"fcm"}. If provided, \code{prob_matrix} must be a string giving the name of the matrix component (e.g., "u"). Defaults to \code{NULL}.
#' @param ... Additional arguments passed to \code{clust_fun}, such as \code{x, centers} for \code{\link[ppclust]{fcm}}.
#'
#' @details
#' Let the posterior probability matrix or cluster membership matrix as
#' \deqn{\Gamma = [\gamma_{ik}]_{n \times K},}
#'
#' The **density-based silhouette width** for observation \eqn{i} is:
#' \deqn{
#' \mathrm{DBS}_i =
#' \frac{\log\left( \frac{\gamma_{ik}}{\max_{k' \neq k} \gamma_{ik'}} \right)}
#' {\max_{j=1,\dots,n} \left| \log\left( \frac{\gamma_{jk}}{\max_{k' \neq k} \gamma_{jk'}} \right) \right|}
#' }
#'
#' #' If `average = "crisp"`, the **crisp silhouette index** is calculated as (\eqn{CS}) is:
#' \deqn{
#'   CS = \frac{1}{n} \sum_{i=1}^{n} S(x_i)
#' }
#' summarizing overall clustering quality.
#'
#' If `average = "fuzzy"` and `prob_matrix` is provided, denoted as \eqn{\Gamma = [\gamma_{ik}]_{n \times K}},
#' with \eqn{\gamma_{ik}} representing the probability of observation \eqn{i} belonging to cluster \eqn{k},
#' the **fuzzy silhouette index** (\eqn{FS}) is calculated as:
#' \deqn{
#'   FS = \frac{\sum_{i=1}^{n}  w_i  S(x_i) }{\sum_{i=1}^{n}  w_i}
#' }
#' where \eqn{w_i = \sum_{i=1}^{n} \left( \gamma_{ik} - \max_{k' \neq k} \gamma_{ik'} \right)^{\alpha}} is `weight` and \eqn{\alpha} (the `a` argument) controls the emphasis on confident assignments.
#'
#' If `average = "median"` then median Silhoutte is Calculated
#'
#' @return A data frame of class \code{"Silhouette"} containing cluster assignments, nearest neighbor clusters, silhouette widths for each observation, and weights (for fuzzy clustering). The object includes the following attributes:
#' \describe{
#'   \item{proximity_type}{The proximity type used i.e., \code{"similarity"}.}
#'   \item{method}{The silhouette calculation method used i.e., \code{"db"}.}
#'   \item{average}{Character — the averaging method: \code{"crisp"}, \code{"fuzzy"}, or \code{"median"}.}
#' }
#' 
#' @section S3 Methods:
#' Objects of class \code{"Silhouette"} have the following S3 methods available:
#' \describe{
#'   \item{\code{\link[=plot.Silhouette]{plot()}}}{Creates a silhouette plot showing silhouette widths for each observation, grouped by cluster. The plot includes a horizontal reference line at the overall average silhouette width.}
#'   \item{\code{\link[=summary.Silhouette]{summary()}}}{Returns a list containing cluster-wise average silhouette widths (\code{clus.avg.widths}), overall average silhouette width (\code{avg.width}), and a summary data frame (\code{sil.sum}) with cluster sizes and average widths.}
#' }
#'
#' @references
#'
#' Menardi, G. (2011). Density-based silhouette diagnostics for clustering methods. \emph{Statistics and Computing}, 21(3), 295--308. \doi{10.1007/s11222-010-9169-0}
#'
#' @seealso \code{\link{Silhouette}}, \code{\link{softSilhouette}}, \code{\link{cerSilhouette}}, \code{\link{getSilhouette}}, \code{\link{is.Silhouette}}, \code{\link{plotSilhouette}}
#' @export
#' @importFrom dplyr mutate arrange %>%
#' @importFrom stats median
#'
#' @examples
#' \donttest{
#' # Compare two soft clustering algorithms using dbSilhouette
#' # Example: FCM vs. FCM2 on iris data, using average silhouette width as a criterion
#' data(iris)
#' if (requireNamespace("ppclust", quietly = TRUE)) {
#'   fcm_result <- ppclust::fcm(iris[, 1:4], 3)
#'   out_fcm <- dbSilhouette(prob_matrix = fcm_result$u, print.summary = TRUE)
#'   plot(out_fcm)
#'   sfcm <- summary(out_fcm, print.summary = FALSE)
#' } else {
#'   message("Install 'ppclust' to run this example: install.packages('ppclust')")
#' }
#' if (requireNamespace("ppclust", quietly = TRUE)) {
#'   fcm2_result <- ppclust::fcm2(iris[, 1:4], 3)
#'   out_fcm2 <- dbSilhouette(prob_matrix = fcm2_result$u, print.summary = TRUE)
#'   plot(out_fcm2)
#'   sfcm2 <- summary(out_fcm2, print.summary = FALSE)
#' } else {
#'   message("Install 'ppclust' to run this example: install.packages('ppclust')")
#' }
#' # Compare average silhouette widths of fcm and fcm2
#' if (requireNamespace("ppclust", quietly = TRUE)) {
#'   cat("FCM average silhouette width:", sfcm$avg.width, "\n",
#'   "FCM2 average silhouette width:", sfcm2$avg.width, "\n")
#' }
#' }
dbSilhouette <- function(prob_matrix,
                         average = c("median", "crisp", "fuzzy"),
                         a = 2,
                         sort = FALSE,
                         print.summary = FALSE,
                         clust_fun = NULL, ...) {

  # Validate prob_matrix and clust_fun
  if (is.null(clust_fun)) {
    if (!is.matrix(prob_matrix) || !is.numeric(prob_matrix)) {
      stop("When clust_fun is NULL, prob_matrix must be a numeric matrix.")
    }
    if (ncol(prob_matrix) < 2) {
      stop("prob_matrix must have at least two columns (clusters).")
    }
  } else {
    if (!is.character(prob_matrix) || length(prob_matrix) != 1) {
      stop("When clust_fun is not NULL, prob_matrix must be a single string naming a matrix component.")
    }
    if (!(is.function(clust_fun) || is.character(clust_fun))) {
      stop("clust_fun must be a function object or a string naming a function (e.g., 'kmeans').")
    }

    if (is.character(clust_fun)) {
      if (exists(clust_fun, mode = "function", envir = parent.frame())) {
        clust_fun <- get(clust_fun, mode = "function", envir = parent.frame())
      } else if (isGeneric(clust_fun)) {
        clust_fun <- getMethod(clust_fun, "ANY")
      } else {
        stop("Function '", clust_fun, "' not found")
      }
    }
    clust_out <- tryCatch(clust_fun(...),
                          error = function(e) stop("Error in clustering function: ", e$message))
    prob_matrix <- if (isS4(clust_out)) {
      if (prob_matrix %in% slotNames(clust_out)) slot(clust_out, prob_matrix) else stop("Slot '", prob_matrix, "' not found in clustering output.")
    } else {
      if (prob_matrix %in% names(clust_out)) clust_out[[prob_matrix]] else stop("Component '", prob_matrix, "' not found in clustering output.")
    }
    if (!is.matrix(prob_matrix) || !is.numeric(prob_matrix)) {
      stop("Extracted prob_matrix must be a numeric matrix.")
    }
    if (ncol(prob_matrix) < 2) {
      stop("Extracted prob_matrix must have at least two columns (clusters).")
    }
  }

  if (any(abs(rowSums(prob_matrix) - 1) > .Machine$double.eps^0.5)) {
    stop("Each row of prob_matrix must sum to 1 after normalization for prob_type = 'pd'.")
  }

  if (!is.numeric(a) || a <= 0) {
    stop("a must be a positive numeric value.")
  }

  average <- match.arg(average)

  maxn <- function(x, n) order(x, decreasing = TRUE)[n]

  cluster <- apply(prob_matrix, 1, maxn, n = 1)
  neighbor <- apply(prob_matrix, 1, maxn, n = 2)

  sil_width_num <- numeric(nrow(prob_matrix))
  weight <- numeric(nrow(prob_matrix))

  for (i in seq_len(nrow(prob_matrix))) {
    sil_width_num[i] <- log(prob_matrix[i, cluster[i]] / prob_matrix[i, neighbor[i]])
    if (average == "fuzzy") {
      weight[i] <- (prob_matrix[i, cluster[i]] - prob_matrix[i, neighbor[i]])^a
    }
  }

  sil_width <- sil_width_num / max(abs(sil_width_num))

  if (average == "fuzzy") {
    widths <- data.frame(cluster = cluster, neighbor = neighbor, weight = weight, sil_width = sil_width)
  } else {
    widths <- data.frame(cluster = cluster, neighbor = neighbor, sil_width = sil_width)
  }
  original_name <- NULL
  if (sort) {
    widths <- widths %>%
      dplyr::mutate(original_name = row.names(widths)) %>%
      dplyr::arrange(cluster, dplyr::desc(sil_width))
    rownames(widths) <- widths$original_name
    widths <- subset(widths, select = -original_name)
  }

  attr(widths, "proximity_type") <- "similarity"
  attr(widths, "method") <- "db"
  attr(widths, "average") <- average

  if (print.summary) {
    if (average == "crisp") {
      clus.avg.widths <- tapply(widths$sil_width, widths$cluster, mean, na.rm = TRUE)
      avg.width <- mean(widths$sil_width, na.rm = TRUE)
    } else if (average == "fuzzy") {
      sil_weight <- widths$weight * widths$sil_width
      clus.avg.widths <- tapply(seq_along(widths$sil_width), widths$cluster, function(idx) {
        sum(sil_weight[idx], na.rm = TRUE) / sum(widths$weight[idx], na.rm = TRUE)
      })
      avg.width <- sum(sil_weight, na.rm = TRUE) / sum(widths$weight, na.rm = TRUE)
    } else {
      clus.avg.widths <- tapply(widths$sil_width, widths$cluster, stats::median, na.rm = TRUE)
      avg.width <- stats::median(widths$sil_width, na.rm = TRUE)
    }
    n <- table(widths$cluster)
    sil.sum <- data.frame(cluster = names(clus.avg.widths), size = as.vector(n), avg.sil.width = round(clus.avg.widths, 4))

    header <- switch(average,
                     crisp = sprintf("Average crisp %s %s silhouette: %.4f", "similarity", "db", avg.width),
                     fuzzy = sprintf("Average fuzzy %s %s silhouette: %.4f", "similarity", "db", avg.width),
                     median = sprintf("Median %s %s silhouette: %.4f", "similarity", "db", avg.width))

    message(strrep("-", nchar(header)))
    message(header)
    message(strrep("-", nchar(header)))
    cat("\n")
    print(sil.sum)
    cat("\n")
    message("Available attributes: ", paste(names(attributes(widths)), collapse = ", "))
  }

  structure(widths, class = c("Silhouette", "data.frame"))
}
