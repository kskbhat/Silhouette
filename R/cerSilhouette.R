#' Certainty Silhouette Width (CeS) for Soft Clustering
#'
#' Computes the certainty silhouette (CeS) of Bhat Kapu and Kiruthika (2024) for each
#' observation: the membership probability of its assigned cluster (by default, its largest
#' membership).
#'
#' The certainty silhouette is the counterpart of the uncertainty used in model-based
#' clustering: values near 1 indicate high certainty that an observation belongs to its
#' cluster, and values near \eqn{1/K} ambiguity. It is bounded below by \eqn{1/K}, so its
#' level falls as \eqn{K} grows; Bhat Kapu and Kiruthika (2024) read the number of clusters
#' from the reverse elbow drop of its average over \eqn{k} (see \code{\link{softSilhouette}}).
#'
#' @param prob_matrix A numeric matrix of posterior probabilities where rows represent observations and columns represent clusters. Must sum to 1 by row. If \code{clust_fun} is provided, \code{prob_matrix} must be a string giving the name of the matrix component (e.g., "u").
#' @param clust_fun Optional S3 or S4 function object or function as character string specifying a clustering function that produces the membership matrix. For example, \code{\link[ppclust]{fcm}} or \code{"fcm"}. If provided, \code{prob_matrix} must be a string giving the name of the matrix component (e.g., "u"). Defaults to \code{NULL}.
#' @param ... Additional arguments passed to \code{clust_fun}, such as \code{x, centers} for \code{\link[ppclust]{fcm}}.
#' @param clust_args A list of further arguments for \code{clust_fun}; use it for arguments whose names could be partially matched to this function's own (for example \code{clust_args = list(m = 3)} for the fuzzifier of \code{\link[ppclust]{fcm}}). Defaults to \code{list()}.
#' @inheritParams softSilhouette
#'
#' @details
#' Let the posterior probability matrix or cluster membership matrix be
#' \eqn{\Gamma = [\gamma_{ik}]_{n \times K}}.
#' The **certainty silhouette width** for observation \eqn{i} assigned to cluster \eqn{c_i} is:
#' \deqn{
#' \mathrm{Cer}_i = \gamma_{ic_i},
#' }
#' which equals \eqn{\max_{k} \gamma_{ik} \in [1/K, 1]} with the default labels. With labels
#' supplied through \code{cluster}, the width lies in \eqn{[0, 1]}.
#' The \code{neighbor} column gives the cluster with the largest membership other than
#' \eqn{c_i}.
#'
#' Ties are resolved in favor of the first column; see \code{\link{Silhouette}}.
#'
#' @inheritSection Silhouette Averages
#'
#' @return A data frame of class \code{"Silhouette"} with one row per observation and columns
#' \code{cluster}, \code{neighbor}, \code{weight} (only when \code{average = "fuzzy"}) and
#' \code{sil_width}. The object includes the following attributes:
#' \describe{
#'   \item{proximity_type}{The proximity type used i.e., \code{"similarity"}.}
#'   \item{method}{The silhouette calculation method used i.e., \code{"certainty"}.}
#'   \item{average}{Character — the averaging method: \code{"crisp"}, \code{"fuzzy"}, or \code{"median"}.}
#' }
#'
#' @references
#'
#' Bhat Kapu, S., & Kiruthika. (2024). Some density-based silhouette diagnostics for soft clustering algorithms. Communications in Statistics: Case Studies, Data Analysis and Applications, 10(3-4), 221-238. \doi{10.1080/23737484.2024.2408534}
#'
#' @seealso \code{\link{Silhouette}}, \code{\link{softSilhouette}}, \code{\link{dbSilhouette}}, \code{\link{calSilhouette}}, \code{\link{getSilhouette}}, \code{\link{is.Silhouette}}, \code{\link{plotSilhouette}}
#'
#' @examples
#' prob <- matrix(c(0.90, 0.10,
#'                  0.60, 0.40,
#'                  0.20, 0.80,
#'                  0.45, 0.55), ncol = 2, byrow = TRUE)
#' cerSilhouette(prob)
#'
#' \donttest{
#' # Compare two soft clustering algorithms on the iris data
#' data(iris)
#' if (requireNamespace("ppclust", quietly = TRUE)) {
#'   fcm_result <- ppclust::fcm(iris[, 1:4], 3)
#'   out_fcm <- cerSilhouette(prob_matrix = fcm_result$u, print.summary = TRUE)
#'   plot(out_fcm)
#'   fcm2_result <- ppclust::fcm2(iris[, 1:4], 3)
#'   out_fcm2 <- cerSilhouette(prob_matrix = fcm2_result$u)
#'   c(fcm = summary(out_fcm, print.summary = FALSE)$avg.width,
#'     fcm2 = summary(out_fcm2, print.summary = FALSE)$avg.width)
#' } else {
#'   message("Install 'ppclust' to run this example: install.packages('ppclust')")
#' }
#' }
#' @export
cerSilhouette <- function(prob_matrix,
                          average = c("crisp", "fuzzy", "median"),
                          a = 2,
                          sort = FALSE,
                          print.summary = FALSE,
                          clust_fun = NULL,
                          ...,
                          cluster = NULL,
                          clust_args = list()) {

  prob_name <- "prob_matrix"
  if (!is.null(clust_fun)) {
    .check_name(prob_matrix, "prob_matrix")
    clust_out <- .run_clust_fun(clust_fun, ..., clust_args = clust_args, envir = parent.frame())
    prob_matrix <- .extract_component(clust_out, prob_matrix, "prob_matrix")
    if (is.character(cluster) && length(cluster) == 1) {
      cluster <- .extract_component(clust_out, cluster, "cluster")
    }
    prob_name <- "Extracted prob_matrix"
  }
  .check_prob(prob_matrix, prob_name)
  .check_a(a)
  average <- match.arg(average)

  n_row <- nrow(prob_matrix)
  cluster <- if (is.null(cluster)) {
    .closest(prob_matrix, larger_is_closer = TRUE)
  } else {
    .check_cluster(cluster, n_row, ncol(prob_matrix))
  }
  neighbor <- .closest_other(prob_matrix, cluster, larger_is_closer = TRUE)
  sil_width <- prob_matrix[cbind(seq_len(n_row), cluster)]

  weight <- if (average == "fuzzy") .fuzzy_weights(prob_matrix, a) else NULL
  widths <- .new_silhouette(cluster, neighbor, sil_width, weight,
                            proximity_type = "similarity", method = "certainty",
                            average = average, sort = sort)

  if (isTRUE(print.summary)) {
    .report_silhouette(widths, .silhouette_summary(widths))
  }
  widths
}
