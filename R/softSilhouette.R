#' Calculate Silhouette Width for Soft Clustering Algorithms
#'
#' Computes silhouette widths for soft clustering results by interpreting cluster membership probabilities (or their transformations) as proximity measures (Bhat Kapu and Kiruthika, 2024). Although originally designed for evaluating clustering quality within a method, this adaptation allows heuristic comparison across soft clustering algorithms using average silhouette widths.
#'
#' With the names used by Bhat Kapu and Kiruthika (2024): \code{prob_type = "pp"} with
#' \code{method = "pac"} is the PACS (the silhouette based on the probability of the
#' alternative class of Raymaekers and Rousseeuw, 2022, applied to posterior probabilities), \code{"pp"} with \code{"medoid"} the PPS (posterior probability silhouette),
#' \code{"nlpp"} with \code{"medoid"} the NLPPS (negative log of posterior probability
#' silhouette), and \code{"pd"} with \code{"medoid"} the PDS (probability density
#' silhouette). The certainty silhouette (CeS) and the density-based silhouette (DBS) are
#' computed by \code{\link{cerSilhouette}} and \code{\link{dbSilhouette}}.
#'
#' @param prob_matrix A numeric matrix where rows represent observations and columns represent cluster membership probabilities. Rows must sum to 1. If \code{clust_fun} is provided, \code{prob_matrix} should be the name of the matrix component as a string (e.g., \code{"u"} for \code{\link[ppclust]{fcm}}).
#' @param prob_type Character string specifying the type transformation of membership matrix considered as proximity matrix in \code{prob_matrix}. Options are:
#' \describe{
#'   \item{\code{"pp"}}{Posterior probabilities \eqn{[\gamma_{ik}]_{n \times K}} (non-negative, summing to 1 per row), treated as similarities (PPS, PACS).}
#'   \item{\code{"nlpp"}}{Negative log of posterior probabilities \eqn{[-\ln\gamma_{ik}]_{n \times K}} (non-negative; \eqn{+\infty} for a zero probability), treated as dissimilarities (NLPPS).}
#'   \item{\code{"pd"}}{Probability density: the component densities \eqn{f_k(x_i)}, treated as similarities (PDS). They are computed from the memberships by Bayes' theorem as \eqn{[\gamma_{ik}/\hat\pi_{k}]_{n \times K}}, with \eqn{\hat\pi_{k} = n^{-1}\sum_i \gamma_{ik}}; for a mixture fitted by EM this equals \eqn{f_k(x_i)/f(x_i)} (see Details), and for other soft clustering algorithms the same ratio is used.}
#' }
#' Defaults to \code{"pp"}.
#' @param method Character string specifying the normalization of the silhouette width. Options are \code{"pac"} (Probability of Alternative Cluster) or \code{"medoid"}; see \code{\link{Silhouette}}. Defaults to \code{"pac"}.
#' @param cluster Optional integer vector of length \eqn{n} giving the cluster of each
#'   observation as a column number of \code{prob_matrix}. If \code{NULL} (the default), each
#'   observation is assigned to the cluster with its largest membership (for \code{"pp"} and
#'   \code{"nlpp"}) or its largest density (for \code{"pd"}); see \emph{Cluster labels}. If
#'   \code{clust_fun} is provided, \code{cluster} may be the name of the label component of its
#'   output. Must be named in the call because it follows \code{...}.
#' @inheritParams Silhouette
#'
#' @details
#' The membership matrix is transformed according to \code{prob_type} and the widths are
#' computed as in \code{\link{Silhouette}}. The PPS and PACS treat the posterior
#' probabilities as a similarity proximity measure, the NLPPS turns them into a dissimilarity
#' through the negative logarithm, and the PDS treats the component densities as a similarity
#' (Bhat Kapu and Kiruthika, 2024). With \code{method = "pac"}, the \code{"pp"} width is
#' \eqn{(\gamma_{ic_i} - \gamma_{i\nu_i})/(\gamma_{ic_i} + \gamma_{i\nu_i})}; with
#' \code{"medoid"} it is \eqn{1 - \gamma_{i\nu_i}/\gamma_{ic_i}}.
#'
#' For a finite mixture \eqn{f(x) = \sum_k \pi_k f_k(x)} fitted by EM,
#' \eqn{\gamma_{ik} = \pi_k f_k(x_i)/f(x_i)} and, at convergence, \eqn{\hat\pi_k} is the
#' fitted mixing proportion, so \eqn{\gamma_{ik}/\hat\pi_k = f_k(x_i)/f(x_i)} (up to the
#' convergence tolerance). The PDS width of Equation (22) of Bhat Kapu and Kiruthika (2024),
#' \eqn{(f_{c_i}(x_i) - \max_{k \neq c_i} f_k(x_i)) / \max(f_{c_i}(x_i), \max_{k \neq c_i} f_k(x_i))},
#' is therefore computed from the memberships alone, and can be used with any soft clustering
#' algorithm.
#'
#' A zero probability gives an infinite \code{"nlpp"} dissimilarity. The width is then 1 if
#' only the neighboring membership is zero, -1 if only the assigned membership is zero, and
#' 0 if both are zero.
#'
#' @section Choosing the number of clusters:
#' The average of these widths over \eqn{k = 2, 3, \ldots} typically starts near 1 and
#' decreases. Bhat Kapu and Kiruthika (2024) read the number of clusters from the reverse
#' elbow drop of this curve, the value of \eqn{k} after which the average drops sharply,
#' rather than from its maximum. Which \eqn{k} is suggested depends on the algorithm that
#' produced the memberships, on the width and on the averaging, so report all three with the
#' result.
#'
#' @section Cluster labels:
#' The labels are \code{cluster} when supplied. Otherwise, for \code{"pp"} and \code{"nlpp"},
#' each observation is assigned to the cluster with its largest membership, the partition a
#' soft clustering algorithm reports, and the widths are non-negative. For \code{"pd"}, each
#' observation is assigned to the cluster with its largest density
#' \eqn{\gamma_{ik}/\hat\pi_k}, as in the definition of the PDS, so the PDS widths lie in
#' \eqn{[0, 1]} for every average. The largest density and the largest membership coincide
#' when the clusters have equal estimated weights; otherwise the PDS can place an observation
#' in a smaller cluster than its largest membership does. The neighboring cluster is the
#' closest other cluster in the transformed matrix.
#'
#' Ties are resolved in favor of the first column; see \code{\link{Silhouette}}.
#'
#' @inheritSection Silhouette Averages
#'
#' @return A data frame of class \code{"Silhouette"} with one row per observation and columns
#' \code{cluster}, \code{neighbor}, \code{weight} (only when \code{average = "fuzzy"}) and
#' \code{sil_width}. The object includes the following attributes:
#' \describe{
#'   \item{proximity_type}{The proximity type used (\code{"similarity"} or \code{"dissimilarity"}).}
#'   \item{method}{The silhouette calculation method used (\code{"medoid"} or \code{"pac"}).}
#'   \item{average}{Character — the averaging method: \code{"crisp"}, \code{"fuzzy"}, or \code{"median"}.}
#' }
#'
#' @seealso \code{\link{Silhouette}}, \code{\link{dbSilhouette}}, \code{\link{cerSilhouette}}, \code{\link{calSilhouette}}, \code{\link{getSilhouette}}, \code{\link{is.Silhouette}}, \code{\link{plotSilhouette}}
#'
#' @references
#'
#' Raymaekers, J., & Rousseeuw, P. J. (2022). Silhouettes and quasi residual plots for neural nets and tree-based classifiers. \emph{Journal of Computational and Graphical Statistics}, 31(4), 1332--1343. \doi{10.1080/10618600.2022.2050249}
#'
#' Campello, R. J., & Hruschka, E. R. (2006). A fuzzy extension of the silhouette width criterion for cluster analysis. \emph{Fuzzy Sets and Systems}, 157(21), 2858--2875. \doi{10.1016/j.fss.2006.07.006}
#'
#' Bhat Kapu, S., & Kiruthika. (2024). Some density-based silhouette diagnostics for soft clustering algorithms. Communications in Statistics: Case Studies, Data Analysis and Applications, 10(3-4), 221-238. \doi{10.1080/23737484.2024.2408534}
#'
#' @examples
#' # A small membership matrix
#' prob <- matrix(c(0.90, 0.10,
#'                  0.60, 0.40,
#'                  0.20, 0.80,
#'                  0.45, 0.55), ncol = 2, byrow = TRUE)
#' softSilhouette(prob)
#' softSilhouette(prob, prob_type = "nlpp", average = "fuzzy")
#'
#' \donttest{
#' # Compare two soft clustering algorithms on the iris data
#' data(iris)
#' if (requireNamespace("ppclust", quietly = TRUE)) {
#'   fcm_result <- ppclust::fcm(iris[, 1:4], 3)
#'   out_fcm <- softSilhouette(prob_matrix = fcm_result$u, print.summary = TRUE)
#'   plot(out_fcm)
#'   fcm2_result <- ppclust::fcm2(iris[, 1:4], 3)
#'   out_fcm2 <- softSilhouette(prob_matrix = fcm2_result$u)
#'   c(fcm = summary(out_fcm, print.summary = FALSE)$avg.width,
#'     fcm2 = summary(out_fcm2, print.summary = FALSE)$avg.width)
#' } else {
#'   message("Install 'ppclust' to run this example: install.packages('ppclust')")
#' }
#' }
#' @export
softSilhouette <- function(prob_matrix,
                           prob_type = c("pp", "nlpp", "pd"),
                           method = c("pac", "medoid"),
                           average = c("crisp", "fuzzy", "median"),
                           a = 2,
                           sort = FALSE,
                           print.summary = FALSE,
                           clust_fun = NULL, ...,
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

  prob_type <- match.arg(prob_type)
  method <- match.arg(method)
  average <- match.arg(average)
  .check_a(a)

  n_row <- nrow(prob_matrix)
  K <- ncol(prob_matrix)
  if (!is.null(cluster)) cluster <- .check_cluster(cluster, n_row, K)

  if (prob_type == "pp") {
    proximity_type <- "similarity"
    prox_matrix <- prob_matrix
  } else if (prob_type == "nlpp") {
    proximity_type <- "dissimilarity"
    prox_matrix <- -log(prob_matrix)
  } else {
    proximity_type <- "similarity"
    pi_hat <- colMeans(prob_matrix)
    if (any(pi_hat == 0)) {
      stop("Column sums in prob_matrix must be non-zero for prob_type = 'pd'.")
    }
    prox_matrix <- sweep(prob_matrix, 2, pi_hat, "/")
  }

  larger_is_closer <- proximity_type == "similarity"
  if (is.null(cluster)) {
    # largest membership for "pp" and "nlpp"; largest density for "pd" (the PDS definition)
    cluster <- if (prob_type == "pd") .closest(prox_matrix, larger_is_closer = TRUE)
               else .closest(prob_matrix, larger_is_closer = TRUE)
  }
  neighbor <- .closest_other(prox_matrix, cluster, larger_is_closer)
  row_idx <- seq_len(n_row)
  sil_width <- .width_from_pair(prox_matrix[cbind(row_idx, cluster)],
                                prox_matrix[cbind(row_idx, neighbor)],
                                larger_is_closer, method)

  weight <- if (average == "fuzzy") .fuzzy_weights(prob_matrix, a) else NULL
  widths <- .new_silhouette(cluster, neighbor, sil_width, weight,
                            proximity_type = proximity_type, method = method,
                            average = average, sort = sort)

  if (isTRUE(print.summary)) {
    .report_silhouette(widths, .silhouette_summary(widths))
  }
  widths
}
