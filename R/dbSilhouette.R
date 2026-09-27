#' Density-Based Silhouette Width (DBS) for Soft Clustering
#'
#' Computes the density-based silhouette (DBS) of Menardi (2011), using log-ratios of
#' posterior probabilities.
#'
#' The DBS compares the largest membership with the second largest on the log scale, scaled
#' by the largest such log ratio in the sample. Its numerator is that of the NLPPS of Bhat
#' Kapu and Kiruthika (2024), with a different denominator. Following Menardi (2011), the
#' median is the default summary, and the number of clusters is read from the local maxima
#' of the average over \eqn{k}.
#'
#' @param average Character string specifying the method for computing the average silhouette width.
#'   Options are:
#'   \itemize{
#'     \item \code{"crisp"} – unweighted (simple) average.
#'     \item \code{"fuzzy"} – weighted average; the weight of an observation is the gap between its two largest memberships raised to the power \code{a}.
#'     \item \code{"median"} – median silhouette width across observations.
#'   }
#'   Defaults to \code{"median"}.
#' @inheritParams cerSilhouette
#'
#' @details
#' Let the posterior probability matrix or cluster membership matrix be
#' \eqn{\Gamma = [\gamma_{ik}]_{n \times K}}, let \eqn{c_i} be the cluster of observation
#' \eqn{i} and \eqn{\nu_i} the cluster with its largest membership other than \eqn{c_i}.
#' With the log ratio
#' \deqn{R_i = \log \frac{\gamma_{ic_i}}{\gamma_{i\nu_i}},}
#' the **density-based silhouette width** is
#' \deqn{
#' \mathrm{DBS}_i = \frac{R_i}{\max_{j=1,\dots,n} |R_j|}.
#' }
#' With the default labels (largest membership) \eqn{R_i \ge 0}, so the widths lie in
#' \eqn{[0, 1]} and the most confidently assigned observation has width 1. With labels
#' supplied through \code{cluster}, \eqn{R_i} can be negative and the widths lie in
#' \eqn{[-1, 1]}.
#'
#' Because of the scaling, the average width depends on how extreme the most confident
#' observation is: a single observation with a very large log ratio shrinks every other
#' width towards zero. Compare the average only across fits with similar extremes, and
#' report it together with the mode-wise or cluster-wise summaries.
#'
#' @section Numerical safeguards:
#' The following rules keep the widths finite and meaningful:
#' \itemize{
#'   \item \strong{Zero probabilities.} When the neighboring membership is exactly zero
#'     (for example, after a posterior probability underflows), \eqn{R_i = +\infty}. Such
#'     observations receive width 1 (and width -1 when the assigned membership is zero,
#'     which can happen only with supplied labels), and a warning reports how many there
#'     are. The remaining widths are scaled by the largest \emph{finite} \eqn{|R_j|}. (In
#'     version 0.9.6 and earlier, the infinite maximum turned every finite width into 0.)
#'   \item \strong{Equal memberships.} When the two memberships are equal, \eqn{R_i = 0}
#'     and the width is 0.
#'   \item \strong{Degenerate fits.} When the largest finite \eqn{|R_j|} is below
#'     \code{sqrt(.Machine$double.eps)} (about \eqn{1.5 \times 10^{-8}}), the memberships
#'     are numerically indistinguishable and scaling would only magnify rounding error.
#'     The finite widths are then set to 0 with a warning.
#' }
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
#'   \item{method}{The silhouette calculation method used i.e., \code{"db"}.}
#'   \item{average}{Character — the averaging method: \code{"crisp"}, \code{"fuzzy"}, or \code{"median"}.}
#' }
#'
#' @references
#'
#' Menardi, G. (2011). Density-based silhouette diagnostics for clustering methods. \emph{Statistics and Computing}, 21(3), 295--308. \doi{10.1007/s11222-010-9169-0}
#'
#' Bhat Kapu, S., & Kiruthika. (2024). Some density-based silhouette diagnostics for soft clustering algorithms. Communications in Statistics: Case Studies, Data Analysis and Applications, 10(3-4), 221-238. \doi{10.1080/23737484.2024.2408534}
#'
#' @seealso \code{\link{Silhouette}}, \code{\link{softSilhouette}}, \code{\link{cerSilhouette}}, \code{\link{calSilhouette}}, \code{\link{getSilhouette}}, \code{\link{is.Silhouette}}, \code{\link{plotSilhouette}}
#' @export
#' @importFrom stats median
#'
#' @examples
#' prob <- matrix(c(0.90, 0.10,
#'                  0.60, 0.40,
#'                  0.20, 0.80,
#'                  0.45, 0.55), ncol = 2, byrow = TRUE)
#' dbSilhouette(prob)
#'
#' # A zero neighboring probability gives width 1 and a warning
#' prob0 <- rbind(prob, c(1, 0))
#' dbSilhouette(prob0)
#'
#' \donttest{
#' # Compare two soft clustering algorithms on the iris data
#' data(iris)
#' if (requireNamespace("ppclust", quietly = TRUE)) {
#'   fcm_result <- ppclust::fcm(iris[, 1:4], 3)
#'   out_fcm <- dbSilhouette(prob_matrix = fcm_result$u, print.summary = TRUE)
#'   plot(out_fcm)
#'   fcm2_result <- ppclust::fcm2(iris[, 1:4], 3)
#'   out_fcm2 <- dbSilhouette(prob_matrix = fcm2_result$u)
#'   c(fcm = summary(out_fcm, print.summary = FALSE)$avg.width,
#'     fcm2 = summary(out_fcm2, print.summary = FALSE)$avg.width)
#' } else {
#'   message("Install 'ppclust' to run this example: install.packages('ppclust')")
#' }
#' }
dbSilhouette <- function(prob_matrix,
                         average = c("median", "crisp", "fuzzy"),
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
  .check_a(a)
  average <- match.arg(average)

  n_row <- nrow(prob_matrix)
  cluster <- if (is.null(cluster)) {
    .closest(prob_matrix, larger_is_closer = TRUE)
  } else {
    .check_cluster(cluster, n_row, ncol(prob_matrix))
  }
  neighbor <- .closest_other(prob_matrix, cluster, larger_is_closer = TRUE)

  row_idx <- seq_len(n_row)
  sil_width <- .db_widths(prob_matrix[cbind(row_idx, cluster)],
                          prob_matrix[cbind(row_idx, neighbor)])

  weight <- if (average == "fuzzy") .fuzzy_weights(prob_matrix, a) else NULL
  widths <- .new_silhouette(cluster, neighbor, sil_width, weight,
                            proximity_type = "similarity", method = "db",
                            average = average, sort = sort)

  if (isTRUE(print.summary)) {
    .report_silhouette(widths, .silhouette_summary(widths))
  }
  widths
}

# Density-based widths from the assigned (p_c) and neighboring (p_n) memberships,
# with the safeguards documented in ?dbSilhouette.
.db_widths <- function(p_c, p_n, tol = sqrt(.Machine$double.eps)) {
  R <- log(p_c) - log(p_n)
  R[p_c == p_n] <- 0
  finite <- is.finite(R)
  n_inf <- sum(!finite)
  if (n_inf > 0) {
    warning(n_inf, " observation(s) have a zero membership in the ",
            "neighboring (or assigned) cluster, so their log ratio is infinite; their ",
            "widths are set to 1 (or -1) and the other widths are scaled by the largest ",
            "finite log ratio.", call. = FALSE)
  }
  widths <- numeric(length(R))
  widths[!finite] <- sign(R[!finite])
  scale <- if (any(finite)) max(abs(R[finite])) else 0
  if (scale >= tol) {
    widths[finite] <- R[finite] / scale
  } else if (scale > 0) {
    warning("The largest finite log ratio (", signif(scale, 3), ") is below ",
            "sqrt(.Machine$double.eps): the memberships are numerically ",
            "indistinguishable, so the finite widths are set to 0.", call. = FALSE)
  }
  widths
}
