#' Calculate Silhouette Widths, Summary, and Plot for Clustering Results
#'
#' Computes the simplified silhouette width of each observation from an
#' \eqn{n \times K} matrix of proximities between observations and clusters
#' (for example, distances to cluster centers). The width compares the
#' proximity of an observation to its own cluster with its proximity to the
#' closest other cluster. Widths lie in \eqn{[-1, 1]}; values near 1 indicate a
#' clear assignment and negative values indicate that another cluster is closer.
#'
#' \code{Silhouette()} is not the same as \code{\link[cluster]{silhouette}} from
#' the \pkg{cluster} package: that function needs all \eqn{n(n-1)/2} pairwise
#' dissimilarities, whereas \code{Silhouette()} needs only the \eqn{n \times K}
#' proximities between observations and cluster representatives (centroids,
#' medoids or prototypes). The two give different, although usually similar,
#' values.
#'
#' @param prox_matrix A numeric matrix where rows represent observations and columns represent proximity measures (e.g., distances or similarities) to clusters. Typically, this is a membership or dissimilarity matrix from clustering results. If \code{clust_fun} is provided, \code{prox_matrix} should be the name of the matrix component as a string (e.g., if \code{clust_fun = \link[ppclust]{fcm}} from \pkg{ppclust} package the \code{prox_matrix = "d"}).
#' @param proximity_type Character string specifying the type of proximity measure in \code{prox_matrix}. Options are \code{"similarity"} (higher values indicate closer proximity) or \code{"dissimilarity"} (lower values indicate closer proximity). Defaults to \code{"dissimilarity"}.
#' @param method Character string specifying the normalization of the silhouette width. Options are \code{"medoid"} (divide by the larger of the two proximities) or \code{"pac"} (Probability of Alternative Cluster; divide by their sum). See Details. Defaults to \code{"medoid"}.
#' @param average Character string specifying the method for computing the average silhouette width.
#'   Options are:
#'   \itemize{
#'     \item \code{"crisp"} – unweighted (simple) average.
#'     \item \code{"fuzzy"} – weighted average; the weight of an observation is the gap between its two largest memberships raised to the power \code{a}.
#'     \item \code{"median"} – median silhouette width across observations.
#'   }
#'   Defaults to \code{"crisp"}.
#' @param prob_matrix A numeric matrix of cluster membership probabilities, where rows represent
#'   observations and columns represent clusters in the same order as \code{prox_matrix}.
#'   If \code{clust_fun} is provided, \code{prob_matrix} can be given as the name of the matrix
#'   component (e.g., \code{"u"} for the \code{\link[ppclust]{fcm}} function).
#'   When given, and \code{cluster} is not, each observation is assigned to the cluster with its
#'   largest membership, and the weights for fuzzy averaging are computed.
#'   When \code{NULL} and \code{average = "fuzzy"}, the function falls back to computing the
#'   \code{"crisp"} silhouette with a warning, since fuzzy silhouette widths require membership
#'   probabilities. Defaults to \code{NULL}.
#' @param a Non-negative number, the exponent \eqn{\alpha} of the weights in fuzzy silhouette averaging. Higher values increase the emphasis on strong membership differences; \code{a = 0} gives every observation the same weight, so the fuzzy average equals the crisp average. Defaults to \code{2}.
#' @param sort Logical; if \code{TRUE}, sorts the output \code{widths} data frame by cluster and descending silhouette width. Defaults to \code{FALSE}.
#' @param print.summary Logical; if \code{TRUE}, reports a summary table of average silhouette widths and sizes for each cluster through \code{\link{message}}. Defaults to \code{FALSE} for \code{Silhouette()} and \code{TRUE} for \code{summary()}.
#' @param clust_fun Optional S3 or S4 function object or function as character string specifying a clustering function that produces the proximity measure matrix. For example, \code{\link[ppclust]{fcm}} or \code{"fcm"}. If provided, \code{prox_matrix} must be the name of the matrix component in the clustering output (e.g., \code{"d"} for \code{\link[ppclust]{fcm}} when \code{proximity_type = "dissimilarity"}). Defaults to \code{NULL}.
#' @param ... Additional arguments passed to \code{clust_fun}, such as \code{x,centers} for \code{\link[ppclust]{fcm}}.
#' @param clust_args A list of further arguments for \code{clust_fun}. Arguments given through \code{...} are matched first against this function's own arguments, and R matches names partially: \code{m = 3} meant for the fuzzifier of \code{\link[ppclust]{fcm}} would be taken as \code{method}. Pass such arguments here instead, e.g. \code{clust_args = list(m = 3)}. Defaults to \code{list()}.
#' @param cluster Optional integer vector of length \eqn{n} giving the cluster of each
#'   observation as a column number of \code{prox_matrix} (\eqn{1, \ldots, K}). Use it to score
#'   a partition produced elsewhere, for example by hierarchical clustering or by a clustering
#'   algorithm whose labels differ from the nearest representative. If \code{clust_fun} is
#'   provided, \code{cluster} may be the name of the label component of its output (e.g.,
#'   \code{"cluster"} for \code{\link[ppclust]{fcm}}). Must be named in the call because it
#'   follows \code{...}. Defaults to \code{NULL}.
#'
#' @details
#' The `Silhouette` function implements the Simplified Silhouette method introduced by Van der Laan, Pollard, & Bryan (2003), which adapts and generalizes the classic silhouette method of Rousseeuw (1987).
#'
#' Clustering quality is evaluated using a proximity matrix, denoted as
#' \eqn{\Delta = [\delta_{ik}]_{n \times K}} for dissimilarity measures or
#' \eqn{\Delta' = [\delta'_{ik}]_{n \times K}} for similarity measures.
#' Here, \eqn{i = 1, \ldots, n} indexes observations, and \eqn{k = 1, \ldots, K} indexes clusters.
#' \eqn{\delta_{ik}} represents the dissimilarity (e.g., distance) between observation \eqn{i} and cluster \eqn{k},
#' while \eqn{\delta'_{ik}} represents similarity values.
#'
#' Let \eqn{c_i} be the cluster of observation \eqn{i} (see \emph{Cluster labels} below)
#' and \eqn{\nu_i} its neighboring cluster, the closest cluster other than \eqn{c_i}.
#' The silhouette width \eqn{S(x_i)} for observation \eqn{i} depends on the proximity type:
#'
#' For **dissimilarity** measures:
#' \deqn{
#'   S(x_i) = \frac{ \delta_{i\nu_i} - \delta_{ic_i} }{ N(x_i) }
#' }
#' For **similarity** measures:
#' \deqn{
#'   S(x_i) = \frac{ \delta'_{ic_i} - \delta'_{i\nu_i} }{ N(x_i) }
#' }
#' where \eqn{N(x_i)} is a normalizing factor defined by the method.
#'
#' **Choice of method:**
#' The normalizer \eqn{N(x_i)} is selected according to the `method` argument:
#' - For `medoid` (Van der Laan et al., 2003):
#'   - Dissimilarity: \eqn{\max(\delta_{ic_i}, \delta_{i\nu_i})}
#'   - Similarity:    \eqn{\max(\delta'_{ic_i}, \delta'_{i\nu_i})}
#' - For `pac` (Raymaekers & Rousseeuw, 2022):
#'   - Dissimilarity: \eqn{\delta_{ic_i} + \delta_{i\nu_i}}
#'   - Similarity:    \eqn{\delta'_{ic_i} + \delta'_{i\nu_i}}
#'
#' The names describe the normalization, not the algorithm. The name \code{"medoid"} is kept
#' for compatibility with earlier versions and with Van der Laan et al. (2003), who
#' introduced this width for partitioning around medoids; it divides by the larger of the two
#' proximities and applies equally to centroids (as in k-means), prototypes, or any other
#' cluster representative. Likewise, \code{"pac"} does not require a classifier. For the same
#' labels and non-negative proximities, the PAC width equals \eqn{s/(2 - |s|)}, where \eqn{s}
#' is the medoid width, so the two rank observations identically and differ only in scale.
#' These methods can also be applied to proximities derived from **classification algorithms**, to assess the separation of groups formed by a classifier.
#'
#' Two equal proximities give a width of 0 (including two zero distances). An
#' infinite proximity to the neighboring cluster gives a width of 1 and an infinite
#' proximity to the assigned cluster gives -1.
#'
#' @section Cluster labels:
#' The cluster \eqn{c_i} of each observation is taken, in order of precedence, from
#' \enumerate{
#'   \item \code{cluster}, when supplied;
#'   \item the largest membership in \code{prob_matrix}, when supplied;
#'   \item the closest cluster in \code{prox_matrix}.
#' }
#' The neighboring cluster \eqn{\nu_i} is always the closest cluster other than \eqn{c_i} in
#' the proximity matrix. When the labels come from the proximity matrix itself (case 3), every
#' width is non-negative, so a poorly assigned observation shows up only as a width near 0.
#' When the labels come from elsewhere (cases 1 and 2), an observation that is closer to another
#' cluster than to its own receives a negative width. To assess the partition produced by a
#' clustering algorithm, supply that partition through \code{cluster} or \code{prob_matrix}.
#'
#' @section Averages:
#' If `average = "crisp"`, the **crisp silhouette index** (\eqn{CS}) is the mean width:
#' \deqn{
#'   CS = \frac{1}{n} \sum_{i=1}^{n} S(x_i)
#' }
#' summarizing overall clustering quality.
#'
#' If `average = "fuzzy"` and `prob_matrix` is provided, denoted as \eqn{\Gamma = [\gamma_{ik}]_{n \times K}},
#' with \eqn{\gamma_{ik}} representing the probability of observation \eqn{i} belonging to cluster \eqn{k},
#' the **fuzzy silhouette index** (\eqn{FS}) of Campello and Hruschka (2006) is:
#' \deqn{
#'   FS = \frac{\sum_{i=1}^{n}  w_i  S(x_i) }{\sum_{i=1}^{n}  w_i},
#'   \qquad w_i = ( \gamma_{i(1)} - \gamma_{i(2)} )^{\alpha},
#' }
#' where \eqn{\gamma_{i(1)}} and \eqn{\gamma_{i(2)}} are the largest and second largest
#' memberships of observation \eqn{i}, and \eqn{\alpha} (the `a` argument) controls the
#' emphasis on confident assignments (\eqn{\alpha \ge 0}; \eqn{\alpha = 0} gives the crisp
#' average). The weight \eqn{w_i} is stored in the \code{weight} column and does not depend on
#' the cluster labels; it equals Equation (8) of Bhat Kapu and Kiruthika (2024) when each
#' observation is assigned to its largest membership. If all weights are zero, the fuzzy
#' average is undefined (\code{NaN}) and a warning is given.
#'
#' If `average = "median"`, the median width is reported.
#'
#' Missing widths are removed before averaging.
#'
#' @section Input checks and ties:
#' \code{prox_matrix} must be a numeric matrix with at least two columns and no missing
#' values; \code{+Inf} is allowed (for example, \eqn{-\log 0}) but \code{-Inf} is not.
#' Negative proximities are rejected, because the widths would then no longer lie in
#' \eqn{[-1, 1]}; transform them first (for example, \eqn{(1 + r)/2} for correlations).
#' \code{prob_matrix} must be non-negative with rows summing to 1 within a tolerance of
#' \code{sqrt(.Machine$double.eps)} (about \eqn{1.5 \times 10^{-8}}); rows are not
#' renormalized.
#'
#' The closest cluster, the neighboring cluster and the largest membership are found with
#' \code{\link{max.col}(..., ties.method = "first")}. Exactly tied proximities or memberships
#' are therefore resolved in favor of the column that comes first, so with exact ties the
#' labels (though not the widths from the proximity matrix itself, which are then 0) depend on
#' the order of the columns. Supply \code{cluster} to control the assignment of tied
#' observations.
#'
#' @return A data frame of class \code{"Silhouette"} with one row per observation and columns
#' \code{cluster}, \code{neighbor}, \code{weight} (only when \code{prob_matrix} is supplied) and
#' \code{sil_width}. The object includes the following attributes:
#' \describe{
#'   \item{proximity_type}{The proximity type used (\code{"similarity"} or \code{"dissimilarity"}).}
#'   \item{method}{The silhouette calculation method used (\code{"medoid"} or \code{"pac"}).}
#'   \item{average}{Character — the averaging method: \code{"crisp"}, \code{"fuzzy"}, or \code{"median"}.}
#' }
#' @seealso \code{\link{softSilhouette}}, \code{\link{dbSilhouette}}, \code{\link{cerSilhouette}}, \code{\link{calSilhouette}}, \code{\link{getSilhouette}}, \code{\link{is.Silhouette}}, \code{\link{plotSilhouette}}, \code{\link[cluster]{silhouette}}
#'
#' @references
#' Rousseeuw, P. J. (1987). Silhouettes: A graphical aid to the interpretation and validation of cluster analysis. \emph{Journal of Computational and Applied Mathematics}, 20, 53--65. \doi{10.1016/0377-0427(87)90125-7}
#'
#' Van der Laan, M., Pollard, K., & Bryan, J. (2003). A new partitioning around medoids algorithm. \emph{Journal of Statistical Computation and Simulation}, 73(8), 575--584. \doi{10.1080/0094965031000136012}
#'
#' Campello, R. J., & Hruschka, E. R. (2006). A fuzzy extension of the silhouette width criterion for cluster analysis. \emph{Fuzzy Sets and Systems}, 157(21), 2858--2875. \doi{10.1016/j.fss.2006.07.006}
#'
#' Raymaekers, J., & Rousseeuw, P. J. (2022). Silhouettes and quasi residual plots for neural nets and tree-based classifiers. \emph{Journal of Computational and Graphical Statistics}, 31(4), 1332--1343. \doi{10.1080/10618600.2022.2050249}
#'
#' Bhat Kapu, S., & Kiruthika. (2024). Some density-based silhouette diagnostics for soft clustering algorithms. Communications in Statistics: Case Studies, Data Analysis and Applications, 10(3-4), 221-238. \doi{10.1080/23737484.2024.2408534}
#'
#' @examples
#' # Simplified silhouette of a k-means partition of the iris data
#' data(iris)
#' out <- kmeans(iris[, -5], 3)
#' if (requireNamespace("proxy", quietly = TRUE)) {
#'   d <- proxy::dist(iris[, -5], out$centers)
#'   silh_out <- Silhouette(d, print.summary = TRUE)
#'   plot(silh_out)
#'
#'   # Score a partition produced elsewhere: average-linkage clustering,
#'   # with the distances to the centroids of its groups
#'   hc <- cutree(hclust(dist(iris[, -5]), "average"), 3)
#'   centers <- apply(iris[, -5], 2, function(v) tapply(v, hc, mean))
#'   silh_hc <- Silhouette(proxy::dist(iris[, -5], centers), cluster = hc)
#'   summary(silh_hc)
#' } else {
#'   message("Install 'proxy': install.packages('proxy')")
#' }
#' \donttest{
#' # Scree plot for optimal clusters (2 to 7)
#' if (requireNamespace("ppclust", quietly = TRUE)) {
#'   library(ppclust)
#'   avg_sil_width <- rep(NA,7)
#'   for (k in 2:7) {
#'     out <- Silhouette(
#'       prox_matrix = "d",
#'       proximity_type = "dissimilarity",
#'       prob_matrix = "u",
#'       clust_fun = ppclust::fcm,
#'       x = iris[, 1:4],
#'       centers = k,
#'       average = "fuzzy"
#'     )
#'     # Compute average silhouette width from widths
#'     avg_sil_width[k] <- summary(out, print.summary = FALSE)$avg.width
#'   }
#'   plot(avg_sil_width,
#'     type = "o",
#'     ylab = "Overall Silhouette Width",
#'     xlab = "Number of Clusters",
#'     main = "Scree Plot"
#'   )
#' } else {
#'   message("Install 'ppclust': install.packages('ppclust')")
#' }
#' }
#' @export
#' @importFrom stats median
#' @importFrom utils capture.output
Silhouette <- function(prox_matrix,
                       proximity_type = c("dissimilarity", "similarity"),
                       method = c("medoid", "pac"),
                       average = c("crisp", "fuzzy", "median"),
                       prob_matrix = NULL,
                       a = 2,
                       sort = FALSE,
                       print.summary = FALSE,
                       clust_fun = NULL, ...,
                       cluster = NULL,
                       clust_args = list()) {

  prox_name <- "prox_matrix"
  prob_name <- "prob_matrix"
  if (!is.null(clust_fun)) {
    .check_name(prox_matrix, "prox_matrix")
    if (!is.null(prob_matrix)) .check_name(prob_matrix, "prob_matrix")
    clust_out <- .run_clust_fun(clust_fun, ..., clust_args = clust_args, envir = parent.frame())
    prox_matrix <- .extract_component(clust_out, prox_matrix, "prox_matrix")
    if (!is.null(prob_matrix)) {
      prob_matrix <- .extract_component(clust_out, prob_matrix, "prob_matrix")
    }
    if (is.character(cluster) && length(cluster) == 1) {
      cluster <- .extract_component(clust_out, cluster, "cluster")
    }
    prox_name <- "Extracted prox_matrix"
    prob_name <- "Extracted prob_matrix"
  }

  .check_prox(prox_matrix, prox_name)
  if (!is.null(prob_matrix)) {
    .check_prob(prob_matrix, prob_name)
    if (!identical(dim(prob_matrix), dim(prox_matrix))) {
      stop("prob_matrix must have the same dimensions as prox_matrix.")
    }
  }

  proximity_type <- match.arg(proximity_type)
  method <- match.arg(method)
  average <- match.arg(average)
  .check_a(a)

  if (average == "fuzzy" && is.null(prob_matrix)) {
    warning("average = 'fuzzy' requires prob_matrix; falling back to 'crisp'.")
    average <- "crisp"
  }

  larger_is_closer <- proximity_type == "similarity"
  n_row <- nrow(prox_matrix)
  K <- ncol(prox_matrix)

  if (!is.null(cluster)) {
    cluster <- .check_cluster(cluster, n_row, K)
  } else if (!is.null(prob_matrix)) {
    cluster <- .closest(prob_matrix, larger_is_closer = TRUE)
  } else {
    cluster <- .closest(prox_matrix, larger_is_closer)
  }
  neighbor <- .closest_other(prox_matrix, cluster, larger_is_closer)

  row_idx <- seq_len(n_row)
  sil_width <- .width_from_pair(prox_matrix[cbind(row_idx, cluster)],
                                prox_matrix[cbind(row_idx, neighbor)],
                                larger_is_closer, method)

  weight <- if (is.null(prob_matrix)) NULL else .fuzzy_weights(prob_matrix, a)

  widths <- .new_silhouette(cluster, neighbor, sil_width, weight,
                            proximity_type = proximity_type, method = method,
                            average = average, sort = sort)

  if (isTRUE(print.summary)) {
    .report_silhouette(widths, .silhouette_summary(widths))
  }
  widths
}
