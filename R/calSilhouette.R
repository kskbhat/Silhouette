#' Calculate All Available Silhouette Summaries
#'
#' Computes every silhouette width available in the package for one clustering and returns
#' a data frame comparing the crisp, fuzzy, and median averages of each. All rows score the
#' same partition.
#'
#' The rows are not independent pieces of evidence: several are exact or monotone
#' transformations of one another (see \emph{Relations between the rows}). Distance-based
#' rows measure separation relative to the cluster representatives; probability-based rows
#' measure how certain the assignments are.
#'
#' @param prox_matrix A numeric matrix where rows represent observations and columns represent proximity measures (e.g., distances or similarities) to clusters. Typically, this is a membership or dissimilarity matrix from clustering results. If \code{clust_fun} is provided, \code{prox_matrix} should be the name of the matrix component as a string (e.g., if \code{clust_fun = \link[ppclust]{fcm}} from \pkg{ppclust} package the \code{prox_matrix = "d"}). Defaults to \code{NULL}.
#' @param prob_matrix A numeric matrix of cluster membership probabilities, where rows represent
#'   observations and columns represent clusters in the same order as \code{prox_matrix}.
#'   If \code{clust_fun} is provided, \code{prob_matrix} can be given as the name of the matrix
#'   component (e.g., \code{"u"} for the \code{\link[ppclust]{fcm}} function).
#'   Defaults to \code{NULL}.
#' @param print.summary Logical; if \code{TRUE}, reports the summary table through \code{\link{message}}. Defaults to \code{FALSE}.
#' @param cluster Optional integer vector of length \eqn{n} giving the cluster of each
#'   observation as a column number (\eqn{1, \ldots, K}). If \code{clust_fun} is provided,
#'   \code{cluster} may be the name of the label component of its output. Must be named in the
#'   call because it follows \code{...}. See \emph{Cluster labels}.
#' @inheritParams Silhouette
#'
#' @details
#' The methods included depend on the available input matrices:
#'
#' **If \code{prox_matrix} is available:**
#' \itemize{
#'   \item \code{medoid} - Medoid-based silhouette using \code{\link{Silhouette}}
#'   \item \code{pac} - PAC-based silhouette using \code{\link{Silhouette}}
#' }
#'
#' **If \code{prob_matrix} is available:**
#' \itemize{
#'   \item \code{pp_pac} - Posterior probabilities silhouette with PAC method using \code{\link{softSilhouette}}
#'   \item \code{pp_medoid} - Posterior probabilities silhouette with Medoid method using \code{\link{softSilhouette}}
#'   \item \code{nlpp_pac} - Negative log posterior probabilities silhouette with PAC method using \code{\link{softSilhouette}}
#'   \item \code{nlpp_medoid} - Negative log posterior probabilities silhouette with Medoid method using \code{\link{softSilhouette}}
#'   \item \code{pd_pac} - Probability distribution silhouette with PAC method using \code{\link{softSilhouette}}
#'   \item \code{pd_medoid} - Probability distribution silhouette with Medoid method using \code{\link{softSilhouette}}
#'   \item \code{cer} - Certainty-based silhouette using \code{\link{cerSilhouette}}
#'   \item \code{db} - Density-based silhouette using \code{\link{dbSilhouette}}
#' }
#'
#' At least one of \code{prox_matrix} or \code{prob_matrix} must be provided. Each width is
#' computed once and summarized by all available averages; the fuzzy average needs
#' \code{prob_matrix}.
#'
#' If a width cannot be computed, its row is \code{NA} and a warning gives the reason.
#' Warnings from the individual functions are collected and each distinct warning is
#' reported once.
#'
#' @section Cluster labels:
#' Every row uses the same labels, taken in order of precedence from \code{cluster}, from the
#' largest membership in \code{prob_matrix}, or from the closest cluster in \code{prox_matrix}.
#' (In version 0.9.6 and earlier, the crisp and median distance-based rows used the closest
#' cluster in \code{prox_matrix} even when \code{prob_matrix} was supplied, and so could
#' describe a different partition from the other rows.)
#'
#' @section Relations between the rows:
#' For the same labels, the rows fall into the following groups.
#' \describe{
#'   \item{Monotone transformations of each other (same ranking of observations).}{
#'     Each \code{pac} width is \eqn{s/(2 - |s|)} of the corresponding \code{medoid} width
#'     \eqn{s}: \code{medoid}/\code{pac}, \code{pp_medoid}/\code{pp_pac},
#'     \code{nlpp_medoid}/\code{nlpp_pac} and \code{pd_medoid}/\code{pd_pac}. With the
#'     default labels, \code{pp_medoid}, \code{pp_pac} and \code{db} are all increasing
#'     functions of the ratio of the two largest memberships, so they too rank observations
#'     identically. The averages still differ, and may peak at different numbers of
#'     clusters.}
#'   \item{Equal in special cases.}{
#'     \code{pd} equals \code{pp} when all estimated cluster weights are equal. For fuzzy
#'     c-means with fuzzifier \eqn{m}, \code{pp_medoid} and \code{pp_pac} equal
#'     \code{medoid} and \code{pac} computed from the dissimilarities
#'     \eqn{d_{ik}^{1/(m-1)}}, where \eqn{d_{ik}} are squared Euclidean distances to the
#'     prototypes (for \eqn{m = 2}, the \code{d} component of \code{\link[ppclust]{fcm}}).}
#'   \item{Genuinely different.}{
#'     The distance-based rows (in general), \code{nlpp} (which also depends on the size of
#'     the neighboring membership), and \code{cer} (the largest membership alone, bounded
#'     below by \eqn{1/K}).}
#' }
#'
#' @return A data frame with one row per method and the following columns:
#' \describe{
#'   \item{Method}{Character vector of method names, as listed in Details.}
#'   \item{Crisp}{Crisp (unweighted mean) average silhouette width.}
#'   \item{Fuzzy}{Fuzzy (membership-weighted) average silhouette width; present only when \code{prob_matrix} is supplied.}
#'   \item{Median}{Median silhouette width.}
#' }
#'
#' @seealso \code{\link{Silhouette}}, \code{\link{softSilhouette}}, \code{\link{dbSilhouette}}, \code{\link{cerSilhouette}}
#'
#' @examples
#' if (requireNamespace("ppclust", quietly = TRUE)) {
#'   # Example with FCM clustering
#'   library(ppclust)
#'   data(iris)
#'   fcm_result <- fcm(iris[, -5], centers = 3)
#'
#'   # Using matrices directly
#'   summary_result <- calSilhouette(
#'     prox_matrix = fcm_result$d,
#'     prob_matrix = fcm_result$u,
#'     proximity_type = "dissimilarity",
#'     print.summary = TRUE
#'   )
#' }
#'
#' if (requireNamespace("ppclust", quietly = TRUE)) {
#'   # Using clustering function
#'   summary_result2 <- calSilhouette(
#'     prox_matrix = "d",
#'     prob_matrix = "u",
#'     proximity_type = "dissimilarity",
#'     clust_fun = ppclust::fcm,
#'     x = iris[, -5],
#'     centers = 3,
#'     print.summary = TRUE
#'   )
#' }
#' @export
calSilhouette <- function(prox_matrix = NULL,
                          proximity_type = c("dissimilarity", "similarity"),
                          prob_matrix = NULL,
                          a = 2,
                          print.summary = FALSE,
                          clust_fun = NULL, ...,
                          cluster = NULL) {

  if (is.null(prox_matrix) && is.null(prob_matrix)) {
    stop("At least one of 'prox_matrix' or 'prob_matrix' must be provided.")
  }
  proximity_type <- match.arg(proximity_type)

  prox_name <- "prox_matrix"
  prob_name <- "prob_matrix"
  if (!is.null(clust_fun)) {
    if (!is.null(prox_matrix)) .check_name(prox_matrix, "prox_matrix")
    if (!is.null(prob_matrix)) .check_name(prob_matrix, "prob_matrix")
    clust_out <- .run_clust_fun(clust_fun, ..., envir = parent.frame())
    if (!is.null(prox_matrix)) {
      prox_matrix <- .extract_component(clust_out, prox_matrix, "prox_matrix")
    }
    if (!is.null(prob_matrix)) {
      prob_matrix <- .extract_component(clust_out, prob_matrix, "prob_matrix")
    }
    if (is.character(cluster) && length(cluster) == 1) {
      cluster <- .extract_component(clust_out, cluster, "cluster")
    }
    prox_name <- "Extracted prox_matrix"
    prob_name <- "Extracted prob_matrix"
  }

  if (!is.null(prox_matrix)) .check_prox(prox_matrix, prox_name)
  if (!is.null(prob_matrix)) .check_prob(prob_matrix, prob_name)
  if (!is.null(prox_matrix) && !is.null(prob_matrix) &&
      !identical(dim(prob_matrix), dim(prox_matrix))) {
    stop("prob_matrix must have the same dimensions as prox_matrix.")
  }
  .check_a(a)

  # One partition for every row.
  ref <- if (is.null(prob_matrix)) prox_matrix else prob_matrix
  if (!is.null(cluster)) {
    cluster <- .check_cluster(cluster, nrow(ref), ncol(ref))
  } else if (!is.null(prob_matrix)) {
    cluster <- .closest(prob_matrix, larger_is_closer = TRUE)
  } else {
    cluster <- .closest(prox_matrix, larger_is_closer = proximity_type == "similarity")
  }

  averages <- if (is.null(prob_matrix)) c("crisp", "median") else c("crisp", "fuzzy", "median")
  collected <- character(0)

  # Compute one width, summarize it by every average, and collect its warnings.
  summarize <- function(name, compute) {
    out <- stats::setNames(rep(NA_real_, length(averages)), averages)
    widths <- withCallingHandlers(
      tryCatch(compute(), error = function(e) {
        collected <<- c(collected, paste0("Error computing silhouette (", name, "): ",
                                          conditionMessage(e)))
        NULL
      }),
      warning = function(w) {
        collected <<- c(collected, conditionMessage(w))
        invokeRestart("muffleWarning")
      })
    if (!is.Silhouette(widths)) return(out)
    for (avg in averages) {
      attr(widths, "average") <- avg
      out[[avg]] <- withCallingHandlers(
        .silhouette_summary(widths)$avg.width,
        warning = function(w) {
          collected <<- c(collected, paste0(name, ": ", conditionMessage(w)))
          invokeRestart("muffleWarning")
        })
    }
    out
  }

  fuzzy_or_crisp <- if (is.null(prob_matrix)) "crisp" else "fuzzy"
  results <- list()

  if (!is.null(prox_matrix)) {
    for (m in c("medoid", "pac")) {
      results[[m]] <- summarize(m, function() {
        Silhouette(prox_matrix, proximity_type = proximity_type, method = m,
                   average = fuzzy_or_crisp, prob_matrix = prob_matrix, a = a,
                   cluster = cluster)
      })
    }
  }

  if (!is.null(prob_matrix)) {
    for (pt in c("pp", "nlpp", "pd")) {
      for (m in c("pac", "medoid")) {
        name <- paste(pt, m, sep = "_")
        results[[name]] <- summarize(name, function() {
          softSilhouette(prob_matrix, prob_type = pt, method = m, average = "fuzzy",
                         a = a, cluster = cluster)
        })
      }
    }
    results[["cer"]] <- summarize("cer", function() {
      cerSilhouette(prob_matrix, average = "fuzzy", a = a, cluster = cluster)
    })
    results[["db"]] <- summarize("db", function() {
      dbSilhouette(prob_matrix, average = "fuzzy", a = a, cluster = cluster)
    })
  }

  values <- do.call(rbind, results)
  summary_df <- data.frame(Method = names(results), stringsAsFactors = FALSE)
  summary_df$Crisp <- unname(values[, "crisp"])
  if (!is.null(prob_matrix)) summary_df$Fuzzy <- unname(values[, "fuzzy"])
  summary_df$Median <- unname(values[, "median"])

  for (msg in unique(collected)) warning(msg, call. = FALSE)

  if (isTRUE(print.summary)) {
    table <- paste(utils::capture.output(print(summary_df, row.names = FALSE)), collapse = "\n")
    message("Summary of All Silhouette Methods\n", strrep("=", 42), "\n", table)
  }

  summary_df
}
