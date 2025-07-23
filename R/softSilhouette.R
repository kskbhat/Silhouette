#' Calculate Silhouette Width for Soft Clustering Algorithms
#'
#' Computes silhouette widths for soft clustering results by interpreting cluster membership probabilities (or their transformations) as proximity measures. Although originally designed for evaluating clustering quality within a method, this adaptation allows heuristic comparison across soft clustering algorithms using average silhouette widths.
#'
#' @param prob_matrix A numeric matrix where rows represent observations and columns represent cluster membership probabilities (or transformed probabilities, depending on \code{prob_type}). If \code{clust_fun} is provided, \code{prob_matrix} should be the name of the matrix component as a string (e.g., \code{"u"} for \code{\link[ppclust]{fcm}}).
#' @param prob_type Character string specifying the type transformation of membership matrix considered as proximity matrix in \code{prob_matrix}. Options are:
#' \describe{
#'   \item{\code{"pp"}}{Posterior probabilities \eqn{[\gamma_{ik}]_{n \times K}} (non-negative, typically summing to 1 per row), treated as similarities}
#'   \item{\code{"nlpp"}}{Negative log of posterior probabilities \eqn{[-\ln\gamma_{ik}]_{n \times K}} (non-positive), treated as dissimilarities.}
#'   \item{\code{"pd"}}{Probability distribution \eqn{[\gamma_{ik}/\pi_{k}]_{n \times K}} (normalized posterior probabilities relative to cluster proportions \eqn{\pi_{k}}), treated as similarities.}
#' }
#' Defaults to \code{"pp"}.
#' @param method Character string specifying the silhouette calculation method. Options are \code{"pac"} (Probability of Alternative Cluster) or \code{"medoid"}. Defaults to \code{"pac"}.
#' @param average Character string specifying the type of average silhouette width calculation. Options are \code{"crisp"} (simple average) or \code{"fuzzy"} (weighted average based on membership differences). Defaults to \code{"crisp"}.
#' @param a Numeric value controlling the fuzzifier or weight scaling in fuzzy silhouette averaging. Higher values increase the emphasis on strong membership differences. Must be positive. Defaults to \code{2}.
#' @param print.summary Logical; if \code{TRUE}, prints a summary table of average silhouette widths and sizes for each cluster. Defaults to \code{TRUE}.
#' @param clust_fun Optional S3 or S4 function object or function as character string specifying a clustering function that produces the proximity measure matrix. For example, \code{\link[ppclust]{fcm}} or \code{"fcm"}. If provided, \code{prox_matrix} must be the name of the matrix component in the clustering output (e.g., \code{"d"} for \code{\link[ppclust]{fcm}} when \code{proximity_type = "dissimilarity"}). Defaults to \code{NULL}.
#' @param ... Additional arguments passed to \code{clust_fun}, such as \code{x,centers} for \code{\link[ppclust]{fcm}}.
#'
#' @details
#' Although the silhouette method was originally developed for evaluating clustering structure within a single result, this implementation allows leveraging cluster membership probabilities from soft clustering methods to construct proximity-based silhouettes. These silhouette widths can be compared heuristically across different algorithms to assess clustering quality.
#'
#' See \doi{10.1080/23737484.2024.2408534} for more details.
#'
#' @return A data frame of class \code{"Silhouette"} containing cluster assignments, nearest neighbor clusters, silhouette widths for each observation, and weights (for fuzzy clustering). The object includes the following attributes:
#' \describe{
#'   \item{proximity_type}{The proximity type used (\code{"similarity"} or \code{"dissimilarity"}).}
#'   \item{method}{The silhouette calculation method used (\code{"medoid"} or \code{"pac"}).}
#' }
#'
#' @seealso \code{\link{Silhouette}},\code{\link{plotSilhouette}}
#'
#' @references
#'
#' Raymaekers, J., & Rousseeuw, P. J. (2022). Silhouettes and quasi residual plots for neural nets and tree-based classifiers. \emph{Journal of Computational and Graphical Statistics}, 31(4), 1332--1343. \doi{10.1080/10618600.2022.2050249}
#'
#' Bhat Kapu, S., & Kiruthika. (2024). Some density-based silhouette diagnostics for soft clustering algorithms. Communications in Statistics: Case Studies, Data Analysis and Applications, 10(3-4), 221-238. \doi{10.1080/23737484.2024.2408534}
#'
#' @examples
#' \donttest{
#' # Compare two soft clustering algorithms using softSilhouett
#' # Example: FCM vs. FCM2 on iris data, using average silhouette width as a criterion
#' data(iris)
#' # install.packages("ppclust")
#' if (requireNamespace("ppclust", quietly = TRUE)) {
#'   fcm_result <- ppclust::fcm(iris[, 1:4], 3)
#'   out_fcm <- softSilhouette(prob_matrix = fcm_result$u)
#'   plot(out_fcm)
#'   sfcm <- summary(out_fcm, print.summary = FALSE)
#' } else {
#'   message("Install 'ppclust' to run this example: install.packages('ppclust')")
#' }
#' if (requireNamespace("ppclust", quietly = TRUE)) {
#'   fcm2_result <- ppclust::fcm2(iris[, 1:4], 3)
#'   out_fcm2 <- softSilhouette(prob_matrix = fcm2_result$u)
#'   plot(out_fcm2)
#'   sfcm2 <- summary(out_fcm2, print.summary = FALSE)
#' } else {
#'   message("Install 'ppclust' to run this example: install.packages('ppclust')")
#' }
#' # Compare average silhouette widths of fcm and fcm2
#' if (requireNamespace("ppclust", quietly = TRUE)) {
#'   cat("FCM average silhouette width:", sfcm$avg.width, "\n")
#'   cat("FCM2 average silhouette width:", sfcm2$avg.width, "\n")
#' }
#' }
#' @export
softSilhouette <- function(prob_matrix,
                           prob_type = c("pp", "nlpp", "pd"),
                           method = c("pac", "medoid"),
                           average = c("crisp", "fuzzy"),
                           a = 2,
                           print.summary = TRUE,
                           clust_fun = NULL, ...) {
  # Validate prob_matrix and clust_fun
  if (is.null(clust_fun)) {
    if (!is.matrix(prob_matrix) || !is.numeric(prob_matrix)) {
      stop("When clust_fun is NULL, prob_matrix must be a numeric matrix.")
    }
    if (ncol(prob_matrix) < 2) {
      stop("prob_matrix must have at least two columns (clusters).")
    }
  } else { # clust_fun is not NULL
    if (!is.character(prob_matrix) || length(prob_matrix) != 1) {
      stop("When clust_fun is not NULL, prob_matrix must be a single string naming a matrix component.")
    }
    if (!(is.function(clust_fun) || is.character(clust_fun))) {
      stop("clust_fun must be a function object or a string naming a function (e.g., 'kmeans').")
    }


    # Get the clustering function (S3 or S4)
    if (is.character(clust_fun)) {
      clust_fun <- if (exists(clust_fun, mode = "function")) {
        get(clust_fun, mode = "function")
      } else if (isGeneric(clust_fun)) {
        getMethod(clust_fun, "ANY") # Adjust signature if needed
      } else {
        stop("Function '", clust_fun, "' not found")
      }
    }

    # Call the clustering function
    clust_out <- tryCatch(
      clust_fun(...),
      error = function(e) stop("Error in clustering function: ", e$message)
    )

    # Extract the matrix (S3 or S4)
    prob_matrix <- if (isS4(clust_out)) {
      if (prob_matrix %in% slotNames(clust_out)) {
        slot(clust_out, prob_matrix)
      } else {
        stop("Slot '", prob_matrix, "' not found in clustering output.")
      }
    } else {
      if (prob_matrix %in% names(clust_out)) {
        clust_out[[prob_matrix]]
      } else {
        stop("Component '", prob_matrix, "' not found in clustering output.")
      }
    }
  }

  # Ensure prob_matrix is a numeric matrix after extraction
  if (!is.matrix(prob_matrix) || !is.numeric(prob_matrix)) {
    stop("Extracted prob_matrix must be a numeric matrix.")
  }
  # Check row sums of prob_matrix
  row_sums <- rowSums(prob_matrix)
  if (any(abs(row_sums - 1) > 1e-6)) {
    stop("Each row of prob_matrix must sum to 1.")
  }
  if (ncol(prob_matrix) < 2) {
    stop("Extracted prob_matrix must have at least two columns (clusters).")
  }

  # Validate a
  if (!is.numeric(a) || a <= 0) {
    stop("a must be a positive numeric value.")
  }

  # Ensure valid argument choices
  prob_type <- match.arg(prob_type)
  method <- match.arg(method)
  average <- match.arg(average)

  # Transform probability matrix based on prob_type
  if (prob_type == "pp") {
    proximity_type <- "similarity"
    prox_matrix <- prob_matrix
  } else if (prob_type == "nlpp") {
    proximity_type <- "dissimilarity"
    prox_matrix <- -log(prob_matrix)
    if (any(is.infinite(prox_matrix), na.rm = TRUE)) {
      stop("Negative log of prob_matrix produced infinite values.")
    }
  } else if (prob_type == "pd") {
    proximity_type <- "similarity"
    pm_den <- matrix(colSums(prob_matrix), nrow = nrow(prob_matrix), ncol = ncol(prob_matrix), byrow = TRUE)
    if (any(pm_den == 0, na.rm = TRUE)) {
      stop("Column sums in prob_matrix must be non-zero for prob_type = 'pd'.")
    }
    prox_matrix <- prob_matrix / pm_den
  }

  # Prepare arguments for Silhouette function
  sil_args <- list(
    prox_matrix = prox_matrix,
    proximity_type = proximity_type,
    method = method,
    a = a,
    print.summary = print.summary
  )

  # Set prob_matrix for fuzzy average
  if (average == "fuzzy") {
    sil_args$prob_matrix <- prob_matrix
  } else {
    sil_args$prob_matrix <- NULL
  }

  # Call Silhouette function
  result <- do.call(Silhouette, sil_args)
  return(result)
}
