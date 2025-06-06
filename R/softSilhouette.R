#' Calculate Silhouette Width for Soft Clustering Algorithms
#'
#' Computes the silhouette width for each observation based on soft clustering results, using cluster membership probabilities. The silhouette width measures how similar an observation is to its own cluster compared to other clusters, ranging from -1 to 1, where higher values indicate better cluster cohesion and separation.
#'
#' @param prob_matrix A numeric matrix where rows represent observations and columns represent cluster membership probabilities (or transformed probabilities, depending on \code{prob_type}). If \code{clust_fun} is provided, \code{prob_matrix} should be the name of the matrix component as a string (e.g., \code{"z"} for \code{Mclust}).
#' @param prob_type Character string specifying the type of membership matrix in \code{prob_matrix}. Options are:
#' \describe{
#'   \item{\code{"pp"}}{Posterior probabilities (non-negative, typically summing to 1 per row).}
#'   \item{\code{"nlpp"}}{Negative log of posterior probabilities (non-positive, treated as dissimilarities).}
#'   \item{\code{"pd"}}{Probability distribution (normalized probabilities relative to cluster totals).}
#' }
#' Defaults to \code{"pp"}.
#' @param method Character string specifying the silhouette calculation method. Options are \code{"pac"} (Probability of Alternative Cluster) or \code{"medoid"}. Defaults to \code{"medoid"}.
#' @param average Character string specifying the type of average silhouette width calculation. Options are \code{"crisp"} (simple average) or \code{"fuzzy"} (weighted average based on membership differences). Defaults to \code{"crisp"}.
#' @param a Numeric value controlling the weight calculation based on membership difference. Must be positive. Defaults to \code{2}.
#' @param clust_fun Optional function object specifying a clustering function that produces the probability matrix, such as \code{Mclust} or \code{fanny}. If provided, \code{prob_matrix} must be the name of the matrix component in the clustering output. Defaults to \code{NULL}.
#' @param ... Additional arguments passed to \code{clust_fun}, such as \code{G} for \code{Mclust} or \code{k} for \code{fanny}.
#'
#' @return A list of class \code{"Silhouette"} with the following components:
#' \describe{
#'   \item{widths}{A data frame containing cluster assignments, nearest neighbor clusters, weights (for fuzzy average), and silhouette widths for each observation.}
#'   \item{clus.avg.widths}{A named numeric vector of average silhouette widths per cluster.}
#'   \item{avg.width}{A numeric value representing the overall average silhouette width across all observations.}
#' }
#'
#' @seealso \code{\link{Silhouette}}, \code{\link{plot.Silhouette}}
#'
#' @references
#' Rousseeuw, P. J. (1987). Silhouettes: A graphical aid to the interpretation and validation of cluster analysis. \emph{Journal of Computational and Applied Mathematics}, 20, 53--65. \href{https://doi.org/10.1016/0377-0427(87)90125-7}{DOI:10.1016/0377-0427(87)90125-7}
#'
#' Van der Laan, M., Pollard, K., & Bryan, J. (2003). A new partitioning around medoids algorithm. \emph{Journal of Statistical Computation and Simulation}, 73(8), 575--584. \href{https://doi.org/10.1080/0094965031000136012}{DOI:10.1080/0094965031000136012}
#'
#' Raymaekers, J., & Rousseeuw, P. J. (2022). Silhouettes and quasi residual plots for neural nets and tree-based classifiers. \emph{Journal of Computational and Graphical Statistics}, 31(4), 1332--1343. \href{https://doi.org/10.1080/10618600.2022.2050249}{DOI:10.1080/10618600.2022.2050249}
#'
#' Campello, R. J., & Hruschka, E. R. (2006). A fuzzy extension of the silhouette width criterion for cluster analysis. \emph{Fuzzy Sets and Systems}, 157(21), 2858--2875. \href{https://doi.org/10.1016/j.fss.2006.07.006}{DOI:10.1016/j.fss.2006.07.006}
#'
#' Schepers, J., Ceulemans, E., & Van Mechelen, I. (2008). Selecting among multi-mode partitioning models of different complexities: A comparison of four model selection criteria. \emph{Journal of Classification}, 25(1), 67--85. \href{https://doi.org/10.1007/s00357-008-9005-9}{DOI:10.1007/s00357-008-9005-9}
#'
#' @examples
#' # Example with synthetic probability matrix
#' set.seed(123)
#' prob_matrix <- matrix(runif(150 * 3), nrow = 150, ncol = 3)
#' prob_matrix <- prob_matrix / rowSums(prob_matrix) # Normalize to sum to 1
#' out <- softSilhouette(prob_matrix = prob_matrix, prob_type = "pp")
#' plot(out)
#'
#' # Example with ppclust (if available)
#' if (requireNamespace("ppclust", quietly = TRUE)) {
#'   data(iris)
#'   fcm_result <- ppclust::fcm(iris[, 1:4], 3)
#'   out <- softSilhouette(prob_matrix = fcm_result$u, prob_type = "pp")
#'   plot(out)
#' } else {
#'   message("Install 'ppclust' to run this example: install.packages('ppclust')")
#' }
#' @export
softSilhouette <- function(prob_matrix,
                           prob_type = c("pp", "nlpp", "pd"),
                           method = c("medoid", "pac"),
                           average = c("crisp", "fuzzy"),
                           a = 2,
                           clust_fun = NULL, ...) {
  # Validate prob_matrix and clust_fun
  if (is.null(clust_fun)) {
    if (!is.matrix(prob_matrix) || !is.numeric(prob_matrix)) {
      stop("When clust_fun is NULL, prob_matrix must be a numeric matrix.")
    }
    if (ncol(prob_matrix) < 2) {
      stop("prob_matrix must have at least two columns (clusters).")
    }
  } else {  # clust_fun is not NULL
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
        getMethod(clust_fun, "ANY")  # Adjust signature if needed
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
    a = a
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
