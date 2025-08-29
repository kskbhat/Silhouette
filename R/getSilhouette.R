#' Create Silhouette Object from User Components
#'
#' Constructs a Silhouette class object directly from user-provided components
#' without performing silhouette calculations. This function allows users to
#' build a Silhouette object when they already have the necessary components.
#'
#' @param cluster Numeric or integer vector of cluster assignments for each observation
#' @param neighbor Numeric or integer vector of nearest neighbor cluster assignments for each observation
#' @param sil_width Numeric vector of silhouette widths for each observation (must be between -1 and +1)
#' @param weight Numeric vector of weights for each observation (must be between 0 and 1, only used when average = "fuzzy")
#' @param proximity_type Character; the proximity type used. Options: "similarity" or "dissimilarity"
#' @param method Character; the silhouette calculation method used (default: NULL, can be any custom name)
#' @param average Character; the averaging method. Options: "crisp", "fuzzy", or "median"
#'
#' @return A data frame of class \code{"Silhouette"} containing cluster assignments, nearest neighbor clusters, silhouette widths for each observation, and weights (for fuzzy clustering). The object includes the following attributes:
#' \describe{
#'   \item{proximity_type}{The proximity type used (\code{"similarity"} or \code{"dissimilarity"}).}
#'   \item{method}{The silhouette calculation method used (\code{"medoid"} or \code{"pac"}).}
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
#' @seealso \code{\link{Silhouette}}, \code{\link{softSilhouette}}, \code{\link{dbSilhouette}}, \code{\link{cerSilhouette}}, \code{\link{is.Silhouette}}, \code{\link{plotSilhouette}}
#'
#' @examples
#' # Create a simple crisp Silhouette object (3 columns)
#' cluster_assignments <- c(1, 1, 2, 2, 3, 3)
#' neighbor_clusters <- c(2, 2, 1, 1, 1, 1)
#' silhouette_widths <- c(0.8, 0.7, 0.6, 0.9, 0.5, 0.4)
#'
#' sil_obj <- getSilhouette(
#'   cluster = cluster_assignments,
#'   neighbor = neighbor_clusters,
#'   sil_width = silhouette_widths,
#'   proximity_type = "dissimilarity",
#'   method = "medoid",
#'   average = "crisp"
#' )
#' sil_obj
#'
#' # Create a fuzzy Silhouette object with weights (4 columns)
#' weights <- c(0.9, 0.8, 0.7, 0.95, 0.6, 0.5)
#'
#' sil_fuzzy <- getSilhouette(
#'   cluster = cluster_assignments,
#'   neighbor = neighbor_clusters,
#'   sil_width = silhouette_widths,
#'   weight = weights,
#'   proximity_type = "similarity",
#'   method = "pac",
#'   average = "fuzzy"
#' )
#' sil_fuzzy
#'
#' # Custom method name
#' sil_custom <- getSilhouette(
#'   cluster = cluster_assignments,
#'   neighbor = neighbor_clusters,
#'   sil_width = silhouette_widths,
#'   proximity_type = "dissimilarity",
#'   method = "my_custom_method",
#'   average = "crisp"
#' )
#' sil_custom
#' @export
getSilhouette <- function(cluster,
                          neighbor,
                          sil_width,
                          weight = NULL,
                          proximity_type = c("dissimilarity", "similarity"),
                          method = NA,
                          average = c("crisp", "fuzzy", "median")) {

  # Validate arguments
  proximity_type <- match.arg(proximity_type)
  average <- match.arg(average)

  # Check input lengths
  n_obs <- length(cluster)
  if (length(neighbor) != n_obs) {
    stop("'neighbor' must have the same length as 'cluster'")
  }
  if (length(sil_width) != n_obs) {
    stop("'sil_width' must have the same length as 'cluster'")
  }

  # Validate numeric inputs
  if (!is.numeric(cluster) && !is.integer(cluster)) {
    stop("'cluster' must be numeric or integer" )
  }
  if (!is.numeric(neighbor) && !is.integer(neighbor)) {
    stop("'neighbor' must be numeric or integer")
  }
  if (!is.numeric(sil_width)) {
    stop("'sil_width' must be numeric")
  }

  # Check that cluster and neighbor are not equal
  if (any(cluster == neighbor)) {
    stop("'cluster' and 'neighbor' should not be equal for any observation")
  }

  # Check that sil_width is between -1 and +1
  if (any(sil_width < -1 | sil_width > 1)) {
    stop("'sil_width' values must be between -1 and +1")
  }

  # Handle weights and averaging method conversion
  include_weight <- FALSE
  if (average == "fuzzy") {
    if (is.null(weight)) {
      # Convert average to crisp and warn
      average <- "crisp"
      warning("'average' was set to 'fuzzy' but no 'weight' provided. Converting to 'crisp' averaging.")
      include_weight <- FALSE
    } else {
      if (length(weight) != n_obs) {
        stop("'weight' must have the same length as 'cluster'")
      }
      if (!is.numeric(weight)) {
        stop("'weight' must be numeric")
      }
      # Check that weight is between 0 and 1
      if (any(weight < 0 | weight > 1)) {
        stop("'weight' values must be between 0 and 1")
      }
      include_weight <- TRUE
    }
  } else {
    # For non-fuzzy averaging, convert to fuzzy if weight is provided
    if (!is.null(weight)) {
      if (length(weight) != n_obs) {
        stop("'weight' must have the same length as 'cluster'")
      }
      if (!is.numeric(weight)) {
        stop("'weight' must be numeric")
      }
      # Check that weight is between 0 and 1
      if (any(weight < 0 | weight > 1)) {
        stop("'weight' values must be between 0 and 1")
      }
      # Convert to fuzzy and warn
      average <- "fuzzy"
      warning("'weight' is provided but 'average' was not 'fuzzy'. Converting to 'fuzzy' averaging.")
      include_weight <- TRUE
    } else {
      include_weight <- FALSE
    }
  }

  # Create the data frame - include weight column only when needed
  if (include_weight) {
    widths <- data.frame(
      cluster = as.integer(cluster),
      neighbor = as.integer(neighbor),
      sil_width = as.numeric(sil_width),
      weight = as.numeric(weight)
    )
  } else {
    widths <- data.frame(
      cluster = as.integer(cluster),
      neighbor = as.integer(neighbor),
      sil_width = as.numeric(sil_width)
    )
  }

  # Create the Silhouette object with proper attributes
  structure(
    widths,
    class = c("Silhouette", "data.frame"),
    proximity_type = proximity_type,
    method = method,
    average = average
  )
}
