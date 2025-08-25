#' Check if Object is of Class Silhouette
#'
#' Tests whether an object is of class "Silhouette". This function checks both
#' the class inheritance and the expected structure of a Silhouette object.
#'
#' @param x An object to test
#' @param strict Logical; if TRUE, performs additional structural validation
#'   beyond just class checking (default: FALSE)
#'
#' @return Logical; TRUE if the object is of class "Silhouette", FALSE otherwise
#'
#' @details
#' When \code{strict = FALSE}, the function only checks if the object inherits
#' from the "Silhouette" class.
#'
#' When \code{strict = TRUE}, the function additionally validates:
#' \itemize{
#'   \item Object is a data frame
#'   \item Has required columns: cluster, neighbor, sil_width
#'   \item Has required attributes: proximity_type, method, average
#'   \item Column types are appropriate (integer for cluster/neighbor, numeric for sil_width)
#' }
#'
#' The Silhouette object attributes are validated as follows:
#' \itemize{
#'   \item \code{proximity_type}: Must be one of "dissimilarity" or "similarity"
#'   \item \code{average}: Must be one of "crisp", "fuzzy", or "median"
#'   \item \code{method}: Can be NULL or any string
#' }
#'
#' @seealso \code{\link{Silhouette}}, \code{\link{softSilhouette}}, \code{\link{dbSilhouette}}, \code{\link{cerSilhouette}}, \code{\link{getSilhouette}}, \code{\link{plotSilhouette}}
#'
#' @examples
#' # Create a Silhouette object
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
#'
#' # Test if object is Silhouette
#' is.Silhouette(sil_obj)          # TRUE
#' is.Silhouette(sil_obj, strict = TRUE)  # TRUE
#'
#' # Test with non-Silhouette objects
#' is.Silhouette(data.frame(a = 1, b = 2))  # FALSE
#' is.Silhouette(matrix(1:10, ncol = 2))    # FALSE
#' is.Silhouette(list(a = 1, b = 2))        # FALSE
#' is.Silhouette(NULL)                       # FALSE
#'
#' @export
is.Silhouette <- function(x, strict = FALSE) {

  # Basic class check
  if (!inherits(x, "Silhouette")) {
    return(FALSE)
  }

  # If not strict, just return TRUE for class inheritance
  if (!strict) {
    return(TRUE)
  }

  # Strict validation
  tryCatch({
    # Check if it's a data frame
    if (!is.data.frame(x)) {
      return(FALSE)
    }

    # Check required columns
    required_cols <- c("cluster", "neighbor", "sil_width")
    if (!all(required_cols %in% names(x))) {
      return(FALSE)
    }

    # Check required attributes
    required_attrs <- c("proximity_type", "method", "average")
    obj_attrs <- names(attributes(x))
    if (!all(required_attrs %in% obj_attrs)) {
      return(FALSE)
    }

    # Check column types
    if (!is.integer(x$cluster) && !is.numeric(x$cluster)) {
      return(FALSE)
    }
    if (!is.integer(x$neighbor) && !is.numeric(x$neighbor)) {
      return(FALSE)
    }
    if (!is.numeric(x$sil_width)) {
      return(FALSE)
    }

    # Check if weight column exists for fuzzy averaging
    average_attr <- attr(x, "average")
    if (!is.null(average_attr) && average_attr == "fuzzy") {
      if (!"weight" %in% names(x)) {
        return(FALSE)
      }
      if (!is.numeric(x$weight)) {
        return(FALSE)
      }
    }

    # Check attribute values
    proximity_type <- attr(x, "proximity_type")
    if (!is.null(proximity_type) && !proximity_type %in% c("similarity", "dissimilarity")) {
      return(FALSE)
    }

    if (!is.null(average_attr) && !average_attr %in% c("crisp", "fuzzy", "median")) {
      return(FALSE)
    }

    # Check value ranges
    if (any(x$sil_width < -1 | x$sil_width > 1, na.rm = TRUE)) {
      return(FALSE)
    }

    if ("weight" %in% names(x)) {
      if (any(x$weight < 0 | x$weight > 1, na.rm = TRUE)) {
        return(FALSE)
      }
    }

    # Check that cluster and neighbor are not equal
    if (any(x$cluster == x$neighbor, na.rm = TRUE)) {
      return(FALSE)
    }

    return(TRUE)

  }, error = function(e) {
    return(FALSE)
  })
}
