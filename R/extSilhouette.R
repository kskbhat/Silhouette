#' Calculate Extended Silhouette Width for Multi-Way Clustering
#'
#' Computes an extended silhouette width for multi-way clustering (e.g., biclustering, triclustering, or n-mode tensor clustering) by combining silhouette widths from a list of Silhouette objects, each representing one mode of clustering. The extended silhouette width is the weighted average of the average silhouette widths from each mode, weighted by the number of observations in each mode's silhouette analysis. The output is an object of class \code{extSilhouette}.
#'
#' @param sil_list A list of objects of class \code{"Silhouette"}, typically the output of \code{\link{Silhouette}} or \code{\link{softSilhouette}}, where each object represents the silhouette analysis for one mode of multi-way clustering (e.g., rows, columns, or other dimensions in biclustering or tensor clustering).
#' @param dim_names An optional character vector of dimension names (e.g., \code{c("Rows", "Columns")}). If \code{NULL}, defaults to \code{"Mode 1"}, \code{"Mode 2"}, etc.
#' @param print.summary Logical; if \code{TRUE}, prints a summary of the extended silhouette width and dimension table. Default is \code{TRUE}.
#'
#' @return A list of class \code{"extSilhouette"} with the following components:
#' \describe{
#'   \item{ext_sil_width}{A numeric scalar representing the extended silhouette width.}
#'   \item{dim_table}{A data frame with columns \code{dimension} (e.g., "Mode 1", "Mode 2"), \code{n_obs} (number of observations), and \code{avg_sil_width} (average silhouette width for each mode).}
#' }
#'
#' @details
#' The extended silhouette width is computed as:
#' \deqn{ ExS = \frac{ \sum (n_i \cdot w_i) }{ \sum n_i } }
#' where \eqn{n_i} is the number of observations in mode \eqn{i} (derived from \code{nrow(x$widths)}), and \eqn{w_i} is the average silhouette width for that mode (from \code{x$avg.width}).
#' Each \code{Silhouette} object in \code{sil_list} must contain a non-empty \code{widths} data frame and a numeric \code{avg.width} value. Modes with zero observations (\eqn{n_i = 0}) are not allowed, as they would result in an undefined weighted average.
#'
#' @seealso \code{\link{Silhouette}}, \code{\link{softSilhouette}}
#'
#' @references
#' Schepers, J., Ceulemans, E., & Van Mechelen, I. (2008). Selecting among multi-mode partitioning models of different complexities: A comparison of four model selection criteria. \emph{Journal of Classification}, 25(1), 67--85. \doi{10.1007/s00357-008-9005-9}
#'
#' @examples
#' # Example using iris dataset with two modes
#' data(iris)
#' \donttest{
#' if (requireNamespace("blockcluster", quietly = TRUE)) {
#'   library(blockcluster)
#'   result <- coclusterContinuous(
#'     as.matrix(iris[, -5]),
#'     nbcocluster = c(3, 2)
#'   )
#'   sil_mode1 <- softSilhouette(
#'     prob_matrix = result@rowposteriorprob,
#'     method = "pac",
#'     print.summary = FALSE
#'   )
#'   sil_mode2 <- softSilhouette(
#'     prob_matrix = result@colposteriorprob,
#'     method = "pac",
#'     print.summary = FALSE
#'   )
#'
#'   # Extended silhouette
#'   ext_sil <- extSilhouette(list(sil_mode1, sil_mode2))
#' } else {
#'   message("Install 'blockcluster': install.packages('blockcluster')")
#' }
#' }
#'
#' @export
extSilhouette <- function(sil_list, dim_names = NULL, print.summary = TRUE) {
  if (!all(sapply(sil_list, inherits, "Silhouette"))) {
    stop("All elements in sil_list must be of class 'Silhouette'.")
  }

  # Extract number of rows and avg.width from each object
  n_rows <- sapply(sil_list, function(x) nrow(x))
  avg_widths <- sapply(sil_list, function(x) summary(x,print.summary = FALSE)$avg.width)

  # Check for valid silhouette widths
  if (any(!is.finite(avg_widths))) {
    stop("One or more Silhouette objects have non-finite average silhouette widths (NA, NaN, or Inf).")
  }

  # Check for zero observations
  if (sum(n_rows) == 0) {
    stop("No observations found in any Silhouette object.")
  }

  # Calculate extended silhouette width
  ext_sil_width <- sum(n_rows * avg_widths) / sum(n_rows)

  # Create dimension table
  if (is.null(dim_names)) {
    dim_names <- paste0("Mode ", seq_along(sil_list))
  } else if (length(dim_names) != length(sil_list)) {
    stop("Length of dim_names must match the length of sil_list.")
  }
  dim_table <- data.frame(
    dimension = dim_names,
    n_obs = n_rows,
    avg_sil_width = round(avg_widths, 4)
  )

  # Prepare result
  result <- list(
    ext_sil_width = round(ext_sil_width, 4),
    dim_table = dim_table
  )

  # Print summary
  if (print.summary) {
    cat("---------------------------\n")
    cat("Extended silhouette:", round(ext_sil_width, 4), "\n")
    cat("---------------------------\n")
    cat("\nDimension Summary:\n")
    print(dim_table)
    cat("\nAvailable components:\n")
    print(names(result))
  }

  # Assign class
  structure(result, class = "extSilhouette")
}
