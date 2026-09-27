#' Calculate Extended Silhouette Width for Multi-Way Clustering
#'
#' @description
#' `r lifecycle::badge('experimental')`
#'
#' Computes an extended silhouette width for multi-way clustering (e.g., biclustering, triclustering, or n-mode tensor clustering) by combining silhouette widths from a list of Silhouette objects, each representing one mode of clustering. The extended silhouette width is a weighted average of the average silhouette widths of the modes. By default each mode is weighted by its number of objects; equal or user-specified weights are also available. The output is an object of class \code{extSilhouette}.
#'
#' @param sil_list A list of objects of class \code{"Silhouette"}, typically the output of \code{\link{Silhouette}} or \code{\link{softSilhouette}}, where each object represents the silhouette analysis for one mode of multi-way clustering (e.g., rows, columns, or other dimensions in biclustering or tensor clustering).
#' @param dim_names An optional character vector of dimension names (e.g., \code{c("Rows", "Columns")}). If \code{NULL}, defaults to \code{"Mode 1"}, \code{"Mode 2"}, etc.
#' @param print.summary Logical; if \code{TRUE}, reports the extended silhouette width and the dimension table through \code{\link{message}}. Default is \code{FALSE}.
#' @param weights How the modes are weighted: \code{"size"} (the default) weights each mode
#'   by its number of objects, \code{"equal"} gives every mode the same weight, and a
#'   non-negative numeric vector with one entry per mode gives user-specified weights (they
#'   are rescaled to sum to 1).
#'
#' @return A list of class \code{"extSilhouette"} with the following components:
#' \describe{
#'   \item{ext_sil_width}{A numeric scalar, the extended silhouette width.}
#'   \item{dim_table}{A data frame with columns \code{dimension} (e.g., "Mode 1", "Mode 2"), \code{n_obs} (number of objects), \code{weight} (the share of the total weight given to the mode) and \code{avg_sil_width} (average silhouette width of the mode).}
#'   \item{weights}{The weighting used: \code{"size"}, \code{"equal"} or \code{"user"}.}
#' }
#' Values are not rounded; the print method shows four decimals.
#'
#' @details
#' The extended silhouette width is computed as:
#' \deqn{ ExS = \frac{ \sum_{i} \omega_i \bar{S}_i }{ \sum_{i} \omega_i } }
#' where \eqn{\bar{S}_i} is the average silhouette width of mode \eqn{i} (from
#' \code{summary(x)$avg.width}, so it follows the averaging rule of that object) and
#' \eqn{\omega_i} is its weight. With \code{weights = "size"}, \eqn{\omega_i = n_i}, the
#' number of objects in mode \eqn{i} (\code{nrow(x)}), as in Schepers et al. (2008); ExS is
#' then the average width over all objects of all modes, each object counting once. With
#' \code{weights = "equal"}, \eqn{\omega_i = 1}, so ExS is the mean of the mode-wise
#' averages.
#'
#' Size weighting treats every clustered object alike, but when the modes differ greatly in
#' size the largest mode dominates: with 200 rows and 30 columns, the rows receive about 87\%
#' of the weight. Equal weighting gives every mode the same say regardless of its size.
#' Because the choice can change which model is selected, report the mode-wise averages in
#' \code{dim_table} together with ExS.
#'
#' Each \code{Silhouette} object in \code{sil_list} must contain at least one observation and have a finite average width. For consistency, compute all \code{Silhouette} objects with the same function, \code{method} and averaging rule.
#'
#' @seealso \code{\link{Silhouette}}, \code{\link{softSilhouette}}, \code{\link{dbSilhouette}}, \code{\link{cerSilhouette}}, \code{\link{getSilhouette}}, \code{\link{is.Silhouette}}
#'
#' @references
#' Schepers, J., Ceulemans, E., & Van Mechelen, I. (2008). Selecting among multi-mode partitioning models of different complexities: A comparison of four model selection criteria. \emph{Journal of Classification}, 25(1), 67--85. \doi{10.1007/s00357-008-9005-9}
#'
#' Bhat Kapu, S., & Kiruthika, C. (2025). Block Probabilistic Distance Clustering: A Unified Framework and Evaluation. PREPRINT (Version 1) available at Research Square. \doi{10.21203/rs.3.rs-6973596/v1}
#'
#' @examples
#' # Two modes: 150 objects in 3 clusters and 4 variables in 2 clusters
#' set.seed(123)
#' prob_mode1 <- matrix(runif(150 * 3), nrow = 150, ncol = 3)
#' prob_mode1 <- prob_mode1 / rowSums(prob_mode1)
#' prob_mode2 <- matrix(runif(4 * 2), nrow = 4, ncol = 2)
#' prob_mode2 <- prob_mode2 / rowSums(prob_mode2)
#'
#' sil_mode1 <- softSilhouette(prob_matrix = prob_mode1, method = "pac")
#' sil_mode2 <- softSilhouette(prob_matrix = prob_mode2, method = "pac")
#'
#' # Size-weighted (default) and equally weighted extended silhouette
#' extSilhouette(list(sil_mode1, sil_mode2), dim_names = c("Rows", "Columns"))
#' extSilhouette(list(sil_mode1, sil_mode2), dim_names = c("Rows", "Columns"),
#'               weights = "equal")
#' @export
extSilhouette <- function(sil_list, dim_names = NULL, print.summary = FALSE,
                          weights = c("size", "equal")) {
  if (!is.list(sil_list) || length(sil_list) == 0) {
    stop("sil_list must be a non-empty list of 'Silhouette' objects.")
  }
  if (!all(vapply(sil_list, inherits, logical(1), "Silhouette"))) {
    stop("All elements in sil_list must be of class 'Silhouette'.")
  }

  n_rows <- vapply(sil_list, nrow, integer(1))
  if (any(n_rows == 0)) {
    stop("Every Silhouette object in sil_list must contain at least one observation.")
  }
  kinds <- vapply(sil_list, function(x) paste(attr(x, "proximity_type"), attr(x, "method"),
                                              attr(x, "average")), character(1))
  if (length(unique(kinds)) > 1) {
    warning("The Silhouette objects in sil_list differ in proximity type, method or averaging (",
            paste(unique(kinds), collapse = "; "), "), so their averages are on different scales.",
            call. = FALSE)
  }
  avg_widths <- vapply(sil_list, function(x) summary(x, print.summary = FALSE)$avg.width, numeric(1))
  if (any(!is.finite(avg_widths))) {
    stop("One or more Silhouette objects have non-finite average silhouette widths (NA, NaN, or Inf).")
  }

  if (is.numeric(weights)) {
    if (length(weights) != length(sil_list) || anyNA(weights) ||
        any(!is.finite(weights)) || any(weights < 0) || sum(weights) == 0) {
      stop("Numeric weights must be finite, non-negative, not all zero, and have one entry per element of sil_list.")
    }
    omega <- weights
    weight_type <- "user"
  } else {
    weight_type <- match.arg(weights)
    omega <- if (weight_type == "size") n_rows else rep(1, length(sil_list))
  }
  share <- omega / sum(omega)
  ext_sil_width <- sum(share * avg_widths)

  if (is.null(dim_names)) {
    dim_names <- paste0("Mode ", seq_along(sil_list))
  } else if (length(dim_names) != length(sil_list)) {
    stop("Length of dim_names must match the length of sil_list.")
  }
  dim_table <- data.frame(
    dimension = as.character(dim_names),
    n_obs = n_rows,
    weight = share,
    avg_sil_width = avg_widths,
    stringsAsFactors = FALSE
  )

  result <- structure(list(ext_sil_width = ext_sil_width,
                           dim_table = dim_table,
                           weights = weight_type),
                      class = "extSilhouette")

  if (isTRUE(print.summary)) {
    message(paste(format(result), collapse = "\n"))
  }
  result
}

#' @rdname extSilhouette
#' @param x An object of class \code{"extSilhouette"}.
#' @param digits Number of decimal places shown.
#' @param ... Further arguments, currently ignored.
#' @export
format.extSilhouette <- function(x, digits = 4, ...) {
  header <- sprintf("Extended silhouette (%s weights): %.*f",
                    x$weights, digits, x$ext_sil_width)
  tab <- x$dim_table
  tab$weight <- round(tab$weight, digits)
  tab$avg_sil_width <- round(tab$avg_sil_width, digits)
  c(strrep("-", nchar(header)), header, strrep("-", nchar(header)),
    "Dimension summary:",
    utils::capture.output(print(tab, row.names = FALSE)))
}

#' @rdname extSilhouette
#' @export
print.extSilhouette <- function(x, digits = 4, ...) {
  writeLines(format(x, digits = digits))
  invisible(x)
}
