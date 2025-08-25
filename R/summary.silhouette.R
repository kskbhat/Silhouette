#' @rdname Silhouette
#' @method summary Silhouette
#' @export
#' @import methods
#' @param object An object of class \code{"Silhouette"}, typically the output of the \code{\link{Silhouette}}, \code{\link{softSilhouette}},  \code{\link{dbSilhouette}}, and  \code{\link{cerSilhouette}} function.
#' @param print.summary Logical; if \code{TRUE}, prints a summary table of average silhouette widths and sizes for each cluster. Defaults to \code{TRUE}.
#'
#' @return Further,  \code{summary} returns a list containing:
#' \itemize{
#'   \item \code{clus.avg.widths}: A named numeric vector of average silhouette widths per cluster.
#'   \item \code{avg.width}: The overall average silhouette width.
#'   \item \code{sil.sum}: A data frame with columns \code{cluster}, \code{size}, and \code{avg.sil.width} summarizing cluster sizes and average silhouette widths.
#' }
summary.Silhouette <- function(object, print.summary = TRUE, ...) {
  if (!is.Silhouette(object, strict = TRUE)) {
    stop("object must be of class 'Silhouette'.")
  }
  proximity_type <- attr(object, "proximity_type")
  method <- attr(object, "method")
  average <- attr(object, "average")
  widths <- object

  if (average == "crisp") {
    clus.avg.widths <- tapply(widths$sil_width, widths$cluster, mean, na.rm = TRUE)
    avg.width <- mean(widths$sil_width, na.rm = TRUE)
  } else if (average == "fuzzy") {
    sil_weight <- widths$weight * widths$sil_width
    clus.avg.widths <- tapply(seq_along(widths$sil_width), widths$cluster, function(idx) {
      sum(sil_weight[idx], na.rm = TRUE) / sum(widths$weight[idx], na.rm = TRUE)
    })
    avg.width <- sum(sil_weight, na.rm = TRUE) / sum(widths$weight, na.rm = TRUE)
  } else {
    clus.avg.widths <- tapply(widths$sil_width, widths$cluster, median, na.rm = TRUE)
    avg.width <- median(widths$sil_width, na.rm = TRUE)
  }
  n <- table(widths$cluster)
  sil.sum <- data.frame(cluster = names(clus.avg.widths), size = as.vector(n), avg.sil.width = round(clus.avg.widths, 4))

  header <- switch(average,
                   crisp = sprintf("Average crisp %s %s silhouette: %.4f", proximity_type, method, avg.width),
                   fuzzy = sprintf("Average fuzzy %s %s silhouette: %.4f", proximity_type, method, avg.width),
                   median = sprintf("Median %s %s silhouette: %.4f", proximity_type, method, avg.width))

  if (print.summary) {
    message(strrep("-", nchar(header)))
    message(header)
    message(strrep("-", nchar(header)))
    cat("\n")
    print(sil.sum)
    cat("\n")
  }

  invisible(list(clus.avg.widths = clus.avg.widths, avg.width = avg.width, sil.sum = sil.sum))
}

