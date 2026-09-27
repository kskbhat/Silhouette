#' @rdname Silhouette
#' @method summary Silhouette
#' @export
#' @param object An object of class \code{"Silhouette"}, typically the output of the \code{\link{Silhouette}}, \code{\link{softSilhouette}},  \code{\link{dbSilhouette}}, and  \code{\link{cerSilhouette}} function.
#'
#' @return Further, \code{summary} returns (invisibly) a list containing:
#' \itemize{
#'   \item \code{clus.avg.widths}: A named numeric vector of average silhouette widths per cluster.
#'   \item \code{avg.width}: The overall average silhouette width.
#'   \item \code{sil.sum}: A data frame with columns \code{cluster}, \code{size}, and \code{avg.sil.width} summarizing cluster sizes and average silhouette widths.
#' }
summary.Silhouette <- function(object, print.summary = TRUE, ...) {
  if (!is.Silhouette(object, strict = TRUE)) {
    stop("object is not a valid 'Silhouette' object; see is.Silhouette(object, strict = TRUE).")
  }
  stats <- .silhouette_summary(object)
  if (isTRUE(print.summary)) {
    .report_silhouette(object, stats)
  }
  invisible(stats)
}
