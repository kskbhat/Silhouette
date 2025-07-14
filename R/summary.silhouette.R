#' @rdname Silhouette
#' @method summary Silhouette
#' @export
#' @import methods
#' @param object An object of class \code{"Silhouette"}, typically the output of the \code{\link{Silhouette}} and \code{\link{softSilhouette}} function.
#' @param print.summary Logical; if \code{TRUE}, prints a summary table of average silhouette widths and sizes for each cluster. Defaults to \code{TRUE}.
#'
#' @return Further,  \code{summary} returns a list containing:
#' \itemize{
#'   \item \code{clus.avg.widths}: A named numeric vector of average silhouette widths per cluster.
#'   \item \code{avg.width}: The overall average silhouette width.
#'   \item \code{sil.sum}: A data frame with columns \code{cluster}, \code{size}, and \code{avg.sil.width} summarizing cluster sizes and average silhouette widths.
#' }
#'
summary.Silhouette <- function(object,print.summary = TRUE,...) {
  x = object
  # Validate input
  if (!inherits(x, "Silhouette")) {
    stop("x must be of class 'Silhouette'.")
  }

  proximity_type = attr(x, "proximity_type")
  method = attr(x, "method")

  # Compute average silhouette widths
  if (is.null(x$weight)) {
    clus.avg.widths <- tapply(x$sil_width, x$cluster, mean, na.rm = TRUE)
    avg.width <- mean(x$sil_width, na.rm = TRUE)
  } else {
    sil_weight <- x$weight * x$sil_width
    clus.avg.widths <- tapply(seq_along(x$sil_width), x$cluster, function(idx) {
      sum(sil_weight[idx], na.rm = TRUE) / sum(x$weight[idx], na.rm = TRUE)
    })
    avg.width <- sum(sil_weight, na.rm = TRUE) / sum(x$weight, na.rm = TRUE)
  }

  # Prepare summary output
  name.proximity_type <- paste(noquote(proximity_type))
  name.method <- paste(noquote(method))
  dash1p <- "--------------------------------------------"
  dash2p <- "--------------------------------------------------"
  dashs1p <- "-----------------------------------------"
  dashs2p <- "-----------------------------------------------"
  dash1 <- "-----------------------------------------------"
  dash2 <- "-----------------------------------------------------"
  dashs1 <- "--------------------------------------------"
  dashs2 <- "--------------------------------------------------"
  dash <- if (method == "pac") {
    ifelse(is.null(x$weight), ifelse(proximity_type == "dissimilarity", dash1p, dashs1p), ifelse(proximity_type == "dissimilarity", dash2p, dashs2p))
  } else {
    ifelse(is.null(x$weight), ifelse(proximity_type == "dissimilarity", dash1, dashs1), ifelse(proximity_type == "dissimilarity", dash2, dashs2))
  }
  name.ave <- paste(noquote(round(avg.width, 4)))


  if (!is.numeric(clus.avg.widths) || is.null(names(clus.avg.widths))) {
    warning("clus.avg.widths is not a named numeric vector; summary may be incorrect.")
  }
  n <- table(x$cluster)
  sil.sum <- data.frame(
    cluster = names(clus.avg.widths),
    size = as.vector(n),
    avg.sil.width = round(clus.avg.widths, 4)
  )
  if (print.summary) {
    cat(dash)
    if (is.null(x$weight)) {
      cat(
        "\nAverage",
        name.proximity_type,
        name.method,
        "silhouette:",
        name.ave, "\n"
      )
    } else {
      cat(
        "\nAverage",
        "fuzzy",
        name.proximity_type,
        name.method,
        "silhouette:",
        name.ave, "\n"
      )
    }
    cat(dash, "\n")
    print(sil.sum)
  }

  # Invisibly return results
  invisible(list(
    clus.avg.widths = clus.avg.widths,
    avg.width = avg.width,
    sil.sum=sil.sum
  ))
}








