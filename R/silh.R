#' Calculate Silhouette Width for Clustering and Block clustering
#'
#' This function calculates the silhouette width for each observation based on the clustering or block clustering results.
#' The silhouette width is used to measure how similar an observation is to its own cluster compared
#' to other clusters.
#'
#' @param pm A matrix or list of matrices where each row represents an observation and each column represents a proximity measure
#'   to different clusters. Typically, this matrix would be a membership matrix or dissimilarity matrix from clustering results or their variable names string in vector.
#' @param pmtype Character string indicating whether `pm` represents similarity ("sim") or dissimilarity ("dissim").
#' @param silhmethod Character string indicating the silhouette calculation method: "pac" or "silh".
#' @param clust_function Optional S3 function name (as a string) that produces the proximity measure matrix.
#' @param ... Additional arguments passed to `clust_function`, if used.
#'
#' @return A list or two lists with the following components for row and column proximity measures:
#' \itemize{
#'   \item \code{widths}: A data frame containing the cluster assignments, nearest neighbor, and silhouette widths.
#'   \item \code{clus.avg.widths}: A named vector of average silhouette widths for each cluster.
#'   \item \code{avg.width}: The overall average silhouette width across all observations.
#' }
#' @return \code{block.shil.matrix} : A matrix containing silhout index of each data point from block clustering (if inputs are from block custering/ Bi-clustering/Coclustering).
#' @export
silh <- function(pm,
                 pmtype = c("sim", "dissim"),
                 silhmethod = c("silh", "pac"),
                 clust_function = NULL, ...) {

  if (is.list(pm)) {
    # Case 1: pm is a list of exactly two matrices with the same dimensions
    if (length(pm) != 2) {
      stop("When pm is a list, it must have exactly 2 elements.")
    }
    if (!all(sapply(pm, is.matrix))) {
      stop("Both elements of pm must be matrices.")
    }

    # Code block for list of two matrices
    rowsilhobj <- silhobj(pm[[1]], pmtype, silhmethod, clust_function, ...)
    colsilhobj <- silhobj(pm[[2]], pmtype, silhmethod, clust_function, ...)
    out <- structure(list(rowsilhobj, colsilhobj), class = "bisilhobj")

  } else if (is.character(pm) && length(pm) == 2) {
    # Case 2: pm is a character vector of exactly two strings
    # Code block for vector of two strings
    rowsilhobj <- silhobj(pm[1], pmtype, silhmethod, clust_function, ...)
    colsilhobj <- silhobj(pm[2], pmtype, silhmethod, clust_function, ...)
    out <- structure(list(rowsilhobj, colsilhobj), class = "bisilhobj")

  } else if (is.matrix(pm) || (is.character(pm) && length(pm) == 1)) {
    # Case 3: pm is a single matrix OR a single string in a character vector
    # Code block for single matrix or single string
    out <- silhobj(pm, pmtype, silhmethod, clust_function, ...)

  } else {
    # Invalid input case
    stop("pm must be either:
         - A list of two matrices with the same dimensions, or
         - A character vector of exactly two strings, or
         - A single matrix, or
         - A single string in a character vector.")
  }

  return(out)
}







