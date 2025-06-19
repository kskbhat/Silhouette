#' Plot Silhouette Analysis Results
#'
#' Creates a silhouette plot for visualizing the silhouette widths of clustering results, with bars colored by cluster and an optional summary of cluster statistics.
#'
#' @param x An object of class \code{"Silhouette"}, typically the output of the \code{\link{Silhouette}} or \code{\link{softSilhouette}} function.
#' @param label Logical; if \code{TRUE}, the x-axis is labeled with observation names from the input data and titled "row index". Defaults to \code{FALSE}.
#' @param summary.legend Logical; if \code{TRUE}, prints a summary of average silhouette widths and sizes for each cluster in legend ("Cluster (Size): Width"). If \code{FALSE}, the legend shows only cluster numbers. Defaults to \code{TRUE}.
#' @param grayscale Logical; if \code{TRUE}, the plot uses a grayscale color palette for clusters. If \code{FALSE}, uses the default or specified color palette. Defaults to \code{FALSE}.
#' @param ... Additional arguments passed to \code{\link[ggpubr]{ggpar}} for customizing the plot (e.g., \code{palette}, \code{legend}).
#'
#' @return A \code{ggplot2} object representing the silhouette plot.
#'
#' @seealso \code{\link{Silhouette}}, \code{\link{softSilhouette}}
#'
#' @references
#' Rousseeuw, P. J. (1987). Silhouettes: A graphical aid to the interpretation and validation of cluster analysis. \emph{Journal of Computational and Applied Mathematics}, 20, 53--65. \doi{10.1016/0377-0427(87)90125-7}
#' @examples
#'   data(iris)
#' # Crisp and fuzzy silhouettes with ppclust::fcm
#' if (requireNamespace("ppclust", quietly = TRUE)) {
#'   library(ppclust)
#'   # Crisp silhouette (3 clusters)
#'   out_crisp <- Silhouette(
#'     prox_matrix = "d",
#'     proximity_type = "dissimilarity",
#'     clust_fun = "fcm",
#'     x = iris[, 1:4],
#'     centers = 3
#'   )
#'   plot(out_crisp)
#' } else {
#'   message("Install 'ppclust': install.packages('ppclust')")
#' }
#' if (requireNamespace("ppclust", quietly = TRUE)) {
#'   # Fuzzy silhouette (3 clusters)
#'   out_fuzzy <- Silhouette(
#'     prox_matrix = "d",
#'     proximity_type = "dissimilarity",
#'     prob_matrix = "u",
#'     clust_fun = "fcm",
#'     x = iris[, 1:4],
#'     centers = 3
#'   )
#'   plot(out_fuzzy,summary.legend = FALSE,grayscale = TRUE)
#' } else {
#'   message("Install 'ppclust': install.packages('ppclust')")
#' }
#'
#' @export
#' @method plot Silhouette
#' @import dplyr ggplot2 ggpubr methods
plot.Silhouette <- function(x,
                            label = FALSE,
                            summary.legend = TRUE,
                            grayscale = FALSE,
                            ...) {
  if (!inherits(x, "Silhouette")) {
    stop("Argument 'x' must be an object of class 'Silhouette'.")
  }

  # Initialize variables to NULL
  cluster <- sil_width <- original_name <- name <- NULL
  df <- as.data.frame(x$widths)

  # Preserve original row names before arranging
  df <- df %>% dplyr::mutate(original_name = row.names(df)) %>%
    dplyr::arrange(cluster, dplyr::desc(sil_width)) %>%
    dplyr::mutate(name = factor(original_name, levels = original_name),
                  cluster = factor(cluster))

  # Create custom legend labels
  cluster_levels <- levels(df$cluster)
  if (summary.legend) {
    n <- table(df$cluster)
    legend_labels <- paste0("C", cluster_levels, " (", n[cluster_levels], "): ", round(x$clus.avg.widths[cluster_levels], 4))
    legend_title <- "Cluster (Size): Width"
  } else {
    legend_labels <- cluster_levels
    legend_title <- "Cluster"
  }

  # Set line color based on grayscale
  line_color <- ifelse(grayscale, "black", "red")

  # Determine subtitle based on presence of weight column
  subtitle_text <- if ("weight" %in% names(x$widths)) {
    paste0("Average Fuzzy silhouette width: ", round(x$avg.width, 4))
  } else {
    paste0("Average silhouette width: ", round(x$avg.width, 4))
  }

  # Set x-axis label based on label parameter
  x_label <- if (label) "Row Index" else ""

  p <- ggplot2::ggplot(df,
                       ggplot2::aes(
                         x = name,
                         y = sil_width,
                         color = cluster,
                         fill = cluster
                       )) +
    ggplot2::geom_bar(stat = "identity") +
    ggplot2::labs(
      y = "Silhouette Width",
      x = x_label,
      title = "Clusters Silhouette Plot",
      subtitle = subtitle_text
    ) +
    ggplot2::ylim(c(NA, 1)) +
    ggplot2::geom_hline(
      yintercept = x$avg.width,
      linetype = "dashed",
      color = line_color
    ) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(hjust = 0.5),
      plot.subtitle = ggplot2::element_text(size = 10, hjust = 0.5, colour = line_color)
    )

  # Apply fill and color scales based on grayscale
  if (grayscale) {
    p <- p +
      ggplot2::scale_fill_grey(name = legend_title, labels = legend_labels, start = 0.2, end = 0.8) +
      ggplot2::scale_color_grey(name = legend_title, labels = legend_labels, start = 0.2, end = 0.8)
  } else {
    p <- p +
      ggplot2::scale_fill_discrete(name = legend_title, labels = legend_labels) +
      ggplot2::scale_color_discrete(name = legend_title, labels = legend_labels)
  }

  p <- ggpubr::ggpar(p, ...)

  # Labels
  if (!label) {
    p <- p + ggplot2::theme(
      axis.text.x = ggplot2::element_blank(),
      axis.ticks.x = ggplot2::element_blank()
    )
  } else {
    p <- p + ggplot2::theme(
      axis.text.x = ggplot2::element_text(angle = 90, vjust = 0.5, hjust = 1)
    )
  }
  return(p)
}
