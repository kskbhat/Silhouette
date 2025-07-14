#' Plot Silhouette Analysis Results
#'
#' Creates a silhouette plot for visualizing the silhouette widths of clustering results, with bars colored by cluster and an optional summary of cluster statistics in legend.
#'
#' @param x An object of class \code{"Silhouette"}, typically the output of the \code{\link{Silhouette}} and \code{\link{softSilhouette}} function. Also supports objects classes \code{\link[factoextra]{eclust}}, \code{\link[factoextra]{hcut}}, \code{\link[cluster]{pam}}, \code{\link[cluster]{clara}}, \code{\link[cluster]{fanny}}, or \code{\link[cluster]{silhouette}} from \pkg{cluster}, \pkg{factoextra} packages. For these classes, explicitly call \code{plotSilhouette()} to generate the plot.
#' @param label Logical; if \code{TRUE}, the x-axis is labeled with observation row indices from the input data and titled "Row Index". Defaults to \code{FALSE}.
#' @param summary.legend Logical; if \code{TRUE}, prints a summary of average silhouette widths and sizes for each cluster in legend ("Cluster (Size): Width"). If \code{FALSE}, the legend shows only cluster numbers. Defaults to \code{TRUE}.
#' @param grayscale Logical; if \code{TRUE}, the plot uses a grayscale color palette for clusters. If \code{FALSE}, uses the default or specified color palette. Defaults to \code{FALSE}.
#' @param ... Additional arguments passed to \code{\link[ggpubr]{ggpar}} for customizing the plot (e.g., \code{palette}, \code{legend}).
#'
#' @details
#' The Silhouette plot displays the silhouette width (\code{sil_width}) for each observation, grouped by cluster, with bars sorted by cluster and descending silhouette width. The \code{summary.legend} option adds cluster sizes and average silhouette widths to the legend.
#'
#' This function replica of S3 method for objects of class \code{"Silhouette"}, typically produced by the \code{\link{Silhouette}} or \code{\link{softSilhouette}} functions in this package. It also supports objects of the following classes, with silhouette information extracted from their respective component:
#' \itemize{
#'   \item \code{"eclust"}: Produced by \code{\link[factoextra]{eclust}} from the \pkg{factoextra} package.
#'   \item \code{"hcut"}: Produced by \code{\link[factoextra]{hcut}} from the \pkg{factoextra} package.
#'   \item \code{"pam"}: Produced by \code{\link[cluster]{pam}} from the \pkg{cluster} package.
#'   \item \code{"clara"}: Produced by \code{\link[cluster]{clara}} from the \pkg{cluster} package.
#'   \item \code{"fanny"}: Produced by \code{\link[cluster]{fanny}} from the \pkg{cluster} package.
#'   \item \code{"silhouette"}: Produced by \code{\link[cluster]{silhouette}} from the \pkg{cluster} package.
#' }
#' For these classes (\code{"eclust"}, \code{"hcut"}, \code{"pam"}, \code{"clara"}, \code{"fanny"}, \code{"silhouette"}), users should explicitly call \code{plotSilhouette()} (e.g., \code{plotSilhouette(pam_result)}) to ensure the correct method is used, as the generic \code{plot()} may not dispatch to this function for these objects.
#'
#' @return A \code{ggplot2} object representing the Silhouette plot.
#'
#' @seealso \code{\link{Silhouette}}, \code{\link{softSilhouette}}, \code{\link[factoextra]{eclust}}, \code{\link[factoextra]{hcut}}, \code{\link[cluster]{pam}}, \code{\link[cluster]{clara}}, \code{\link[cluster]{fanny}}, \code{\link[cluster]{silhouette}}
#'
#' @references
#' Rousseeuw, P. J. (1987). Silhouettes: A graphical aid to the interpretation and validation of cluster analysis. \emph{Journal of Computational and Applied Mathematics}, 20, 53--65. \doi{10.1016/0377-0427(87)90125-7}
#'
#' @examples
#' \donttest{
#' data(iris)
#'
#' # Crisp Silhouette with k-means
#' out = kmeans(iris[,-5], 3)
#' if (requireNamespace("proxy", quietly = TRUE)) {
#'   library(proxy)
#'   dist = dist(iris[,-5], out$centers)
#'   plot(Silhouette(dist))
#' }
#'
#' #' # Fuzzy Silhouette with ppclust::fcm
#' if (requireNamespace("ppclust", quietly = TRUE)) {
#'   library(ppclust)
#'   out_fuzzy <- Silhouette(
#'     prox_matrix = "d",
#'     proximity_type = "dissimilarity",
#'     prob_matrix = "u",
#'     clust_fun = ppclust::fcm,
#'     x = iris[, 1:4],
#'     centers = 3,
#'     sort = TRUE
#'   )
#'   plot(out_fuzzy, summary.legend = FALSE, grayscale = TRUE)
#' } else {
#'   message("Install 'ppclust': install.packages('ppclust')")
#' }
#'
#' # Silhouette plot for pam clustering
#' if (requireNamespace("cluster", quietly = TRUE)) {
#'   library(cluster)
#'   pam_result <- pam(iris[, 1:4], k = 3)
#'   plotSilhouette(pam_result)
#' }
#'
#' # Silhouette plot for clara clustering
#' if (requireNamespace("cluster", quietly = TRUE)) {
#'   clara_result <- clara(iris[, 1:4], k = 3)
#'   plotSilhouette(clara_result)
#' }
#'
#' # Silhouette plot for fanny clustering
#' if (requireNamespace("cluster", quietly = TRUE)) {
#'   fanny_result <- fanny(iris[, 1:4], k = 3)
#'   plotSilhouette(fanny_result)
#' }
#'
#' # Example using base silhouette() object
#' if (requireNamespace("cluster", quietly = TRUE)) {
#'   sil <- silhouette(pam_result)
#'   plotSilhouette(sil)
#' }
#'
#' # Silhouette plot for eclust clustering
#' if (requireNamespace("factoextra", quietly = TRUE)) {
#'   library(factoextra)
#'   eclust_result <- eclust(iris[, 1:4], "kmeans", k = 3, graph = FALSE)
#'   plotSilhouette(eclust_result)
#' }
#'
#' # Silhouette plot for hcut clustering
#' if (requireNamespace("factoextra", quietly = TRUE)) {
#'   hcut_result <- hcut(iris[, 1:4], k = 3)
#'   plotSilhouette(hcut_result)
#' }
#' }
#' @export
#' @import dplyr ggplot2 ggpubr methods
plotSilhouette <- function(x,
                            label = FALSE,
                            summary.legend = TRUE,
                            grayscale = FALSE,
                            ...) {
  # Initialize variables to NULL
  cluster <- sil_width <- original_name <- name <- NULL

  if(inherits(x, "Silhouette")) {
    df <- x
    summary_stats <- summary(df,print.summary = FALSE)
    clus.avg.widths <- summary_stats$clus.avg.widths
    avg.width <- summary_stats$avg.width
  } else if(inherits(x, c("eclust", "hcut", "pam", "clara", "fanny"))){
    df <- as.data.frame(x$silinfo$widths, stringsAsFactors = TRUE)
    clus.avg.widths <- x$silinfo$clus.avg.widths
    avg.width <- x$silinfo$avg.width
  }  else if(inherits(x, "silhouette")){
    df <- as.data.frame(x[, 1:3], stringsAsFactors = TRUE)
    clus.avg.widths <- tapply(df$sil_width, df$cluster, mean, na.rm = TRUE)
    avg.width <- mean(df$sil_width, na.rm = TRUE)
  } else {
    stop("Don't support an object of class ", class(x))
  }

  # Preserve original row names before arranging
  df <- df %>% dplyr::mutate(original_name = row.names(df)) %>%
    dplyr::arrange(cluster, dplyr::desc(sil_width)) %>%
    dplyr::mutate(name = factor(original_name, levels = original_name),
                  cluster = factor(cluster))

  # Create custom legend labels
  cluster_levels <- as.integer(levels(df$cluster))
  if (summary.legend) {
    n <- table(df$cluster)
    legend_labels <- paste0("C", cluster_levels, " (", n[cluster_levels], "): ", round(clus.avg.widths[cluster_levels], 4))
    legend_title <- "Cluster (Size): Width"
  } else {
    legend_labels <- cluster_levels
    legend_title <- "Cluster"
  }

  # Set line color based on grayscale
  line_color <- ifelse(grayscale, "black", "red")

  # Determine subtitle based on presence of weight column
  subtitle_text <- if ("weight" %in% names(x)) {
    paste0("Average Fuzzy silhouette width: ", round(avg.width, 4))
  } else {
    paste0("Average silhouette width: ", round(avg.width, 4))
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
      yintercept = avg.width,
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
#' Plot method for Silhouette objects
#'
#' @inheritParams plotSilhouette
#' @rdname Silhouette
#' @method plot Silhouette
#' @export
plot.Silhouette <- plotSilhouette
