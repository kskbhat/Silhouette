#' Calculate Silhouette Width for Clustering
#'
#' This function calculates the silhouette width for each observation based on the clustering results.
#' The silhouette width is used to measure how similar an observation is to its own cluster compared
#' to other clusters.
#'
#' @param pm A matrix where each row represents an observation and each column represents a proximity measure
#'   to different clusters. Typically, this matrix would be a membership matrix or dissimilarity matrix from clustering results or their variable name.
#' @param pmtype Character string indicating whether `pm` represents similarity ("sim") or dissimilarity ("dissim").
#' @param silhmethod Character string indicating the silhouette calculation method: "pac" or "silh".
#' @param clust_function Optional function name (as a string) that produces the proximity measure matrix.
#' @param ... Additional arguments passed to `clust_function`, if used.
#'
#' @return A list with the following components:
#' \itemize{
#'   \item \code{widths}: A data frame containing the cluster assignments, nearest neighbor, and silhouette widths.
#'   \item \code{clus.avg.widths}: A named vector of average silhouette widths for each cluster.
#'   \item \code{avg.width}: The overall average silhouette width across all observations.
#' }
#'
#' @references{
#' Kaufman, L., & Rousseeuw, P. J. (1990). *Finding Groups in Data: An Introduction to Cluster Analysis*. Wiley.doi:10.1002/9780470316801
#'  }
#' @export
silhobj <- function(pm,
                    pmtype = c("sim", "dissim"),
                    silhmethod = c("silh", "pac"),
                    clust_function = NULL, ...) {

  # Validate pm based on clust_function
  if (is.null(clust_function)) {
    if (!is.matrix(pm)) {
      stop("When clust_function is NULL, pm must be a matrix.")
    }
  } else {  # clust_function is not NULL
    if (!is.character(pm)) {
      stop("When clust_function is not NULL, pm must be a variable name string containing a cluster membership matrix.")
    }
    if (!is.character(clust_function)) {
      stop("clust_function must be a valid function name as a string.")
    }

    #################### Calculating pm when pm is a string ###########################
    clust_fun <- get(clust_function, mode = "function")
    clust_out <- clust_fun(...)  # Call the clustering function
    pm <- clust_out[pm]
  }

  # Ensure pm is a matrix at this point
  if (!is.matrix(pm)) {
    stop("pm must be a matrix after processing.")
  }

  # Ensure valid argument choices
  pmtype <- match.arg(pmtype)
  silhmethod <- match.arg(silhmethod)

  # Define helper functions
  maxn <- function(x, n) order(x, decreasing = TRUE)[n]
  minn <- function(x, n) order(x, decreasing = FALSE)[n]

  # Determine cluster assignments
  if (pmtype == "sim") {
    cluster <- apply(pm, 1, maxn, n = 1)  # Get cluster assignment
    neighbor <- apply(pm, 1, maxn, n = 2)  # Get second-highest assignment
  } else {  # pmtype == "dissim"
    cluster <- apply(pm, 1, minn, n = 1)  # Get cluster assignment
    neighbor <- apply(pm, 1, minn, n = 2)  # Get second-lowest assignment
  }

  # Initialize silhouette width vector
  sil_width <- numeric(nrow(pm))

  # Calculate silhouette width
  for (i in 1:nrow(pm)) {
    if (pmtype == "sim") {
      if (silhmethod == "pac") {
        sil_width[i] <- (pm[i, cluster[i]] - pm[i, neighbor[i]]) / (pm[i, cluster[i]] + pm[i, neighbor[i]])
      } else if (silhmethod == "silh") {
        sil_width[i] <- (pm[i, cluster[i]] - pm[i, neighbor[i]]) / max(pm[i, cluster[i]], pm[i, neighbor[i]])
      }
    } else if (pmtype == "dissim") {
      if (silhmethod == "pac") {
        sil_width[i] <- ( pm[i, neighbor[i]] - pm[i, cluster[i]]) / (pm[i, cluster[i]] + pm[i, neighbor[i]])
      } else if (silhmethod == "silh") {
        sil_width[i] <- (pm[i, neighbor[i]] - pm[i, cluster[i]]) / max(pm[i, cluster[i]], pm[i, neighbor[i]])
      }
    }
  }

  # Create output data
  widths <- data.frame(cluster = cluster,
                       neighbor = neighbor,
                       sil_width = sil_width)
  avg.width <- mean(sil_width, na.rm = TRUE)
  clus.avg.widths <- tapply(sil_width, cluster, mean, na.rm = TRUE)

  result = structure(list(widths = widths,
                          clus.avg.widths = clus.avg.widths,
                          avg.width = avg.width), class = "silhobj")
}

#' @rdname silhobj
#' @export
#' @method plot silhobj
#'
#' @param silhobj An object of class `silhobj`, created by `silhobj`
#' @param label A logical value indicating whether to label the x-axis with observation names. Defaults to FALSE.
#' @param print.summary A logical value indicating whether to print a summary of average silhouette widths for each cluster. Defaults to TRUE.
#'
#' @return A ggplot2 object representing the silhouette plot.
#'
#' @examples
#' # Method 1:
#' library(cluster)
#' out <- fanny(iris[,-5], 3)
#' silhout <- silhobj(out$membership)
#' plot(silhout)
#'
#' library(mclust)
#' out1 <- Mclust(iris[,-5], 3)
#' silhout1 <- silhobj(out1$z)
#' plot(silhout1)
#'
#' # Load the iris dataset
#' data(iris)
#' # Run k-means clustering (e.g., 3 clusters)
#' kmeans_result <- kmeans(iris[, 1:4], centers = 3)
#'
#' # Compute distance matrix (obs x clusters)
#' distance_matrix <- as.matrix(dist(rbind(kmeans_result$centers, iris[, 1:4])))[-(1:3), 1:3]
#' out = silhobj(pm = distance_matrix,pmtype = "dissim")
#'plot(out)
#'
#' @export
plot.silhobj <- function(silhobj,
                         label = FALSE,
                         print.summary = TRUE,
                         ...) {
  df <- as.data.frame(silhobj$widths)

  # Order by cluster and silhouette width using dplyr
  # require(dplyr)
  df <- df %>%dplyr::arrange(cluster, dplyr::desc(sil_width)) %>%
    dplyr::mutate(name = factor(row.names(df), levels = row.names(df)),cluster = factor(cluster))

  p <- ggplot2::ggplot(df,
                       ggplot2::aes(
                         x = name,
                         y = sil_width,
                         color = cluster,
                         fill = cluster
                       )) +
    ggplot2::geom_bar(stat = "identity") +
    ggplot2::labs(
      y = "Silhouette width Si",
      x = "",
      title = paste0(
        "Clusters silhouette plot",
        "\n Average silhouette width: ",
        round(mean(df$sil_width), 2)
      )
    ) +
    ggplot2::ylim(c(NA, 1)) +
    ggplot2::geom_hline(
      yintercept = mean(df$sil_width),
      linetype = "dashed",
      color = "red"
    )

  p <- ggpubr::ggpar(p, ...)
  # Labels
  if (!label) {
    p <- p + ggplot2::theme(
      axis.text.x = ggplot2::element_blank(),
      axis.ticks.x = ggplot2::element_blank()
    )
  } else {
    p <- p + ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45))
  }

  # Print summary
  if (print.summary) {
    ave <- tapply(df$sil_width, df$cluster, mean)
    n <- table(df$cluster)
    sil.sum <- data.frame(
      cluster = as.vector(names(ave)),
      size = as.vector(n),
      avg.sil.width = c(round(ave, 2))
    )
    print(sil.sum)
  }

  p
}

