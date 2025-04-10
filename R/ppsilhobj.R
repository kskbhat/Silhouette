#' Calculate Silhouette Width for Soft Clustering Algorithms using Cluster Membership Probability
#'
#' This function calculates the silhouette width for each observation based on the clustering results.
#' The silhouette width measures how similar an observation is to its own cluster compared to other clusters.
#'
#' @param pp A matrix representing cluster membership probabilities.
#' @param pptype Character string specifying the type of membership matrix:
#'   \describe{
#'     \item{"pp"}{Posterior Probability}
#'     \item{"nlpp"}{Negative Log of Posterior Probability}
#'     \item{"pd"}{Probability Distribution}
#'   }
#' @param silhmethod Character string indicating the silhouette calculation method:
#'   \describe{
#'     \item{"silh"}{Standard silhouette calculation}
#'     \item{"pac"}{Poserior Probability of adjucent cluster}
#'     \item{"cer"}{Certainty}
#'   }
#' @param average Character string specifying the type of average silhouette width calculation:
#'   \describe{
#'     \item{"crisp"}{Simple average of silhouette widths}
#'     \item{"fuzzy"}{Weighted average based on membership probabilities}
#'   }
#' @param a Numeric value controlling the weight calculation based on membership difference.
#' @param clust_function Optional S3 function name (as a string) to produce the proximity measure matrix.
#' @param ... Additional arguments passed to `clust_function`, if used.
#'
#' @return A list with the following components:
#' \describe{
#'   \item{widths}{A data frame containing cluster assignments, nearest neighbor, weight, and silhouette widths.}
#'   \item{clus.avg.widths}{A named vector of average silhouette widths for each cluster.}
#'   \item{avg.width}{The overall average silhouette width across all observations.}
#' }
#'
#' @export

ppsilhobj <- function(pp,
                    pptype = c("pp", "nlpp", "pd"),
                    silhmethod = c("silh", "pac","cer"),
                    average = c("crisp", "fuzzy"),
                    a = 2,
                    clust_function = NULL, ...) {

  # Validate pm based on clust_function
  if (is.null(clust_function)) {
    if (!is.matrix(pp)) {
      stop("When clust_function is NULL, pm must be a matrix.")
    }
  } else {  # clust_function is not NULL
    if (!is.character(pp)) {
      stop("When clust_function is not NULL, pp must be a variable name string containing a cluster membership matrix.")
    }
    if (!is.character(clust_function)) {
      stop("clust_function must be a valid function name as a string.")
    }

    #################### Calculating pm when pm is a string ###########################
    clust_fun <- get(clust_function, mode = "function")
    clust_out <- clust_fun(...)  # Call the clustering function
    pp <- clust_out[pp]
  }

  # Ensure pm is a matrix at this point
  if (!is.matrix(pp)) {
    stop("pm must be a matrix after processing.")
  }

  # Ensure valid argument choices
  pptype <- match.arg(pptype)
  silhmethod <- match.arg(silhmethod)
  average <- match.arg(average)

  # Define helper functions
  maxn <- function(x, n) order(x, decreasing = TRUE)[n]
  minn <- function(x, n) order(x, decreasing = FALSE)[n]

  if (pptype == "pp") {
    pmtype = "sim"
    pm = pp
  }
  if (pptype == "nlpp") {
    pmtype = "dissim"
    pm = -log(pp)
  }
  if (pptype == "pd"){
    pmtype = "sim"
    pm.den = matrix(colSums(pp),nrow = nrow(pp), ncol = ncol(pp), byrow = TRUE)
    pm = pp/pm.den
  }
  if (silhmethod == "cer"){
    pmtype = "sim"
    pm = pp
  }
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
  weight <- numeric(nrow(pm))
  sil_weight <- numeric(nrow(pm))

  # Calculate silhouette width
  if (silhmethod == "cer") {
    sil_width = apply(pm,1,max)
    message("Certainity Silhoutte uses Crisp Average and pptype is pp by default")
  } else {
    for (i in 1:nrow(pm)) {
      if (pmtype == "sim") {
        if (silhmethod == "pac") {
          sil_width[i] <- (pm[i, cluster[i]] - pm[i, neighbor[i]]) / (pm[i, cluster[i]] + pm[i, neighbor[i]])
        } else if (silhmethod == "silh") {
          sil_width[i] <- (pm[i, cluster[i]] - pm[i, neighbor[i]]) / max(pm[i, cluster[i]], pm[i, neighbor[i]])
        }
        weight[i] = (pp[i, cluster[i]] - pp[i, neighbor[i]]) ^ a #similarity based weight
      } else if (pmtype == "dissim") {
        if (silhmethod == "pac") {
          sil_width[i] <- ( pm[i, neighbor[i]] - pm[i, cluster[i]]) / (pm[i, cluster[i]] + pm[i, neighbor[i]])
        } else if (silhmethod == "silh") {
          sil_width[i] <- (pm[i, neighbor[i]] - pm[i, cluster[i]]) / max(pm[i, cluster[i]], pm[i, neighbor[i]])
        }
        weight[i] = (pp[i, neighbor[i]] - pp[i, cluster[i]]) ^ a #dissimilarity based weight
      }
    }
  }
  sil_weight = weight * sil_width #for fuzzy average
  # Create output data
  widths <- data.frame(cluster = cluster,
                       neighbor = neighbor,
                       weight = weight,
                       sil_width = sil_width)

  if (average == "crisp" | silhmethod == "cer") {
    clus.avg.widths <- tapply(sil_width, cluster, mean, na.rm = TRUE)
    avg.width <- mean(sil_width, na.rm = TRUE)
  } else if (average == "fuzzy") {
    # Compute weighted average per cluster
    clus.avg.widths <- tapply(seq_along(sil_width), cluster, function(idx) {
      sum(sil_weight[idx], na.rm = TRUE) / sum(weight[idx], na.rm = TRUE)
    })

    # Compute overall weighted average
    avg.width <- sum(sil_weight, na.rm = TRUE) / sum(weight, na.rm = TRUE)
  }


  result = structure(list(widths = widths,
                          clus.avg.widths = clus.avg.widths,
                          avg.width = avg.width), class = "ppsilhobj")
}
#' @rdname ppsilhobj
#' @export
#' @method plot ppsilhobj
#'
#' @param ppsilhobj An object of class `ppsilhobj`, created by `ppsilhobj`
#' @param label A logical value indicating whether to label the x-axis with observation names. Defaults to FALSE.
#' @param print.summary A logical value indicating whether to print a summary of average silhouette widths for each cluster. Defaults to TRUE.
#' @param ... Additional arguments passed to \code{ggpubr::ggpar}.
#'
#' @return A ggplot2 object representing the silhouette plot.
#'
#' @examples
#' library(cluster)
#' out <- fanny(iris[,-5], 3)
#'
#' silhout1 <- ppsilhobj(out$membership,average = "fuzzy",pptype = "pd")
#' plot(silhout1)
#'
#' silhout2 <- ppsilhobj(out$membership,average = "crisp",pptype = "pd")
#' plot(silhout2)
#' @export
plot.ppsilhobj <- function(ppsilhobj,
                         label = FALSE,
                         print.summary = TRUE,
                         ...) {
  df <- as.data.frame(ppsilhobj$widths)

  # Order by cluster and silhouette width using dplyr
  require(dplyr)
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
        round(ppsilhobj$avg.width, 2)
      )
    ) +
    ggplot2::ylim(c(NA, 1)) +
    ggplot2::geom_hline(
      yintercept = mean(df$sil_width),
      linetype = "dashed",
      color = "red"
    )

  p <- ggpubr::ggpar(p, ...)
  p <- ggpubr::ggpar(p)
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
    ave <- ppsilhobj$clus.avg.widths
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

