#' Calculate Silhouette Widths, Summary, and Plot for Clustering Results
#'
#' @description
#' `r lifecycle::badge('stable')`
#'
#' Computes the silhouette width for each observation based on clustering results, measuring how similar an observation is to its own cluster compared to nearest neighbor cluster. The silhouette width ranges from -1 to 1, where higher values indicate better cluster cohesion and separation.
#'
#' @param prox_matrix A numeric matrix where rows represent observations and columns represent proximity measures (e.g., distances or similarities) to clusters. Typically, this is a membership or dissimilarity matrix from clustering results. If \code{clust_fun} is provided, \code{prox_matrix} should be the name of the matrix component as a string (e.g., if \code{clust_fun = \link[ppclust]{fcm}} from \pkg{ppclust} package the \code{prox_matrix = "d"}).
#' @param proximity_type Character string specifying the type of proximity measure in \code{prox_matrix}. Options are \code{"similarity"} (higher values indicate closer proximity) or \code{"dissimilarity"} (lower values indicate closer proximity). Defaults to \code{"dissimilarity"}.
#' @param method Character string specifying the silhouette calculation method. Options are \code{"pac"} (Probability of Alternative Cluster) or \code{"medoid"}. Defaults to \code{"medoid"}.
#' @param prob_matrix A numeric matrix where rows represent observations and columns represent cluster membership probabilities, depending on \code{prob_type}). If \code{clust_fun} is provided, \code{prob_matrix} should be the name of the matrix component as a string (e.g., \code{"u"} for \code{\link[ppclust]{fcm}}). When not \code{NULL}, fuzzy silhouette width is calculated. Defaults to \code{NULL} for crisp silhouette.
#' @param a Numeric value controlling the fuzzifier or weight scaling in fuzzy silhouette averaging. Higher values increase the emphasis on strong membership differences. Must be positive. Defaults to \code{2}.
#' @param sort Logical; if \code{TRUE}, sorts the output \code{widths} data frame by cluster and descending silhouette width. Defaults to \code{FALSE}.
#' @param print.summary Logical; if \code{TRUE}, prints a summary table of average silhouette widths and sizes for each cluster. Defaults to \code{FALSE}.
#' @param clust_fun Optional S3 or S4 function object or function as character string specifying a clustering function that produces the proximity measure matrix. For example, \code{\link[ppclust]{fcm}} or \code{"fcm"}. If provided, \code{prox_matrix} must be the name of the matrix component in the clustering output (e.g., \code{"d"} for \code{\link[ppclust]{fcm}} when \code{proximity_type = "dissimilarity"}). Defaults to \code{NULL}.
#' @param ... Additional arguments passed to \code{clust_fun}, such as \code{x,centers} for \code{\link[ppclust]{fcm}}.
#'
#' @details
#' The `Silhouette` function implements the Simplified Silhouette method introduced by Van der Laan, Pollard, & Bryan (2003), which adapts and generalizes the classic silhouette method of Rousseeuw (1987).
#'
#' Clustering quality is evaluated using a proximity matrix, denoted as
#' \eqn{\Delta = [\delta_{ik}]_{n \times K}} for dissimilarity measures or
#' \eqn{\Delta' = [\delta'_{ik}]_{n \times K}} for similarity measures.
#' Here, \eqn{i = 1, \ldots, n} indexes observations, and \eqn{k = 1, \ldots, K} indexes clusters.
#' \eqn{\delta_{ik}} represents the dissimilarity (e.g., distance) between observation \eqn{i} and cluster \eqn{k},
#' while \eqn{\delta'_{ik}} represents similarity values.
#'
#' The silhouette width \eqn{S(x_i)} for observation \eqn{i} depends on the proximity type:
#'
#' For **dissimilarity** measures:
#' \deqn{
#'   S(x_i) = \frac{ \min_{k' \neq k} \delta_{ik'} - \delta_{ik} }{ N(x_i) }
#' }
#' For **similarity** measures:
#' \deqn{
#'   S(x_i) = \frac{ \delta'_{ik} - \max_{k' \neq k} \delta'_{ik'} }{ N(x_i) }
#' }
#' where \eqn{N(x_i)} is a normalizing factor defined by the method.
#'
#' **Choice of method:**
#' The normalizer \eqn{N(x_i)} is selected according to the `method` argument. The method names reference their origins but may be used with any proximity matrix, not exclusively certain clustering algorithms:
#' - For `medoid` (Van der Laan et al., 2003):
#'   - Dissimilarity: \eqn{\max(\delta_{ik}, \min_{k' \neq k} \delta_{ik'})}
#'   - Similarity:    \eqn{\max(\delta'_{ik}, \max_{k' \neq k} \delta'_{ik'})}
#' - For `pac` (Raymaekers & Rousseeuw, 2022):
#'   - Dissimilarity: \eqn{\delta_{ik} + \min_{k' \neq k} \delta_{ik'}}
#'   - Similarity:    \eqn{\delta'_{ik} + \max_{k' \neq k} \delta'_{ik'}}
#'
#' **Note:**
#' The `"medoid"` and `"pac"` options reflect the normalization formula—not a requirement to use the PAM algorithm or posterior/ensemble methods—and are general scoring approaches. These methods can be applied to any suitable proximity matrix, including proximity, similarity, or dissimilarity matrices derived from **classification algorithms**. This flexibility means silhouette indices may be computed to assess group separation when clusters or groups are formed from classification-derived proximities, not only from unsupervised clustering.
#'
#' If `prob_matrix` is `NULL`, the **crisp silhouette index** (\eqn{CS}) is:
#' \deqn{
#'   CS = \frac{1}{n} \sum_{i=1}^{n} S(x_i)
#' }
#' summarizing overall clustering quality.
#'
#' If `prob_matrix` is provided, denoted as \eqn{\Gamma = [\gamma_{ik}]_{n \times K}},
#' with \eqn{\gamma_{ik}} representing the probability of observation \eqn{i} belonging to cluster \eqn{k},
#' the **fuzzy silhouette index** (\eqn{FS}) is used:
#' \deqn{
#'   FS = \frac{ \sum_{i=1}^{n} \left( \gamma_{ik} - \max_{k' \neq k} \gamma_{ik'} \right)^{\alpha} S(x_i) }{ \sum_{i=1}^{n} \left( \gamma_{ik} - \max_{k' \neq k} \gamma_{ik'} \right)^{\alpha} }
#' }
#' where \eqn{\alpha} (the `a` argument) controls the emphasis on confident assignments.
#'
#' @return A data frame of class \code{"Silhouette"} containing cluster assignments, nearest neighbor clusters, silhouette widths for each observation, and weights (for fuzzy clustering). The object includes the following attributes:
#' \describe{
#'   \item{proximity_type}{The proximity type used (\code{"similarity"} or \code{"dissimilarity"}).}
#'   \item{method}{The silhouette calculation method used (\code{"medoid"} or \code{"pac"}).}
#' }
#' @seealso \code{\link{softSilhouette}}, \code{\link{plotSilhouette}}
#'
#' @references
#' Rousseeuw, P. J. (1987). Silhouettes: A graphical aid to the interpretation and validation of cluster analysis. \emph{Journal of Computational and Applied Mathematics}, 20, 53--65. \doi{10.1016/0377-0427(87)90125-7}
#'
#' Van der Laan, M., Pollard, K., & Bryan, J. (2003). A new partitioning around medoids algorithm. \emph{Journal of Statistical Computation and Simulation}, 73(8), 575--584. \doi{10.1080/0094965031000136012}
#'
#' Campello, R. J., & Hruschka, E. R. (2006). A fuzzy extension of the silhouette width criterion for cluster analysis. \emph{Fuzzy Sets and Systems}, 157(21), 2858--2875. \doi{10.1016/j.fss.2006.07.006}
#'
#' Raymaekers, J., & Rousseeuw, P. J. (2022). Silhouettes and quasi residual plots for neural nets and tree-based classifiers. \emph{Journal of Computational and Graphical Statistics}, 31(4), 1332--1343. \doi{10.1080/10618600.2022.2050249}
#'
#' Bhat Kapu, S., & Kiruthika. (2024). Some density-based silhouette diagnostics for soft clustering algorithms. Communications in Statistics: Case Studies, Data Analysis and Applications, 10(3-4), 221-238. \doi{10.1080/23737484.2024.2408534}
#'
#' @examples
#' # Standard silhouette with k-means on iris dataset
#' data(iris)
#' # Crisp Silhouette with k-means
#' out <- kmeans(iris[, -5], 3)
#' if (requireNamespace("ppclust", quietly = TRUE)) {
#'   library(proxy)
#'   dist <- proxy::dist(iris[, -5], out$centers)
#'   silh_out <- Silhouette(dist,print.summary = TRUE)
#'   plot(silh_out)
#' } else {
#'   message("Install 'ppclust': install.packages('ppclust')")
#' }
#' \donttest{
#' # Scree plot for optimal clusters (2 to 7)
#' if (requireNamespace("ppclust", quietly = TRUE)) {
#'   library(ppclust)
#'   avg_sil_width <- numeric(6)
#'   for (k in 2:7) {
#'     out <- Silhouette(
#'       prox_matrix = "d",
#'       proximity_type = "dissimilarity",
#'       prob_matrix = "u",
#'       clust_fun = ppclust::fcm,
#'       x = iris[, 1:4],
#'       centers = k,
#'       sort = TRUE
#'     )
#'     # Compute average silhouette width from widths
#'     avg_sil_width[k - 1] <- summary(out, print.summary = FALSE)$avg.width
#'   }
#'   plot(avg_sil_width,
#'     type = "o",
#'     ylab = "Overall Silhouette Width",
#'     xlab = "Number of Clusters",
#'     main = "Scree Plot"
#'   )
#' } else {
#'   message("Install 'ppclust': install.packages('ppclust')")
#' }
#' }
#' @export
#' @import dplyr lifecycle
Silhouette <- function(prox_matrix,
                       proximity_type = c("dissimilarity", "similarity"),
                       method = c("medoid", "pac"),
                       prob_matrix = NULL,
                       a = 2,
                       sort = FALSE,
                       print.summary = FALSE,
                       clust_fun = NULL, ...) {
  # Validate prox_matrix and clust_fun
  if (is.null(clust_fun)) {
    if (!is.matrix(prox_matrix) || !is.numeric(prox_matrix)) {
      stop("When clust_fun is NULL, prox_matrix must be a numeric matrix.")
    }
    if (ncol(prox_matrix) < 2) {
      stop("prox_matrix must have at least two columns (clusters).")
    }
  } else { # clust_fun is not NULL
    if (!is.character(prox_matrix) || length(prox_matrix) != 1) {
      stop("When clust_fun is not NULL, prox_matrix must be a single string naming a matrix component.")
    }
    if (!(is.function(clust_fun) || is.character(clust_fun))) {
      stop("clust_fun must be a function object or a string naming a function (e.g., 'kmeans').")
    }

    # Get the clustering function (S3 or S4)
    if (is.character(clust_fun)) {
      clust_fun <- if (exists(clust_fun, mode = "function")) {
        get(clust_fun, mode = "function")
      } else if (isGeneric(clust_fun)) {
        getMethod(clust_fun, "ANY") # Adjust signature if needed
      } else {
        stop("Function '", clust_fun, "' not found")
      }
    }

    # Call the clustering function
    clust_out <- tryCatch(
      clust_fun(...),
      error = function(e) stop("Error in clustering function: ", e$message)
    )

    # Extract the matrix (S3 or S4)
    prox_matrix <- if (isS4(clust_out)) {
      if (prox_matrix %in% slotNames(clust_out)) {
        slot(clust_out, prox_matrix)
      } else {
        stop("Slot '", prox_matrix, "' not found in clustering output.")
      }
    } else {
      if (prox_matrix %in% names(clust_out)) {
        clust_out[[prox_matrix]]
      } else {
        stop("Component '", prox_matrix, "' not found in clustering output.")
      }
    }

    # Extract prob_matrix (S3 or S4)
    if (!is.null(prob_matrix)) {
      prob_matrix <- if (isS4(clust_out)) {
        if (prob_matrix %in% slotNames(clust_out)) {
          slot(clust_out, prob_matrix)
        } else {
          stop("Slot '", prob_matrix, "' not found in clustering output.")
        }
      } else {
        if (prob_matrix %in% names(clust_out)) {
          clust_out[[prob_matrix]]
        } else {
          stop("Component '", prob_matrix, "' not found in clustering output.")
        }
      }
    }
  }
  # Ensure prox_matrix is a numeric matrix after extraction
  if (!is.matrix(prox_matrix) || !is.numeric(prox_matrix)) {
    stop("Extracted prox_matrix must be a numeric matrix.")
  }
  if (ncol(prox_matrix) < 2) {
    stop("Extracted prox_matrix must have at least two columns (clusters).")
  }
  # Ensure prob_matrix is a numeric matrix after extraction
  if (!is.null(prob_matrix)) {
    if (!is.matrix(prob_matrix) || !is.numeric(prob_matrix)) {
      stop("Extracted prob_matrix must be a numeric matrix.")
    }
    # Check row sums of prob_matrix
    row_sums <- rowSums(prob_matrix)
    if (any(abs(row_sums - 1) > 1e-6)) {
      stop("Each row of prob_matrix must sum to 1.")
    }
    if (ncol(prob_matrix) < 2) {
      stop("Extracted prob_matrix must have at least two columns (clusters).")
    }
  }
  # Ensure valid argument choices
  proximity_type <- match.arg(proximity_type)
  method <- match.arg(method)

  # Validate a
  if (!is.numeric(a) || a <= 0) {
    stop("a must be a positive numeric value.")
  }

  # Define helper functions
  maxn <- function(x, n) order(x, decreasing = TRUE)[n]
  minn <- function(x, n) order(x, decreasing = FALSE)[n]

  # Determine cluster assignments
  if (proximity_type == "similarity") {
    cluster <- apply(prox_matrix, 1, maxn, n = 1) # Get cluster assignment
    neighbor <- apply(prox_matrix, 1, maxn, n = 2) # Get second-highest assignment
  } else { # proximity_type == "dissimilarity"
    cluster <- apply(prox_matrix, 1, minn, n = 1) # Get cluster assignment
    neighbor <- apply(prox_matrix, 1, minn, n = 2) # Get second-lowest assignment
  }

  # Initialize silhouette width vector
  sil_width <- numeric(nrow(prox_matrix))
  weight <- numeric(nrow(prox_matrix))

  # Calculate silhouette width and weights
  for (i in 1:nrow(prox_matrix)) {
    if (proximity_type == "similarity") {
      if (method == "pac") {
        sil_width[i] <- (prox_matrix[i, cluster[i]] - prox_matrix[i, neighbor[i]]) /
          (prox_matrix[i, cluster[i]] + prox_matrix[i, neighbor[i]])
      } else if (method == "medoid") {
        sil_width[i] <- (prox_matrix[i, cluster[i]] - prox_matrix[i, neighbor[i]]) /
          max(prox_matrix[i, cluster[i]], prox_matrix[i, neighbor[i]])
      }
    } else if (proximity_type == "dissimilarity") {
      if (method == "pac") {
        sil_width[i] <- (prox_matrix[i, neighbor[i]] - prox_matrix[i, cluster[i]]) /
          (prox_matrix[i, cluster[i]] + prox_matrix[i, neighbor[i]])
      } else if (method == "medoid") {
        sil_width[i] <- (prox_matrix[i, neighbor[i]] - prox_matrix[i, cluster[i]]) /
          max(prox_matrix[i, cluster[i]], prox_matrix[i, neighbor[i]])
      }
    }
    if (!is.null(prob_matrix)) {
      weight[i] <- (prob_matrix[i, cluster[i]] - prob_matrix[i, neighbor[i]])^a # weight
    }
  }

  # Create output data
  if (is.null(prob_matrix)) {
    widths <- data.frame(
      cluster = cluster,
      neighbor = neighbor,
      sil_width = sil_width
    )
  } else {
    widths <- data.frame(
      cluster = cluster,
      neighbor = neighbor,
      weight = weight,
      sil_width = sil_width
    )
  }
  # Sort widths if sort = TRUE
  if (sort) {
    # Initialize variables to NULL
    original_name <- NULL
    widths <- widths %>%
      dplyr::mutate(original_name = row.names(widths)) %>%
      dplyr::arrange(cluster, dplyr::desc(sil_width))
    rownames(widths) <- widths$original_name
    widths <- subset(widths, select = -original_name)
  }

  # Add attributes to the widths data frame
  attr(widths, "proximity_type") <- proximity_type
  attr(widths, "method") <- method

  if (print.summary) {
    # Compute average silhouette widths for summary
    if (is.null(prob_matrix)) {
      clus.avg.widths <- tapply(widths$sil_width, widths$cluster, mean, na.rm = TRUE)
      avg.width <- mean(widths$sil_width, na.rm = TRUE)
    } else {
      sil_weight <- widths$weight * widths$sil_width
      clus.avg.widths <- tapply(seq_along(widths$sil_width), widths$cluster, function(idx) {
        sum(sil_weight[idx], na.rm = TRUE) / sum(widths$weight[idx], na.rm = TRUE)
      })
      avg.width <- sum(sil_weight, na.rm = TRUE) / sum(widths$weight, na.rm = TRUE)
    }

    # Summary data
    n <- table(widths$cluster)
    sil.sum <- data.frame(
      cluster = names(clus.avg.widths),
      size = as.vector(n),
      avg.sil.width = round(clus.avg.widths, 4)
    )

    # Construct summary output string
    header <- if (is.null(prob_matrix)) {
      sprintf("Average %s %s silhouette: %.4f", proximity_type, method, avg.width)
    } else {
      sprintf("Average fuzzy %s %s silhouette: %.4f", proximity_type, method, avg.width)
    }

    message(strrep("-", nchar(header)))
    message(header)
    message(strrep("-", nchar(header)))
    cat("\n")
    print(sil.sum)
    cat("\n")
    message("Available attributes: ", paste(names(attributes(widths)), collapse = ", "))
  }

  structure(widths, class = c("Silhouette", "data.frame"))
}
