#' Calculate Silhouette Width for Clustering
#'
#' Computes the silhouette width for each observation based on clustering results, measuring how similar an observation is to its own cluster compared to other clusters. The silhouette width ranges from -1 to 1, where higher values indicate better cluster cohesion and separation.
#'
#' @param prox_matrix A numeric matrix where rows represent observations and columns represent proximity measures (e.g., distances or similarities) to clusters. Typically, this is a membership or dissimilarity matrix from clustering results. If \code{clust_fun} is provided, \code{prox_matrix} should be the name of the matrix component as a string.
#' @param proximity_type Character string specifying the type of proximity measure in \code{prox_matrix}. Options are \code{"similarity"} (higher values indicate closer proximity) or \code{"dissimilarity"} (lower values indicate closer proximity). Defaults to \code{"dissimilarity"}.
#' @param method Character string specifying the silhouette calculation method. Options are \code{"pac"} (Probability of Alternative Cluster) or \code{"medoid"}. Defaults to \code{"medoid"}.
#' @param prob_matrix A numeric matrix where rows represent observations and columns represent cluster membership probabilities (or transformed probabilities, depending on \code{prob_type}). If \code{clust_fun} is provided, \code{prob_matrix} should be the name of the matrix component as a string (e.g., \code{"u"} for \code{fcm}). When not \code{NULL}, fuzzy silhouette width is calculated. Defaults to \code{NULL} for crisp silhouette.
#' @param a Numeric value controlling the weight calculation based on membership difference for fuzzy silhouettes. Must be positive. Defaults to \code{2}.
#' @param print.summary Logical; if \code{TRUE}, prints a summary table of average silhouette widths and sizes for each cluster. Defaults to \code{TRUE}.
#' @param clust_fun Optional S3 or S4 function object specifying a clustering function that produces the proximity measure matrix. For example, \code{fanny}. If provided, \code{prox_matrix} must be the name of the matrix component in the clustering output (e.g., \code{"membership"} for \code{fanny} when \code{proximity_type = "similarity"}). Defaults to \code{NULL}.
#' @param ... Additional arguments passed to \code{clust_fun}, such as \code{x,centers} for \code{fcm}.
#'
#' @details
#' The `Silhouette` function employs the Simplified Silhouette method introduced by Van der Laan et al. (2003), differing from the original silhouette method proposed by Rousseeuw (1987).
#'
#' It evaluates clustering quality using a proximity matrix, denoted as \eqn{\Delta = [\delta_{ik}]_{n \times K}} for dissimilarity measures or \eqn{\Delta' = [\delta'_{ik}]_{n \times K}} for similarity measures. Here, \eqn{i = 1, \ldots, n} represents the number of observations, and \eqn{k = 1, \ldots, K} denotes the number of clusters. The elements \eqn{\delta_{ik}} indicate dissimilarity (e.g., distance) between observation \eqn{i} and cluster \eqn{k}, while \eqn{\delta'_{ik}} represent similarity between observation with the cluster \eqn{k}.
#'
#' The silhouette width \eqn{S(x_i)} for observation \eqn{i} is calculated based on the proximity type. For dissimilarity measures, it is given by:
#' \deqn{S(x_i) = \dfrac{(\min_{k' \neq k} \delta_{ik'}) - \delta_{ik}}{N(x_i)},}
#' where \eqn{\min_{k' \neq k} \delta_{ik'}} is the smallest dissimilarity to any other cluster \eqn{k' \neq k}. For similarity measures, it is:
#' \deqn{S(x_i) = \dfrac{\delta'_{ik} - (\max_{k' \neq k} \delta'_{ik'})}{N(x_i)},}
#' where \eqn{\max_{k' \neq k} \delta'_{ik'}} is the largest similarity to any other cluster and \eqn{N(x_i)} is the normaliser.
#'
#' The Normaliser depends on the `method` argument:
#'
#' for `medoid`, it is \eqn{\max(\delta_{ik}, \min_{k' \neq k} \delta_{ik'})} (dissimilarity) or \eqn{\max(\delta'_{ik}, \max_{k' \neq k} \delta'_{ik'})} (similarity);
#' for `pac`, it is \eqn{\delta_{ik} + \min_{k' \neq k} \delta_{ik'}} (dissimilarity) or \eqn{\delta'_{ik} + \max_{k' \neq k} \delta'_{ik'}} (similarity).
#'
#' When `prob_matrix` is `NULL`, the function computes the crisp overall silhouette index \eqn{CS} as:
#' \deqn{CS = \frac{1}{n} \sum_{i=1}^{n} S(x_i).}
#' This represents the average silhouette width across all observations, providing a measure of overall clustering quality.
#'
#' If `prob_matrix` is provided, it is denoted as \eqn{\Gamma = [\gamma_{ik}]_{n \times K}}, where \eqn{\gamma_{ik}} represents the membership probability of observation \eqn{i} to cluster \eqn{k}. The fuzzy silhouette index \eqn{FS} is then calculated as:
#' \deqn{FS = \frac{\sum_{i=1}^{n} \left( \gamma_{ik} - \max_{k' \neq k} \gamma_{ik'} \right)^{\alpha} S(x_i)}{\sum_{i=1}^{n} \left( \gamma_{ik} - \max_{k' \neq k} \gamma_{ik'} \right)^{\alpha}},}
#' where \eqn{\alpha} (set by the argument `a`) controls the weighting based on membership differences. Higher \eqn{\alpha} values emphasize observations with more confident cluster assignments, accounting for uncertainty in fuzzy clustering.
#'
#' @return A list of class \code{"silhouette"} with the following components:
#' \describe{
#'   \item{widths}{A data frame containing cluster assignments, nearest neighbor clusters, and silhouette widths for each observation.}
#'   \item{clus.avg.widths}{A named numeric vector of average silhouette widths per cluster.}
#'   \item{avg.width}{A numeric value representing the overall average silhouette width across all observations.}
#' }
#'
#' @seealso \code{\link{softSilhouette}}, \code{\link{plot.Silhouette}}
#'
#' @references
#' Rousseeuw, P. J. (1987). Silhouettes: A graphical aid to the interpretation and validation of cluster analysis. \emph{Journal of Computational and Applied Mathematics}, 20, 53--65. \doi{10.1016/0377-0427(87)90125-7}
#'
#' Van der Laan, M., Pollard, K., & Bryan, J. (2003). A new partitioning around medoids algorithm. \emph{Journal of Statistical Computation and Simulation}, 73(8), 575--584. \doi{10.1080/0094965031000136012}
#'
#' Raymaekers, J., & Rousseeuw, P. J. (2022). Silhouettes and quasi residual plots for neural nets and tree-based classifiers. \emph{Journal of Computational and Graphical Statistics}, 31(4), 1332--1343. \doi{10.1080/10618600.2022.2050249}
#'
#' Campello, R. J., & Hruschka, E. R. (2006). A fuzzy extension of the silhouette width criterion for cluster analysis. \emph{Fuzzy Sets and Systems}, 157(21), 2858--2875. \doi{10.1016/j.fss.2006.07.006}
#'
#' Bhat, K.S., Kiruthika. Some density-based silhouette diagnostics for soft clustering algorithms. \emph{Communications in Statistics: Case Studies, Data Analysis and Applications}, 10(3–4), 221–238 (2024). \doi{10.1080/23737484.2024.2408534}
#'
#' @examples
#' # Standard silhouette with k-means on iris dataset
#' data(iris)
#' kmeans_result <- kmeans(iris[, 1:4], centers = 3)
#' # Compute distance matrix
#' dist_mat <- as.matrix(dist(rbind(kmeans_result$centers,
#'                                 iris[, 1:4])))[-(1:3), 1:3]
#' out <- Silhouette(prox_matrix = dist_mat)
#' plot(out)
#'
#'   # Scree plot for optimal clusters (2 to 7)
#' if (requireNamespace("ppclust", quietly = TRUE)) {
#'   library(ppclust)
#'   data(iris)
#'   avg_sil_width <- numeric(6)
#'   for (k in 2:7) {
#'     avg_sil_width[k-1] <- Silhouette(
#'       prox_matrix = "d",
#'       proximity_type = "dissimilarity",
#'       prob_matrix = "u",
#'       clust_fun = ppclust::fcm,
#'       x = iris[, 1:4],
#'       centers = k,
#'       print.summary = FALSE
#'     )$avg.width
#'   }
#'   plot(avg_sil_width, type = "o",
#'        ylab = "Overall Silhouette Width",
#'        xlab = "Number of Clusters",
#'        main = "Scree Plot")
#' } else {
#'   message("Install 'ppclust': install.packages('ppclust')")
#' }
#' @export
Silhouette <- function(prox_matrix,
                       proximity_type = c("dissimilarity","similarity"),
                       method = c("medoid", "pac"),
                       prob_matrix = NULL,
                       a = 2,
                       print.summary = TRUE,
                       clust_fun = NULL, ...) {
  # Validate prox_matrix and clust_fun
  if (is.null(clust_fun)) {
    if (!is.matrix(prox_matrix) || !is.numeric(prox_matrix)) {
      stop("When clust_fun is NULL, prox_matrix must be a numeric matrix.")
    }
    if (ncol(prox_matrix) < 2) {
      stop("prox_matrix must have at least two columns (clusters).")
    }
  } else {  # clust_fun is not NULL
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
        getMethod(clust_fun, "ANY")  # Adjust signature if needed
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

    # Extract the matrix (S3 or S4)
    if(!is.null(prob_matrix)){
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
  if(!is.null(prob_matrix)){
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
    cluster <- apply(prox_matrix, 1, maxn, n = 1)  # Get cluster assignment
    neighbor <- apply(prox_matrix, 1, maxn, n = 2)  # Get second-highest assignment
  } else {  # proximity_type == "dissimilarity"
    cluster <- apply(prox_matrix, 1, minn, n = 1)  # Get cluster assignment
    neighbor <- apply(prox_matrix, 1, minn, n = 2)  # Get second-lowest assignment
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
      weight[i] <- (prob_matrix[i, cluster[i]] - prob_matrix[i, neighbor[i]]) ^ a  # weight
    }
  }

  # Calculate weighted silhouette for fuzzy average
  if (!is.null(prob_matrix)) {
    sil_weight <- weight * sil_width
  }

  # Create output data
  if (is.null(prob_matrix)) {
    widths <- data.frame(cluster = cluster,
                         neighbor = neighbor,
                         sil_width = sil_width)
  } else {
    widths <- data.frame(cluster = cluster,
                         neighbor = neighbor,
                         weight = weight,
                         sil_width = sil_width)
  }

  # Compute average silhouette widths
  if (is.null(prob_matrix)) {
    clus.avg.widths <- tapply(sil_width, cluster, mean, na.rm = TRUE)
    avg.width <- mean(sil_width, na.rm = TRUE)
  } else {
    clus.avg.widths <- tapply(seq_along(sil_width), cluster, function(idx) {
      sum(sil_weight[idx], na.rm = TRUE) / sum(weight[idx], na.rm = TRUE)
    })
    avg.width <- sum(sil_weight, na.rm = TRUE) / sum(weight, na.rm = TRUE)
  }

  result <- list(widths = widths,
                 clus.avg.widths = clus.avg.widths,
                 avg.width = avg.width)
  if (print.summary) {
    name.proximity_type <- paste(noquote(proximity_type))
    name.ave <- paste(noquote(round(avg.width,4)))
    name.method  <- paste(noquote(method))
    dash1p = "--------------------------------------------"
    dash2p = "--------------------------------------------------"
    dashs1p = "-----------------------------------------"
    dashs2p = "-----------------------------------------------"
    dash1 = "-----------------------------------------------"
    dash2 = "-----------------------------------------------------"
    dashs1 = "--------------------------------------------"
    dashs2 = "--------------------------------------------------"
    dash = if (method == "pac") {
      ifelse(is.null(prob_matrix),ifelse(proximity_type == "dissimilarity",dash1p,dashs1p),ifelse(proximity_type == "dissimilarity",dash2p,dashs2p))
    } else {
      ifelse(is.null(prob_matrix),ifelse(proximity_type == "dissimilarity",dash1,dashs1),ifelse(proximity_type == "dissimilarity",dash2,dashs2))
    }
    cat(dash)
    if (is.null(prob_matrix)) {
      cat(
        "\nAverage",
        name.proximity_type,
        name.method,
        "silhouette:",
        name.ave,"\n")
    } else {cat(
      "\nAverage",
      "fuzzy",
      name.proximity_type,
      name.method,
      "silhouette:",
      name.ave,"\n")}
    cat(dash,"\n")
    if (!is.numeric(clus.avg.widths) || is.null(names(clus.avg.widths))) {
      warning("clus.avg.widths is not a named numeric vector; summary may be incorrect.")
    }
    n <- table(cluster)
    sil.sum <- data.frame(
      cluster = names(clus.avg.widths),
      size = as.vector(n),
      avg.sil.width = round(clus.avg.widths, 4)
    )
    print(sil.sum)
    cat("\n")
    cat("Available components:\n")
    print(names(result))
  }
  structure(result, class = "Silhouette")
}
