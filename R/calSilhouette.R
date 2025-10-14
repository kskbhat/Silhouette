#' Compute Calculate of All Possible Silhouette Methods
#'
#' Computes all possible silhouette indices from available functions in the package and returns a summary data frame comparing crisp, fuzzy, and median silhouette values across different methods.
#'
#' @param prox_matrix A numeric matrix where rows represent observations and columns represent proximity measures (e.g., distances or similarities) to clusters. Typically, this is a membership or dissimilarity matrix from clustering results. If \code{clust_fun} is provided, \code{prox_matrix} should be the name of the matrix component as a string (e.g., if \code{clust_fun = \link[ppclust]{fcm}} from \pkg{ppclust} package the \code{prox_matrix = "d"}).
#' @param proximity_type Character string specifying the type of proximity measure in \code{prox_matrix}. Options are \code{"similarity"} (higher values indicate closer proximity) or \code{"dissimilarity"} (lower values indicate closer proximity). Defaults to \code{"dissimilarity"}.
#' @param prob_matrix A numeric matrix of cluster membership probabilities, where rows represent
#'   observations and columns represent clusters (depending on \code{prob_type}).
#'   If \code{clust_fun} is provided, \code{prob_matrix} can be given as the name of the matrix
#'   component (e.g., \code{"u"} for the \code{\link[ppclust]{fcm}} function).
#'   Defaults to \code{NULL}.
#' @param a Numeric value controlling the fuzzifier or weight scaling in fuzzy silhouette averaging. Higher values increase the emphasis on strong membership differences. Must be positive. Defaults to \code{2}.
#' @param print.summary Logical; if \code{TRUE}, prints a summary table of average silhouette widths and sizes for each cluster. Defaults to \code{FALSE}.
#' @param clust_fun Optional S3 or S4 function object or function as character string specifying a clustering function that produces the proximity measure matrix. For example, \code{\link[ppclust]{fcm}} or \code{"fcm"}. If provided, \code{prox_matrix} must be the name of the matrix component in the clustering output (e.g., \code{"d"} for \code{\link[ppclust]{fcm}} when \code{proximity_type = "dissimilarity"}). Defaults to \code{NULL}.
#' @param ... Additional arguments passed to \code{clust_fun}, such as \code{x,centers} for \code{\link[ppclust]{fcm}}.
#'
#' @details
#' This function computes all available silhouette methods from the package and returns a comparative summary. The methods included depend on the available input matrices:
#'
#' **If \code{prox_matrix} is available:**
#' \itemize{
#'   \item \code{medoid} - Medoid-based silhouette using \code{\link{Silhouette}}
#'   \item \code{pac} - PAC-based silhouette using \code{\link{Silhouette}}
#' }
#'
#' **If \code{prob_matrix} is available:**
#' \itemize{
#'   \item \code{pp_pac} - Posterior probabilities silhouette with PAC method using \code{\link{softSilhouette}}
#'   \item \code{pp_medoid} - Posterior probabilities silhouette with Medoid method using \code{\link{softSilhouette}}
#'   \item \code{nlpp_pac} - Negative log posterior probabilities silhouette with PAC method using \code{\link{softSilhouette}}
#'   \item \code{nlpp_medoid} - Negative log posterior probabilities silhouette with Medoid method using \code{\link{softSilhouette}}
#'   \item \code{pd_pac} - Probability distribution silhouette with PAC method using \code{\link{softSilhouette}}
#'   \item \code{pd_medoid} - Probability distribution silhouette with Medoid method using \code{\link{softSilhouette}}
#'   \item \code{cer} - Certainty-based silhouette using \code{\link{cerSilhouette}}
#'   \item \code{db} - Density-based silhouette using \code{\link{dbSilhouette}}
#' }
#'
#' At least one of \code{prox_matrix} or \code{prob_matrix} must be provided.
#'
#' @return A data frame with the following columns:
#' \describe{
#'   \item{Method}{Character vector of method names}
#'   \item{Crisp_Silhouette}{Numeric vector of crisp (unweighted) average silhouette values}
#'   \item{Fuzzy_Silhouette}{Numeric vector of fuzzy (weighted) average silhouette values (NA if \code{prob_matrix} is not available for the method)}
#'   \item{Median_Silhouette}{Numeric vector of median silhouette values}
#' }
#'
#' @seealso \code{\link{Silhouette}}, \code{\link{softSilhouette}}, \code{\link{dbSilhouette}}, \code{\link{cerSilhouette}}
#'
#' @examples
#' if (requireNamespace("ppclust", quietly = TRUE)) {
#'   # Example with FCM clustering
#'   library(ppclust)
#'   data(iris)
#'   fcm_result <- fcm(iris[, -5], centers = 3)
#'
#'   # Using matrices directly
#'   summary_result <- calSilhouette(
#'     prox_matrix = fcm_result$d,
#'     prob_matrix = fcm_result$u,
#'     proximity_type = "dissimilarity",
#'     print.summary = TRUE
#'   )
#' }
#'
#' if (requireNamespace("ppclust", quietly = TRUE)) {
#'   # Using clustering function
#'   summary_result2 <- calSilhouette(
#'     prox_matrix = "d",
#'     prob_matrix = "u",
#'     proximity_type = "dissimilarity",
#'     clust_fun = ppclust::fcm,
#'     x = iris[, -5],
#'     centers = 3,
#'     print.summary = TRUE
#'   )
#' }
#' @export
calSilhouette <- function(prox_matrix = NULL,
                          proximity_type = c("dissimilarity", "similarity"),
                          prob_matrix = NULL,
                          a = 2,
                          print.summary = FALSE,
                          clust_fun = NULL, ...) {

  # Validate inputs
  if (is.null(prox_matrix) && is.null(prob_matrix)) {
    stop("At least one of 'prox_matrix' or 'prob_matrix' must be provided.")
  }

  proximity_type <- match.arg(proximity_type)

  # Initialize results list
  results <- list()

  # Handle clustering function if provided
  if (!is.null(clust_fun)) {
    if (!(is.function(clust_fun) || is.character(clust_fun))) {
      stop("clust_fun must be a function object or a string naming a function.")
    }

    if (is.character(clust_fun)) {
      if (exists(clust_fun, mode = "function", envir = parent.frame())) {
        clust_fun <- get(clust_fun, mode = "function", envir = parent.frame())
      } else if (isGeneric(clust_fun)) {
        clust_fun <- getMethod(clust_fun, "ANY")
      } else {
        stop("Function '", clust_fun, "' not found")
      }
    }

    # Run clustering function
    clust_out <- tryCatch(clust_fun(...),
                          error = function(e) stop("Error in clustering function: ", e$message))

    # Extract matrices from clustering output
    if (!is.null(prox_matrix)) {
      if (!is.character(prox_matrix) || length(prox_matrix) != 1) {
        stop("When clust_fun is not NULL, prox_matrix must be a single string naming a matrix component.")
      }
      prox_matrix <- if (isS4(clust_out)) {
        if (prox_matrix %in% slotNames(clust_out)) slot(clust_out, prox_matrix) else stop("Slot '", prox_matrix, "' not found in clustering output.")
      } else {
        if (prox_matrix %in% names(clust_out)) clust_out[[prox_matrix]] else stop("Component '", prox_matrix, "' not found in clustering output.")
      }
    }

    if (!is.null(prob_matrix)) {
      if (!is.character(prob_matrix) || length(prob_matrix) != 1) {
        stop("When clust_fun is not NULL, prob_matrix must be a single string naming a matrix component.")
      }
      prob_matrix <- if (isS4(clust_out)) {
        if (prob_matrix %in% slotNames(clust_out)) slot(clust_out, prob_matrix) else stop("Slot '", prob_matrix, "' not found in clustering output.")
      } else {
        if (prob_matrix %in% names(clust_out)) clust_out[[prob_matrix]] else stop("Component '", prob_matrix, "' not found in clustering output.")
      }
    }
  }

  # Helper function to safely compute silhouette and extract summary
  safe_compute <- function(func, ...) {
    tryCatch({
      result <- func(...)
      if (is.Silhouette(result)) {
        return(summary(result, print.summary = FALSE))
      } else {
        return(NULL)
      }
    }, error = function(e) {
      if (print.summary) {
        warning("Error computing silhouette: ", e$message)
      }
      return(NULL)
    })
  }

  # Compute proximity-based methods if prox_matrix is available
  if (!is.null(prox_matrix)) {
    if (!is.matrix(prox_matrix) || !is.numeric(prox_matrix)) {
      stop("prox_matrix must be a numeric matrix.")
    }
    if (ncol(prox_matrix) < 2) {
      stop("prox_matrix must have at least two columns (clusters).")
    }

    # Medoid method
    medoid_crisp <- safe_compute(Silhouette,
                                 prox_matrix = prox_matrix,
                                 proximity_type = proximity_type,
                                 method = "medoid",
                                 average = "crisp",
                                 print.summary = FALSE)

    medoid_fuzzy <- if (!is.null(prob_matrix)) {
      safe_compute(Silhouette,
                   prox_matrix = prox_matrix,
                   proximity_type = proximity_type,
                   method = "medoid",
                   average = "fuzzy",
                   prob_matrix = prob_matrix,
                   a = a,
                   print.summary = FALSE)
    } else { NULL }

    medoid_median <- safe_compute(Silhouette,
                                  prox_matrix = prox_matrix,
                                  proximity_type = proximity_type,
                                  method = "medoid",
                                  average = "median",
                                  print.summary = FALSE)

    results[["medoid"]] <- list(
      crisp = if (!is.null(medoid_crisp)) medoid_crisp$avg.width else NA,
      fuzzy = if (!is.null(medoid_fuzzy)) medoid_fuzzy$avg.width else NA,
      median = if (!is.null(medoid_median)) medoid_median$avg.width else NA
    )

    # PAC method
    pac_crisp <- safe_compute(Silhouette,
                              prox_matrix = prox_matrix,
                              proximity_type = proximity_type,
                              method = "pac",
                              average = "crisp",
                              print.summary = FALSE)

    pac_fuzzy <- if (!is.null(prob_matrix)) {
      safe_compute(Silhouette,
                   prox_matrix = prox_matrix,
                   proximity_type = proximity_type,
                   method = "pac",
                   average = "fuzzy",
                   prob_matrix = prob_matrix,
                   a = a,
                   print.summary = FALSE)
    } else { NULL }

    pac_median <- safe_compute(Silhouette,
                               prox_matrix = prox_matrix,
                               proximity_type = proximity_type,
                               method = "pac",
                               average = "median",
                               print.summary = FALSE)

    results[["pac"]] <- list(
      crisp = if (!is.null(pac_crisp)) pac_crisp$avg.width else NA,
      fuzzy = if (!is.null(pac_fuzzy)) pac_fuzzy$avg.width else NA,
      median = if (!is.null(pac_median)) pac_median$avg.width else NA
    )
  }

  # Compute probability-based methods if prob_matrix is available
  if (!is.null(prob_matrix)) {
    if (!is.matrix(prob_matrix) || !is.numeric(prob_matrix)) {
      stop("prob_matrix must be a numeric matrix.")
    }
    if (ncol(prob_matrix) < 2) {
      stop("prob_matrix must have at least two columns (clusters).")
    }

    # Posterior probabilities with PAC (pp_pac)
    pp_pac_crisp <- safe_compute(softSilhouette,
                                 prob_matrix = prob_matrix,
                                 prob_type = "pp",
                                 method = "pac",
                                 average = "crisp",
                                 print.summary = FALSE)

    pp_pac_fuzzy <- safe_compute(softSilhouette,
                                 prob_matrix = prob_matrix,
                                 prob_type = "pp",
                                 method = "pac",
                                 average = "fuzzy",
                                 a = a,
                                 print.summary = FALSE)

    pp_pac_median <- safe_compute(softSilhouette,
                                  prob_matrix = prob_matrix,
                                  prob_type = "pp",
                                  method = "pac",
                                  average = "median",
                                  print.summary = FALSE)

    results[["pp_pac"]] <- list(
      crisp = if (!is.null(pp_pac_crisp)) pp_pac_crisp$avg.width else NA,
      fuzzy = if (!is.null(pp_pac_fuzzy)) pp_pac_fuzzy$avg.width else NA,
      median = if (!is.null(pp_pac_median)) pp_pac_median$avg.width else NA
    )

    # Posterior probabilities with Medoid (pp_medoid)
    pp_medoid_crisp <- safe_compute(softSilhouette,
                                    prob_matrix = prob_matrix,
                                    prob_type = "pp",
                                    method = "medoid",
                                    average = "crisp",
                                    print.summary = FALSE)

    pp_medoid_fuzzy <- safe_compute(softSilhouette,
                                    prob_matrix = prob_matrix,
                                    prob_type = "pp",
                                    method = "medoid",
                                    average = "fuzzy",
                                    a = a,
                                    print.summary = FALSE)

    pp_medoid_median <- safe_compute(softSilhouette,
                                     prob_matrix = prob_matrix,
                                     prob_type = "pp",
                                     method = "medoid",
                                     average = "median",
                                     print.summary = FALSE)

    results[["pp_medoid"]] <- list(
      crisp = if (!is.null(pp_medoid_crisp)) pp_medoid_crisp$avg.width else NA,
      fuzzy = if (!is.null(pp_medoid_fuzzy)) pp_medoid_fuzzy$avg.width else NA,
      median = if (!is.null(pp_medoid_median)) pp_medoid_median$avg.width else NA
    )

    # Negative log posterior probabilities with PAC (nlpp_pac)
    nlpp_pac_crisp <- safe_compute(softSilhouette,
                                   prob_matrix = prob_matrix,
                                   prob_type = "nlpp",
                                   method = "pac",
                                   average = "crisp",
                                   print.summary = FALSE)

    nlpp_pac_fuzzy <- safe_compute(softSilhouette,
                                   prob_matrix = prob_matrix,
                                   prob_type = "nlpp",
                                   method = "pac",
                                   average = "fuzzy",
                                   a = a,
                                   print.summary = FALSE)

    nlpp_pac_median <- safe_compute(softSilhouette,
                                    prob_matrix = prob_matrix,
                                    prob_type = "nlpp",
                                    method = "pac",
                                    average = "median",
                                    print.summary = FALSE)

    results[["nlpp_pac"]] <- list(
      crisp = if (!is.null(nlpp_pac_crisp)) nlpp_pac_crisp$avg.width else NA,
      fuzzy = if (!is.null(nlpp_pac_fuzzy)) nlpp_pac_fuzzy$avg.width else NA,
      median = if (!is.null(nlpp_pac_median)) nlpp_pac_median$avg.width else NA
    )

    # Negative log posterior probabilities with Medoid (nlpp_medoid)
    nlpp_medoid_crisp <- safe_compute(softSilhouette,
                                      prob_matrix = prob_matrix,
                                      prob_type = "nlpp",
                                      method = "medoid",
                                      average = "crisp",
                                      print.summary = FALSE)

    nlpp_medoid_fuzzy <- safe_compute(softSilhouette,
                                      prob_matrix = prob_matrix,
                                      prob_type = "nlpp",
                                      method = "medoid",
                                      average = "fuzzy",
                                      a = a,
                                      print.summary = FALSE)

    nlpp_medoid_median <- safe_compute(softSilhouette,
                                       prob_matrix = prob_matrix,
                                       prob_type = "nlpp",
                                       method = "medoid",
                                       average = "median",
                                       print.summary = FALSE)

    results[["nlpp_medoid"]] <- list(
      crisp = if (!is.null(nlpp_medoid_crisp)) nlpp_medoid_crisp$avg.width else NA,
      fuzzy = if (!is.null(nlpp_medoid_fuzzy)) nlpp_medoid_fuzzy$avg.width else NA,
      median = if (!is.null(nlpp_medoid_median)) nlpp_medoid_median$avg.width else NA
    )

    # Probability distribution with PAC (pd_pac)
    pd_pac_crisp <- safe_compute(softSilhouette,
                                 prob_matrix = prob_matrix,
                                 prob_type = "pd",
                                 method = "pac",
                                 average = "crisp",
                                 print.summary = FALSE)

    pd_pac_fuzzy <- safe_compute(softSilhouette,
                                 prob_matrix = prob_matrix,
                                 prob_type = "pd",
                                 method = "pac",
                                 average = "fuzzy",
                                 a = a,
                                 print.summary = FALSE)

    pd_pac_median <- safe_compute(softSilhouette,
                                  prob_matrix = prob_matrix,
                                  prob_type = "pd",
                                  method = "pac",
                                  average = "median",
                                  print.summary = FALSE)

    results[["pd_pac"]] <- list(
      crisp = if (!is.null(pd_pac_crisp)) pd_pac_crisp$avg.width else NA,
      fuzzy = if (!is.null(pd_pac_fuzzy)) pd_pac_fuzzy$avg.width else NA,
      median = if (!is.null(pd_pac_median)) pd_pac_median$avg.width else NA
    )

    # Probability distribution with Medoid (pd_medoid)
    pd_medoid_crisp <- safe_compute(softSilhouette,
                                    prob_matrix = prob_matrix,
                                    prob_type = "pd",
                                    method = "medoid",
                                    average = "crisp",
                                    print.summary = FALSE)

    pd_medoid_fuzzy <- safe_compute(softSilhouette,
                                    prob_matrix = prob_matrix,
                                    prob_type = "pd",
                                    method = "medoid",
                                    average = "fuzzy",
                                    a = a,
                                    print.summary = FALSE)

    pd_medoid_median <- safe_compute(softSilhouette,
                                     prob_matrix = prob_matrix,
                                     prob_type = "pd",
                                     method = "medoid",
                                     average = "median",
                                     print.summary = FALSE)

    results[["pd_medoid"]] <- list(
      crisp = if (!is.null(pd_medoid_crisp)) pd_medoid_crisp$avg.width else NA,
      fuzzy = if (!is.null(pd_medoid_fuzzy)) pd_medoid_fuzzy$avg.width else NA,
      median = if (!is.null(pd_medoid_median)) pd_medoid_median$avg.width else NA
    )

    # Certainty-based (cer)
    cer_crisp <- safe_compute(cerSilhouette,
                              prob_matrix = prob_matrix,
                              average = "crisp",
                              print.summary = FALSE)

    cer_fuzzy <- safe_compute(cerSilhouette,
                              prob_matrix = prob_matrix,
                              average = "fuzzy",
                              a = a,
                              print.summary = FALSE)

    cer_median <- safe_compute(cerSilhouette,
                               prob_matrix = prob_matrix,
                               average = "median",
                               print.summary = FALSE)

    results[["cer"]] <- list(
      crisp = if (!is.null(cer_crisp)) cer_crisp$avg.width else NA,
      fuzzy = if (!is.null(cer_fuzzy)) cer_fuzzy$avg.width else NA,
      median = if (!is.null(cer_median)) cer_median$avg.width else NA
    )

    # Density-based (db)
    db_crisp <- safe_compute(dbSilhouette,
                             prob_matrix = prob_matrix,
                             average = "crisp",
                             print.summary = FALSE)

    db_fuzzy <- safe_compute(dbSilhouette,
                             prob_matrix = prob_matrix,
                             average = "fuzzy",
                             a = a,
                             print.summary = FALSE)

    db_median <- safe_compute(dbSilhouette,
                              prob_matrix = prob_matrix,
                              average = "median",
                              print.summary = FALSE)

    results[["db"]] <- list(
      crisp = if (!is.null(db_crisp)) db_crisp$avg.width else NA,
      fuzzy = if (!is.null(db_fuzzy)) db_fuzzy$avg.width else NA,
      median = if (!is.null(db_median)) db_median$avg.width else NA
    )
  }

  # Create summary data frame
  if (length(results) == 0) {
    stop("No valid silhouette methods could be computed.")
  }

  method_names <- names(results)
  crisp_values <- sapply(results, function(x) x$crisp)
  fuzzy_values <- sapply(results, function(x) x$fuzzy)
  median_values <- sapply(results, function(x) x$median)

  if(is.null(prob_matrix)){
    summary_df <- data.frame(
      Method = method_names,
      Crisp = crisp_values,
      Median = median_values,
      stringsAsFactors = FALSE,
      row.names = NULL)
  } else {
    summary_df <- data.frame(
      Method = method_names,
      Crisp = crisp_values,
      Fuzzy = fuzzy_values,
      Median = median_values,
      stringsAsFactors = FALSE,
      row.names = NULL
    )
  }

  if (print.summary) {
    cat("\nSummary of All Silhouette Methods\n")
    cat(strrep("=", 42), "\n")
    print(summary_df, row.names = FALSE)
  }

  return(summary_df)
}
