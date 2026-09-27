# Internal helpers shared by the width functions. None of these are exported.

# Take a component (or S4 slot) out of the object a clustering function returned.
# Callers check first that `name` is a single string (see .check_name()).
.extract_component <- function(clust_out, name, arg) {
  if (isS4(clust_out)) {
    if (name %in% methods::slotNames(clust_out)) return(methods::slot(clust_out, name))
    stop("Slot '", name, "' not found in clustering output.")
  }
  if (name %in% names(clust_out)) return(clust_out[[name]])
  stop("Component '", name, "' not found in clustering output.")
}

# When clust_fun is used, matrix arguments name components of its output.
.check_name <- function(x, arg) {
  if (!is.character(x) || length(x) != 1) {
    stop("When clust_fun is not NULL, ", arg, " must be a single string naming a matrix component.")
  }
  invisible(x)
}

# The exponent of the fuzzy weights; a = 0 gives equal weights (the crisp average).
.check_a <- function(a) {
  if (!is.numeric(a) || length(a) != 1 || !is.finite(a) || a < 0) {
    stop("a must be a non-negative numeric value.")
  }
  invisible(a)
}

# Run clust_fun (a function, or the name of one, possibly as "pkg::fun") with the
# arguments in `...` and in the list `clust_args`.
.run_clust_fun <- function(clust_fun, ..., clust_args = list(), envir = parent.frame(2)) {
  if (!(is.function(clust_fun) || (is.character(clust_fun) && length(clust_fun) == 1))) {
    stop("clust_fun must be a function object or a string naming a function (e.g., 'kmeans').")
  }
  if (!is.list(clust_args)) {
    stop("clust_args must be a list of arguments for clust_fun.")
  }
  if (is.character(clust_fun)) {
    parts <- strsplit(clust_fun, ":::?")[[1]]
    fun <- if (length(parts) == 2) {
      if (!requireNamespace(parts[1], quietly = TRUE)) NULL
      else get0(parts[2], envir = asNamespace(parts[1]), mode = "function")
    } else {
      get0(clust_fun, envir = envir, mode = "function")
    }
    if (is.null(fun)) stop("Function '", clust_fun, "' not found")
    clust_fun <- fun
  }
  tryCatch(do.call(clust_fun, c(list(...), clust_args)),
           error = function(e) stop("Error in clustering function: ", conditionMessage(e), call. = FALSE))
}

# Checks on a proximity matrix: numeric matrix, two or more columns, no missing
# values. +Inf is allowed (for example -log(0) from prob_type = "nlpp").
.check_prox <- function(prox_matrix, name = "prox_matrix") {
  if (!is.matrix(prox_matrix) || !is.numeric(prox_matrix)) {
    stop(name, " must be a numeric matrix.")
  }
  if (ncol(prox_matrix) < 2) {
    stop(name, " must have at least two columns (clusters).")
  }
  if (anyNA(prox_matrix)) {
    stop(name, " must not contain missing values (NA or NaN).")
  }
  if (any(prox_matrix == -Inf)) {
    stop(name, " must not contain -Inf.")
  }
  if (any(prox_matrix < 0)) {
    stop(name, " must not contain negative values: the widths assume non-negative ",
         "proximities. Transform them first, for example (1 + r) / 2 for correlations.",
         call. = FALSE)
  }
  invisible(prox_matrix)
}

# Checks on a membership matrix: numeric, two or more columns, no missing or
# negative values, rows summing to 1 within sqrt(.Machine$double.eps).
.check_prob <- function(prob_matrix, name = "prob_matrix") {
  if (!is.matrix(prob_matrix) || !is.numeric(prob_matrix)) {
    stop(name, " must be a numeric matrix.")
  }
  if (ncol(prob_matrix) < 2) {
    stop(name, " must have at least two columns (clusters).")
  }
  if (anyNA(prob_matrix)) {
    stop(name, " must not contain missing values (NA or NaN).")
  }
  if (any(prob_matrix < 0)) {
    stop(name, " must not contain negative values.")
  }
  if (any(abs(rowSums(prob_matrix) - 1) > .Machine$double.eps^0.5)) {
    stop("Each row of ", name, " must sum to 1 (tolerance sqrt(.Machine$double.eps)).")
  }
  invisible(prob_matrix)
}

# Validate user-supplied cluster labels: column numbers 1..K, one per row.
.check_cluster <- function(cluster, n, K) {
  if (is.factor(cluster)) {
    stop("cluster must contain column numbers of the matrix, not a factor. ",
         "Use as.integer() if the factor levels are 1, ..., ", K, ".")
  }
  if (!is.numeric(cluster) || length(cluster) != n) {
    stop("cluster must be a numeric vector with one entry per row of the matrix (", n, ").")
  }
  if (anyNA(cluster) || any(cluster != round(cluster)) || any(cluster < 1 | cluster > K)) {
    stop("cluster must contain whole numbers between 1 and ", K,
         " (the column numbers of the matrix).")
  }
  as.integer(cluster)
}

# Column of the closest cluster in each row. Exact ties go to the first column.
.closest <- function(mat, larger_is_closer) {
  sel <- .finite_for_selection(mat)
  if (larger_is_closer) max.col(sel, ties.method = "first") else max.col(-sel, ties.method = "first")
}

# Column of the closest cluster other than `cluster` in each row.
.closest_other <- function(mat, cluster, larger_is_closer) {
  sel <- .finite_for_selection(mat)
  idx <- cbind(seq_len(nrow(mat)), cluster)
  if (larger_is_closer) {
    sel[idx] <- -Inf
    max.col(sel, ties.method = "first")
  } else {
    sel[idx] <- Inf
    max.col(-sel, ties.method = "first")
  }
}

# Replace infinite entries by the largest finite doubles so that a masked entry
# (set to +/-Inf) can never tie with them when selecting columns.
.finite_for_selection <- function(mat) {
  big <- .Machine$double.xmax
  mat[mat == Inf] <- big
  mat[mat == -Inf] <- -big
  mat
}

# Silhouette widths from the proximities to the assigned (a) and neighboring (b)
# clusters. Equal proximities give 0; an infinite proximity gives +1 or -1.
.width_from_pair <- function(a, b, larger_is_closer, method) {
  if (larger_is_closer) {
    num <- a - b
  } else {
    num <- b - a
  }
  den <- if (method == "pac") a + b else pmax(a, b)
  w <- num / den
  w[a == b] <- 0
  if (larger_is_closer) {
    w[is.infinite(a) & is.finite(b)] <- 1
    w[is.infinite(b) & is.finite(a)] <- -1
  } else {
    w[is.infinite(b) & is.finite(a)] <- 1
    w[is.infinite(a) & is.finite(b)] <- -1
  }
  w
}

# Fuzzy weights of Campello and Hruschka (2006): the gap between the two largest
# memberships of each observation, raised to the power a. They do not depend on
# which cluster the observation is assigned to.
.fuzzy_weights <- function(prob_matrix, a) {
  i <- seq_len(nrow(prob_matrix))
  c1 <- max.col(prob_matrix, ties.method = "first")
  masked <- prob_matrix
  masked[cbind(i, c1)] <- -Inf
  c2 <- max.col(masked, ties.method = "first")
  (prob_matrix[cbind(i, c1)] - prob_matrix[cbind(i, c2)])^a
}

# Assemble the "Silhouette" data frame and its attributes, optionally sorted.
.new_silhouette <- function(cluster, neighbor, sil_width, weight = NULL,
                            proximity_type, method, average, sort = FALSE) {
  if (is.null(weight)) {
    widths <- data.frame(cluster = cluster, neighbor = neighbor, sil_width = sil_width)
  } else {
    widths <- data.frame(cluster = cluster, neighbor = neighbor, weight = weight, sil_width = sil_width)
  }
  if (sort) {
    ord <- order(widths$cluster, -widths$sil_width)
    widths <- widths[ord, , drop = FALSE]
  }
  attr(widths, "proximity_type") <- proximity_type
  attr(widths, "method") <- method
  attr(widths, "average") <- average
  structure(widths, class = c("Silhouette", "data.frame"))
}

# Cluster-wise and overall averages under the object's averaging rule.
.silhouette_summary <- function(widths) {
  average <- attr(widths, "average")
  if (average == "crisp") {
    clus.avg.widths <- tapply(widths$sil_width, widths$cluster, mean, na.rm = TRUE)
    avg.width <- mean(widths$sil_width, na.rm = TRUE)
  } else if (average == "fuzzy") {
    sil_weight <- widths$weight * widths$sil_width
    clus.avg.widths <- tapply(seq_along(widths$sil_width), widths$cluster, function(idx) {
      sum(sil_weight[idx], na.rm = TRUE) / sum(widths$weight[idx], na.rm = TRUE)
    })
    total_weight <- sum(widths$weight, na.rm = TRUE)
    if (total_weight == 0) {
      warning("All fuzzy weights are zero (every observation has two equal largest ",
              "memberships), so the fuzzy average is undefined.", call. = FALSE)
    }
    avg.width <- sum(sil_weight, na.rm = TRUE) / total_weight
  } else {
    clus.avg.widths <- tapply(widths$sil_width, widths$cluster, stats::median, na.rm = TRUE)
    avg.width <- stats::median(widths$sil_width, na.rm = TRUE)
  }
  n <- table(widths$cluster)
  sil.sum <- data.frame(cluster = names(clus.avg.widths),
                        size = as.vector(n[names(clus.avg.widths)]),
                        avg.sil.width = round(as.vector(clus.avg.widths), 4))
  list(clus.avg.widths = clus.avg.widths, avg.width = avg.width, sil.sum = sil.sum)
}

# Report a summary through message(), as the R Journal asks of non-print functions.
.report_silhouette <- function(widths, stats) {
  average <- attr(widths, "average")
  proximity_type <- attr(widths, "proximity_type")
  method <- attr(widths, "method")
  header <- switch(average,
                   crisp = sprintf("Average crisp %s %s silhouette: %.4f", proximity_type, method, stats$avg.width),
                   fuzzy = sprintf("Average fuzzy %s %s silhouette: %.4f", proximity_type, method, stats$avg.width),
                   median = sprintf("Median %s %s silhouette: %.4f", proximity_type, method, stats$avg.width))
  rule <- strrep("-", nchar(header))
  table <- paste(utils::capture.output(print(stats$sil.sum, row.names = FALSE)), collapse = "\n")
  message(rule, "\n", header, "\n", rule, "\n\n", table, "\n")
  invisible(NULL)
}
