# Benchmark: time and memory of silhouette computations as n grows.
# Each timed expression includes the construction of its own proximity
# input, so the comparison is end-to-end from data + partition to widths.
#
# Usage: Rscript benchmark.R <lib_for_Silhouette> <label> <outfile>
#   lib_for_Silhouette: library path holding the Silhouette version to test
#                       ("default" = normal library path)

args <- commandArgs(trailingOnly = TRUE)
lib <- if (length(args) >= 1) args[1] else "default"
label <- if (length(args) >= 2) args[2] else "0.9.7"
outfile <- if (length(args) >= 3) args[3] else "bench_results.rds"
full_max_n <- if (length(args) >= 4) as.numeric(args[4]) else 20000

if (lib != "default") .libPaths(c(lib, .libPaths()))
suppressPackageStartupMessages({
  library(Silhouette)
  library(cluster)
  library(bench)
})
stopifnot(packageVersion("Silhouette") == label)

here <- dirname(normalizePath(sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE))))
source(file.path(here, "bench_data.R"))   # K, p and make_data()
ns <- c(1000, 2000, 5000, 10000, 20000, 25000, 50000, 1e5, 2e5)

res <- list()
for (n in ns) {
  dat <- make_data(n, seed = n)
  X <- dat$X; cl <- dat$cl; C <- dat$centers; U <- dat$U
  iters <- if (n <= 10000) 10 else 3

  exprs <- list(
    simplified_crisp = quote(
      summary(Silhouette(proxy::dist(X, C)), print.summary = FALSE)$avg.width),
    simplified_fuzzy = quote(
      summary(Silhouette(proxy::dist(X, C), prob_matrix = U,
                         average = "fuzzy"),
              print.summary = FALSE)$avg.width),
    soft_nlpp = quote(
      summary(softSilhouette(U, prob_type = "nlpp"),
              print.summary = FALSE)$avg.width)
  )
  if (label == "0.9.7" && requireNamespace("bluster", quietly = TRUE)) {
    exprs$approx_bluster <- quote(
      mean(bluster::approxSilhouette(X, cl)$width))
  }
  if (label == "0.9.7" && n <= full_max_n) {
    exprs$rousseeuw_cluster <- quote({
      D <- stats::dist(X); summary(cluster::silhouette(cl, D))$avg.width })
  }
  if (label == "0.9.7" && n <= min(full_max_n, 10000) &&
      requireNamespace("fclust", quietly = TRUE)) {
    exprs$fuzzy_fclust <- quote(fclust::SIL.F(X, U, alpha = 2))
  }

  for (nm in names(exprs)) {
    gc()
    b <- tryCatch(
      bench::mark(eval(exprs[[nm]]), iterations = iters,
                  check = FALSE, memory = TRUE, filter_gc = FALSE),
      error = function(e) NULL)
    if (is.null(b)) {
      row <- data.frame(version = label, n = n, method = nm,
                        median_s = NA, mem_MB = NA, iters = NA)
    } else {
      row <- data.frame(version = label, n = n, method = nm,
                        median_s = as.numeric(b$median),
                        mem_MB = as.numeric(b$mem_alloc) / 1024^2,
                        iters = b$n_itr)
    }
    print(row)
    res[[length(res) + 1]] <- row
  }
  saveRDS(do.call(rbind, res), outfile)
}
out <- do.call(rbind, res)

# Probes in fresh R processes: elapsed time and added peak resident memory for
# one end-to-end computation, including the classical silhouette above the
# in-process limit, where it may fail for lack of memory.
if (label == "0.9.7") {
  probe_ns <- c(1000, 5000, 10000, 25000, 50000, 1e5, 2e5)
  pr <- list()
  for (n in probe_ns) for (m in c("classical", "silhouette", "bluster")) {
    o <- suppressWarnings(system2("timeout", c("1200", "Rscript", file.path(here, "probe.R"), m, n),
                                  stdout = TRUE, stderr = FALSE))
    st <- attr(o, "status")
    line <- tail(grep(paste0("^", m, ","), o, value = TRUE), 1)
    if (length(line) == 1) {
      f <- strsplit(line, ",", fixed = TRUE)[[1]]
      row <- data.frame(method = m, n = n, status = f[3], elapsed_s = as.numeric(f[4]),
                        peak_MB = as.numeric(f[5]), baseline_MB = as.numeric(f[6]),
                        message = if (length(f) >= 7) f[7] else "")
    } else {
      row <- data.frame(method = m, n = n, status = if (identical(st, 124L)) "timeout" else "killed",
                        elapsed_s = NA, peak_MB = NA, baseline_MB = NA,
                        message = paste("exit status", if (is.null(st)) NA else st))
    }
    print(row)
    pr[[length(pr) + 1]] <- row
    saveRDS(do.call(rbind, pr), sub("\\.rds$", "_probe.rds", outfile))
  }
}
attr(out, "session") <- list(R = R.version.string,
                             cpu = system("lscpu | grep 'Model name'", intern = TRUE),
                             ncores = parallel::detectCores(),
                             mem = system("free -g | grep Mem", intern = TRUE),
                             Silhouette = as.character(packageVersion("Silhouette")),
                             cluster = as.character(packageVersion("cluster")),
                             proxy = as.character(packageVersion("proxy")),
                             bluster = if (requireNamespace("bluster", quietly = TRUE)) as.character(packageVersion("bluster")) else NA)
saveRDS(out, outfile)
