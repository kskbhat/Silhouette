# Probe: run one end-to-end silhouette computation in a fresh R process and
# report its elapsed time and the peak resident memory of the process during
# the computation. Used for the classical silhouette above the in-process
# limit, where the computation may fail for lack of memory, and to measure
# peak memory for every method.
#
# Usage: Rscript probe.R <method> <n>
#   method: classical | silhouette | bluster
# Prints one line: method,n,status,elapsed_s,added_peak_MB,baseline_MB,message

args <- commandArgs(trailingOnly = TRUE)
method <- args[1]
n <- as.numeric(args[2])
here <- dirname(normalizePath(sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE))))
source(file.path(here, "bench_data.R"))
suppressPackageStartupMessages({
  library(Silhouette)
  library(cluster)
  library(proxy)
  if (method == "bluster") library(bluster)
})

dat <- make_data(n, seed = n)
X <- dat$X; cl <- dat$cl; C <- dat$centers
rm(dat)
invisible(gc())

mem_mb <- function(field) {
  s <- readLines("/proc/self/status")
  as.numeric(gsub("[^0-9]", "", grep(paste0("^", field), s, value = TRUE))) / 1024
}

expr <- switch(method,
  classical  = quote({ D <- stats::dist(X); summary(cluster::silhouette(cl, D))$avg.width }),
  silhouette = quote(summary(Silhouette(proxy::dist(X, C)), print.summary = FALSE)$avg.width),
  bluster    = quote(mean(bluster::approxSilhouette(X, cl)$width)))

# warm-up on a small subset so that one-off loading costs are not timed
local({ X <- X[1:50, ]; cl <- cl[1:50]; invisible(eval(expr)) })
invisible(gc())
writeLines("5", "/proc/self/clear_refs")   # reset the peak-memory mark
base <- mem_mb("VmRSS")

t0 <- proc.time()[["elapsed"]]
res <- tryCatch({ eval(expr); "ok" }, error = function(e) conditionMessage(e))
el <- proc.time()[["elapsed"]] - t0
status <- if (identical(res, "ok")) "ok" else "error"
msg <- if (status == "ok") "" else gsub(",", ";", res)
cat(sprintf("%s,%d,%s,%.4f,%.1f,%.1f,%s\n", method, as.integer(n), status, el,
            mem_mb("VmHWM") - base, base, msg))
