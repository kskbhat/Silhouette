# Benchmark and agreement scripts

These scripts produce the cached results in `vignettes/articles/data/`, which the
website article "Performance and comparison with other packages"
(`vignettes/articles/benchmarks.Rmd`) reads. The folder is excluded from the
CRAN build by `.Rbuildignore`.

| Script | Produces | What it does |
|:---|:---|:---|
| `bench_data.R` | (sourced) | Simulated data: five-dimensional Gaussian mixture with K = 5 components, the k-means partition and fuzzy c-means memberships (m = 2). |
| `benchmark.R` | `bench_097.rds`, `bench_097_probe.rds` (or `bench_096.rds`) | Median time with **bench** for n from 1,000 to 200,000, end to end (each method builds its own input). With version 0.9.7 it also runs `probe.R` in fresh R processes to record peak memory and the failures of the classical silhouette. |
| `probe.R` | (called by `benchmark.R`) | One computation in a fresh R process; prints elapsed time and added peak resident memory (Linux only: reads `/proc/self/status`). |
| `agreement.R` | `agreement_results.rds` | 50 **MixSim** data sets for each of six settings; compares the classical, simplified (medoid and PAC) and `bluster::approxSilhouette()` widths for k-means with K = 2 to 8. |
| `agreement_extra.R` | `agreement_extra.rds` | The same comparison for three further settings: unequal cluster sizes, p = 10, and 5% outliers. |
| `check_versions.R` | (an `.rds` file named on the command line) | Widths, neighbors and weights from one Silhouette version on the benchmark data, to check that versions agree. |

## Requirements

Silhouette, cluster, proxy, bench, fclust, MixSim, and the Bioconductor package
bluster. The memory probes use `/proc`, so they need Linux.

## Running

Run from this folder. Each script writes its results to the working directory;
copy them to `vignettes/articles/data/` afterwards.

```sh
cd data-raw/benchmarks
# Silhouette 0.9.7 from the normal library; classical silhouette timed in-process
# up to n = 10,000 (larger sizes are attempted by the fresh-process probes).
# SILHOUETTE_SOURCE records the tested source (e.g. the git commit) with the results.
SILHOUETTE_SOURCE="commit $(git rev-parse --short HEAD)" Rscript benchmark.R default 0.9.7 bench_097.rds 10000
# Silhouette 0.9.6 installed in a separate library
Rscript benchmark.R /path/to/lib-0.9.6 0.9.6 bench_096.rds
Rscript agreement.R
Rscript agreement_extra.R
cp bench_097.rds bench_097_probe.rds bench_096.rds agreement_results.rds agreement_extra.rds ../../vignettes/articles/data/
```

The cached results were produced with R 4.3.3 on Ubuntu 24.04 (reference BLAS and
LAPACK), on a two-core Intel Xeon virtual machine at 2.80 GHz with 8 GB of memory.
The machine details are stored in `attr(readRDS("bench_097.rds"), "session")`, and
`sessionInfo()`, the BLAS and LAPACK libraries and the tested source in the
`"session_info"` and `"environment"` attributes.
