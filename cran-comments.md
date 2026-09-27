## Release summary

Version 0.9.7 adds an argument `cluster` to score a partition supplied by the user,
makes `dbSilhouette()` robust to zero probabilities, adds weights to `extSilhouette()`,
aligns the soft silhouettes with Bhat Kapu and Kiruthika (2024), and fixes several bugs
(see NEWS.md).

## R CMD check results

0 errors | 0 warnings | 0 notes

## Test environments

* Ubuntu 24.04, R 4.3.3: `R CMD check --as-cran` (the only notes came from the check
  machine having no network access, so URLs and the clock could not be verified)
* GitHub Actions (R-CMD-check.yaml): Ubuntu, Windows and macOS with R oldrel, release
  and devel [confirm after pushing]
* win-builder, R-devel [run `devtools::check_win_devel()` before submitting]

## Reverse dependencies

There are no reverse dependencies on CRAN.
