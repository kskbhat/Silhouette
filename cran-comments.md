## Test environments

- Local R installation: R 4.4.0 on Windows 11
- R-hub: Windows, macOS, and Linux platforms (via GitHub Actions and standalone checks)
- win-builder: R-release and R-devel

## R CMD check results

- No ERRORs, WARNINGs, or NOTEs on all tested environments.

## Downstream dependencies

- No known reverse dependencies.

## Comments

This update improves compatibility with R 4.4.0 and maintains cross-platform support. All examples, unit tests (testthat), and vignettes build and run successfully. Continuous integration via R-hub and GitHub Actions confirms package stability on Windows, macOS, and Linux. No breaking changes were introduced.
