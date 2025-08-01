#' Flood Frequency Analysis Framework
#'
#' This package provides tools for stationary (S-FFA) and nonstationary (NS-FFA) 
#' flood frequency analysis of annual maximum series data. The package supports 
#' flexible workflows with the following core components:
#' 
#' 1. **Test** for evidence of nonstationarity in annual maximum series data to 
#'    inform approach selection (S-FFA or NS-FFA) using the `eda_*` methods:
#' 	  - Detect statistically significant change points.
#' 	  - Detect statistically significant temporal trends in the *mean* and *variance*.
#' 2. **Select** a suitable probability distribution using the `select_* ` methods.
#' 3. **Fit** model parameters using the `fit_*` methods.
#' 4. **Quantify** uncertainty using the `uncertainty_*` methods.
#' 5. **Evaluate** model performance using `model_diagnostics()`.
#' 
#' Additional utility functions support visualization and computation:
#' 
#' - `plot_*` methods produce diagnostic and summary plots.
#' - `data_*` methods load, transform, and decompose annual maximum series data.
#' - `quantile_*`, `loglik_*`, and `lmom_*` implement distribution-specific computations.
#' 
#' This package assumes familiarity with statistical techniques used in FFA, including 
#' L-moments, maximum likelihood estimation, and parametric bootstrap. 
#' For an explanation of these methods, see the 
#' [FFA Framework wiki](https://rileywheadon.github.io/ffa-docs/).
#' For examples, see the vignettes on exploratory data analysis and flood frequency 
#' analysis.
#'
#' @name ffaframework-package
#'
#' @keywords package
"_PACKAGE"

## usethis namespace: start
## usethis namespace: end
NULL
