#' Orchestrate Flood Frequency Analysis
#'
#' Performs frequency analysis of annual maximum series (AMS) data including
#' distribution selection, parameter estimation, uncertainty quantification,
#' and model assessment. Supports both stationary frequency analysis (S-FFA)
#' and nonstationary frequency analysis (NS-FFA).
#'
#' @section Optional Arguments:
#'
#' - `selection`: Distribution selection method (default is `"L-distance"`). Must be one of 
#'   `"L-distance"`, `"L-kurtosis"` or `"Z-statistic"`. Alternatively, set `selection` 
#'   to a three-letter distribution code (eg. `"GUM"`) to use a specific distribution.
#' - `s_estimation`: Parameter estimation method for S-FFA (default is `"L-moments"`). Must be 
#'   `"L-moments"`, `"MLE"`, or `"GMLE"`. Method `"GMLE"` requires `selection = "GEV"`.
#' - `ns_estimation`: Parameter estimation method for NS-FFA (default is `"MLE"`). Must be 
#'    `"MLE"` or `"GMLE"`. Method `"GMLE"` requires `selection = "GEV"`.
#' - `s_uncertainty`: Uncertainty quantification method for S-FFA (default is `"Bootstrap"`).
#'   Must be one of `"Bootstrap"`, `"RFPL"`, or `RFGPL"`. Using method `"RFPL"` requires 
#'   `s_estimation = "MLE"` and method `"RFGPL"` requires `s_estimation = "GMLE"`.
#' - `ns_uncertainty`: Uncertainty quantification method for NS-FFA (default is `"RFPL"`).
#'   Must be one of `"Bootstrap"`, `"RFPL"`, or `RFGPL"`. Using method `"RFPL"` requires 
#'   `ns_estimation = "MLE"` and method `"RFGPL"` requires `ns_estimation = "GMLE"`.
#' - `z_samples`: Integer number of bootstrap samples for selection method `"Z-statistic"` 
#'   (default is 10000).
#' - `gev_prior`: Parameters for the prior distribution of the shape parameter of the GEV 
#'   distribution (default is 6, 9). Used with estimation method `"GMLE"`.
#' - `return_periods`: Integer list of return periods (in years) for estimating return levels.
#'   Uses the 2, 5, 10, 20, 50, and 100 year return periods by default.
#' - `slices`: Integer vector of years at which to estimate the return levels for nonstationary
#'   models. Slices outside of the period will be ignored (default is 1900, 1950, 2000).
#' - `sb_samples`: Integer number of samples for uncertainty quantification method `"Bootstrap"`
#'   (default is 10000).
#' - `rfpl_tolerance`: Log-likelihood tolerance for uncertainty quantification method `"RFPL"`
#'   (default is 0.01).
#' - `pp_formula`: Plotting position formula for model assessment. Must be one of:
#'   - "Weibull" (default): \eqn{i / (n + 1)}
#'   - "Blom": \eqn{(i - 0.375) / (n + 0.25)}
#'   - "Cunnane": \eqn{(i - 0.4) / (n + 0.2)}
#'   - "Gringorten": \eqn{(i - 0.44) / (n + 0.12)}
#'   - "Hazen": \eqn{(i - 0.5) / n}
#'
#' @inheritParams param-data
#' @inheritParams param-years
#'
#' @param ns_splits An integer list of years at which to split the data. Use `splits = integer(0)`
#' to avoid splitting the data. Note that `integer(0)` is NOT the same as `NULL`.
#'
#' @param ns_structures An list of structure objects of size `length(splits) + 1`. Each 
#' structure object is a list with boolean items `location` and `scale` indicating a trend 
#' in the mean/variability respectively.
#'
#' @param ... Additional arguments. See the "Optional Arguments" section for a complete list.
#'
#' @return A list of submodules containing the results of frequency analysis. Each list contains
#' a `name`, which is either `"Distribution Selection"`, `"Parameter Estimation"`,  `"Uncertainty 
#' Quantification"`, or `"Model Assessment"`. Then, the submodule will contain a sublist of
#' the results for each subperiod. 
#'
#' @seealso [select_ldistance()], [select_lkurtosis()], [select_zstatistic()],
#'   [fit_lmoments()], [fit_mle()], [fit_gmle()], [uncertainty_bootstrap()], 
#'   [uncertainty_rfpl()], [uncertainty_rfgpl()], [model_assessment()]
#'
#' @export
framework_ffa <- function(data, years, ns_splits = NULL, ns_structures = NULL) {
	NULL
}
