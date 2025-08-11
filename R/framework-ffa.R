#' Orchestrate Flood Frequency Analysis
#'
#' Performs frequency analysis of annual maximum series data including distribution 
#' selection, parameter estimation, uncertainty quantification, and model assessment. 
#' Supports both stationary (S-FFA) or nonstationary (NS-FFA) flood frequency analysis.
#'
#' @inheritParams param-data
#' @inheritParams param-years
#'
#' @param ns_splits An integer vector of years used to split the data into homogeneous
#' subperiods. For S-FFA, set to `NULL` (default). For NS-FFA, specify an integer vector 
#' of years (e.g., `1950L`) with physical justification for change points, or `NULL` 
#' if no such years exist.
#'
#' @param ns_structures For S-FFA, set to `NULL` (default) to use a stationary model 
#' for all homogeneous subperiods. For NS-FFA, provide a list of `length(ns_splits) + 1` 
#' sublists specifying the nonstationary model structure for each homogeneous subperiod. 
#' Each sublist must contain logical elements `location` and `scale`, indicating 
#' monotonic trends in the mean and variability, respectively. 
#'
#' @param ... Additional arguments. See the "Optional Arguments" section for a complete 
#' list.
#'
#' @section Optional Arguments:
#'
#' - `selection`: Distribution selection method (default is `"L-distance"`). Must be one of 
#'   `"L-distance"`, `"L-kurtosis"` or `"Z-statistic"`. Alternatively, set `selection` 
#'   to a three-letter distribution code (e.g., `"GUM"`) to use a specific distribution.
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
#' @return 
#' `summary`: A list describing the model(s) used for the analysis.
#' - `approach`: Either "S-FFA", "NS-FFA", or "Piecewise NS-FFA".
#' - `ns_splits`: The `ns_splits` argument, if given.
#' - `ns_structures`: The `ns_structures` argument, if given. 
#'
#' `submodules`: A list of lists of containing the results of frequency analysis. 
#' Each list contains:
#' - `name`: Either "Distribution Selection", "Parameter Estimation", "Uncertainty 
#'   Quantification", or "Model Assessment".
#' - `start`: The first year of the homogeneous subperiod.
#' - `end`: The last year of the homogeneous subperiod.
#' - Additional items specific to the the submodule.
#'
#' @seealso [select_ldistance()], [select_lkurtosis()], [select_zstatistic()],
#'   [fit_lmoments()], [fit_mle()], [fit_gmle()], [uncertainty_bootstrap()], 
#'   [uncertainty_rfpl()], [uncertainty_rfgpl()], [model_assessment()]
#'
#' @export
framework_ffa <- function(data, years, ns_splits = NULL, ns_structures = NULL) {
	NULL
}
