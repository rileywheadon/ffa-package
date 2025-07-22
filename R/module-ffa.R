#' Conduct Frequency Analysis
#'
#' Performs frequency analysis of annual maximum series (AMS) data including
#' distribution selection, parameter estimation, uncertainty quantification,
#' and model assessment. Supports both stationary frequency analysis (S-FFA)
#' and nonstationary frequency analysis (NS-FFA).
#'
#' @details Additional configuration options:
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
#' @param splits An integer list of years at which to split the data. Use `splits = integer(0)`
#' to avoid splitting the data. Note that `integer(0)` is NOT the same as `NULL`.
#'
#' @param structures An list of structure objects of size `length(splits) + 1`. Each structure
#' object is a list with boolean items `location` and `scale` indicating a trend in the
#' mean/variability respectively.
#'
#' @param ... Additional arguments. See details for a complete list.
#'
#' @return List of blocks containing the results of frequency analysis. Each block contains:
#' - `name`: `"Frequency Analysis"`.
#' - `start`: The first year of the subperiod.
#' - `end`: The first year of the subperiod.
#' - `structure`: The value of the `structures` argument for the subperiod.
#' - `selection`: Results of model selection.
#' - `estimation`: Results of parameter estimation.
#' - `uncertainty`: Results of uncertainty quantification.
#' - `assessment`: Results of model assessment.
#'
#' @seealso [select_ldistance()], [select_lkurtosis()], [select_zstatistic()],
#'   [fit_lmom_fast()], [fit_maximum_likelihood()], [uncertainty_bootstrap()], 
#'   [uncertainty_rfpl()], [model_diagnostics()]
#'
#' @examples 
#' data <- rnorm(n = 100, mean = 100, sd = 10)
#' years <- seq(from = 1901, to = 2000)
#' structures <- list(list(location = TRUE, scale = FALSE))
#' module_ffa(data, years, integer(0), structures)
#'
#' @export

module_frequency_analysis <- function(data, years, splits, structures, ...) {

	# # Set start and end years if not defined
	# if (is.null(start)) start <- min(years)
	# if (is.null(end)) end <- max(years)

	# # Subset data and years based on start and end
	# idx <- which(years >= start & years <= end)
	# data <- data[idx]
	# years <- years[idx]

	# # Compute the decomposed dataset
	# decomposed <- ams_decomposition(data, years, trend)

	# # Define list for storing the results
	# results <- list(start = start, end = end)

	# # Run distribution selection
	# results$selection <- if (options$distribution_selection == "L-distance") {
	# 	select_ldistance(decomposed)
	# } else if (options$distribution_selection == "L-kurtosis") {
	# 	select_lkurtosis(decomposed)
	# } else if (options$distribution_selection == "Z-statistic") {
	# 	select_zstatistic(decomposed, options$z_samples)
	# } else if (options$distribution_selection == "Preset") {
	# 	list(method = "Preset", recommendation = options$distribution_name)
	# }
	
	# # Get the probability model 
	# model <- results$selection$recommendation

	# # Run parameter estimation
	# estimation_method <- if (!trend$location && !trend$scale) {
	# 	options$s_estimation
	# } else {
	# 	options$ns_estimation
	# }

	# results$estimation <- if (estimation_method == "L-moments") {
	# 	fit_lmom_fast(data, model)
	# } else {
	# 	fit_maximum_likelihood(data, model, options$gev_prior, years, trend)
	# } 

	# params <- results$estimation$params

	# # Subset the slices based on the range of the data
	# if (trend$location | trend$scale) {
	# 	slices <- options$slices
	# 	slices <- slices[slices >= start & slices <= end]
	# } else {
	# 	slices <- 1900
	# }

	# # Run uncertainty quantification
	# uncertainty_method <- if (!trend$location && !trend$scale) {
	# 	options$s_uncertainty
	# } else {
	# 	options$ns_uncertainty
	# }

	# results$uncertainty <- if (uncertainty_method == "Bootstrap") {
	# 	uncertainty_bootstrap(
	# 		data,
	# 		model,
	# 		estimation_method,
	# 		prior = options$gev_prior,
	# 		years = years,
	# 		trend = trend,
	# 		slices = slices,
	# 		alpha = options$alpha,
	# 		samples = options$sb_samples,
	# 		periods = options$return_periods
	# 	)
	# } else {
	# 	uncertainty_rfpl(
	# 		data,
	# 		model,
	# 		prior = options$gev_prior,
	# 		years = years,
	# 		trend = trend,
	# 		slices = slices,
	# 		alpha = options$alpha,
	# 		eps = options$rfpl_tolerance,
	# 		periods = options$return_periods
	# 	)
	# }

	# # Run model assessment (S-FFA only)
	# if (!trend$location && !trend$scale) {
	# 	results$assessment <- model_diagnostics(
	# 		data,
	# 		model,
	# 		params,
	# 		results$uncertainty,
	# 		years = years,
	# 		trend = trend,
	# 		alpha = options$alpha,
	# 		pp_formula = options$pp_formula
	# 	)
	# }

	# results

}
