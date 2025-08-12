#' Orchestrate Flood Frequency Analysis
#'
#' Performs frequency analysis of annual maximum series data including distribution 
#' selection, parameter estimation, uncertainty quantification, and model assessment. 
#' Supports both stationary (S-FFA) or nonstationary (NS-FFA) flood frequency analysis.
#'
#' @inheritParams param-data
#' @inheritParams param-years
#' @inheritParams param-ns-splits
#' @inheritParams param-ns-structures
#' @inheritParams param-generate-report
#' @inheritParams param-report-path
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
framework_ffa <- function(
	data,
	years,
	ns_splits = NULL,
	ns_structures = NULL,
	generate_report = TRUE,
	report_path = NULL,
	...
) {

	# Get the configuration options
	args <- list(...)
	config <- generate_config(args)
	options <- validate_config(config)

	# Set path to NULL if generate_report is FALSE
	if (!generate_report) { 
		img_dir <- NULL
	} 

	# Otherwise create an image directory and print a diagnostic message
	else {
		report_dir <- if (is.null(report_path)) tempdir() else report_path 
		img_dir <- paste0(report_dir, "/img")
 		if (!dir.exists(img_dir)) dir.create(img_dir)
		message(paste0("Saving report to '", report_dir, "'"))
	}

	# Run distribution selection, get distributions list
	results_03 <- submodule_03(
		data,
		years,
		options,
		ns_splits,
		ns_structures, 
		img_dir
	)

	distributions <- vapply(
		results_03,
		function(x) x$selection$recommendation,
		character(1)
	)

	# Run parameter estimation and uncertainty quantification, get confidence intervals
	results_04 <- submodule_04(
		data,
		years,
		distributions,
		options,
		ns_splits,
		ns_structures,
		img_dir
	)

	results_05 <- submodule_05(
		data,
		years,
		distributions,
		options,
		ns_splits,
		ns_structures,
		img_dir
	)

	intervals <- lapply(
		results_05,
		function(x) if("ci" %in% names(x$uncertainty)) x$uncertainty$ci else NULL
	)

	# Run model assessment
	results_06 <- submodule_06(
		data,
		years,
		distributions,
		intervals,
		options,
		ns_splits,
		ns_structures,
		img_dir
	)

	# Determine the approach used
	if (length(ns_splits) == 0) {

		if (is.null(ns_structures)) {
			approach <- "S-FFA"
		} else {
			approach <- "NS-FFA"
		}

	} else {
		approach <- "Piecewise NS-FFA"
	}

	# Generate the summary
	summary <- list(
		approach = approach,
		ns_splits = if (is.null(ns_splits)) integer(0) else ns_splits,
		ns_structures = structures_helper(ns_structures, rep(1, length(ns_splits) + 1))
	)

	# Combine the results of EDA into a single list
	results <- list(
		summary = summary,
		submodules = c(results_03, results_04, results_05, results_06)
	)

	if (generate_report) {
		rmarkdown::render(
			system.file("templates", "_master.Rmd", package = "ffaframework"),
			params = c(results, list(title = "FFA Report", img_dir = img_dir)),
			output_format = "html_document",
			output_dir = report_dir,
			output_file = "report",
			quiet = TRUE
		)
	}

	# Return the results
	return (results)
}
