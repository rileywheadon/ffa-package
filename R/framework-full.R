#' Orchestrate the Full FFA Framework
#'
#' Runs the entire flood frequency analysis framework using the results of exploratory 
#' data analysis (EDA) to guide approach selection (stationary or nonstationary) and 
#' perform flood frequency analysis. Returns a comprehensive and reproducible summary 
#' of the results.
#'
#' @inheritParams param-data
#' @inheritParams param-years
#' @inheritParams param-ns-splits
#' @inheritParams param-ns-structures
#' @inheritParams param-generate-report
#' @inheritParams param-report-path
#'
#' @param ... Additional arguments to be passed to the statistical tests and frequency
#' analysis functions. See the details of [framework_eda()] and [framework_ffa()] for a 
#' complete list.
#'
#' @return
#' `recommendations`: See [framework_eda()]. 
#'
#' `summary`: See [framework_ffa()]. 
#'
#' `submodules`: A list of lists of results. Each list contains:
#' - `name`: Either "Change Point Detection", "Trend Detection", "Distribution 
#'   Selection", "Parameter Estimation", "Uncertainty Quantification", or "Model 
#'   Assessment".
#' - `start`: The first year of the homogeneous subperiod.
#' - `end`: The last year of the homogeneous subperiod.
#' - Additional items specific to the the submodule.
#'
#' @seealso [framework_eda()], [framework_ffa()]
#'
#' @export
framework_full <- function(
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
		path <- NULL
	} 

	# Otherwise create an image directory and print a diagnostic message
	else {
		report_dir <- if (is.null(report_path)) tempdir() else report_path 
		img_dir <- paste0(report_dir, "/img")
 		if (!dir.exists(img_dir)) dir.create(img_dir)
		message(paste0("Saving report to '", report_dir, "'"))
	}

	# Get the results of EDA and FFA
	results_eda <- framework_eda(data, years, ns_splits, FALSE)
	results_ffa <- framework_ffa(data, years, ns_splits, ns_structures, FALSE)

	# Combine the results of EDA and FFA into a single list
	results <- list(
		recommendations = results_eda$recommendations,
		summary = results_ffa$summary,
		submodules = c(results_eda$submodules, results_ffa$submodules)
	)

	# Generate the report
	rmarkdown::render(
		system.file("templates", "_master.Rmd", package = "ffaframework"),
		params = c(results, list(title = "Full Framework Report", img_dir = img_dir)),
		output_format = "html_document",
		output_dir = report_dir,
		output_file = "report",
		quiet = TRUE
	)

	return (results)
}
