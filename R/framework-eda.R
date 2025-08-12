#' Orchestrate Exploratory Data Analysis
#'
#' First, this method identifies change points in the original annual maximum series 
#' data. Then, the user is given the option to split the dataset into two or more 
#' homogenous subperiods (trend-free or with monotonic trends). Finally, this method 
#' performs a collection of statistical tests for identifying monotonic nonstationarity 
#' in the mean and variability of each subperiod. The results of EDA can help guide 
#' FFA approach selection (stationary or nonstationary) and FFA model determination.
#'
#' @inheritParams param-data
#' @inheritParams param-years
#' @inheritParams param-ns-splits
#' @inheritParams param-generate-report
#' @inheritParams param-report-path
#'
#' @param ... Additional arguments. See the "Optional Arguments" section for a 
#' complete list.
#'
#' @section Optional Arguments:
#' - `alpha`: The numeric significance level for all statistical tests (default is 0.05).
#' - `bbmk_samples`: The number of samples used in the Block-Bootstrap Mann-Kendall 
#'   (BBMK) test (default is 10000). Must be an integer.
#'
#' @return 
#' `recommendations`: A list containing the recommended FFA approach, split point(s) 
#' and nonstationary structure(s) from EDA:
#' - `approach`: Either "S-FFA", "NS-FFA" (for a single homogeneous period), or 
#'   "Piecewise NS-FFA" (for multiple homogeneous subperiods).
#' - `ns_splits`: The split point(s) identified by the change point detection test with 
#'   the the lowest statistically significant p-value, or `NULL` if no such point exists.
#' - `ns_structures`: A list of structure objects for each homogeneous subperiod. Each 
#'   structure is a list with boolean items `location` and `scale`, which represent a 
#'   linear trend in the in the mean or variability of the data, respectively. If no
#'   trends were found in any homogeneous subperiod, `ns_structures` will be `NULL`.
#'
#' `submodules`: A list of lists of statistical tests. Each list contains:
#' - `name`: Either "Change Point Detection" or "Trend Detection".
#' - `start`: The first year of the homogeneous subperiod.
#' - `end`: The last year of the homogeneous subperiod.
#' - Additional items from the statistical tests within the submodule.
#'
#' @seealso [eda_pettitt_test()], [eda_mks_test()], [eda_mk_test()], 
#' [eda_spearman_test()], [eda_bbmk_test()], [eda_pp_test()], [eda_kpss_test()], 
#' [eda_sens_trend()], [eda_runs_test()], [eda_white_test()]
#'
#' @export
framework_eda <- function(
	data,
	years,
	ns_splits = NULL,
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

	# Get the results of EDA
	results_01 <- submodule_01(data, years, options, img_dir)
	results_02 <- submodule_02(data, years, options, ns_splits, img_dir)

	# Determine the recommended split points
	pettitt <- results_01[[1]]$tests$pettitt
	mks <- results_01[[1]]$tests$mks

	if (!pettitt$reject && !mks$reject) {
		recommended_splits <- numeric(0)
	} 

	if (pettitt$p_value < mks$p_value) {
		recommended_splits <- pettitt$change_points$year
	} else {
		recommended_splits <- mks$change_points$year
	}

	# Determine the recommended nonstationary structures
	recommended_structures <- vector("list", length(results_02))

	for (i in seq_along(results_02)) {
		test_names <- names(results_02[[i]]$tests)
		recommended_structures[[i]] <- list(
			location = ("sens_mean" %in% test_names),
			scale = ("sens_variance" %in% test_names)
		)
	}

	# Determine the recommended approach
	if (length(recommended_splits) == 0) {

		structure <- recommended_structures[[1]]
		if (!structure$location && !structure$scale) {
			approach <- "S-FFA"
		} else {
			approach <- "NS-FFA"
		}

	} else {
		approach <- "Piecewise NS-FFA"
	}

	# Define the recommendations
	recommendations <- list(
		approach = approach,
		ns_splits = recommended_splits,
		ns_structures = recommended_structures
	)

	# Combine the results of EDA into a single list
	results <- list(
		recommendations = recommendations,
		submodules = c(results_01, results_02)
	)

	if (generate_report) {
		rmarkdown::render(
			system.file("templates", "_master.Rmd", package = "ffaframework"),
			params = c(results, list(title = "EDA Report", img_dir = img_dir)),
			output_format = "html_document",
			output_dir = report_dir,
			output_file = "report",
			quiet = TRUE
		)
	}

	# Return the results
	return (results)
}

