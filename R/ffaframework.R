#' Flood Frequency Analysis Framework
#'
#' Runs the complete flood frequency analysis framework including exploratory data 
#' analysis (change point detection and trend detection/classification), approach 
#' selection, and frequency analysis (distribution selection, parameter estimation, 
#' uncertainty quantification, and model assessment). Returns a comprehensive and 
#' reproducible summary of the results.
#'
#' @inheritParams param-data
#' @inheritParams param-years
#'
#' @param splits An integer vector of years at which to split the data (default is `NULL`).
#' A split point is the *first* year in a subperiod.
#'
#' @param structures An list of structure objects of size `length(splits) + 1` (default is `NULL`).
#' Each structure object is a list with boolean items `location` and `scale` indicating a trend 
#' in the mean/variability respectively.
#' 
#' @param automatic If `TRUE`, the split points and nonstationary structures are chosen
#' automatically using the results of EDA (default is `FALSE`). This argument is ignored
#' if both `splits` and `structures` are not `NULL`.
#'
#' @param path An absolute path to a directory for storing the results (default is `NULL`). 
#' If path is `NULL`, images and reports will be exported to `tempdir()`.
#'
#' @param formats List of file formats to export the results (default is `"html"`). The 
#' supported formats are `"html"`, `"pdf"`, `"json"`, and `"md"` (markdown).
#' 
#' @param ... Additional arguments. See the note below for more information. 
#'
#' @details If `automatic = FALSE` and either `splits` and/or `structures` is `NULL`, the 
#' user will be prompted to choose split points and/or nonstationary structures manually. 
#' However, this is not possible if R is running in non-interactive mode, so the function 
#' will return an error instead.
#'
#' To avoid this issue, the user has three options:
#' 1. Pass `splits` and `structures` arguments to [ffaframework()].
#' 2. Run R in interactive mode using RStudio or the R console.
#' 3. Use `automatic = TRUE`.
#'
#' @return
#' `summary`: A list containing high-level information about the results:
#' - `periods`: A list of length two vectors containing the start/end of the subperiods.
#' - `structures`: A list of nonstationary structure objects with size `length(periods) + 1`.
#'
#' `blocks`: A list of results for each module. Each block is a list containing:
#' - `name`: Either `"Change Points"`, `"Trend Detection"`, or `"Frequency Analysis"`.
#' - `period`: The two-element vector containing the start/end of the subperiod.
#' - `items`: A list of additional items (e.g. statistical tests) depending on the module.
#'
#' @section EDA Settings:
#'
#' - `alpha = 0.05`: The numeric significance level for all statistical tests.
#' - `bbmk_samples = 10000L`: The number of bootstrap samples used by the Block-Bootstrap 
#'   Mann-Kendall (BBMK) test. 
#'
#' @section FFA Settings:
#'
#' Model Selection:
#'
#' - `selection = "L-distance"`: Distribution selection method. Must be one of 
#'   `"L-distance"`, `"L-kurtosis"` or `"Z-statistic"`. Alternatively, set `selection` 
#'   to a three-letter distribution code (eg. `"GUM"`) to use a specific distribution.
#' - `z_samples = 10000L`: Number of samples for `"Z-statistic"` distribution selection.
#' - `window_size = 10L`: The size of the moving window used to estimate the variability.
#' - `window_step = 5L`: The distance between moving windows for variability estimation.
#'
#' Parameter Estimation:
#'
#' - `s_estimation = "L-moments"`: Parameter estimation method for S-FFA. Must be 
#'   `"L-moments"`, `"MLE"`, or `"GMLE"`. Method `"GMLE"` requires `selection = "GEV"`.
#' - `ns_estimation = "MLE"`: Parameter estimation method for NS-FFA. Must be 
#'    `"MLE"` or `"GMLE"`. Method `"GMLE"` requires `selection = "GEV"`.
#' - `gev_prior = c(6, 9)`: Parameters for the prior distribution of the shape parameter 
#'   of the GEV distribution. Used with estimation method `"GMLE"`.
#'
#' Uncertainty Quantification:
#' 
#' - `s_uncertainty = "Bootstrap"`: Uncertainty quantification method for S-FFA.
#'   Must be one of `"Bootstrap"`, `"RFPL"`, or `RFGPL"`. Using method `"RFPL"` requires 
#'   `s_estimation = "MLE"` and method `"RFGPL"` requires `s_estimation = "GMLE"`.
#' - `ns_uncertainty = "RFPL"`: Uncertainty quantification method for NS-FFA.
#'   Must be one of `"Bootstrap"`, `"RFPL"`, or `RFGPL"`. Method `"RFPL"` requires 
#'   `ns_estimation = "MLE"` and method `"RFGPL"` requires `ns_estimation = "GMLE"`.
#' - `return_periods = c(2L, 5L, 10L, 20L, 50L, 100L)`: Integer list of return periods 
#'   (in years) at which to estimate return levels during uncertainty quantification.
#' - `slices = c(1925L, 1975L, 2025L)`: Integer vector of years at which to estimate the 
#'   return levels for nonstationary models. Slices outside of the period will be ignored.
#' - `sb_samples = 1000L`: Number of samples for `"Bootstrap"` uncertainty method.
#' - `rfpl_tolerance = 0.01`: Log-likelihood tolerance for `"RFPL"` uncertainty method.
#'
#' Model Assessment:
#' 
#' - `pp_formula = "Weibull"`: Plotting position formula for model assessment. Must be one 
#'   of `"Weibull"`, `"Blom"`, `"Cunnane"`, `"Gringorten"`, or `"Hazen"` (see 
#'   \href{https://rileywheadon.github.io/ffa-docs/model-assessment/}{here}).
#'
#' @examples 
#' data <- rnorm(n = 100, mean = 100, sd = 10)
#' years <- seq(from = 1901, to = 2000)
#' structures <- list(list(location = FALSE, scale = FALSE))
#' ffaframework(data, years, splits = numeric(0), structures = structures, formats = NULL)
#'
#' @importFrom glue glue
#' @importFrom jsonlite write_json 
#' @export

ffaframework <- function(
	data,
	years,
	splits = NULL,
	structures = NULL,
	automatic = FALSE,
	path = NULL,
	formats = "html",
	...
) {

	# Define paths for exporting results
	report_dir <- if (is.null(path)) tempdir() else path 
	img_dir <- glue("{report_dir}/img")
 	if (!dir.exists(img_dir)) dir.create(img_dir)
	message(glue("Saving results to '{report_dir}'"))

	# Capture optional arguments
	args <- list(...)

	# Generate a list of options
	options <- list(
		alpha = args$alpha %||% 0.05,
		bbmk_samples = args$bbmk_samples %||% 10000L,
		window_size = args$window_size %||% 10L,
		window_step = args$window_step %||% 5L,
		selection = args$selection %||% "L-distance",
		z_samples = args$z_samples %||% 10000L,
		s_estimation = args$s_estimation %||% "L-moments",
		ns_estimation = args$ns_estimation %||% "MLE",
		gev_prior = args$gev_perior %||% c(6, 9),
		s_uncertainty = args$s_uncertainty %||% "Bootstrap",
		ns_uncertainty = args$ns_uncertainty %||% "RFPL",
		return_periods = args$return_periods %||% c(2L, 5L, 10L, 20L, 50L, 100L),
		slices = args$slices %||% c(1925L, 1975L, 2025L),
		sb_samples = args$sb_samples %||% 1000L,
		rfpl_tolerance = args$rfpl_tolerance %||% 0.01,
		pp_formula = args$pp_formula %||% "Weibull"
	)

	# Validate arguments
	config <- validate_config(options)

	# Initialize output objects "summary" and "blocks"
	summary <- list()
	blocks <- list()

	# Add a block containing the results of change point detection
	change_results <- change_point_detection(data, years, options, img_dir) 
	blocks[[length(blocks) + 1]] <- change_results

	# Determine the split points if not set manually by the user
	if (is.null(splits)) {
		if (automatic) {
			splits <- splits_automatic(change_results)	
		} else {
			splits <- splits_manual(change_results)	
		}
	}

	# Update the summary with the periods
	starts <- c(min(years), splits)
	ends <- c(splits - 1, max(years))
	periods <- Map(c, starts, ends)
	summary$splits <- splits
	summary$periods <- periods

	# Create a temporary list for storing structures
	temp <- list()

	# Iterate through the homogeneous periods
	for (period in periods) {

		# Run trend detection and add to results
		trend_results <- trend_detection(
			data,
			years,
			period,
			options,
			img_dir
		)

		blocks[[length(blocks) + 1]] <- trend_results

		if (is.null(structures)) {
			if (automatic) {
				structure <- structures_automatic(trend_results)	
			} else {
				structure <- structures_manual(trend_results)	
			}

			temp[[length(temp) + 1]] <- structure
		}

	}

	# Set the stationary or nonstationary structures
	structures <- if (is.null(structures)) temp else structures 
	summary$structures <- structures

    # Run frequency analysis on each homogeneous period
	for (i in seq_along(periods)) {

		# Run frequency analysis and add to results
		ffa_results <- tryCatch(
			frequency_analysis(
				data, 
				years,
				periods[[i]],
				structures[[i]],
				options,
				img_dir
			),
			error = function(e) stop(conditionMessage(e))
		)

		# Append the FFA results to the output object
		blocks[[length(blocks) + 1]] <- ffa_results 
	}

	# Print informational message
	message("\n\nFFA Complete.\n")

	# Iterate through report options
	results <- list(
		summary = summary,
		blocks = blocks,
		img_dir = img_dir
	)

	for (format in formats) {
		if (format == "json") {
			write_json(
				results, 
				path = glue("{report_dir}/report.json"),
				pretty = TRUE,
				auto_unbox = TRUE
			)
		} else {
			rmarkdown::render(
				system.file("templates", "_master.Rmd", package = "ffaframework"),
				params = results,
				output_format = glue("{format}_document"),
				output_dir = report_dir,
				output_file = "report",
				quiet = TRUE
			)
		} 
	}

	# Return the results
	list(summary = summary, blocks = blocks)
}

# Function for validating a configuration file
validate_config <- function(config) {

	# Load the configuration file 
    keys <- names(config)

	# Specify the types and lengths for each option
	specification <- list(
        alpha = list(type = "numeric", size = 1),
        bbmk_samples = list(type = "integer", size = 1),
		window_size = list(type = "integer", size = 1),
		window_step = list(type = "integer", size = 1),
		selection = list(type = "character", size = 1),
        z_samples = list(type = "integer", size = 1),
        s_estimation = list(type = "character", size = 1),
        ns_estimation = list(type = "character", size = 1),
        gev_prior = list(type = "numeric", size = 2),
        s_uncertainty = list(type = "character", size = 1),
        ns_uncertainty = list(type = "character", size = 1),
        return_periods = list(type = "integer"),
        slices = list(type = "integer"),
        sb_samples = list(type = "integer", size = 1),
        rfpl_tolerance = list(type = "numeric", size = 1),
        pp_formula = list(type = "character", size = 1)
    )

	# Values, ranges, and logical constraints
	distributions <- c("GUM", "NOR", "LNO", "GEV", "GLO", "GNO", "PE3", "LP3", "WEI")

	constraints <- list(
		alpha = c(0.01, 0.1),
        bbmk_samples = c(0, Inf),
		window_size = c(0, Inf),
		window_step = c(0, Inf),
		selection = c("L-distance", "L-kurtosis", "Z-statistic", distributions),
		z_samples = c(0, Inf),
		s_estimation = c("L-moments", "MLE", "GMLE"),
		ns_estimation = c("MLE", "GMLE"),
		gev_prior = c(0, Inf),
		s_uncertainty = c("Bootstrap", "RFPL", "RFGPL"),
		ns_uncertainty = c("Bootstrap", "RFPL", "RFGPL"),
		return_periods = c(2, Inf),
		sb_samples = c(0, Inf),
		pp_formula = c("Weibull", "Blom", "Cunnane", "Gringorten", "Hazen")
	)

    # Check for unknown fields
    unknown_keys <- setdiff(keys, names(specification))
    if (length(unknown_keys) > 0) {
		unknown_text <- paste(sQuote(unknown_keys), collapse = ", ")
		stop(glue("Unknown options: {unknown_text}"))
	}

    # Validate each option in config
    for (key in names(config)) {

        value <- config[[key]]
        rule <- specification[[key]]
		constraint <- constraints[[key]]

		# Validate types
        if (!any(vapply(rule$type, function(t) inherits(value, t), logical(1)))) {
			observed <- class(value)
			expected <- paste(rule$type, collapse = '/')
        	stop(glue("Invalid type for '{key}': expected {expected} but got {observed}"))
        }

		# Validate lengths
        if (!is.null(rule$size) && all(length(value) != rule$size)) {
            stop(glue("'{key}' must have length {rule$size}, got {length(value)}"))
        }

		# Validate character constraints
		if (!is.null(constraint) && is.character(constraint)) {
			if (!all(value %in% constraint)) {
				valid_entries <- paste(sQuote(constraint), collapse = ", ")
				stop(glue("'{key}' must be one of {valid_entries}"))
			}
		}

		# Validate functional constraints
        if (!is.null(constraint) && is.numeric(constraint)) {
			if (!all(value >= constraint[1]) || !all(value <= constraint[2])) {
				stop(glue("'{key}' must be within ({constraint[1]}, {constraint[2]})"))
			}
        }

    }

    # Cross-field checks for GMLE parameter estimation
    if (config$s_estimation == "GMLE" && config$selection != "GEV") {
    	stop(glue("s_estimation: 'GMLE' requires selection: 'GEV'"))
    }

    if (config$ns_estimation == "GMLE" && config$selection != "GEV") {
   	 	stop(glue("ns_estimation: 'GMLE' requires selection: 'GEV'"))
    }

	# Cross-field checks for RFPL uncertainty quantification
    if (config$s_uncertainty == "RFPL" && config$s_estimation != "MLE") {
        stop("s_uncertainty: 'RFPL' requires s_estimation: 'MLE'")
    }

	if (config$ns_uncertainty == "RFPL" && config$ns_estimation != "MLE") {
        stop("ns_uncertainty: 'RFPL' requires ns_estimation: 'MLE'")
    }

	# Cross-field checks for RFGPL uncertainty quantification
    if (config$s_uncertainty == "RFGPL" && config$s_estimation != "GMLE") {
        stop("s_uncertainty: 'RFGPL' requires s_estimation: 'GMLE'")
    }

    if (config$ns_uncertainty == "RFGPL" && config$ns_estimation != "GMLE") {
        stop("ns_uncertainty: 'RFGPL' requires ns_estimation: 'GMLE'")
    }

    config

} 

# Helper function for saving images
save_plot <- function(name, plot, period, dir) {
	path <- glue("{dir}/{name}_{period[1]}_{period[2]}.png")
 	ggsave(path, plot = plot, height = 8, width = 10, dpi = 300)
}

# Change point detection function
change_point_detection <- function(data, years, options, img_dir) {

	# Run the Pettitt and MKS tests
	pettitt <- eda_pettitt_test(data, years, options$alpha)
	mks <- eda_mks_test(data, years, options$alpha)

	# Generate and save the plots
	period <- c(min(years), max(years))
	save_plot("pettitt", plot_pettitt_test(pettitt), period, img_dir)
	save_plot("mks", plot_mks_test(mks), period, img_dir)

	# Return the results as a list
	list(
		name = "Change Points",
		period = period,
		items = list(pettitt = pettitt, mks = mks)
	)

}

# Helper function for automatic splitting
splits_automatic <- function(change_results) {

	pettitt <- change_results$items$pettitt
	mks <- change_results$items$mks

	# If there are no change points return empty vector
	if (!pettitt$reject && !mks$reject) {
		return (numeric(0))	
	} 

	# Otherwise prioritize based on p-value
	if (pettitt$p_value < mks$p_value) {
		return (pettitt$change_year)
	} else {
		return (mks$change_df$year) 
	}

}

# Helper function for manual splitting
splits_manual <- function(change_results) {

	# If R is not running in interactive mode, throw an error 
	if (!interactive()) stop("R must be in interactive mode for manual split point selection.")

	pettitt <- change_results$items$pettitt
	mks <- change_results$items$mks

	# If there are no change points return empty vector
	if (!pettitt$reject && !mks$reject) {
		cat("\nNo change points found.\n")
		return (numeric(0))	
	} 

	cat("\nPettitt Test Results:")
	cat(pettitt$msg)
	cat("\n\nMKS Test Results:")
	cat(mks$msg)

	# Helper function to get user input
	get_user_input <- function(n) {

		cat("\n\nEnter a number: ")
		option <- readLines(file("stdin"), 1)
		regex <- paste0("[1-", as.character(n), "]$")

		if (!grepl(regex, option)) {
			cat("\nInvalid input. Please try again.")
			get_user_input(n)
		} 

		option

	}

	# Print options for the user
	cat("\n\nPlease select an option:")
	cat("\n 1. Reject change points.")

	# Only the Pettitt test found change points
	if (pettitt$reject && !mks$reject) {
		cat("\n 2. Accept change points from the Pettitt test.")
		option <- get_user_input(2)
		if (option == 2) return (pettitt$change_year)
	} 

	# Only the MKS test found change points
	else if (!pettitt$reject && mks$reject) {
		cat("\n 2. Accept change points from the MKS test.")
		option <- get_user_input(2)
		if (option == 2) return (mks$change_df$year)
	}

	# Both tests found chang epoints
	else {
		cat("\n 2. Accept change points from the Pettitt test.")
		cat("\n 3. Accept change points from the MKS test.")
		option <- get_user_input(3)
		if (option == 2) return (pettitt$change_year)
		if (option == 3) return (mks$change_df$year)
	}

	numeric(0)

}

# Trend detection function
trend_detection <- function(data, years, period, options, img_dir) {

	# Subset data and years based on period
	idx <- which(years >= period[1] & years <= period[2])
	data <- data[idx]
	years <- years[idx]

	# Define list for storing the results
	items <- list()

	# White (1): go to MW-MK (2) regardless of the result
	trend01 <- function() {
		items$white <<- eda_white_test(data, years, options$alpha)
		return (2)
	}

	# MW-MK (2): go to Sen's variance (3) if there is non-stationarity, MK test (5) if not.
	trend02 <- function() {
		mw <- data_mw_variability(data, years, options$window_size, options$window_step)
		items$mwmk <<- eda_mk_test(mw$std, options$alpha)
		if (items$white$reject || items$mwmk$reject) 3 else 5
	}

	# Sen's variance (3): go to Runs variance (4) regardless of the result
	trend03 <- function() {
		mw <- data_mw_variability(data, years, options$window_size, options$window_step)
		items$sens_variance <<- eda_sens_trend(mw$std, mw$year)
		plot <- plot_sens_trend(mw$std, mw$year, items$sens_variance)
		save_plot("sens_variance", plot, period, img_dir)
		return (4)	
	}

	# Runs variance (4): go to MK test (5) regardless of the results.
	trend04 <- function() {
		items$runs_variance <<- eda_runs_test(items$sens_variance, options$alpha)
		plot <- plot_runs_test(items$runs_variance)
		save_plot("runs_variance", plot, period, img_dir)
		return (5)	
	}

	# MK (5): go to Spearman (6) if there is a trend, end (NULL) if not.
	trend05 <- function() {
		items$mk <<- eda_mk_test(data, options$alpha)
		if (items$mk$reject) 6 else NULL
	} 

	# Spearman (6): go to BB-MK (7) if there is serial correlation, Sen's means (10) if not.
	trend06 <- function() {
		items$spearman <<- eda_spearman_test(data, options$alpha)
		plot <- plot_spearman_test(items$spearman)
		save_plot("spearman", plot, period, img_dir)
		if (items$spearman$reject) 7 else 10
	} 

	# BB-MK (7): go to PP (8) if there is a trend, end (NULL) if not.
	trend07 <- function() {
		items$bbmk <<- eda_bbmk_test(data, options$alpha, options$bbmk_samples)
		plot <- plot_bbmk_test(items$bbmk)
		save_plot("bbmk", plot, period, img_dir)
		if (items$bbmk$reject) 8 else NULL
	} 

	# PP (8): go to KPSS (9) regardless of the result
	trend08 <- function() {
		items$pp <<- eda_pp_test(data, options$alpha)
		return (9)
	}

	# KPSS (9): go to Sen's (10) regardless of the result
	trend09 <- function() {
		items$kpss <<- eda_kpss_test(data, options$alpha)
		return (10)
	}

	# Sen's means (10): go to Runs means (11) regardless of the result
	trend10 <- function() {
		items$sens_mean <<- eda_sens_trend(data, years)
		plot <- plot_sens_trend(data, years, items$sens_mean, items$sens_variance)
		save_plot("sens_mean", plot, period, img_dir)
		return (11)
	}

	# Runs means (11): go to end (NULL) regardless of the result
	trend11 <- function() {
		items$runs_mean <<- eda_runs_test(items$sens_mean, options$alpha)
		plot <- plot_runs_test(items$runs_mean)
		save_plot("runs_mean", plot, period, img_dir)
		return (NULL)
	}

	# Iterate through the flowchart
	location <- 1
	while (!is.null(location)) {
		fname <- sprintf("trend%02d", location)
		location <- get(fname)()
	} 

	# Return the results
	list(
		name = "Trend Detection",
		period = period,
		items = items
	)

}

# Helper function for automatic trend selection
structures_automatic <- function(trend_results) {
	trend <- list()
	items <- trend_results$items
	trend$location <- ("sens_mean" %in% names(items))
	trend$scale <- ("sens_variance" %in% names(items))
	trend
}

# Helper function for manual trend selection
structures_manual <- function(trend_results) {

	# If R is not running in interactive mode, throw an error 
	if (!interactive()) stop("R must be in interactive mode for manual structure selection.")

	# Unpack the results
	items <- trend_results$items
	period <- trend_results$period

	# Set the recommended option
	rec <- 1 
	if ("sens_mean" %in% names(items)) rec <- rec + 1
	if ("sens_variance" %in% names(items)) rec <- rec + 2

	period_text <- glue("{period[1]}-{period[2]}")
	cat(glue("\n\nChoose a structure for period {period_text}:"))
	cat("\n 1. No trend.")
	cat("\n 2. Trend in mean.")
	cat("\n 3. Trend in variability.")
	cat("\n 4. Trend in mean and variability.")

	# Helper function to get user input
	get_user_input <- function() {

		cat(glue("\n\n\nSelect an option ({rec} recommended): "))
		option <- readLines(file("stdin"), 1)

		if (!grepl("[1-4]$", option)) {
			cat("\nInvalid input. Please try again.")
			get_user_input()
		} 

		option

	}

	option <- get_user_input()

	# Return the structure corresponding to the selected option
	switch(
		option,
		"1" = list(location = FALSE, scale = FALSE),
		"2" = list(location = TRUE, scale = FALSE),
		"3" = list(location = FALSE, scale = TRUE),
		"4" = list(location = TRUE, scale = TRUE)
	)

}

frequency_analysis <- function(data, years, period, structure, options, img_dir) {

	# Subset data and years based on period
	idx <- which(years >= period[1] & years <= period[2])
	data <- data[idx]
	years <- years[idx]

	# Compute the decomposed dataset
	decomposed <- data_decomposition(data, years, structure)

	# Define list for storing the results
	items <- list()

	# Run distribution selection
	items$selection <- if (options$selection == "L-distance") {
		select_ldistance(decomposed)
	} else if (options$selection == "L-kurtosis") {
		select_lkurtosis(decomposed)
	} else if (options$selection == "Z-statistic") {
		select_zstatistic(decomposed, options$z_samples)
	} else {
		list(method = "Preset", recommendation = options$selection)
	}

	# Generate L-moments plot
	if (options$selection %in% c("L-distance", "L-kurtosis", "Z-statistic")) {
		pdf(nullfile())
		plot <- plot_lmom_diagram(items$selection)
		save_plot("selection", plot, period, img_dir)
	}
	
	# Get the probability model 
	model <- items$selection$recommendation

	# Run parameter estimation
	estimation_method <- if (!structure$location && !structure$scale) {
		options$s_estimation
	} else {
		options$ns_estimation
	}

	items$estimation <- if (estimation_method == "L-moments") {
		fit_lmom_fast(data, model)
	} else if (estimation_method == "MLE") {
		fit_maximum_likelihood(data, model, NULL, years, structure)
	} else {
		fit_maximum_likelihood(data, model, options$gev_prior, years, structure)
	}

	params <- items$estimation$params

	# Run uncertainty quantification
	if (!structure$location && !structure$scale) {
		slices <- 1900
		uncertainty_method <- options$s_uncertainty
	} else {
		slices <- options$slices
		slices <- slices[slices >= period[1] & slices <= period[2]]
		uncertainty_method <- options$ns_uncertainty
	}

	items$uncertainty <- if (uncertainty_method == "Bootstrap") {
		uncertainty_bootstrap(
			data,
			model,
			estimation_method,
			prior = options$gev_prior,
			years = years,
			structure = structure,
			slices = options$slices,
			alpha = options$alpha,
			samples = options$sb_samples,
			periods = options$return_periods
		)
	} else {
		uncertainty_rfpl(
			data,
			model,
			prior = if (uncertainty_method == "RFPL") NULL else options$gev_prior,
			years = years,
			structure = structure,
			slices = options$slices,
			alpha = options$alpha,
			eps = options$rfpl_tolerance,
			periods = options$return_periods
		)
	}

	# Generate uncertainty quantificaiton plot
	if (!structure$location && !structure$scale) {
		plot <- plot_sffa(items$uncertainty)
	} else {
		plot <- plot_nsffa(items$uncertainty)
	}

	save_plot("uncertainty", plot, period, img_dir)

	# Run model assessment (S-FFA only)
	if (!structure$location && !structure$scale) {
		items$assessment <- model_diagnostics(
			data,
			model,
			params,
			items$uncertainty,
			years = years,
			structure = structure,
			alpha = options$alpha,
			pp_formula = options$pp_formula
		)

		plot <- plot_model_diagnostics(items$assessment)
		save_plot("assessment", plot, period, img_dir)
	}

	list(
		name = "Frequency Analysis",
		period = period,
		structure = structure,
		items = items
	)

}
