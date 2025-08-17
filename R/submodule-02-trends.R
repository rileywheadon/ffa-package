# Helper function for trend detection
submodule_02 <- function(
	data,
	years,
	options,
	splits,
	path = NULL,
	serialize = FALSE
) {

	# Get the homogeneous periods as (start, end) pairs
	periods <- splits_helper(splits, years)

	# Return the results as a list of lists (one for each period)
	lapply(periods, function(period) { 
		submodule_02_helper(data, years, options, period, path, serialize) 
	})

}

submodule_02_helper <- function(data, years, options, period, path, serialize) {

	# Subset data and years based on period
	idx <- which(years >= period[1] & years <= period[2])
	data <- data[idx]
	years <- years[idx]

	# Initialize a list of tests
	tests <- list()

	# Define helper function for writing and serializing plots
	plot_helper <- function(plot, name) {
		if (!is.null(path)) {
			write_plot(plot, path, name, period)
		}

		if (serialize) {
			tests[[name]]$plot <<- serialize_plot(plot) 
		}
	}

	# White (1): go to MW-MK (2) regardless of the result
	trend01 <- function() {
		tests$white <<- eda_white_test(data, years, options$alpha)
		return (2)
	}

	# MW-MK (2): go to Sen's variance (3) if there is non-stationarity, else MK test (5)
	trend02 <- function() {
		mw <- data_mw_variability(data, years, options$window_size, options$window_step)
		tests$mwmk <<- eda_mk_test(mw$std, options$alpha)
		if (tests$white$reject || tests$mwmk$reject) 3 else 5
	}

	# Sen's variance (3): go to Runs variance (4) regardless of the result
	trend03 <- function() {
		mw <- data_mw_variability(data, years, options$window_size, options$window_step)
		tests$sens_variance <<- eda_sens_trend(mw$std, mw$year)

		plot <- plot_ams_data(
			mw$std,
			mw$year,
			"Trend",
			title = "Sen's Trend Estimator (Variability)"
		)

		plot_helper(plot, "sens_variance")
		return (4)	
	}

	# Runs variance (4): go to MK test (5) regardless of the results.
	trend04 <- function() {
		mw <- data_mw_variability(data, years, options$window_size, options$window_step)
		residuals <- tests$sens_variance$residuals
		tests$runs_variance <<- eda_runs_test(residuals, mw$year, options$alpha)
		plot_helper(plot_runs_test(tests$runs_variance), "runs_variance")
		return (5)	
	}

	# MK (5): go to Spearman (6) if there is a trend, end (NULL) if not.
	trend05 <- function() {
		tests$mk <<- eda_mk_test(data, options$alpha)
		if (tests$mk$reject) 6 else NULL
	} 

	# Spearman (6): go to BB-MK (7) if there is serial correlation, else Sen's means (10)
	trend06 <- function() {
		tests$spearman <<- eda_spearman_test(data, options$alpha)
		plot_helper(plot_spearman_test(tests$spearman), "spearman")
		if (tests$spearman$reject) 7 else 10
	} 

	# BB-MK (7): go to PP (8) if there is a trend, end (NULL) if not.
	trend07 <- function() {
		tests$bbmk <<- eda_bbmk_test(data, options$alpha, options$bbmk_samples)
		plot_helper(plot_bbmk_test(tests$bbmk), "bbmk")
		if (tests$bbmk$reject) 8 else NULL
	} 

	# PP (8): go to KPSS (9) regardless of the result
	trend08 <- function() {
		tests$pp <<- eda_pp_test(data, options$alpha)
		return (9)
	}

	# KPSS (9): go to Sen's (10) regardless of the result
	trend09 <- function() {
		tests$kpss <<- eda_kpss_test(data, options$alpha)
		return (10)
	}

	# Sen's means (10): go to Runs means (11) regardless of the result
	trend10 <- function() {
		tests$sens_mean <<- eda_sens_trend(data, years)

		plot <- plot_ams_data(
			data,
			years,
			"Trend",
			if ("sens_variance" %in% names(tests)) "Trend" else "Constant",
			title = "Sen's Trend Estimator (Mean)"
		)

		plot_helper(plot, "sens_mean")
		return (11)
	}

	# Runs means (11): go to end (NULL) regardless of the result
	trend11 <- function() {
		residuals <- tests$sens_mean$residuals
		tests$runs_mean <<- eda_runs_test(residuals, years, options$alpha)
		plot_helper(plot_runs_test(tests$runs_mean), "runs_mean")
		return (NULL)
	}

	# Iterate through the flowchart to get the test tests
	location <- 1
	while (!is.null(location)) {
		fname <- sprintf("trend%02d", location)
		location <- get(fname)()
	} 

	# Return the results
	list(
		name = "Trend Detection",
		start = period[1], 
		end = period[2], 
		tests = tests
	)

}

