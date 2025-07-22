#' Conduct Exploratory Data Analysis
#'
#' Identifies change points in annual maximum series (AMS) data. Allows the user to split 
#' the dataset at one or more years. Finally, performs a collection of statistical tests 
#' for identifying nonstationarity in the mean/variability of each subperiod.
#'
#' @details The behaviour of the `splits` and `automatic` arguments can be confusing:
#' - If `splits = NULL` and `automatic = TRUE`, the data will be split automatically based on 
#'   the change point detection test with the lowest statistically significant p-value. 
#'   If neither test yielded a statistically significant p-value, the data will not be split.
#' - If `splits = NULL` and `automatic = FALSE`, the user will be prompted to select the split
#'   points. If R is not running in interactive mode, the function will return the results of
#'   change point detection along with an error message.
#' - If splits is not `NULL`, it will be used to split the data. To avoid splitting the data, 
#'   use `splits = integer(0)`. Note that `integer(0)` is NOT the same as `NULL`.
#'
#' In addition to the required arguments, `module_eda` also accepts:
#' - `alpha`: The numeric significance level for all statistical tests (default is 0.05).
#' - `bbmk_samples`: The number of samples used in the Block-Bootstrap Mann-Kendall (BBMK) 
#'   test (default is 10000). Must be an integer.
#'
#' @inheritParams param-data
#' @inheritParams param-years
#'
#' @param splits An integer vector of years at which to split the data (default is `NULL`).
#' A split point is the *first* year in a subperiod.
#'
#' @param automatic If `TRUE`, the data is split automatically using the results of the Pettitt 
#' and MKS tests (default is `FALSE`). This argument is ignored if `splits` is not `NULL`.
#'
#' @param ... Additional arguments to be passed to the statistical tests. See details.
#'
#' @return 
#' `recommendations`: A list of recommended split points and nonstationary structures:
#' - `splits`: The change point(s) identified by the change point detection test with the
#'   the lowest statistically significant p-value, or an empty vector if no such test exists.
#' - `structures`: A list of nonstationary structure objects for each subperiod. Each structure
#'   is a list with boolean items `location` and `scale` which represent a linear trend in the
#'   in the mean or variability of the data respectively.
#'
#' `blocks`: A list of grouped statistical tests. Each block is a list containing:
#' - `name`: Either `"Change Points"` or `"Trend Detection"`.
#' - `start`: The first year of the subperiod.
#' - `end`: The last year of the subperiod.
#' - Additional lists corresponding to the individual statistical tests within the block.
#'
#' @seealso [eda_pettitt_test()], [eda_mks_test()], [eda_mk_test()], [eda_spearman_test()]
#' [eda_bbmk_test()], [eda_pp_test()], [eda_kpss_test()], [eda_sens_trend()], 
#' [eda_runs_test()], [eda_white_test()]
#'
#' @examples 
#' data <- rnorm(n = 100, mean = 100, sd = 10)
#' years <- seq(from = 1901, to = 2000)
#' module_eda(data, years, automatic = TRUE)
#'
#' @export

module_eda <- function(data, years, splits = NULL, automatic = FALSE, ...) {
	NULL
}

# # Set partition if not defined
# if (is.null(partition)) partition <- c(min(years), max(years))

# # Subset data and years based on the partition
# idx <- which(years >= partition[1] & years <= partition[2])
# data <- data[idx]
# years <- years[idx]

# # Run the Pettitt and MKS tests
# pettitt_results <- eda_pettitt_test(data, years, options$alpha)
# mks_results <- eda_mks_test(data, years, options$alpha)

# # Return the results as a list
# list(partition = partition, pettitt = pettitt_results, mks = mks_results)


# # Trend detection function
# trend_detection <- function(data, years, options, start = NULL, end = NULL) {

# 	# Set start and end years if not defined
# 	if (is.null(start)) start <- min(years)
# 	if (is.null(end)) end <- max(years)

# 	# Subset data and years based on start and end
# 	idx <- which(years >= start & years <= end)
# 	data <- data[idx]
# 	years <- years[idx]

# 	# Define list for storing the results
# 	results <- list(start = start, end = end)

# 	# MK (1): go to Spearman (2) if there is a trend, White (8) if not.
# 	trend01 <- function() {
# 		results$mk <<- eda_mk_test(data, options$alpha)
# 		if (results$mk$reject) 2 else 8
# 	} 

# 	# Spearman (2): go to BB-MK (3) if there is serial correlation, Sen's means (6) if not.
# 	trend02 <- function() {
# 		results$spearman <<- eda_spearman_test(data, options$alpha)
# 		if (results$spearman$reject) 3 else 6
# 	} 

# 	# BB-MK (3): go to PP (4) if there is a trend, White (8) if not.
# 	trend03 <- function() {
# 		results$bbmk <<- eda_bbmk_test(data, options$alpha, options$bbmk_samples)
# 		if (results$bbmk$reject) 4 else 8
# 	} 

# 	# PP (4): go to KPSS (5) regardless of the result
# 	trend04 <- function() {
# 		results$pp <<- eda_pp_test(data, options$alpha)
# 		return (5)
# 	}

# 	# KPSS (5): go to Sen's (6) regardless of the result
# 	trend05 <- function() {
# 		results$kpss <<- eda_kpss_test(data, options$alpha)
# 		return (6)
# 	}

# 	# Sen's means (6): go to Runs means (7) regardless of the result
# 	trend06 <- function() {
# 		results$sens_mean <<- eda_sens_trend(data, years)
# 		return (7)
# 	}

# 	# Runs means (7): go to White (8) regardless of the result
# 	trend07 <- function() {
# 		results$runs_mean <<- eda_runs_test(results$sens_mean, options$alpha)
# 		return (8)
# 	}

# 	# White (8): go to MW-MK (9) regardless of the result
# 	trend08 <- function() {
# 		results$white <<- eda_white_test(data, years, options$alpha)
# 		return (9)
# 	}

# 	# MW-MK (9): go to Sen's variance (10) if there is non-stationarity, end (NULL) if not.
# 	trend09 <- function() {
# 		mw <- ams_mw_variability(data, years, options$window_size, options$window_step)
# 		results$mwmk <<- eda_mk_test(mw$std, options$alpha)
# 		if (results$white$reject || results$mwmk$reject) 10 else NULL
# 	}

# 	# Sen's variance (10): go to Runs variance (11) regardless of the result
# 	trend10 <- function() {
# 		mw <- ams_mw_variability(data, years, options$window_size, options$window_step)
# 		results$sens_variance <<- eda_sens_trend(mw$std, mw$year)
# 		return (11)	
# 	}

# 	# Runs variance (11): go to end (NULL) regardless of the results
# 	trend11 <- function() {
# 		results$runs_variance <<- eda_runs_test(results$sens_variance, options$alpha)
# 		return (NULL)	
# 	}

# 	# Iterate through the flowchart
# 	location <- 1
# 	while (!is.null(location)) {
# 		fname <- sprintf("trend%02d", location)
# 		location <- get(fname)()
# 	} 

# 	results

# }
