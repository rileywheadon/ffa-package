#' Pettitt Test for Abrupt Changes in the Mean of a Time Series
#'
#' Performs the non-parametric Pettitt test to detect a single change point in the
#' mean of a time series, often used for abrupt shifts in hydrological data.
#'
#' @param data Numeric; a vector of annual maximum streamflow data.
#'
#' @param years Numeric; a vector of years with the same length as `data`.
#'
#' @param alpha Numeric (1); the significance level (default is 0.05).
#'
#' @param quiet Logical (1); if FALSE, prints a summary of results (default is TRUE).
#'
#' @return A named list containing:
#' - `ut`: Vector of absolute U-statistics for all time indices.
#' - `k.statistic`: Maximum absolute U-statistic (test statistic).
#' - `k.critical`: Critical K-statistic value for given `alpha`.
#' - `p.value`: Approximate p-value for the test.
#' - `change.index`: Index of the detected change point (0 if none).
#' - `change.year`: Year of the detected change point (0 if none).
#' - `reject`: Logical. TRUE if the null hypothesis was rejected.
#' - `msg`: Formatted summary message describing the test result.
#'
#' @details
#' The Pettitt test is a rank-based non-parametric test that evaluates the
#' hypothesis of a change point in the median/mean of a time series.
#' It computes the maximum of the absolute value of the U-statistic over all
#' possible split points. The p-value is approximated using an asymptotic formula.
#'
#' @seealso \link{pettitt.plot}
#'
#' @examples
#' data <- rnorm(n = 100, mean = 100, sd = 10)
#' years <- seq(from = 1901, to = 2000)
#' pettitt.test(data, years)
#'
#' @export
 
pettitt.test <- function(data, years, alpha = 0.05, quiet = TRUE) {

	# Extract the length of data for convenience
	n <- length(data)

	# Compute the U-statistic for all t-values from 1 to n
	ut <- numeric(n)

	for (t in 1:n) {
		u <- 0

		# ut = sum(sign(data[j] - data[i])) for all i <= t, j > t
		for (i in 1:t) {
			for (j in min(t + 1, n):n) {
				u = u + sign(data[j] - data[i])
			}
		}

		ut[t] = abs(u)
	}

	# The K-statistic is the maximum absolute value of the U-statistics
	k_statistic <- max(ut)

	# Compute the p-value using an approximate formula
	p_value <- round(exp((-6 * k_statistic^2) / (n^3 + n^2)), digits=3)

	# Find the minimum statistically significant K-statistic 
	k_critical <- (-log(alpha) * ((n^3) + (n^2)) / 6)^0.5;

	# Determine the change index if the change is statistically significant
	reject <- (p_value <= alpha)
	change_index <- ifelse(reject, which.max(ut), 0)
	change_year <- ifelse(reject, years[change_index], 0)

	# Print the results of the test
	msg <- test_message(
		"Pettitt",
		reject,
		p_value,
		alpha,
		sprintf("evidence of a change point in %f.", change_year),
		"NO evidence of a change point."
	)

	if (!quiet) message(msg)

	# Return a list containing the results of the test
	list(
		ut = ut,
		k.statistic = k_statistic,
		k.critical = k_critical,
		p.value = p_value,
		change.index = change_index,
		change.year = change_year,
		reject = reject,
		msg = msg
	)

}
