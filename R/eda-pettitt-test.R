#' Pettitt Test for Abrupt Changes in the Mean of a Time Series
#'
#' Performs the non-parametric Pettitt test to detect a single change point in the
#' mean of a time series, used to identify abrupt shifts in hydrological data.
#'
#' @inheritParams param-data
#' @inheritParams param-years
#' @inheritParams param-alpha
#' @inheritParams param-quiet
#'
#' @return A list containing the test results, including:
#' - `data`: The `data` argument.
#' - `years`: The `years` argument.
#' - `u_t`: Numeric vector of absolute U-statistics for all time indices.
#' - `k_statistic`: Numeric scalar.Tthe maximum absolute U-statistic.
#' - `k_critical`: Numeric scalar. The critical K-statistic value for given `alpha`.
#' - `p_value`: Numeric scalar. Approximate p-value for the test.
#' - `change_index`: Integer scalar. Index of the detected change point (0 if none).
#' - `change_year`: Integer scalar. Year of the detected change point (0 if none).
#' - `reject`: Logical scalar. If `TRUE`, the null hypothesis was rejected.
#' - `msg`: Character scalar. A formatted summary message describing the test result.
#'
#' @details
#' The Pettitt test is a rank-based non-parametric test that evaluates the
#' hypothesis of a change point in the mean of a time series. It computes the 
#' maximum of the absolute value of the U-statistic over all possible split 
#' points. The p-value is approximated using an asymptotic formula.
#'
#' @seealso \link{plot_pettitt_test}
#'
#' @examples
#' data <- rnorm(n = 100, mean = 100, sd = 10)
#' years <- seq(from = 1901, to = 2000)
#' eda_pettitt_test(data, years)
#'
#' @export
 
eda_pettitt_test <- function(data, years, alpha = 0.05, quiet = TRUE) {

	data <- validate_data(data)
	years <- validate_years(years, data)
 	alpha <- validate_alpha(alpha)
	quiet <- validate_quiet(quiet)

	n <- length(data)
	u_t <- numeric(n)

	# u_t = sum(sign(data[j] - data[i])) for all i <= t, j > t
	for (t in 1:n) {
		u <- 0

		for (i in 1:t) {
			for (j in min(t + 1, n):n) {
				u = u + sign(data[j] - data[i])
			}
		}

		u_t[t] = abs(u)
	}

	# Use an approximation since there is no closed-form solution for the p-value
	k_statistic <- max(u_t)
	k_critical <- (-log(alpha) * ((n^3) + (n^2)) / 6)^0.5;
	p_value <- round(exp((-6 * k_statistic^2) / (n^3 + n^2)), 3)

	# The Pettitt test identifies either 0 or 1 change points
	reject <- (p_value <= alpha)
	change_index <- ifelse(reject, which.max(u_t), 0)
	change_year <- ifelse(reject, years[change_index], 0)

	msg <- stats_message(
		"Pettitt",
		reject,
		p_value,
		alpha,
		"NO evidence of a change point",
		sprintf("evidence of a change point in %f", change_year)
	)

	if (!quiet) message(msg)

	list(
		data = data,
		years = years,
		u_t = u_t,
		k_statistic = k_statistic,
		k_critical = k_critical,
		p_value = p_value,
		change_index = change_index,
		change_year = change_year,
		reject = reject,
		msg = msg
	)

}
