#' Pettitt Test for Abrupt Changes in the Mean of a Time Series
#'
#' Performs the nonparametric Pettitt test to detect a single abrupt change in the
#' mean of a time series. Under the null hypothesis, there is no change point.
#'
#' @inheritParams param-data
#' @inheritParams param-years
#' @inheritParams param-alpha
#'
#' @return A list containing the test results, including:
#' - `data`: The `data` argument.
#' - `years`: The `years` argument.
#' - `alpha`: The significance level as specified in the `alpha` argument.
#' - `null_hypothesis`: A string describing the null hypothesis.
#' - `alternative_hypothesis`: A string describing the alternative hypothesis.
#' - `u_series`: Numeric vector of absolute U-statistics for all years.
#' - `statistic`: The test statistic and maximum absolute U-statistic.
#' - `bound`: The critical value of the test statistic based on `alpha`.
#' - `change_points`: A dataframe containing the potential change point.
#' - `p_value`: An asymptotic approximation of the  p-value for the test.
#' - `reject`: Logical scalar. If `TRUE`, the null hypothesis was rejected.
#'
#' `change_points` contains the years, test statistics, and p-values of each
#' potential change point. If no change points were identified, `change_points`
#' is empty.
#'
#' @seealso [plot_pettitt_test()], [eda_mks_test()]
#'
#' @examples
#' data <- rnorm(n = 100, mean = 100, sd = 10)
#' years <- seq(from = 1901, to = 2000)
#' eda_pettitt_test(data, years)
#'
#' @references
#' Pettitt, A.N., 1979. A Non-parametric Approach to the Change-point Problem. J. 
#' Royal Statistics Society 28 (2), 126–135. \url{http://www.jstor.org/stable/2346729}
#'
#' @export
 
eda_pettitt_test <- function(data, years, alpha = 0.05) {

	data <- validate_numeric("data", data, bounds = c(0, Inf))
	years <- validate_numeric("years", years, size = length(data))
	alpha <- validate_float("alpha", alpha, bounds = c(0.01, 0.1))

	n <- length(data)
	u_series <- numeric(n)

	# u_series = sum(sign(data[j] - data[i])) for all i <= t, j > t
	for (t in 1:n) {
		u <- 0

		for (i in 1:t) {
			for (j in min(t + 1, n):n) {
				u = u + sign(data[j] - data[i])
			}
		}

		u_series[t] = abs(u)
	}

	# Use an approximation since there is no closed-form solution for the p-value
	statistic <- max(u_series)
	bound <- (-log(alpha) * ((n^3) + (n^2)) / 6)^0.5;
	p_value <- round(exp((-6 * statistic^2) / (n^3 + n^2)), 3)
	reject <- (p_value <= alpha)

	# Store the candidate change point in a dataframe
	change_df <- data.frame(
		index = which.max(u_series),
		year = years[which.max(u_series)],
		value = data[which.max(u_series)],
		statistic = statistic,
		p_value = p_value
	)

	# Filter out the change point if it is not significant
	change_df <- change_df[which(change_df$statistic > bound), ]

	list(
		data = data,
		years = years,
		alpha = alpha,
		null_hypothesis = "There are no abrupt changes in the mean of the data.",
		alternative_hypothesis = "There is one abrupt change in the mean of the data.",
		u_series = u_series,
		statistic = statistic,
		bound = bound,
		p_value = p_value,
		change_points = change_df,
		reject = reject
	)

}
