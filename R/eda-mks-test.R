#' Mann–Kendall–Sneyers Test for Change Point Detection
#'
#' Performs the Mann–Kendall–Sneyers (MKS) test to detect a trend change in annual 
#' maximum series data. The test computes normalized progressive and regressive 
#' Mann–Kendall statistics and identifies statistically significant crossing points, 
#' indicating potential change points. Under the null hypothesis, there are no trend
#' changes.
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
#' - `progressive_series`: Normalized progressive Mann–Kendall-Sneyers statistics.
#' - `regressive_series`: Normalized regressive Mann–Kendall-Sneyers statistics.
#' - `bound`: Critical confidence bound for significance based on `alpha`.
#' - `change_points`: A dataframe of potential change points.
#' - `p_value`: Two-sided p-value of the most significant crossing point.
#' - `reject`: If `TRUE`, the null hypothesis was rejected at significance `alpha`.
#'
#' `change_points` contains the years, test statistics, and p-values of each
#' potential change point. If no change points were identified, `change_points`
#' is empty.
#'
#' @details
#' This function computes progressive and regressive Mann–Kendall-Sneyers statistics,
#' normalized by their expected values and variances under the null hypothesis. The 
#' crossing points occur when the difference between the progressive and regressive 
#' statistics switches sign. The significance of detected crossing points is assessed 
#' using the quantiles of the normal distribution.
#'
#' @seealso [plot_mks_test()], [eda_pettitt_test()]
#'
#' @examples
#' data <- rnorm(n = 100, mean = 100, sd = 10)
#' years <- seq(from = 1901, to = 2000)
#' eda_mks_test(data, years)
#'
#' @references
#' Sneyers, R. (1990). On the statistical analysis of series of observations.
#' Technical note No. 143, World Meteorological Organization, Geneva.
#'
#' @importFrom stats pnorm qnorm lm coef
#' @export

eda_mks_test <- function(data, years, alpha = 0.05) {

	data <- validate_numeric("data", data, bounds = c(0, Inf))
	years <- validate_numeric("years", years, size = length(data))
	alpha <- validate_float("alpha", alpha, bounds = c(0.01, 0.1))

	# Compute number of elements such that data[i] > data[j] for all j < i < t for all t.
	s_statistic <- function(vt, data) {

		# Computes the number of elements such that data[i] > data[j] for j < i (given i).
		sum_i <- function(i) sum(data[i] > data[1:i-1])

		# Applies the sum_i function to a vector values.
		n_i <- sapply(vt, sum_i)

		# Compute cumulative sum of n_i values to get the S-statistic for all t.
		cumsum(n_i)
	}

	# Compute the forward and backwards s-statistics
	idx <- 1:length(data)
	s_prog_non_normal <- s_statistic(idx, data)
	s_regr_non_normal <- s_statistic(idx, rev(data))

	# Get the variance and expectation of the S-statistics
	s_expectation = idx * (idx - 1) / 4
	s_variance = (idx * (idx - 1) * ((2 * idx) + 5)) / 72

	# Prevent a division by zero error at idx = 1
	s_variance[s_variance == 0] <- 1

	# Compute the normalized progressive and regressive s-statistics
	s_prog <- (s_prog_non_normal - s_expectation) / sqrt(s_variance)
	s_regr <- rev((s_regr_non_normal - s_expectation) / sqrt(s_variance))

	# Compute confidence bounds for the normalized s-statistics
	bound <- qnorm(1 - (alpha / 2))

	# Find all crossings between progressive/regressive series
	# Increment by one so that the crossing index is the start of the time series
	s_sign <- sign(s_prog - s_regr)
	cross <- which(s_sign[-1] != s_sign[-length(s_sign)]) + 1

	# Compute the test statistic of each crossing using linear interpolation
	get_crossings <- function(i) {

		# Fit linear models 
		fit_prog <- lm(s_prog[(i - 1):i] ~ years[(i - 1):i])
		fit_regr <- lm(s_regr[(i - 1):i] ~ years[(i - 1):i])

		# Get the slope and y-intercept of each line
		b_prog <- coef(fit_prog)[1]
		b_regr <- coef(fit_regr)[1]
		m_prog <- coef(fit_prog)[2]
		m_regr <- coef(fit_regr)[2]

		# Compute and return y-coordinate of the intersection point
		x_inter = (b_regr - b_prog) / (m_prog - m_regr)
		y_inter = (m_prog * x_inter) + b_prog
		as.numeric(y_inter)

	}

	# Get crossing locations
	statistics <- if (length(cross) == 0) numeric() else sapply(cross, get_crossings)

	# Create a dataframe of all crossings
	crossing_df <- data.frame(
		index = cross,
		year = years[cross],
		value = data[cross],
		statistic = statistics,
		p_value = 2 * (1 - pnorm(abs(statistics)))
	)

	# Get the the statistically significant crossings
	change_df <- crossing_df[which(abs(crossing_df$statistic) > bound), ]
	p_value <- if (nrow(crossing_df) > 0) min(crossing_df$p_value) else 1
	reject <- (p_value <= alpha)

	# Return a list of values results from the test
	list(
		data = data,
		years = years,
		alpha = alpha,
		null_hypothesis = "There are trend changes in the data.",
		alternative_hypothesis = "There is at least one trend change in the data.",
		progressive_series = s_prog,
		regressive_series = s_regr,
		bound = bound,
		change_points = change_df,
		p_value = p_value,
		reject = reject
	)
	
}
