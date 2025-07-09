#' Mann–Kendall–Sneyers Test for Change Point Detection
#'
#' Performs the Mann–Kendall–Sneyers (MKS) test to detect the beginning of a monotonic
#' trend in annual maximum streamflow (AMS) data. The test computes normalized
#' progressive and regressive Mann–Kendall statistics and identifies statistically
#' significant crossing points, indicating potential change points in the trend.
#'
#' @inheritParams param-data
#' @inheritParams param-years
#' @inheritParams param-alpha
#' @inheritParams param-quiet
#'
#' @return A list containing the test results, including:
#' - `data`: The `data` argument.
#' - `years`: The `years` argument.
#' - `s_progressive`: Normalized progressive Mann–Kendall-Sneyers statistics.
#' - `s_regressive`: Normalized regressive Mann–Kendall-Sneyers statistics.
#' - `bound`: Critical confidence bound for significance based on `alpha`.
#' - `crossing_df`: Crossing points, including indices, years, statistics, and AMS.
#' - `change_df`: Subset of `crossing.df` with statistically significant crossings.
#' - `p_value`: Two-sided p-value derived from the maximum crossing statistic.
#' - `reject`: Logical. If `TRUE`, the null hypothesis of no change point is rejected.
#' - `msg`: Character string summarizing the test result (printed if `quiet = FALSE`).
#'
#' @details
#' The function computes progressive and regressive Mann–Kendall statistics \eqn{S_t},
#' normalized by their expected values and variances under the null hypothesis. The 
#' crossing points where the difference between these normalized statistics changes 
#' sign are identified using linear interpolation. The significance of detected 
#' crossings is assessed using quantiles of the normal distribution.
#'
#' @references
#' Sneyers, R. (1990). On the statistical analysis of series of observations.
#' Technical note No. 143, World Meteorological Organization, Geneva.
#'
#' @seealso \link{plot_mks_test}
#'
#' @examples
#' data <- rnorm(n = 100, mean = 100, sd = 10)
#' years <- seq(from = 1901, to = 2000)
#' eda_mks_test(data, years)
#'
#' @importFrom stats pnorm qnorm lm coef
#' @export

eda_mks_test <- function(data, years, alpha = 0.05, quiet = TRUE) {

	data <- validate_data(data)
	years <- validate_years(years, data)
	alpha <- validate_alpha(alpha)
	quiet <- validate_quiet(quiet)

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

	# Compute the location of each crossing using linear interpolation
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
	locations <- if (length(cross) == 0) numeric() else sapply(cross, get_crossings)

	# Create a dataframe of all crossings
	crossing_df <- data.frame(
		cross = cross,
		year = years[cross],
		statistic = locations,
		max = data[cross]
	)

	# Compute the p-value of the test and the statistically significant crossings
	if (nrow(crossing_df) > 0) {
		p_value <- 2 * (1 - pnorm(max(abs(crossing_df$statistic))))
		change_df <- crossing_df[which(abs(crossing_df$statistic) > bound), ]
	} else {
		p_value <- 1
		change_df <- crossing_df
	}

	# Determine whether we reject or fail to reject based on p_value and alpha
	reject <- (p_value <= alpha)

	# Print the results of the test
	msg <- stats_message(
		"Mann-Kendall-Sneyers",
		reject,
		p_value,
		alpha,
		"NO evidence of change point(s)",
		sprintf("evidence of change point(s) at %s", toString(change_df$years))
	)

	if (!quiet) message(msg)

	# Return a list of values results from the test
	list(
		data = data,
		years = years,
		s_progressive = s_prog,
		s_regressive = s_regr,
		bound = bound,
		crossing_df = crossing_df,
		change_df = change_df,
		p_value = p_value,
		reject = reject,
		msg = msg
	)
	
}
