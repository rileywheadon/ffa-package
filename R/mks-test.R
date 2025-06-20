#' Mann–Kendall–Sneyers Test for Detecting Change Points in Trends
#'
#' Performs the Mann–Kendall–Sneyers (MKS) test to detect the approximate time of onset
#' of a monotonic trend in annual maximum streamflow (AMS) data. The test computes
#' progressive and regressive Mann–Kendall statistics and identifies statistically
#' significant crossing points, indicating potential change points in the trend.
#'
#' @param data Numeric vector of annual maximum streamflow values with no missing values.
#' @param years Numeric vector of years corresponding to \code{data}, with no missing values.
#' @param alpha Numeric significance level for the test (default 0.05).
#' @param quiet Logical flag to suppress or print a summary message (default TRUE).
#'
#' @return A named list containing:
#' \describe{
#'   \item{s.progressive}{Normalized progressive Mann–Kendall statistics over time.}
#'   \item{s.regressive}{Normalized regressive Mann–Kendall statistics over time.}
#'   \item{bound}{Critical confidence bound for significance based on \code{alpha}.}
#'   \item{crossing.df}{Data frame of crossing points with indices, years, statistics, and AMS values.}
#'   \item{change.df}{Subset of \code{crossing_df} where crossing statistics exceed confidence bounds.}
#'   \item{p.value}{Two-sided p-value assessing the significance of maximum crossing statistic.}
#'   \item{reject}{Logical indicating whether null hypothesis of no change point is rejected.}
#'   \item{msg}{Character string summarizing the test result (printed if \code{quiet = FALSE}).}
#' }
#'
#' @details
#' The function computes progressive and regressive Mann–Kendall statistics \(S_t\),
#' normalized by their expected values and variances under the null hypothesis. The crossing
#' points where the difference between these normalized statistics changes sign are
#' identified using linear interpolation. The significance of detected crossings is
#' assessed using normal quantiles and the maximum absolute crossing statistic.
#'
#' @references
#' Sneyers, R. (1990). On the statistical analysis of series of observations.
#' Technical note No. 143, World Meteorological Organization, Geneva.
#'
#' @seealso \code{\link{mk.test}} for the classical Mann–Kendall test.
#'
#' @importFrom stats pnorm qnorm lm coef
#' @export

mks.test <- function(data, years, alpha = 0.05, quiet = TRUE) {

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
		years = years[cross],
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
	msg <- test_message(
		"MKS",
		reject,
		p_value,
		alpha,
		sprintf("evidence of change point(s) at %s.", toString(change_df$years)),
		"NO evidence of change point(s)."
	)

	if (!quiet) message(msg)

	# Return a list of values results from the test
	list(
		s.progressive = s_prog,
		s.regressive = s_regr,
		bound = bound,
		crossing.df = crossing_df,
		change.df = change_df,
		p.value = p_value,
		reject = reject,
		msg = msg
	)
	
}
