#' Mann–Kendall Trend Test
#'
#' Performs the Mann–Kendall trend test on a numeric vector to detect the presence 
#' of a monotonic trend (increasing or decreasing) over time. The test is 
#' non-parametric and accounts for tied observations in the data. The null
#' hypothesis assumes there is no monotonic trend.
#'
#' @inheritParams param-data
#' @inheritParams param-alpha
#' @inheritParams param-quiet
#'
#' @return A list containing the test results, including:
#' - `data`: The `data` argument.
#' - `s_statistic`: The Mann–Kendall test statistic \eqn{S}.
#' - `s_variance`: The variance of the test statistic under the null hypothesis.
#' - `p_value`: The p-value associated with the two-sided hypothesis test.
#' - `reject`: Logical. If `TRUE`, the null hypothesis is rejected at `alpha`.
#' - `msg`: A character string summarizing the result, printed if `quiet = FALSE`.
#'
#' @details
#' The statistic \eqn{S} is computed as the sum over all pairs \eqn{i < j} of the 
#' sign of the difference \eqn{x_j - x_i}. Ties are explicitly accounted for when 
#' calculating the variance of \eqn{S}, using grouped frequencies of tied observations. 
#' The test statistic \eqn{Z} is then computed based on the sign and magnitude of 
#' \eqn{S}, and the p-value is derived from the standard normal distribution.
#'
#' @seealso \link{eda_bbmk_test}
#'
#' @examples
#' data <- rnorm(n = 100, mean = 100, sd = 10)
#' eda_mk_test(data)
#'
#' @importFrom stats pnorm
#' @export

eda_mk_test <- function(data, alpha = 0.05, quiet = TRUE) {

	data <- validate_numeric("data", data, bounds = c(0, Inf))
	alpha <- validate_float("alpha", alpha, bounds = c(0.01, 0.1))
	quiet <- validate_logical("quiet", quiet)

	n <- length(data)

	# Compute the test statistic S by iterating through all pairs of values in data
	s <- 0
	for (i in 1:(n-1)) {
		for (j in (i+1):n) {
			s = s + sign(data[j] - data[i])
		}
	}

	# Identify tied groups and find the number of elements in each group
	freqs <- table(data)        
	ties <- freqs[freqs > 1]    
	g <- length(ties)           
	tp <- as.vector(ties)       

	# Compute the the S-variance including the contribution from groups
	group_sum <- sum(tp * (tp - 1) * (2 * tp + 5))
	s_variance <- (1 / 18) * ((n * (n-1) * (2 * n + 5)) - group_sum)

	# Compute the normalized test statistic Z
	z <- if (s > 0) { 
		(s - 1) / sqrt(s_variance) 
	} else if (s == 0) { 
		0 
	} else { 
		(s + 1) / sqrt(s_variance) 
	}

	# Compute the p-value for a two-sided test
	p_value <- 2 * pnorm(abs(z), lower.tail=FALSE)

	reject <- (p_value <= alpha)

	msg <- stats_message(
		"Mann-Kendall",
		reject,
		p_value,
		alpha,
		"NO evidence of a monotonic trend",
		"evidence of a monotonic trend"
	)

	if (!quiet) message(msg)

	list(
		data = data,
		s_statistic = s,
		s_variance = s_variance,
		p_value = p_value,
		reject = reject,
		msg = msg
	)

}

