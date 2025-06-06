#' Mann–Kendall Test for Monotonic Trends
#'
#' Performs the Mann–Kendall trend test on a numeric vector to detect the presence of a monotonic
#' trend (increasing or decreasing) over time. The test is non-parametric and accounts for tied
#' observations in the data.
#'
#' @param data A numeric vector of AMS values or their variances. Must not contain NA values.
#' @param alpha A numeric value specifying the significance level (default is 0.05).
#' @param quiet Logical. If FALSE, prints a summary of the test result to the console.
#'
#' @return A named list with the following components:
#' \describe{
#'   \item{s.statistic}{The raw Mann–Kendall test statistic \(S\).}
#'   \item{s.variance}{The variance of the test statistic under the null hypothesis.}
#'   \item{p.value}{The p-value associated with the two-sided hypothesis test.}
#'   \item{reject}{Logical. TRUE if the null hypothesis of no trend is rejected at \code{alpha}.}
#' }
#'
#' @details
#' The statistic \(S\) is computed as the sum over all pairs \(i < j\) of the sign of the
#' difference \(x_j - x_i\). Ties are explicitly accounted for when calculating the variance of
#' \(S\), using grouped frequencies of tied observations.
#'
#' The test statistic \(Z\) is then computed based on the sign and magnitude of \(S\), and the
#' p-value is derived from the standard normal distribution.
#'
#' @seealso \code{\link{bbmk_test}} for a bootstrap-based variant of this test.
#' @export

mk.test <- function(data, alpha = 0.05, quiet = TRUE) {

	# Assign a variable to number of data points for convenience
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

	# Determine whether we reject or fail to reject based on p_value and alpha
	reject <- (p_value <= alpha)

	# Return the results of the test as a list
	list(s.statistic = s, s.variance = s_variance, p.value = p_value, reject = reject)

}

