#' Sample L-moments
#'
#' Computes the first four sample L-moments and L-moment ratios from a numeric 
#' vector of data. L-moments are linear combinations of order statistics that 
#' provide robust alternatives to conventional moments, with advantages in 
#' parameter estimation for heavy-tailed and skewed distributions.
#'
#' @inheritParams param-data
#'
#' @details Given probability weighted moments \eqn{\beta_0, \beta_1, \beta_2, \beta_3}, 
#' the first four sample L-moments are: 
#' - \eqn{l_1 = \beta_0}
#' - \eqn{l_2 = 2\beta_1 - \beta_0}
#' - \eqn{l_3 = 6\beta_2 - 6\beta_1 + \beta_0}
#' - \eqn{l_4 = 20\beta_3 - 30\beta_2 + 12\beta_1 - \beta_0}
#'
#' Then, the sample L-skewness is \eqn{t_3 = l_3 / l_2} and the sample L-kurtosis
#' is \eqn{t_4 = l_4 / l_2}.
#'
#' @return A numeric vector containing the first four sample L-moments and L-moment ratios:
#' - \eqn{l_1}: L-mean
#' - \eqn{l_2}: L-variance
#' - \eqn{t_3}: L-skewness
#' - \eqn{t_4}: L-kurtosis
#'
#' @examples
#' data <- rnorm(n = 100, mean = 100, sd = 10)
#' utils_sample_lmoments(data)
#'
#' @references
#' Hosking, J. R. M. (1990). L-moments: Analysis and estimation of distributions
#' using linear combinations of order statistics. *Journal of the Royal Statistical
#' Society: Series B (Methodological)*, 52(1), 105–124.
#'
#' @export
utils_sample_lmoments <- function(data) {

	data <- validate_numeric("data", data, FALSE)

	# Sort the data in increasing order
	x <- sort(data)
	n <- length(x)
	i <- 1:n

	# Recursively compute the probability weighted moments
	vb0 <- x / n
	vb1 <- vb0 * (i - 1) / (n - 1)
	vb2 <- vb1 * (i - 2) / (n - 2)
	vb3 <- vb2 * (i - 3) / (n - 3)

	b0 <- sum(vb0)
	b1 <- sum(vb1)
	b2 <- sum(vb2)
	b3 <- sum(vb3)

	# Compute the first four sample L-moments
	l1 <- b0
	l2 <- (2 * b1) - b0
	l3 <- (6 * b2) - (6 * b1) + b0
	l4 <- (20 * b3) - (30 * b2) + (12 * b1) - b0

	# Return the results (l1, l2, t3, t4)
	c(l1 ,l2, l3 / l2, l4 / l2)

}
