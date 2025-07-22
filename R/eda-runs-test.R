#' Wald–Wolfowitz Runs Test for Randomness
#'
#' Applies the Wald–Wolfowitz runs test to a numeric vector of residuals in 
#' order to assess whether they behave as a random sequence. The test statistic’s 
#' p-value is compared to the significance level `alpha`, and a decision is 
#' returned along with a human-readable summary message.
#'
#' @param results A fitted linear model produced by [eda_sens_trend()].
#'
#' @inheritParams param-alpha
#' @inheritParams param-quiet
#'
#' @return A list containing the test results, including:
#' - `residuals`: Numeric vector of residual values from a fitted linear model.
#' - `n`: The length of the residuals vector after removing the median.
#' - `n_plus`: The number of residuals above the median.
#' - `n_minus`: The number of residuals below the median.
#' - `runs`: The number of runs in the transformed sequence of residuals.
#' - `statistic`: The runs test statistic, computed using `runs`.
#' - `p_value`: P-value from the Wald–Wolfowitz runs test applied to residuals.
#' - `reject`: Logical. If TRUE, the null hypothesis of random residuals is rejected.
#' - `msg`: Character string summarizing the test result, printed if `quiet = FALSE`.
#'
#' @details
#' The Wald–Wolfowitz runs test examines the sequence of residuals to test for
#' randomness around the median. A small p-value suggests nonrandom clustering, 
#' which may indicate that a linear model is inappropriate for the data.
#'
#' @references
#' Wald, A. and Wolfowitz, J. (1940). On a test whether two samples are from the 
#' same population. Annals of Mathematical Statistics, 11(2), 147–162.
#'
#' @seealso [plot_runs_test()], [eda_sens_trend()]
#'
#' @examples
#' data <- rnorm(n = 100, mean = 100, sd = 10)
#' years <- seq(from = 1901, to = 2000)
#' sens <- eda_sens_trend(data, years)
#' eda_runs_test(sens)
#'
#' @export

eda_runs_test <- function(results, alpha = 0.05, quiet = TRUE) {

	residuals <- validate_numeric('results$residuals', results$residuals)
	alpha <- validate_float("alpha", alpha, bounds = c(0.01, 0.1))
	quiet <- validate_logical("quiet", quiet)

	# Remove values from the residuals that are equal to the median
	filtered_residuals <- residuals[residuals != median(residuals)]

	# Transform the residuals into a boolean vector (TRUE if >median, FALSE if <median)	
	boolean_residuals <- (filtered_residuals > median(filtered_residuals))

	# Count the number of data points in each category
	n <- length(boolean_residuals)
	np <- sum(boolean_residuals == TRUE)
	nm <- sum(boolean_residuals == FALSE)

	# Determine the number of "runs" (contiguous blocks of + or -) in the data
	runs <- length(rle(boolean_residuals)$values)
	
	# Compute the distribution parameters under the null hypothesis
	mu <- (2 * np * nm / n) + 1
	sigma <- sqrt((2 * np * nm * (2 * np * nm - n)) / ((n^2) * (n - 1)))
	
	# Compute the p-value of the two-sided runs test
	z <- (runs - mu) / sigma
	p_value <- 2 * (1 - pnorm(abs(z)))

	# Determine whether we reject or fail to reject based on p_value and alpha
	reject <- (p_value <= alpha)

	# Print the results of the test
	msg <- stats_message(
		"runs",
		reject,
		p_value,
		alpha,
		"NO evidence that a linear model is inappropriate",
		"evidence that a linear model is inappropriate"
	)

	if (!quiet) message(msg)

	# Return the results as a list
	list(
		years = results$years,
		residuals = residuals,
		n = n,
		n_plus = np,
		n_minus = nm,
		runs = runs,
		statistic = z,
		p_value = p_value,
		reject = reject,
		msg = msg
	)

}

