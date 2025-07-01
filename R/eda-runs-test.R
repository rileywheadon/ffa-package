#' Wald–Wolfowitz Runs Test for Randomness
#'
#' @order $1
#'
#' Applies the Wald–Wolfowitz runs test to a numeric vector of residuals in 
#' order to assess whether they behave as a random sequence. The test statistic’s 
#' p-value is compared to the significance level `alpha`, and a decision is 
#' returned along with a human-readable summary message.
#'
#' @param residuals Numeric; A vector of residuals produced by \link{eda_sens_trend}.
#'
#' @param alpha Numeric (1); the significance level (default is 0.05).
#'
#' @param quiet Logical (1); if FALSE, prints a summary of results (default is TRUE).
#'
#' @return List; test results, including:
#' - `p.value`: P-value from the Wald–Wolfowitz runs test applied to residuals.
#' - `residuals`: Numeric vector of residual values from a fitted linear model.
#' - `reject`: Logical. TRUE if the null hypothesis of random residuals is rejected.
#' - `msg`: Character string summarizing the test result.
#'
#' @details
#' The Wald–Wolfowitz runs test examines the sequence of residuals to test for
#' randomness around the median. A small p-value suggests non-random clustering, 
#' which may indicate that a linear model is inappropriate for the data.
#'
#' @references
#' Wald, A. and Wolfowitz, J. (1940). On a test whether two samples are from the 
#' same population. Annals of Mathematical Statistics, 11(2), 147–162.
#'
#' @seealso \link{runs.plot}, \link{eda_sens_trend}
#'
#' @examples
#' # Initialize data and years
#' data <- rnorm(n = 100, mean = 100, sd = 10)
#' years <- seq(from = 1901, to = 2000)
#'
#' # Perform the runs test
#' residuals <- eda_sens_trend(data, years)$residuals
#' eda_runs_test(residuals)
#'
#' @export

eda_runs_test <- function(residuals, alpha = 0.05, quiet = TRUE) {

	# Run parameter validation (see helpers.R)
	validate.alpha(alpha)

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
	msg <- stats.message(
		"runs",
		reject,
		p_value,
		alpha,
		"evidence that a linear model is inappropriate.",
		"NO evidence that a linear model is inappropriate."
	)

	if (!quiet) message(msg)

	# Return the results as a list
	list(
		n = n,
		n.plus = np,
		n.minus = nm,
		runs = runs,
		statistic = z,
		p.value = p_value,
		residuals = residuals,
		reject = reject,
		msg = msg
	)

}

