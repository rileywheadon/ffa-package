#' Perform a Wald–Wolfowitz runs test on model residuals
#'
#' \code{runs.test} applies the Wald–Wolfowitz runs test to a numeric vector of
#' residuals in order to assess whether they behave as a random sequence.  The
#' test statistic’s p-value is compared to the specified significance level
#' \code{alpha}, and a pass/fail decision is returned along with a human-readable
#' summary message.
#'
#' @param residuals Numeric vector of residual values from a fitted linear model.
#' @param alpha Numeric significance level for the runs test (default is 0.05).
#' @param quiet Logical. If FALSE, prints a summary message describing results (default is TRUE).
#'
#' @return A named list with components:
#' \describe{
#'   \item{p.value}{P-value from the Wald–Wolfowitz runs test applied to residuals.}
#'   \item{residuals}{Numeric vector of residual values from a fitted linear model.}
#'   \item{reject}{Logical. TRUE if null hypothesis of random residuals is rejected.}
#'   \item{msg}{Character string summarizing the test result.}
#' }
#'
#' @details
#' The Wald–Wolfowitz runs test examines the sequence of positive and negative
#' residuals to test for randomness around the median. A small p-value suggests
#' non-random clustering, which may indicate that a linear model is inappropriate 
#' for the data.
#'
#' Internally, this function calls \code{\link[randtests]{runs.test}}.
#'
#' @references
#' Wald, A. and Wolfowitz, J. (1940). On a test whether two samples are from the same population.
#'   Annals of Mathematical Statistics, 11(2), 147–162.
#'
#' @seealso \code{\link[randtests]{runs.test}}, \code{\link{sens.trend}}
#'
#' @importFrom randtests runs.test
#' @export

runs.test <- function(residuals, alpha = 0.05, quiet = TRUE) {

	# Remove values from the residuals that are equal to the median
	residuals <- residuals[residuals != median(residuals)]

	# Transform the residuals into a boolean vector (TRUE if >median, FALSE if <median)	
	boolean_residuals <- (residuals > median(residuals))

	# Count the number of data points in each category
	n <- length(boolean_residuals)
	n_plus <- sum(boolean_residuals == TRUE)
	n_minus <- sum(boolean_residuals == FALSE)

	# Determine the number of "runs" (contiguous blocks of + or -) in the data
	runs <- length(rle(boolean_residuals)$values)
	
	# Compute the distribution parameters under the null hypothesis
	mu <- (2 * n_plus * n_minus / n) + 1
	sigma <- sqrt((2 * n_plus * n_minus * (2 * n_plus * n_minus - n)) / ((n^2) * (n - 1)))
	
	# Compute the p-value of the two-sided runs test
	z <- (runs - mu) / sigma
	p_value <- 2 * (1 - pnorm(abs(z)))

	# Determine whether we reject or fail to reject based on p_value and alpha
	reject <- (p_value <= alpha)

	# Print the results of the test
	msg <- test_message(
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
		n.plus = n_plus,
		n.minus = n_minus,
		runs = runs,
		statistic = z,
		p.value = p_value,
		residuals = residuals,
		reject = reject,
		msg = msg
	)

}

