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

	# Check for randomness of the residuals using the Wald-Wolfowitz runs test
	results <- randtests::runs.test(residuals)

	# Determine whether we reject or fail to reject based on p_value and alpha
	p_value <- results$p.value
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
		p.value = p_value,
		residuals = residuals,
		reject = reject,
		msg = msg
	)

}

