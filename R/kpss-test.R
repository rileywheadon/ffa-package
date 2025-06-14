#' Kwiatkowski–Phillips–Schmidt–Shin (KPSS) Unit Root Test
#'
#' Performs the KPSS test for stationarity in annual maximum streamflow (AMS) data using the
#' \pkg{aTSA} package. The null hypothesis is that the time series is stationary.
#'
#' @param ams A numeric vector of annual maximum streamflow data. Must not contain NA values.
#' @param alpha A numeric value indicating the significance level. Must be 0.01, 0.05, or 0.10.
#' @param quiet Logical. If FALSE, prints a summary message to the console (default is TRUE).
#'
#' @return A named list with the following components:
#' \describe{
#'   \item{p.value}{The reported p-value from the test. See notes regarding discrete thresholds.}
#'   \item{reject}{Logical. TRUE if the null hypothesis of stationarity is rejected at \code{alpha}.}
#'   \item{msg}{Character string summarizing the test outcome, printed if \code{quiet = FALSE}.}
#' }
#'
#' @details
#' The KPSS test implementation in \pkg{aTSA} uses interpolation from the Hobjin et al. (2004)
#' significance table, which only includes thresholds for 0.01, 0.05, and 0.10. As such, the
#' returned p-values are discretized. Specifically, \eqn{p = 0.01} implies \eqn{p \leq 0.01}, 
#' and \eqn{p = 0.10} implies \eqn{p \geq 0.10}. We use the Type III KPSS test, which accounts 
#' for drift and a nonstationary trend in the data by fitting an auxillary model.
#'
#' @seealso \code{\link[aTSA]{kpss.test}}, \code{\link{pp.test}}
#' @export

kpss.test <- function(ams, alpha = 0.05, quiet = TRUE) {

	# Run the KPSS test and get the p_value
	result <- aTSA::kpss.test(ams, output = FALSE)
	p_value <- result[3, 3]

	# Determine whether we reject or fail to reject based on p_value and alpha
	reject <- if (alpha == 0.10) p_value < alpha else p_value <= alpha

	# Set the P-value text to inform the user of the limited significance thresholds
	p_text <- if (p_value == 0.10) {
		"*at least* 0.10"
	} else if (p_value == 0.01) {
		"*at most* 0.01"
	} else {
		round(p_value, 3)
	}

	# Print the results of the test
	msg <- test_message(
		"KPSS",
		reject,
		p_value,
		alpha,
		"evidence of a unit root.",
		"NO evidence of a unit root."
	)

	if (!quiet) message(msg)

	# Return the results
	list(p.value = p_value, reject = reject, msg = msg)

}

