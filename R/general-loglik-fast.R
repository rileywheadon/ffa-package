#' Generalized Log-Likelihood Helper Function
#'
#' A helper function used by \link{gllgev}, \link{gllgev100}, and \link{gllgev110}.
#' 
#' @note 
#' This function does not perform parameter validation, which improves performance
#' but may cause unpredictable behaviour. Use at your own risk.
#'
#' @param model Character (1); three character distribution code. Must be one of: 
#'   `"GUM"`, `"NOR"`, `"LNO"`, `"GEV"`, `"GLO"`, `"GNO"`, `"PE3"`, `"LP3"`, or `"WEI"`.
#'
#' @param trend List; information about non-stationary trend(s) to use:
#' - `location` Logical (1); if TRUE, there is a trend in the location parameter.
#' - `scale` Logical (1); if TRUE, there is a trend in the scale parameter.
#'
#' @param data Numeric; a vector of annual maximum streamflow data.
#'
#' @param params Numeric; a vector of parameters. Must have the correct length for the model.
#'
#' @param prior Numeric (2); a vector of parameters \eqn{(p, q)} of the Beta prior 
#'   on \eqn{\kappa}.
#'
#' @param years Numeric; a vectorof years with the same length as `data`. 
#'   Required if `trend$location` or ic	`trend$scale` is TRUE.
#'
#' @return Numeric (1); the generalized log-likelihood value.
#'
#' @seealso \link{gll-functions}
#'
#' @examples
#' # Initialize data, params, params, prior, and years
#' data <- rnorm(n = 100, mean = 100, sd = 10)
#' params <- c(0, 1, 1, 0, 0)
#' prior <- c(5, 10)
#' years <- seq(from = 1901, to = 2000)
#'
#' # Compute the generalized log-likelihood
#' general_loglik_fast("GEV", "11", data, params, prior, years)
#'
#' @export
general_loglik_fast <- function(model, trend, data, params, prior, years = NULL) {

	# The prior is Beta(p, q)
	p <- prior[1]
	q <- prior[2]

	# Compute the contribution of the prior to the likelihood
	k <- params[length(params)]
	pll <- (p - 1) * log(0.5 - k) + (q - 1) * log(0.5 + k) - lbeta(p, q)

	# Compute the likelihood
	llv <- llvxxx(model, trend, data, params, years)

	# Return the sum of (pll, llv) over all the data points
	n <- length(data)
	sum((n * pll) + llv)

}


