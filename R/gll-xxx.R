#' Generalized Log-Likelihood Helper Function
#'
#' A helper function used by \link{gllgev}, \link{gllgev100}, and \link{gllgev110}.
#' 
#' @note 
#' This function does not perform parameter validation, which improves performance
#' but may cause unpredictable behaviour. Use at your own risk.
#'
#' @param name Character (1); the name of the probability distribution.
#'
#' @param signature Character (1); the non-stationary signature (`NULL`, `"10"`, or `"11"`).
#'
#' @param data Numeric; a vector of annual maximum streamflow data.
#'
#' @param params Numeric; a vector of parameters. Must have the correct length for the model.
#'
#' @param prior Numeric (2); a vector of parameters \eqn{(p, q)} of the Beta prior 
#'   on \eqn{\kappa}.
#'
#' @param covariate Numeric; a vector with the same length as `data`. 
#'   Required if `signature` is `"10"` or `"11"`.
#'
#' @return Numeric (1); the generalized log-likelihood value.
#'
#' @seealso \link{gll-functions}
#'
#' @examples
#' # Initialize data, params, params, prior, and covariate
#' data <- rnorm(n = 100, mean = 100, sd = 10)
#' params <- c(0, 1, 1, 0, 0)
#' prior <- c(5, 10)
#' covariate <- seq(0, 1, length.out = 100)
#'
#' # Compute the generalized log-likelihood
#' gllxxx("GEV", "11", data, params, prior, covariate)
#'
#' @export
gllxxx <- function(name, signature, data, params, prior, covariate = NULL) {

	# The prior is Beta(p, q)
	p <- prior[1]
	q <- prior[2]

	# Compute the contribution of the prior to the likelihood
	k <- params[length(params)]
	pll <- (p - 1) * log(0.5 - k) + (q - 1) * log(0.5 + k) - lbeta(p, q)

	# Compute the likelihood
	llv <- llvxxx(name, signature, data, params, covariate)

	# Return the sum of (pll, llv) over all the data points
	n <- length(data)
	sum((n * pll) + llv)

}


