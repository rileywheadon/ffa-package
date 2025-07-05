#' Generalized Log-Likelihood Helper Function
#'
#' A helper function used by \link{general_loglik_gev}.
#' This function does not validate parameters and is intended for internal use.
#'
#' @inheritParams param-data
#' @inheritParams param-model
#' @inheritParams param-params
#' @inheritParams param-prior
#' @inheritParams param-years
#' @inheritParams param-trend
#'
#' @return Numeric scalar. The generalized log-likelihood value.
#'
#' @seealso \link{general_loglik_gev}.
#'
#' @examples
#' # Initialize data, params, prior, years, and trend
#' data <- rnorm(n = 100, mean = 100, sd = 10)
#' params <- c(0, 1, 1, 0, 0)
#' prior <- c(5, 10)
#' years <- seq(from = 1901, to = 2000)
#' trend <- list(location = TRUE, scale = TRUE)
#'
#' # Compute the generalized log-likelihood
#' general_loglik_fast(data, "GEV", params, prior, years, trend)
#'
#' @export

general_loglik_fast <- function(
	data,
	model,
	params,
	prior,
	years,
	trend
) {

	# The prior is Beta(p, q)
	p <- prior[1]
	q <- prior[2]

	# Compute the contribution of the prior to the likelihood
	k <- params[length(params)]
	pll <- (p - 1) * log(0.5 - k) + (q - 1) * log(0.5 + k) - lbeta(p, q)

	# Compute the likelihood
	llv <- loglik_fast(data, model, params, years, trend)

	# Return the sum of (pll, llv) over all the data points
	n <- length(data)
	sum((n * pll) + llv)

}


