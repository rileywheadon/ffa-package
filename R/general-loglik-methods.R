#' Generalized Log-Likelihood Functions for GEV Models
#'
#' Computes the generalized log-likelihood for stationary and nonstationary 
#' variants of the  Generalized Extreme Value (GEV) distribution with a Beta 
#' prior on the shape parameter.
#' 
#' @details 
#' The generalized log-likelihood is defined as sum of the log-likelihood of the 
#' specified model and the log-density of the Beta prior with parameters \eqn{(p, q)}.
#' The contribution of the prior is: \deqn{\log \pi(\kappa) = (p-1) \log(0.5-\kappa) 
#' + (q-1) \log(0.5+\kappa) - \log (\beta(p, q))}
#'
#' @inheritParams param-data
#' @inheritParams param-params
#' @inheritParams param-prior
#' @inheritParams param-years
#' @inheritParams param-trend
#'
#' @return Numeric scalar. The generalized log-likelihood value.
#'
#' @seealso \link{general_loglik_fast}
#'
#' @examples
#' # Initialize data, params, prior, years, and trend
#' data <- rnorm(n = 100, mean = 100, sd = 10)
#' params <- c(0, 1, 1, 0)
#' prior <- c(5, 10)
#' years <- seq(from = 1901, to = 2000)
#' trend <- list(location = TRUE, scale = FALSE)
#'
#' # Compute the generalized log-likelihood
#' general_loglik_gev(data, params, prior, years, trend)
#'
#' @export

general_loglik_gev <- function(
	data,
	params,
	prior,
	years = NULL,
	trend = NULL
) {

	data <- validate_numeric("data", data, optional = FALSE)
	params <- validate_params("GEV", params, trend)
	prior <- validate_numeric("prior", prior, size = 2, bounds = c(0, Inf))
	years <- validate_numeric("years", years, size = length(data))
	trend <- validate_trend(trend)
	general_loglik_fast(data, "GEV", params, prior, years, trend)

}

