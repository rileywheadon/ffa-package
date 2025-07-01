#' Generalized Log-Likelihood Functions for GEV Models
#'
#' Computes the generalized log-likelihood for stationary and non-stationary variants of the 
#' Generalized Extreme Value (GEV) distribution with a Beta prior on the shape parameter.
#' 
#' @details 
#' The generalized log-likelihood is defined as sum of the log-likelihood of the specified
#' model and the log-density of the Beta prior with parameters \eqn{(p, q)}. The contribution 
#' of the prior is: \deqn{\log \pi(\kappa) = (p-1) \log(0.5-\kappa) + (q-1) \log(0.5+\kappa) 
#' - \log (\beta(p, q))}
#'
#' Each function corresponds to a different parameterization of the GEV model:
#' 
#' - `gllgev()`: Stationary location and scale, 3 parameters.
#' - `gllgev100()`: Time-varying location, stationary scale, 4 parameters.
#' - `gllgev110()`: Time-varying location and scale, 5 parameters.
#'
#' @note
#' The `gllgev`, `gllgev100`, and `gllgev110` functions perform extensive parameter validation, 
#' which can be slow. If you plan to call these methods often, it is recommended to use 
#' the `general_loglik_fast` helper function instead.
#'
#' @param data Numeric; a vector of annual maximum streamflow data.
#'
#' @param params Numeric; a vector of parameters. Must have the correct length for the model.
#'
#' @param prior Numeric (2); a vector of parameters\eqn{(p, q)} of the Beta prior on \eqn{\kappa}.
#'
#' @param years Numeric; a vector of years with the same length as `data`.
#'   Only required for `gllgev100()` and `gllgev110()`.
#'
#' @return Numeric (1); the generalized log-likelihood value.
#'
#' @seealso \link{general_loglik_fast}
#'
#' @examples
#' # Initialize data, years, params, and prior
#' data <- rnorm(n = 100, mean = 100, sd = 10)
#' years <- seq(from = 1901, to = 2000)
#' params <- c(0, 1, 1, 0)
#' prior <- c(5, 10)
#'
#' # Compute the generalized log-likelihood
#' gllgev100(data, params, prior, years)
#'
#' @name gll-functions
NULL


#' @rdname gll-functions
#' @export
general_loglik_gev <- function(data, years, trend, params, prior) {
	validate.data(data)
	validate.years(years, data)
	validate.trend(trend)
	validate.params(params, "GEV", trend)
	validate.prior(prior)
	general_loglik_fast("GEV", NULL, data, params, prior, NULL)
}

#' @rdname gll-functions
#' @export
gllgev100 <- function(data, params, prior, years) {
	validate.data(data)
	validate.params(params, "GEV", "10")
	validate.prior(prior)
	validate.years(years, data)
	general_loglik_fast("GEV", "10", data, params, prior, years)
}

#' @rdname gll-functions
#' @export
gllgev110 <- function(data, params, prior, years) {
	validate.data(data)
	validate.params(params, "GEV", "11")
	validate.prior(prior)
	validate.years(years, data)
	general_loglik_fast("GEV", "11", data, params, prior, years)
}

