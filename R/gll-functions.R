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
#' the `gllxxx` helper function instead.
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
#' @seealso \link{gllxxx}
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


gllvalidate <- function(n, data, params, prior, years = NULL) {

	# Validate the data vector
	if (!is.numeric(data) | !is.vector(data)) stop("'data' must be a numeric vector.")
	if (any(is.nan(data)) | any(is.na(data))) stop("'data' must not contain NaN/NA values.")
	if (any(data <= 0)) stop("'data' must not contain negative values.")

	# Validate the parameter vector
	if (!is.numeric(params) | !is.vector(params)) stop("'params' must be a numeric vector.")
	if (any(is.nan(params)) | any(is.na(params))) stop("'params' must not contain NaN/NA values.")
	if (length(params) != n) stop(sprintf("'params' must have length %d.", n))

	# Validate the prior vector
	if (!is.numeric(prior) | !is.vector(prior)) stop("'prior' must be a numeric vector.")
	if (any(is.nan(prior)) | any(is.na(prior))) stop("'prior' must not contain NaN/NA values.")
	if (length(prior) != 2) stop("'prior' must have length 2.")

	# Validate the years vector
	if (!is.null(years)) {
		if (!is.numeric(years) | !is.vector(years)) stop("'years' must be a numeric vector.")
		if (any(is.nan(years)) | any(is.na(years))) stop("'years' must not contain NaN/NA values.")
		if (length(years) != length(data)) stop("'years' must have the same length as 'data'.")
	}

}


#' @rdname gll-functions
#' @export
gllgev    <- function(data, params, prior, years = NULL) {
	gllvalidate(3, data, params, prior)
	gllxxx("GEV", NULL, data, params, prior, NULL)
}

#' @rdname gll-functions
#' @export
gllgev100 <- function(data, params, prior, years) {
	gllvalidate(4, data, params, prior, years)
	covariate <- get.covariates(years)
	gllxxx("GEV", "10", data, params, prior, covariate)
}

#' @rdname gll-functions
#' @export
gllgev110 <- function(data, params, prior, years) {
	gllvalidate(5, data, params, prior, years)
	covariate <- get.covariates(years)
	gllxxx("GEV", "11", data, params, prior, covariate)
}

