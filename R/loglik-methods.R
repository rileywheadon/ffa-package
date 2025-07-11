#' Log-Likelihood Functions for Probability Models
#'
#' Compute the log-likelihood value for stationary and non-stationary variants 
#' of nine different distributions: `"GUM"`, `"NOR"`, `"LNO"`, `"GEV"`, `"GLO"`, 
#' `"GNO"`, `"PE3"`, `"LP3"`, or `"WEI"`.
#' 
#' @inheritParams param-data
#' @inheritParams param-params
#' @inheritParams param-years
#' @inheritParams param-trend
#'
#' @return Numeric scalar. The log-likelihood value.
#'
#' @seealso \link{loglik_fast}
#'
#' @examples
#' # Initialize data, params, years, and trend
#' data <- rnorm(n = 100, mean = 100, sd = 10)
#' params <- c(0, 1, 1, 0)
#' years <- seq(from = 1901, to = 2000)
#' trend <- list(location = TRUE, scale = FALSE)
#'
#' # Compute the log-likelihood
#' loglik_gno(data, params, years, trend)
#'
#' @name loglik_methods
NULL


#' @rdname loglik_methods
#' @export
loglik_gum <- function(data, params, years = NULL, trend = NULL) {
	data <- validate_numeric("data", data, optional = FALSE)
	params <- validate_params("GUM", params, trend)
	years <- validate_numeric("years", years, size = length(data))
	trend <- validate_trend(trend)
	loglik_fast(data, "GUM", params, years, trend)
}


#' @rdname loglik_methods
#' @export
loglik_nor <- function(data, params, years = NULL, trend = NULL) {
	data <- validate_numeric("data", data, optional = FALSE)
	params <- validate_params("NOR", params, trend)
	years <- validate_numeric("years", years, size = length(data))
	trend <- validate_trend(trend)
	loglik_fast(data, "NOR", params, years, trend)
}


#' @rdname loglik_methods
#' @export
loglik_lno <- function(data, params, years = NULL, trend = NULL) {
	data <- validate_numeric("data", data, optional = FALSE)
	years <- validate_numeric("years", years, size = length(data))
	trend <- validate_trend(trend)
	params <- validate_params("LNO", params, trend)
	loglik_fast(data, "LNO", params, years, trend)
}


#' @rdname loglik_methods
#' @export
loglik_gev <- function(data, params, years = NULL, trend = NULL) {
	data <- validate_numeric("data", data, optional = FALSE)
	params <- validate_params("GEV", params, trend)
	years <- validate_numeric("years", years, size = length(data))
	trend <- validate_trend(trend)
	loglik_fast(data, "GEV", params, years, trend)
}


#' @rdname loglik_methods
#' @export
loglik_glo <- function(data, params, years = NULL, trend = NULL) {
	data <- validate_numeric("data", data, optional = FALSE)
	params <- validate_params("GLO", params, trend)
	years <- validate_numeric("years", years, size = length(data))
	trend <- validate_trend(trend)
	loglik_fast(data, "GLO", params, years, trend)
}


#' @rdname loglik_methods
#' @export
loglik_gno <- function(data, params, years = NULL, trend = NULL) {
	data <- validate_numeric("data", data, optional = FALSE)
	params <- validate_params("GNO", params, trend)
	years <- validate_numeric("years", years, size = length(data))
	trend <- validate_trend(trend)
	loglik_fast(data, "GNO", params, years, trend)
}


#' @rdname loglik_methods
#' @export
loglik_pe3 <- function(data, params, years = NULL, trend = NULL) {
	data <- validate_numeric("data", data, optional = FALSE)
	params <- validate_params("PE3", params, trend)
	years <- validate_numeric("years", years, size = length(data))
	trend <- validate_trend(trend)
	loglik_fast(data, "PE3", params, years, trend)
}


#' @rdname loglik_methods
#' @export
loglik_lp3 <- function(data, params, years = NULL, trend = NULL) {
	data <- validate_numeric("data", data, optional = FALSE)
	params <- validate_params("LP3", params, trend)
	years <- validate_numeric("years", years, size = length(data))
	trend <- validate_trend(trend)
	loglik_fast(data, "LP3", params, years, trend)
}


#' @rdname loglik_methods
#' @export
loglik_wei <- function(data, params, years = NULL, trend = NULL) {
	data <- validate_numeric("data", data, optional = FALSE)
	params <- validate_params("WEI", params, trend)
	years <- validate_numeric("years", years, size = length(data))
	trend <- validate_trend(trend)
	loglik_fast(data, "WEI", params, years, trend)
}
