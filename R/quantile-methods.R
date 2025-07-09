#' Quantile Functions for Probability Models
#'
#' Compute the quantiles for stationary and non-stationary variants  of nine 
#' different distributions: `"GUM"`, `"NOR"`, `"LNO"`, `"GEV"`, `"GLO"`, `"GNO"`, 
#' `"PE3"`, `"LP3"`, or `"WEI"`.
#' 
#' @param p Numeric; a vector of probabilities between 0 and 1.
#'
#' @inheritParams param-params
#' @inheritParams param-slice
#' @inheritParams param-trend
#'
#' @return A numeric vector of quantiles with the same length as `p`.
#'
#' @seealso \link{quantile_fast}
#'
#' @examples
#' # Initialize p and params
#' p <- runif(n = 10)
#' params <- c(0, 1, 0)
#'
#' # Compute the quantiles
#' quantile_wei(p, params)
#'
#' @name quantile_methods
NULL


#' @rdname quantile_methods
#' @export
quantile_gum <- function(p, params, slice = 1900, trend = NULL) {
	p <- validate_probabilities(p)
	params <- validate_params(params, "GUM", trend)
	slice <- validate_slice(slice)
	trend <- validate_trend(trend)
	quantile_fast(p, "GUM", params, slice, trend)
}


#' @rdname quantile_methods
#' @export
quantile_nor <- function(p, params, slice = 1900, trend = NULL) {
	p <- validate_probabilities(p)
	params <- validate_params(params, "NOR", trend)
	slice <- validate_slice(slice)
	trend <- validate_trend(trend)
	quantile_fast(p, "NOR", params, slice, trend)
}


#' @rdname quantile_methods
#' @export
quantile_lno <- function(p, params, slice = 1900, trend = NULL) {
	p <- validate_probabilities(p)
	params <- validate_params(params, "LNO", trend)
	slice <- validate_slice(slice)
	trend <- validate_trend(trend)
	quantile_fast(p, "LNO", params, slice, trend)
}


#' @rdname quantile_methods
#' @export
quantile_gev <- function(p, params, slice = 1900, trend = NULL) {
	p <- validate_probabilities(p)
	params <- validate_params(params, "GEV", trend)
	slice <- validate_slice(slice)
	trend <- validate_trend(trend)
	quantile_fast(p, "GEV", params, slice, trend)
}


#' @rdname quantile_methods
#' @export
quantile_glo <- function(p, params, slice = 1900, trend = NULL) {
	p <- validate_probabilities(p)
	params <- validate_params(params, "GLO", trend)
	slice <- validate_slice(slice)
	trend <- validate_trend(trend)
	quantile_fast(p, "GLO", params, slice, trend)
}


#' @rdname quantile_methods
#' @export
quantile_gno <- function(p, params, slice = 1900, trend = NULL) {
	p <- validate_probabilities(p)
	params <- validate_params(params, "GNO", trend)
	slice <- validate_slice(slice)
	trend <- validate_trend(trend)
	quantile_fast(p, "GNO", params, slice, trend)
}


#' @rdname quantile_methods
#' @export
quantile_pe3 <- function(p, params, slice = 1900, trend = NULL) {
	p <- validate_probabilities(p)
	params <- validate_params(params, "PE3", trend)
	slice <- validate_slice(slice)
	trend <- validate_trend(trend)
	quantile_fast(p, "PE3", params, slice, trend)
}


#' @rdname quantile_methods
#' @export
quantile_lp3 <- function(p, params, slice = 1900, trend = NULL) {
	p <- validate_probabilities(p)
	params <- validate_params(params, "LP3", trend)
	slice <- validate_slice(slice)
	trend <- validate_trend(trend)
	quantile_fast(p, "LP3", params, slice, trend)
}


#' @rdname quantile_methods
#' @export
quantile_wei <- function(p, params, slice = 1900, trend = NULL) {
	p <- validate_probabilities(p)
	params <- validate_params(params, "WEI", trend)
	slice <- validate_slice(slice)
	trend <- validate_trend(trend)
	quantile_fast(p, "WEI", params, slice, trend)
}


#' @rdname quantile_methods
#' @export
quantile_kap <- function(p, params, slice = 1900, trend = NULL) {
	p <- validate_probabilities(p)
	params <- validate_params(params, "KAP", trend)
	slice <- validate_slice(slice)
	trend <- validate_trend(trend)
	quantile_fast(p, "KAP", params, slice, trend)
}

