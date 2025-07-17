#' Quantile Functions for Probability Models
#'
#' Compute the quantiles for stationary and nonstationary variants  of nine 
#' different distributions: `"GUM"`, `"NOR"`, `"LNO"`, `"GEV"`, `"GLO"`, `"GNO"`, 
#' `"PE3"`, `"LP3"`, or `"WEI"`.
#' 
#' @inheritParams param-p
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
	p <- validate_numeric("p", p, FALSE, bounds = c(0, 1))
	params <- validate_params("GUM", params, trend)
	slice <- validate_float("slice", slice)
	trend <- validate_trend(trend)
	quantile_fast(p, "GUM", params, slice, trend)
}


#' @rdname quantile_methods
#' @export
quantile_nor <- function(p, params, slice = 1900, trend = NULL) {
	p <- validate_numeric("p", p, FALSE, bounds = c(0, 1))
	params <- validate_params("NOR", params, trend)
	slice <- validate_float("slice", slice)
	trend <- validate_trend(trend)
	quantile_fast(p, "NOR", params, slice, trend)
}


#' @rdname quantile_methods
#' @export
quantile_lno <- function(p, params, slice = 1900, trend = NULL) {
	p <- validate_numeric("p", p, FALSE, bounds = c(0, 1))
	params <- validate_params("LNO", params, trend)
	slice <- validate_float("slice", slice)
	trend <- validate_trend(trend)
	quantile_fast(p, "LNO", params, slice, trend)
}


#' @rdname quantile_methods
#' @export
quantile_gev <- function(p, params, slice = 1900, trend = NULL) {
	p <- validate_numeric("p", p, FALSE, bounds = c(0, 1))
	params <- validate_params("GEV", params, trend)
	slice <- validate_float("slice", slice)
	trend <- validate_trend(trend)
	quantile_fast(p, "GEV", params, slice, trend)
}


#' @rdname quantile_methods
#' @export
quantile_glo <- function(p, params, slice = 1900, trend = NULL) {
	p <- validate_numeric("p", p, FALSE, bounds = c(0, 1))
	params <- validate_params("GLO", params, trend)
	slice <- validate_float("slice", slice)
	trend <- validate_trend(trend)
	quantile_fast(p, "GLO", params, slice, trend)
}


#' @rdname quantile_methods
#' @export
quantile_gno <- function(p, params, slice = 1900, trend = NULL) {
	p <- validate_numeric("p", p, FALSE, bounds = c(0, 1))
	params <- validate_params("GNO", params, trend)
	slice <- validate_float("slice", slice)
	trend <- validate_trend(trend)
	quantile_fast(p, "GNO", params, slice, trend)
}


#' @rdname quantile_methods
#' @export
quantile_pe3 <- function(p, params, slice = 1900, trend = NULL) {
	p <- validate_numeric("p", p, FALSE, bounds = c(0, 1))
	params <- validate_params("PE3", params, trend)
	slice <- validate_float("slice", slice)
	trend <- validate_trend(trend)
	quantile_fast(p, "PE3", params, slice, trend)
}


#' @rdname quantile_methods
#' @export
quantile_lp3 <- function(p, params, slice = 1900, trend = NULL) {
	p <- validate_numeric("p", p, FALSE, bounds = c(0, 1))
	params <- validate_params("LP3", params, trend)
	slice <- validate_float("slice", slice)
	trend <- validate_trend(trend)
	quantile_fast(p, "LP3", params, slice, trend)
}


#' @rdname quantile_methods
#' @export
quantile_wei <- function(p, params, slice = 1900, trend = NULL) {
	p <- validate_numeric("p", p, FALSE, bounds = c(0, 1))
	params <- validate_params("WEI", params, trend)
	slice <- validate_float("slice", slice)
	trend <- validate_trend(trend)
	quantile_fast(p, "WEI", params, slice, trend)
}


#' @rdname quantile_methods
#' @export
quantile_kap <- function(p, params, slice = 1900, trend = NULL) {
	p <- validate_numeric("p", p, FALSE, bounds = c(0, 1))
	params <- validate_params("KAP", params, trend)
	slice <- validate_float("slice", slice)
	trend <- validate_trend(trend)
	quantile_fast(p, "KAP", params, slice, trend)
}

