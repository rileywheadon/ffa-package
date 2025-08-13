#' Cumulative Distribution Functions for Probability Models
#'
#' @description
#' Compute probabilities from quantiles for both stationary and nonstationary models. 
#' 
#' **For NS-FFA**: To compute the probabilities for a nonstationary model, specify a 
#' time slice (`ns_slice`) and the nonstationary model structure (`ns_structure`).
#'
#' @inheritParams param-q
#' @inheritParams param-distribution
#' @inheritParams param-params
#' @inheritParams param-ns-slice
#' @inheritParams param-ns-structure
#'
#' @return A numeric vector of quantiles with the same length as `q`.
#'
#' @examples
#' q <- seq(1, 10)
#' params <- c(1, 1, 1)
#' utils_cdf(q, "GEV", params)
#'
#' @importFrom stats pgamma plnorm
#' @export
utils_cdf <- function(
	q,
	distribution,
	params,
	ns_slice = 0,
	ns_structure = NULL
) {
	q <- validate_numeric("q", q)
	distribution <- validate_enum("distribution", distribution)
	params <- validate_params(distribution, params, ns_structure)
	slice <- validate_float("slice", ns_slice)
	structure <- validate_structure(ns_structure)
	cdf_fast(q, distribution, params, slice, structure)
}

