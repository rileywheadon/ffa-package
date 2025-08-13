#' Quantile Functions for Probability Models
#'
#' @description
#' Compute the quantiles for stationary and nonstationary probability models.
#' 
#' **For NS-FFA**: To compute the quantiles for a nonstationary probability model, 
#' specify a time slice (`ns_slice`) and the nonstationary model structure 
#' (`ns_structure`).
#'
#' @inheritParams param-p
#' @inheritParams param-distribution
#' @inheritParams param-params
#' @inheritParams param-ns-slice
#' @inheritParams param-ns-structure
#'
#' @return A numeric vector of quantiles with the same length as `p`.
#'
#' @examples
#' p <- runif(n = 100)
#' params <- c(1, 1, 1)
#' utils_quantiles(p, "GEV", params)
#'
#' @importFrom stats qgamma qlnorm
#' @export
utils_quantiles <- function(
	p,
	distribution,
	params,
	ns_slice = 0,
	ns_structure = NULL
) {
	p <- validate_numeric("p", p, bounds = c(0, 1))
	distribution <- validate_enum("distribution", distribution)
	params <- validate_params(distribution, params, ns_structure)
	slice <- validate_float("slice", ns_slice)
	structure <- validate_structure(ns_structure)
	quantiles_fast(p, distribution, params, slice, structure)
}

