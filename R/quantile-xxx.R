#' Quantile Functions for Probability Models
#'
#' Compute the quantiles for stationary and nonstationary variants of nine 
#' different distributions: `"GUM"`, `"NOR"`, `"LNO"`, `"GEV"`, `"GLO"`, `"GNO"`, 
#' `"PE3"`, `"LP3"`, or `"WEI"`. In total, these methods compute the quantiles
#' for 36 different probability models.
#' 
#' @inheritParams param-p
#' @inheritParams param-params
#' @inheritParams param-slice
#' @inheritParams param-structure
#'
#' @return A numeric vector of quantiles with the same length as `p`.
#'
#' @seealso [quantile_fast()]
#'
#' @examples
#' # Initialize p and params
#' p <- runif(n = 10)
#' params <- c(0, 1, 0)
#'
#' # Compute the quantiles
#' quantile_wei(p, params)
#'
#' @name quantile_xxx
NULL


#' @rdname quantile_xxx
#' @export
quantile_gum <- function(p, params, slice = 1900, structure = NULL) {
	p <- validate_numeric("p", p, FALSE, bounds = c(0, 1))
	params <- validate_params("GUM", params, structure)
	slice <- validate_float("slice", slice)
	structure <- validate_structure(structure)
	quantile_fast(p, "GUM", params, slice, structure)
}


#' @rdname quantile_xxx
#' @export
quantile_nor <- function(p, params, slice = 1900, structure = NULL) {
	p <- validate_numeric("p", p, FALSE, bounds = c(0, 1))
	params <- validate_params("NOR", params, structure)
	slice <- validate_float("slice", slice)
	structure <- validate_structure(structure)
	quantile_fast(p, "NOR", params, slice, structure)
}


#' @rdname quantile_xxx
#' @export
quantile_lno <- function(p, params, slice = 1900, structure = NULL) {
	p <- validate_numeric("p", p, FALSE, bounds = c(0, 1))
	params <- validate_params("LNO", params, structure)
	slice <- validate_float("slice", slice)
	structure <- validate_structure(structure)
	quantile_fast(p, "LNO", params, slice, structure)
}


#' @rdname quantile_xxx
#' @export
quantile_gev <- function(p, params, slice = 1900, structure = NULL) {
	p <- validate_numeric("p", p, FALSE, bounds = c(0, 1))
	params <- validate_params("GEV", params, structure)
	slice <- validate_float("slice", slice)
	structure <- validate_structure(structure)
	quantile_fast(p, "GEV", params, slice, structure)
}


#' @rdname quantile_xxx
#' @export
quantile_glo <- function(p, params, slice = 1900, structure = NULL) {
	p <- validate_numeric("p", p, FALSE, bounds = c(0, 1))
	params <- validate_params("GLO", params, structure)
	slice <- validate_float("slice", slice)
	structure <- validate_structure(structure)
	quantile_fast(p, "GLO", params, slice, structure)
}


#' @rdname quantile_xxx
#' @export
quantile_gno <- function(p, params, slice = 1900, structure = NULL) {
	p <- validate_numeric("p", p, FALSE, bounds = c(0, 1))
	params <- validate_params("GNO", params, structure)
	slice <- validate_float("slice", slice)
	structure <- validate_structure(structure)
	quantile_fast(p, "GNO", params, slice, structure)
}


#' @rdname quantile_xxx
#' @export
quantile_pe3 <- function(p, params, slice = 1900, structure = NULL) {
	p <- validate_numeric("p", p, FALSE, bounds = c(0, 1))
	params <- validate_params("PE3", params, structure)
	slice <- validate_float("slice", slice)
	structure <- validate_structure(structure)
	quantile_fast(p, "PE3", params, slice, structure)
}


#' @rdname quantile_xxx
#' @export
quantile_lp3 <- function(p, params, slice = 1900, structure = NULL) {
	p <- validate_numeric("p", p, FALSE, bounds = c(0, 1))
	params <- validate_params("LP3", params, structure)
	slice <- validate_float("slice", slice)
	structure <- validate_structure(structure)
	quantile_fast(p, "LP3", params, slice, structure)
}


#' @rdname quantile_xxx
#' @export
quantile_wei <- function(p, params, slice = 1900, structure = NULL) {
	p <- validate_numeric("p", p, FALSE, bounds = c(0, 1))
	params <- validate_params("WEI", params, structure)
	slice <- validate_float("slice", slice)
	structure <- validate_structure(structure)
	quantile_fast(p, "WEI", params, slice, structure)
}


#' @rdname quantile_xxx
#' @export
quantile_kap <- function(p, params, slice = 1900, structure = NULL) {
	p <- validate_numeric("p", p, FALSE, bounds = c(0, 1))
	params <- validate_params("KAP", params, structure)
	slice <- validate_float("slice", slice)
	structure <- validate_structure(structure)
	quantile_fast(p, "KAP", params, slice, structure)
}

