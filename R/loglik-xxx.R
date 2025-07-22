#' Log-Likelihood Functions for Probability Models
#'
#' Compute the log-likelihood value for stationary and nonstationary variants 
#' of nine different distributions: `"GUM"`, `"NOR"`, `"LNO"`, `"GEV"`, `"GLO"`, 
#' `"GNO"`, `"PE3"`, `"LP3"`, or `"WEI"`. In total, these methods compute the
#' log-likelihood for 36 different probability models.
#' 
#' @inheritParams param-data
#' @inheritParams param-params
#' @inheritParams param-years
#' @inheritParams param-structure
#'
#' @return Numeric scalar. The log-likelihood value.
#'
#' @seealso [loglik_fast()]
#'
#' @examples
#' # Initialize data, params, years, and structure
#' data <- rnorm(n = 100, mean = 100, sd = 10)
#' params <- c(0, 1, 1, 0)
#' years <- seq(from = 1901, to = 2000)
#' structure <- list(location = TRUE, scale = FALSE)
#'
#' # Compute the log-likelihood
#' loglik_gno(data, params, years, structure)
#'
#' @name loglik_xxx
NULL


#' @rdname loglik_xxx
#' @export
loglik_gum <- function(data, params, years = NULL, structure = NULL) {
	data <- validate_numeric("data", data, optional = FALSE)
	params <- validate_params("GUM", params, structure)
	years <- validate_numeric("years", years, size = length(data))
	structure <- validate_structure(structure)
	loglik_fast(data, "GUM", params, years, structure)
}


#' @rdname loglik_xxx
#' @export
loglik_nor <- function(data, params, years = NULL, structure = NULL) {
	data <- validate_numeric("data", data, optional = FALSE)
	params <- validate_params("NOR", params, structure)
	years <- validate_numeric("years", years, size = length(data))
	structure <- validate_structure(structure)
	loglik_fast(data, "NOR", params, years, structure)
}


#' @rdname loglik_xxx
#' @export
loglik_lno <- function(data, params, years = NULL, structure = NULL) {
	data <- validate_numeric("data", data, optional = FALSE)
	years <- validate_numeric("years", years, size = length(data))
	structure <- validate_structure(structure)
	params <- validate_params("LNO", params, structure)
	loglik_fast(data, "LNO", params, years, structure)
}


#' @rdname loglik_xxx
#' @export
loglik_gev <- function(data, params, years = NULL, structure = NULL) {
	data <- validate_numeric("data", data, optional = FALSE)
	params <- validate_params("GEV", params, structure)
	years <- validate_numeric("years", years, size = length(data))
	structure <- validate_structure(structure)
	loglik_fast(data, "GEV", params, years, structure)
}


#' @rdname loglik_xxx
#' @export
loglik_glo <- function(data, params, years = NULL, structure = NULL) {
	data <- validate_numeric("data", data, optional = FALSE)
	params <- validate_params("GLO", params, structure)
	years <- validate_numeric("years", years, size = length(data))
	structure <- validate_structure(structure)
	loglik_fast(data, "GLO", params, years, structure)
}


#' @rdname loglik_xxx
#' @export
loglik_gno <- function(data, params, years = NULL, structure = NULL) {
	data <- validate_numeric("data", data, optional = FALSE)
	params <- validate_params("GNO", params, structure)
	years <- validate_numeric("years", years, size = length(data))
	structure <- validate_structure(structure)
	loglik_fast(data, "GNO", params, years, structure)
}


#' @rdname loglik_xxx
#' @export
loglik_pe3 <- function(data, params, years = NULL, structure = NULL) {
	data <- validate_numeric("data", data, optional = FALSE)
	params <- validate_params("PE3", params, structure)
	years <- validate_numeric("years", years, size = length(data))
	structure <- validate_structure(structure)
	loglik_fast(data, "PE3", params, years, structure)
}


#' @rdname loglik_xxx
#' @export
loglik_lp3 <- function(data, params, years = NULL, structure = NULL) {
	data <- validate_numeric("data", data, optional = FALSE)
	params <- validate_params("LP3", params, structure)
	years <- validate_numeric("years", years, size = length(data))
	structure <- validate_structure(structure)
	loglik_fast(data, "LP3", params, years, structure)
}


#' @rdname loglik_xxx
#' @export
loglik_wei <- function(data, params, years = NULL, structure = NULL) {
	data <- validate_numeric("data", data, optional = FALSE)
	params <- validate_params("WEI", params, structure)
	years <- validate_numeric("years", years, size = length(data))
	structure <- validate_structure(structure)
	loglik_fast(data, "WEI", params, years, structure)
}
