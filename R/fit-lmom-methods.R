#' Parameter Estimation with L-Moments
#'
#' Estimate the parameters of one of nine different distributions (`GUM`, `NOR`, 
#' `LNO`, `GEV`, `GLO`, `GNO`, `PE3`, `LP3`, and `WEI`) using the method of L-moments. 
#'
#' @inheritParams param-data
#'
#' @details 
#' First, the sample L-moments of the data are computed using the \link{lmom_sample} 
#' method. Then formulas from Hosking (1997) are used to compute the parameters from 
#' the L-moments. Distributions `GNO`, `PE3`, and `LP3` use a rational approximation 
#' to compute the parameters.
#'
#' @return A numeric vector of parameters:
#' - If `model` is `"GUM"`, `"NOR"`, or `"LNO"`, returns a vector of length 2.
#' - Otherwise, returns a vector of length 3.
#'
#' @references
#' Hosking, J.R.M. & Wallis, J.R., 1997. Regional frequency analysis: an approach based 
#' on L-Moments. Cambridge University Press, New York, USA.
#'
#' @seealso \link{fit_lmom_fast}, \link{fit_lmom_kappa}
#'
#' @examples
#' data <- rnorm(n = 100, mean = 100, sd = 10)
#' fit_lmom_lp3(data)
#'
#' @name fit_lmom_methods
NULL

#' @rdname fit_lmom_methods
#' @export
fit_lmom_gum <- function(data) {
	data <- validate_data(data)
	fit_lmom_fast(data, "GUM")
}

#' @rdname fit_lmom_methods
#' @export
fit_lmom_nor <- function(data) {
	data <- validate_data(data)
	fit_lmom_fast(data, "NOR")
}

#' @rdname fit_lmom_methods
#' @export
fit_lmom_lno <- function(data) {
	data <- validate_data(data)
	fit_lmom_fast(data, "LNO")
}

#' @rdname fit_lmom_methods
#' @export
fit_lmom_gev <- function(data) {
	data <- validate_data(data)
	fit_lmom_fast(data, "GEV")
}

#' @rdname fit_lmom_methods
#' @export
fit_lmom_glo <- function(data) {
	data <- validate_data(data)
	fit_lmom_fast(data, "GLO")
}

#' @rdname fit_lmom_methods
#' @export
fit_lmom_gno <- function(data) {
	data <- validate_data(data)
	fit_lmom_fast(data, "GNO")
}

#' @rdname fit_lmom_methods
#' @export
fit_lmom_pe3 <- function(data) {
	data <- validate_data(data)
	fit_lmom_fast(data, "PE3")
}

#' @rdname fit_lmom_methods
#' @export
fit_lmom_lp3 <- function(data) {
	data <- validate_data(data)
	fit_lmom_fast(data, "LP3")
}

#' @rdname fit_lmom_methods
#' @export
fit_lmom_wei <- function(data) {
	data <- validate_data(data)
	fit_lmom_fast(data, "WEI")
}
