#' Parameter Estimation with L-Moments
#'
#' Estimate the parameters of one of nine different distributions (`GUM`, `NOR`, 
#' `LNO`, `GEV`, `GLO`, `GNO`, `PE3`, `LP3`, and `WEI`) using the method of L-moments. 
#'
#' @param data Numeric; a vector of annual maximum streamflow data.
#'
#' @details 
#' First, the sample L-moments of the data are computed using the \link{lmom.sample} 
#' method. Then formulas from Hosking (1997) are used to compute the parameters from the
#' L-moments. Distributions `GNO`, `PE3`, and `LP3` use a rational approximation to compute
#' the parameters.
#'
#' @return Numeric; a vector of parameters. 
#'
#' @references
#' Hosking, J.R.M. & Wallis, J.R., 1997. Regional frequency analysis: an approach based 
#' on L-Moments. Cambridge University Press, New York, USA.
#'
#' @examples
#' data <- rnorm(n = 100, mean = 100, sd = 10)
#' fit_lmom_lp3(data)
#'
#' @name pel-functions
NULL

#' @rdname pel-functions
#' @export
fit_lmom_gum <- function(data) {
	validate.data(data)
	fit_lmom_fast("GUM", data)
}

#' @rdname pel-functions
#' @export
fit_lmom_nor <- function(data) {
	validate.data(data)
	fit_lmom_fast("NOR", data)
}

#' @rdname pel-functions
#' @export
fit_lmom_lno <- function(data) {
	validate.data(data)
	fit_lmom_fast("LNO", data)
}

#' @rdname pel-functions
#' @export
fit_lmom_gev <- function(data) {
	validate.data(data)
	fit_lmom_fast("GEV", data)
}

#' @rdname pel-functions
#' @export
fit_lmom_glo <- function(data) {
	validate.data(data)
	fit_lmom_fast("GLO", data)
}

#' @rdname pel-functions
#' @export
fit_lmom_gno <- function(data) {
	validate.data(data)
	fit_lmom_fast("GNO", data)
}

#' @rdname pel-functions
#' @export
fit_lmom_pe3 <- function(data) {
	validate.data(data)
	fit_lmom_fast("PE3", data)
}

#' @rdname pel-functions
#' @export
fit_lmom_lp3 <- function(data) {
	validate.data(data)
	fit_lmom_fast("LP3", data)
}

#' @rdname pel-functions
#' @export
fit_lmom_wei <- function(data) {
	validate.data(data)
	fit_lmom_fast("WEI", data)
}
