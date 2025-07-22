#' Parameter Estimation with L-Moments
#'
#' Estimate the parameters of nine different distributions (`"GUM"`, `"NOR"`,  
#' `"LNO"`, `"GEV"`, `"GLO"`, `"GNO"`, `"PE3"`, `"LP3"`, and `"WEI"`) using 
#' the method of L-moments. 
#'
#' @inheritParams param-data
#'
#' @details 
#' First, the sample L-moments of the data are computed using the [lmom_sample()] 
#' method. Then formulas from Hosking (1997) are used to compute the parameters from 
#' the L-moments. Distributions `"GNO"`, `"PE3"`, and `"LP3"` use a rational 
#' approximation of the parameters.
#'
#' @return A list containing the results of parameter estimation:
#' - `method`: `"L-moments"`.
#' - `params`: numeric vector of 2 or 3 parameters depending on the distribution.
#'
#' @seealso [fit_lmom_fast()], [fit_lmom_kappa()]
#'
#' @examples
#' data <- rnorm(n = 100, mean = 100, sd = 10)
#' fit_lmom_lp3(data)
#'
#' @references
#' Hosking, J.R.M. & Wallis, J.R., 1997. Regional frequency analysis: an approach based 
#' on L-Moments. Cambridge University Press, New York, USA.
#'
#' @name fit_lmom_xxx
NULL

#' @rdname fit_lmom_xxx
#' @export
fit_lmom_gum <- function(data) {
	data <- validate_numeric("data", data, bounds = c(0, Inf))
	fit_lmom_fast(data, "GUM")
}

#' @rdname fit_lmom_xxx
#' @export
fit_lmom_nor <- function(data) {
	data <- validate_numeric("data", data, bounds = c(0, Inf))
	fit_lmom_fast(data, "NOR")
}

#' @rdname fit_lmom_xxx
#' @export
fit_lmom_lno <- function(data) {
	data <- validate_numeric("data", data, bounds = c(0, Inf))
	fit_lmom_fast(data, "LNO")
}

#' @rdname fit_lmom_xxx
#' @export
fit_lmom_gev <- function(data) {
	data <- validate_numeric("data", data, bounds = c(0, Inf))
	fit_lmom_fast(data, "GEV")
}

#' @rdname fit_lmom_xxx
#' @export
fit_lmom_glo <- function(data) {
	data <- validate_numeric("data", data, bounds = c(0, Inf))
	fit_lmom_fast(data, "GLO")
}

#' @rdname fit_lmom_xxx
#' @export
fit_lmom_gno <- function(data) {
	data <- validate_numeric("data", data, bounds = c(0, Inf))
	fit_lmom_fast(data, "GNO")
}

#' @rdname fit_lmom_xxx
#' @export
fit_lmom_pe3 <- function(data) {
	data <- validate_numeric("data", data, bounds = c(0, Inf))
	fit_lmom_fast(data, "PE3")
}

#' @rdname fit_lmom_xxx
#' @export
fit_lmom_lp3 <- function(data) {
	data <- validate_numeric("data", data, bounds = c(0, Inf))
	fit_lmom_fast(data, "LP3")
}

#' @rdname fit_lmom_xxx
#' @export
fit_lmom_wei <- function(data) {
	data <- validate_numeric("data", data, bounds = c(0, Inf))
	fit_lmom_fast(data, "WEI")
}
