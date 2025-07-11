#' Theoretical L-moments of Probability Distributions
#'
#' Computes the first four L-moments and L-moment ratios of seven different 
#' probability distributions (`GUM`, `NOR`, `GEV`, `GLO`, `GNO`, `PE3`, and `WEI`)
#' given the parameters of the distribution.
#'
#' @inheritParams param-params
#'
#' @details
#' The distributions `GUM`, `NOR`, `GEV`, `GLO`, and `WEI` have closed-form solutions
#' for the L-moments and L-moment ratios in terms of the parameters. The distributions 
#' `GNO`, `PE3` use rational approximations of the L-moment ratios from Hosking (1997).
#'
#' @return A numeric vector of length 4 containing:
#' - \eqn{\lambda_1}: L-mean
#' - \eqn{\lambda_2}: L-variance
#' - \eqn{\tau_3}: L-skewness
#' - \eqn{\tau_4}: L-kurtosis
#'
#' @seealso \link{lmom_fast}
#'
#' @references
#' Hosking, J.R.M. & Wallis, J.R., 1997. Regional frequency analysis: an approach based 
#' on L-Moments. Cambridge University Press, New York, USA.
#'
#' @examples
#' lmom_theoretical_gev(c(0, 1, 0))
#'
#' @name lmom_theoretical
NULL

#' @rdname lmom_theoretical
#' @export
lmom_theoretical_gum <- function(params) {
	params <- validate_params("GUM", params, NULL)
	lmom_fast("GUM", params)
}

#' @rdname lmom_theoretical
#' @export
lmom_theoretical_nor <- function(params) {
	params <- validate_params("NOR", params, NULL)
	lmom_fast("NOR", params)
}

#' @rdname lmom_theoretical
#' @export
lmom_theoretical_gev <- function(params) {
	params <- validate_params("GEV", params, NULL)
	lmom_fast("GEV", params)
}

#' @rdname lmom_theoretical
#' @export
lmom_theoretical_glo <- function(params) {
	params <- validate_params("GLO", params, NULL)
	lmom_fast("GLO", params)
}

#' @rdname lmom_theoretical
#' @export
lmom_theoretical_gno <- function(params) {
	params <- validate_params("GNO", params, NULL)
	lmom_fast("GNO", params)
}

#' @rdname lmom_theoretical
#' @export
lmom_theoretical_pe3 <- function(params) {
	params <- validate_params("PE3", params, NULL)
	lmom_fast("PE3", params)
}

#' @rdname lmom_theoretical
#' @export
lmom_theoretical_wei <- function(params) {
	params <- validate_params("WEI", params, NULL)
	lmom_fast("WEI", params)
}
