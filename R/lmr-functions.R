#' Theoretical L-moments of Probability Distributions
#'
#' Computes the first four L-moments and L-moment ratios of seven different 
#' probability distributions (`GUM`, `NOR`, `GEV`, `GLO`, `GNO`, `PE3`, and `WEI`)
#' given the parameters of the distribution.
#'
#' @param params Numeric; a vector of parameters. Must have the correct length for the model.
#'
#' @details
#' The distributions `GUM`, `NOR`, `GEV`, `GLO`, and `WEI` have closed-form solutions
#' for the L-moments and L-moment ratios in terms of the parameters. The distributions 
#' `GNO` and `PE3` use rational approximations of the L-moment ratios from Hosking (1997).
#'
#' @return A numeric vector of length 4 containing:
#' - \eqn{\lambda_1}: L-mean
#' - \eqn{\lambda_2}: L-variance
#' - \eqn{\tau_3}: L-skewness
#' - \eqn{\tau_4}: L-kurtosis
#'
#' @references
#' Hosking, J.R.M. & Wallis, J.R., 1997. Regional frequency analysis: an approach based 
#' on L-Moments. Cambridge University Press, New York, USA.
#'
#' @examples
#' lmrgev(c(0, 1, 0))
#'
#' @name lmr-functions
NULL

#' @rdname lmr-functions
#' @export
lmrgum <- function(params) lmrxxx("GUM", params)

#' @rdname lmr-functions
#' @export
lmrnor <- function(params) lmrxxx("NOR", params)

#' @rdname lmr-functions
#' @export
lmrgev <- function(params) lmrxxx("GEV", params)

#' @rdname lmr-functions
#' @export
lmrglo <- function(params) lmrxxx("GLO", params)

#' @rdname lmr-functions
#' @export
lmrgno <- function(params) lmrxxx("GNO", params)

#' @rdname lmr-functions
#' @export
lmrpe3 <- function(params) lmrxxx("PE3", params)

#' @rdname lmr-functions
#' @export
lmrwei <- function(params) lmrxxx("WEI", params)
