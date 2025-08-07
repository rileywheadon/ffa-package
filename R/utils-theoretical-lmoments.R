#' Theoretical L-moments of Probability Distributions
#'
#' @description
#' Computes the first four L-moments and L-moment ratios for *stationary* probability models.
#'
#' @inheritParams param-distribution
#' @inheritParams param-params
#'
#' @details The distributions `"GUM"`, `"NOR"`, `"GEV"`, `"GLO"`, and `"WEI"` have 
#' closed-form solutions for the L-moments and L-moment ratios in terms of the parameters.
#' The distributions `"GNO"` and `"PE3"` use rational approximations of the L-moment ratios 
#' from Hosking (1997). The L-moments ratios for the `"LNO"` and `"LP3"` distributions 
#' are should be compared to the log-transformed data and are thus identical to the `"NOR"` 
#' and `"PE3"` distributions respectively.
#'
#' @return A numeric vector of with four elements:
#' - \eqn{\lambda_1}: L-mean
#' - \eqn{\lambda_2}: L-variance
#' - \eqn{\tau_3}: L-skewness
#' - \eqn{\tau_4}: L-kurtosis
#'
#' @examples
#' utils_theoretical_lmoments("GEV", c(1, 1, 1))
#'
#' @references
#' Hosking, J.R.M. & Wallis, J.R., 1997. Regional frequency analysis: an approach based 
#' on L-Moments. Cambridge University Press, New York, USA.
#'
#' @export
utils_theoretical_lmoments <- function(distribution, params) {
	distribution <- validate_enum("distribution", distribution)
	params <- validate_params(distribution, params, NULL)
	theoretical_lmoments_fast(distribution, params)
}
