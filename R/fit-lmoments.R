#' L-Moments Parameter Estimation
#'
#' Estimates the parameters of a *stationary* probability model using the L-moments. 
#'
#' @inheritParams param-data
#' @inheritParams param-distribution
#'
#' @details 
#' First, the sample L-moments of the data are computed using [utils_sample_lmoments()].
#' Then, formulas from Hosking (1997) are used to match the parameters to 
#' the sample L-moments. The distributions `"GNO"`, `"PE3"`, and `"LP3"` use a 
#' rational approximation of the parameters since no closed-form expression is known.
#'
#' @return A list containing the results of parameter estimation:
#' - `data`: The `data` argument.
#' - `distribution`: The `distribution` argument.
#' - `method`: `"L-moments"`.
#' - `params`: Numeric vector of estimated parameters.
#'
#' @seealso [fit_lmoments_kappa()], [fit_mle()], [fit_gmle()]
#'
#' @examples
#' data <- rnorm(n = 100, mean = 100, sd = 10)
#' fit_lmoments(data, "GUM")
#'
#' @references
#' Hosking, J.R.M. & Wallis, J.R., 1997. Regional frequency analysis: an approach based 
#' on L-Moments. Cambridge University Press, New York, USA.
#'
#' @export
fit_lmoments <- function(data, distribution) {
	data <- validate_numeric("data", data, bounds = c(0, Inf))
	distribution <- validate_enum("distribution", distribution)
	fit_lmoments_fast(data, distribution)
}


