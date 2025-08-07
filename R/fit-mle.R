#' Maximum Likelihood Parameter Estimation
#'
#' @description
#' Estimates the parameters of a probability distribution by maximizing the log‐likelihood. 
#' Initial parameter estimates are obtained using the method of L‐moments and optimization 
#' is performed via [stats::nlminb()] with repeated perturbations if needed.
#'
#' **NS-FFA**: To estimate parameters for a nonstationary model, include the observation
#' years (`ns_years`) and the nonstationary model structure (`ns_structure`).
#'
#' @inheritParams param-data
#' @inheritParams param-distribution
#' @inheritParams param-ns-years
#' @inheritParams param-ns-structure
#'
#' @return A list containing the results of parameter estimation:
#' - `data`: The `data` argument.
#' - `distribution`: The `distribution` argument.
#' - `ns_years`: The `ns_years` argument, if given.
#' - `ns_structure`: The `ns_structure` argument, if given.
#' - `method`: `"MLE"`.
#' - `params`: Numeric vector of estimated parameters.
#' - `mll`: The maximum value of the log‐likelihood.
#'
#' @details
#' 1. Calls [fit_lmoments()] on `data` to obtain initial parameter estimates.
#' 2. Initializes trend parameters to zero if necessary. 
#' 3. For `WEI` models, sets the location parameter to zero to ensure support. 
#' 4. Defines an objective function using [utils_log_likelihood()]. 
#' 5. Runs [stats::nlminb()] with box constraints. Attempts minimization up to 100 times.
#'
#' @seealso [utils_log_likelihood()], [fit_lmoments()], [stats::nlminb()]
#'
#' @examples
#' data <- rnorm(n = 100, mean = 100, sd = 10)
#' ns_years <- seq(from = 1901, to = 2000)
#' ns_structure <- list(location = TRUE, scale = FALSE)
#' fit_mle(data, "GNO", ns_years, ns_structure)
#' 
#' @importFrom stats rnorm nlminb
#' @export

fit_mle <- function(
	data,
	distribution,
	ns_years = NULL,
	ns_structure = NULL
) {
	data <- validate_numeric("data", data, optional = FALSE)
	distribution <- validate_enum("distribution", distribution)
	ns_years <- validate_numeric("ns_years", ns_years, TRUE, size = length(data))
	ns_structure <- validate_structure(ns_structure)
	fit_maximum_likelihood(data, distribution, NULL, ns_years, ns_structure)
}
