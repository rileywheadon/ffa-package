#' Log-Likelihood Functions for Probability Models
#'
#' @description
#' Compute the log-likelihood for stationary and nonstationary probability models.
#' 
#' **NS-FFA**: To compute the log-likelihood for a nonstationary probability model, 
#' include the observation years (`ns_years`) and the nonstationary model structure
#' (`ns_structure`).
#'
#' @inheritParams param-data
#' @inheritParams param-distribution
#' @inheritParams param-params
#' @inheritParams param-ns-years
#' @inheritParams param-ns-structure
#'
#' @return Numeric scalar. The log-likelihood value.
#'
#' @seealso [utils_generalized_likelihood()]
#'
#' @examples
#' data <- rnorm(n = 100, mean = 100, sd = 10)
#' params <- c(0, 1, 1, 0)
#' ns_years <- seq(from = 1901, to = 2000)
#' ns_structure <- list(location = TRUE, scale = FALSE)
#'
#' # Compute the log-likelihood
#' utils_log_likelihood(data, "GNO", params, ns_years, ns_structure)
#'
#' @export
utils_log_likelihood <- function(
	data,
	distribution,
	params,
	ns_years = NULL,
	ns_structure = NULL
) {
	data <- validate_numeric("data", data, optional = FALSE)
	distribution <- validate_enum("distribution", distribution)
	params <- validate_params(distribution, params, ns_structure)
	years <- validate_numeric("ns_years", ns_years, TRUE, size = length(data))
	structure <- validate_structure(ns_structure)
	log_likelihood_fast(data, distribution, params, years, structure)
}


