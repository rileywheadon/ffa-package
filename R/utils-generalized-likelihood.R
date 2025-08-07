#' Generalized Log-Likelihood Functions for GEV Models
#'
#' @description
#' Computes the generalized log-likelihood for stationary and nonstationary 
#' variants of the Generalized Extreme Value (GEV) distribution with a geophysical 
#' (Beta) prior distribution for the shape parameter (Martins and Stedinger, 2000).
#'
#' **NS-FFA**: To compute the generalized log-likelihood for a nonstationary probability
#' model, include the observation years (`ns_years`) and the nonstationary model structure
#' (`ns_structure`).
#' 
#' @details 
#' The generalized log-likelihood is defined as sum of (1) the log-likelihood and (2) 
#' the log-density of the Beta prior with parameters \eqn{(p, q)}. The contribution of 
#' the prior is: \deqn{\log \pi(\kappa) = (p-1) \log(0.5-\kappa) + (q-1) \log(0.5+\kappa) 
#' - \log (\beta(p, q))}
#'
#' @inheritParams param-data
#' @inheritParams param-params
#' @inheritParams param-prior
#' @inheritParams param-ns-years
#' @inheritParams param-ns-structure
#'
#' @return Numeric scalar. The generalized log-likelihood value.
#'
#' @seealso [utils_log_likelihood()]
#'
#' @examples
#' data <- rnorm(n = 100, mean = 100, sd = 10)
#' params <- c(0, 1, 1, 0)
#' prior <- c(5, 10)
#' ns_years <- seq(from = 1901, to = 2000)
#' ns_structure <- list(location = TRUE, scale = FALSE)
#'
#' # Compute the generalized log-likelihood
#' utils_generalized_likelihood(data, params, prior, ns_years, ns_structure)
#'
#' @references
#' El Adlouni, S., Ouarda, T.B.M.J., Zhang, X., Roy, R., Bobee, B., 2007. Generalized 
#' maximum likelihood estimators for the nonstationary generalized extreme value 
#' model. Water Resources Research 43 (3), 1–13. \doi{10.1029/2005WR004545}
#'
#' Martins, E. S., and Stedinger, J. R. (2000). Generalized maximum-likelihood generalized 
#' extreme-value quantile estimators for hydrologic data. Water Resources Research, 36(3), 
#' 737–744. \doi{10.1029/1999WR900330}
#'
#' @export
utils_generalized_likelihood <- function(
	data,
	params,
	prior,
	ns_years = NULL,
	ns_structure = NULL
) {

	data <- validate_numeric("data", data)
	params <- validate_params("GEV", params, ns_structure)
	prior <- validate_numeric("prior", prior, size = 2, bounds = c(0, Inf))
	years <- validate_numeric("ns_years", ns_years, TRUE, size = length(data))
	structure <- validate_structure(ns_structure)
	generalized_likelihood_fast(data, params, prior, years, structure)

}

