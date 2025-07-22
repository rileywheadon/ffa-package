#' Generalized Log-Likelihood Functions for GEV Models
#'
#' Computes the generalized log-likelihood for stationary and nonstationary 
#' variants of the Generalized Extreme Value (GEV) distribution with a geophysical 
#' (Beta) prior distribution for the shape parameter (Martins and Stedinger, 2000).
#' 
#' @details 
#' The generalized log-likelihood is defined as sum of the log-likelihood of the 
#' specified model and the log-density of the Beta prior with parameters \eqn{(p, q)}.
#' The contribution of the prior is: \deqn{\log \pi(\kappa) = (p-1) \log(0.5-\kappa) 
#' + (q-1) \log(0.5+\kappa) - \log (\beta(p, q))}
#'
#' @inheritParams param-data
#' @inheritParams param-params
#' @inheritParams param-prior
#' @inheritParams param-years
#' @inheritParams param-structure
#'
#' @return Numeric scalar. The generalized log-likelihood value.
#'
#' @seealso [general_loglik_fast()]
#'
#' @examples
#' # Initialize data, params, prior, years, and structure
#' data <- rnorm(n = 100, mean = 100, sd = 10)
#' params <- c(0, 1, 1, 0)
#' prior <- c(5, 10)
#' years <- seq(from = 1901, to = 2000)
#' structure <- list(location = TRUE, scale = FALSE)
#'
#' # Compute the generalized log-likelihood
#' general_loglik_gev(data, params, prior, years, structure)
#'
#' @references
#' El Adlouni, S., Ouarda, T.B.M.J., Zhang, X., Roy, R., Bob´ee, B., 2007. Generalized 
#' maximum likelihood estimators for the nonstationary generalized extreme value 
#' model. Water Resour. Res. 43 (3), 1–13. \doi{10.1029/2005WR004545}
#'
#' Martins, E. S., and Stedinger, J. R. (2000). Generalized maximum-likelihood generalized 
#' extreme-value quantile estimators for hydrologic data. Water Resources Research, 36(3), 
#' 737–744. \doi{10.1029/1999WR900330}
#'
#' @export

general_loglik_gev <- function(
	data,
	params,
	prior,
	years = NULL,
	structure = NULL
) {

	data <- validate_numeric("data", data, optional = FALSE)
	params <- validate_params("GEV", params, structure)
	prior <- validate_numeric("prior", prior, size = 2, bounds = c(0, Inf))
	years <- validate_numeric("years", years, size = length(data))
	structure <- validate_structure(structure)
	general_loglik_fast(data, "GEV", params, prior, years, structure)

}

