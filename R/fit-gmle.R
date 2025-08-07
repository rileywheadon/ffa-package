#' Generalized Maximum Likelihood Parameter Estimation
#'
#' @description
#' Estimates the parameters of the generalized extreme value (GEV) distribution by 
#' maximizing the generalized log‐likelihood, which incorporates a Beta prior on the 
#' shape parameter. Initial parameter estimates are obtained using the method of L‐moments
#' and optimization is performed via [stats::nlminb()] with repeated perturbations if needed.
#'
#' **NS-FFA**: To estimate parameters for a nonstationary model, include the observation
#' years (`ns_years`) and the nonstationary model structure (`ns_structure`).
#'
#' @inheritParams param-data
#' @inheritParams param-prior
#' @inheritParams param-ns-years
#' @inheritParams param-ns-structure
#'
#' @return A list containing the results of parameter estimation:
#' - `data`: The `data` argument.
#' - `prior`: The `prior` argument.
#' - `ns_years`: The `ns_years` argument, if given.
#' - `ns_structure`: The `ns_structure` argument, if given.
#' - `method`: `"GMLE"`.
#' - `params`: Numeric vector of estimated parameters.
#' - `mll`: The maximum value of the generalized log‐likelihood.
#'
#' @details
#' 1. Calls [fit_lmoments()] on the data to obtain initial parameter estimates.
#' 2. Initializes trend parameters to zero if necessary. 
#' 3. Defines an objective function using [utils_generalized_likelihood()]. 
#' 4. Runs [stats::nlminb()] with box constraints. Attempts minimization up to 100 times.
#'
#' @seealso [utils_generalized_likelihood()], [fit_lmoments()], [stats::nlminb()]
#'
#' @examples
#' data <- rnorm(n = 100, mean = 100, sd = 10)
#' prior <- c(6, 9)
#' ns_years <- seq(from = 1901, to = 2000)
#' ns_structure <- list(location = TRUE, scale = FALSE)
#' fit_gmle(data, prior, ns_years, ns_structure)
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
#' @importFrom stats rnorm nlminb
#' @export

fit_gmle <- function(
	data,
	prior,
	ns_years = NULL,
	ns_structure = NULL
) {
	data <- validate_numeric("data", data)
	prior <- validate_numeric("prior", prior, size = 2, bounds = c(0, Inf))
	ns_years <- validate_numeric("ns_years", ns_years, TRUE, size = length(data))
	ns_structure <- validate_structure(ns_structure)
	fit_maximum_likelihood(data, "GEV", prior, ns_years, ns_structure)
}
