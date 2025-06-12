#' Perform Generalized MLE for GEV Models with Trend and Prior
#'
#' @description
#' Fits a Generalized Extreme Value (GEV) model—stationary or with linear trends
#' in location and/or scale—by maximizing the posterior log‐likelihood (MLE with
#' Beta prior on the shape). Uses L‐moment estimates for initialization and
#' nlminb for constrained optimization.
#'
#' @param data Numeric vector of observations (e.g. annual maxima). Any ’NaN’
#'   values are removed before fitting.
#'
#' @param model Character string specifying the GEV model form:
#'   ’GEV’   = stationary,  
#'   ’GEV100’ = trend in location only,  
#'   ’GEV110’ = trends in location and scale.  
#'
#' @param prior Numeric vector of length 2 giving the Beta prior parameters
#'   (p, q) on the shape parameter kappa.
#'
#' @return
#' A list with components:
#' - params: Numeric vector of the estimated GEV parameters in the same order as
#'   used by generalized.likelihood().  
#' - mll   : The maximized log‐posterior (i.e. log‐likelihood plus log‐prior).
#'
#' @details
#' 1. Calls lmom.estimation() on the cleaned data (removes NaN) to obtain initial
#'    estimates of (mu, sigma, kappa).  
#' 2. Sets initial trend parameters to 0 for non‐stationary models.  
#' 3. Defines lower and upper bounds to enforce sigma > 0 and |kappa| < 0.5.  
#' 4. Constructs an objective function that returns the negative of
#'    generalized.likelihood().  
#' 5. Runs nlminb() with box constraints to find the posterior mode.  
#'
#' @importFrom stats nlminb
#' @export

gmle.estimation <- function(data, model, prior) {

	p <- lmom.estimation(data[!is.nan(data)], "GEV")

	# Initialize the non-stationary parameters to 0 (if necessary)
	if (model == "GEV") {
		initial_params <- c(p[1], p[2], p[3])
		lower <- c(-Inf, 1e-8, -0.5 + 1e-8)
		upper <- c( Inf,  Inf,  0.5 - 1e-8)
	} else if (model == "GEV100") {
		initial_params <- c(p[1], 0, p[2], p[3])	
		lower <- c(-Inf, -Inf, 1e-8, -0.5 + 1e-8)
		upper <- c( Inf,  Inf,  Inf,  0.5 - 1e-8)
	} else if  (model == "GEV110") {
		initial_params <- c(p[1], 0, p[2], 0, p[3])
		lower <- c(-Inf, -Inf, 1e-8, -Inf, -0.5 + 1e-8)
		upper <- c( Inf,  Inf,  Inf,  Inf,  0.5 - 1e-8)
	} 

	# Maximize the log-likelihood by minimizing the negative log-likelihood
	objective <- function(theta) {
		0 - generalized.likelihood(data, model, theta, prior)
	} 

	# Run parameter optimization
	result <- nlminb(initial_params, objective, lower = lower, upper = upper)

	# Return the optimal parameters if fitting succeeded, otherwise return NULL
	list(params = result$par, mll = -result$objective)

}
