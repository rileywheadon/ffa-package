#' Fit Extreme‐Value Model using Maximum Likelihood
#'
#' @description
#' Estimates parameters of an extreme‐value distribution (GUM, NOR, LNO, GEV, 
#' GLO, GNO, PE3, LP3, or WEI) with optional linear covariate trends in 
#' location and/or scale by maximizing the log‐likelihood.
#' Initial values are obtained from L‐moment estimation, and optimization is
#' performed via nlminb with repeated perturbations if needed.
#'
#' @param data Numeric vector of observations. 
#'   Any NaN values are removed prior to fitting.
#'
#' @param model Character string specifying the distribution code. The first
#'   three letters denote the family: 'GUM', 'NOR', 'LNO', 'GEV',
#'   'GLO', 'GNO', 'PE3', 'LP3', or 'WEI'. A trailing signature of
#'   '10' or '100' indicates a linear trend in location; '11' or '110' 
#'   indicates linear trends in both location and scale.
#'
#' @return
#' A list with components:
#' - params: Numeric vector of estimated parameters.
#' - mll   : Maximized log‐likelihood value.
#'
#' @details
#' 1. Removes NaN values from data.  
#' 2. Calls lmom.estimation() on cleaned data to obtain initial estimates of
#'    location, scale, and shape (if applicable).  
#' 3. Initializes trend parameters to zero if there is a trailing signature.  
#' 4. For Weibull models, forces the location estimate to zero to ensure support.  
#' 5. Defines an objective function as the negative of likelihood().  
#' 6. Runs nlminb() with box constraints on scale (>0) and repeats up to 100
#'    times with random perturbations if convergence returns Inf.  
#'
#' @importFrom stats nlminb rnorm
#' @export

mle.estimation <- function(data, model) {

	# Get the name and signature for the model 
	name <- substr(model, 1, 3)
	signature <- if(nchar(model) == 3) NULL else substr(model, 4, 5)
	
	# Determine the initial parameters using L-moments estimation
	p <- lmom.estimation(data[!is.nan(data)], name)

	# Initialize the non-stationary parameters to 0 (if necessary)
	if (is.null(signature)) {
		initial_params <- c(p[1], p[2])
		lower <- c(-Inf, 1e-8)
		upper <- c( Inf,  Inf)
	} else if (signature == "10") {
		initial_params <- c(p[1], 0, p[2])	
		lower <- c(-Inf, -Inf, 1e-8)
		upper <- c( Inf,  Inf,  Inf)
	} else if  (signature == "11") {
		initial_params <- c(p[1], 0, p[2], 0)
		lower <- c(-Inf, -Inf, 1e-8, -Inf)
		upper <- c( Inf,  Inf,  Inf,  Inf)
	} 

	# Add the kappa parameter (if necessary)
	if (name %in% c("GEV", "GLO", "GNO", "PE3", "LP3", "WEI")) {
		initial_params <- c(initial_params, p[3])
		lower <- c(lower, -Inf)
		upper <- c(upper,  Inf)
	}

	# NOTE: The parameters for the Weibull distribution produced by lmom.estimation
	# often do not have support for smaller data points. This isn't a problem for FFA but
	# it is a problem for MLE. Therefore, we need to adjust the location parameter manually.
	if (name == "WEI") initial_params[1] <- 0

	# Maximize the log-likelihood by minimizing the negative log-likelihood
	objective <- function(theta) {
		0 - likelihood(data, model, theta)
	} 

	# Helper function for running parameter optimization
	optimizer <- function(params) {
		nlminb(params, objective, lower = lower, upper = upper)
	}

	# Repeatedly attempt optimization using the optim() function
	attempts <- 1
	current_params <- initial_params

	while (attempts <= 100) {

		# Attempt to run the optimize() function
		result <- optimizer(current_params)

		# If optimization succeeded, end the loop
		if (result$objective != Inf) break

		# If optimization failed, perturb the parameters and try again
		perturbation <- rnorm(n = length(initial_params), mean = 0, sd = 0.2)
		current_params <- initial_params * (1 + perturbation)
		attempts <- attempts + 1

	}

	# Return the optimal parameters if fitting succeeded, otherwise return NULL
	list(params = result$par, mll = -result$objective)

}
