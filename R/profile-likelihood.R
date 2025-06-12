#' Compute Profile Log-Likelihood with Fixed Quantile Parameter
#'
#' @description
#' Maximizes the log-likelihood over nuisance parameters while holding a quantile
#' parameter (yp) and exceedance probability (pe) fixed. Uses nlminb with repeated
#' perturbations to ensure convergence.
#'
#' @param data Numeric vector of observations. NaN values are handled internally.
#'
#' @param model Character string specifying the distribution code (e.g. 'GEV',
#'   'GLO', 'GNO', 'PE3', 'LP3', 'WEI') with optional signature '10'/'100'
#'   or '11'/'110' for covariate trends in location and/or scale.
#'
#' @param yp Numeric value of a quantile at the specified exceedance probability.
#'
#' @param pe Numeric exceedance probability associated with yp.
#'
#' @param params Numeric vector of initial values for the nuisance.
#'
#' @return
#' A single numeric value: the maximized profile log-likelihood for the specified
#' model, holding yp and pe fixed.
#'
#' @details
#' 1. Constructs box constraints for nuisance parameters: scale > 0, optional shape
#'    unbounded.  
#' 2. Defines an objective function as the negative of reparameterized.likelihood().  
#' 3. Runs nlminb() with the given initial params, repeating up to 100 times with
#'    random perturbations if an error occurs.  
#' 4. Returns the profile log-likelihood (the negative of the final objective).
#'
#' @importFrom stats nlminb rnorm
#' @export

fixed.likelihood <- function(data, model, yp, pe, params) {

	# Get the name and signature for the model 
	name <- substr(model, 1, 3)
	signature <- if(nchar(model) == 3) NULL else substr(model, 4, 5)

	# Initialize the non-stationary parameters to 0 (if necessary)
	if (is.null(signature)) {
		lower <- c(1e-8)
		upper <- c( Inf)
	} else if (signature == "10") {
		lower <- c(-Inf, 1e-8)
		upper <- c( Inf,  Inf)
	} else if  (signature == "11") {
		lower <- c(-Inf, 1e-8, -Inf)
		upper <- c( Inf,  Inf,  Inf)
	} 

	# Add the kappa parameter (if necessary)
	if (name %in% c("GEV", "GLO", "GNO", "PE3", "LP3", "WEI")) {
		lower <- c(lower, -Inf)
		upper <- c(upper,  Inf)
	}

	# Maximize the log-likelihood by minimizing the negative log-likelihood
	objective <- function(theta) {
		0 - reparameterized.likelihood(data, model, yp, pe, theta)
	} 

	# Helper function for running parameter optimization
	optimizer <- function(theta) {
		nlminb(theta, objective, lower = lower, upper = upper)
	}

	# Repeatedly attempt optimization using the optim() function
	attempts <- 1
	current_params <- params

	while (attempts <= 100) {
		
		# Attempt to run the optimize() function
		result <- tryCatch(
			{ optimizer(current_params) },
			error = function(e) { NULL }
		)

		# If optimization succeeded, end the loop
		if (!is.null(result)) break

		# If optimization failed, perturb the parameters and try again
		perturbation <- rnorm(n = length(params), mean = 0, sd = 0.2)
		current_params <- params * (1 + perturbation)
		attempts <- attempts + 1

	}

	# Flip the sign because we optimized the negative log-likelihood earlier
	0 - result$objective

} 


