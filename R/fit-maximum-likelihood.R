#' Maximum Likelihood Parameter Estimation
#'
#' Estimates parameters of a probability distribution with an optional nonstationary 
#' structure by  maximizing the log‐likelihood. Initial values are obtained through 
#' L‐moment parameter estimation, and optimization is performed via [stats::nlminb()] 
#' with repeated perturbations if needed.
#'
#' @inheritParams param-data
#' @inheritParams param-years
#' @inheritParams param-prior
#' @inheritParams param-distribution
#' @inheritParams param-structure
#'
#' @return A list containing the results of parameter estimation:
#' - `params`: Numeric vector of estimated parameters.
#' - `mll`: Maximum log‐likelihood value.
#'
#' @details
#' 1. Calls [fit_lmom_fast()] on `data` to obtain initial parameter estimates.
#' 2. Initializes trend parameters to zero if necessary.  
#' 3. For `WEI` models, sets the location parameter to zero to ensure support.  
#' 4. Defines an objective function using [loglik_fast()] or [general_loglik_fast()].  
#' 5. Runs [stats::nlminb()] with box constraints. Attempts optimization
#'    up to 100 times if a maximum cannot be found.  
#'
#' @note 
#' Although the more modern [stats::optim()] function is preferred over 
#' [stats::nlminb()], we use [stats::nlminb()] because it supports infinite
#' values of the likelihood function. 
#'
#' @seealso [loglik_fast()], [general_loglik_fast()], [fit_lmom_fast()],
#'   [stats::nlminb()]
#'
#' @examples
#' data <- rnorm(n = 100, mean = 100, sd = 10)
#' years <- seq(from = 1901, to = 2000)
#' structure <- list(location = TRUE, scale = FALSE)
#' fit_maximum_likelihood(data, "GNO", NULL, years, structure)
#' 
#' @importFrom stats rnorm nlminb
#' @export

fit_maximum_likelihood <- function(
	data,
	distribution,
	prior = NULL,
	years = NULL,
	structure = NULL
) {

	data <- validate_numeric("data", data, optional = FALSE)
	distribution <- validate_enum("distribution", distribution)
	prior <- validate_numeric("prior", prior, size = 2, bounds = c(0, Inf))
	years <- validate_numeric("years", years, size = length(data))
	structure <- validate_structure(structure)
	
	# Assume stationarity. Determine the initial parameters using L-moments.
	p <- fit_lmom_fast(data, distribution)$params

	# Set the bounds and initial values for the location parameter(s)
	if (structure$location) {
		initial <- c(p[1], 0)
		lower <- c(-Inf, -Inf)
		upper <- c( Inf,  Inf)
	} else {
		initial <- c(p[1])
		lower <- c(-Inf)
		upper <- c( Inf)
	}

	# Set the bounds and initial values for the scale parameter(s)
	if (structure$scale) {
		initial <- c(initial, p[2], 0)	
		lower <- c(lower, 1e-8, -Inf)
		upper <- c(upper,  Inf, Inf)
	} else {
		initial <- c(initial, p[2])
		lower <- c(lower, 1e-8)
		upper <- c(upper,  Inf)
	} 

	# Set the bounds and initial values for the shape parameter (if necessary)
	info <- model_info(distribution)
	if (info$n_params == 3) {
		initial <- c(initial, p[3])

		# Shape parameter must between -0.5 and 0.5 if we are doing GMLE 
		if (!is.null(prior)) {
			lower <- c(lower, -0.5 + 1e-8)
			upper <- c(upper,  0.5 - 1e-8)
		} else {
			lower <- c(lower, -Inf)
			upper <- c(upper,  Inf)
		} 

	}

	# Initialize location parameter to 0 for Weibull distribution to ensure support
	if (distribution == "WEI") initial[1] <- 0

	# Maximize the log-likelihood by minimizing the negative log-likelihood
	objective <- function(theta) {
		if (!is.null(prior)) {
			0 - general_loglik_fast(data, distribution, theta, prior, years, structure)
		} else {
			0 - loglik_fast(data, distribution, theta, years, structure) 
		}
	} 

	# Helper function for running parameter optimization
	optimizer <- function(params) {
		nlminb(params, objective, lower = lower, upper = upper)
	}

	# Repeatedly attempt optimization using the nlminb() function
	attempts <- 1
	params <- initial

	while (attempts <= 100) {

		# Attempt to run the optimize() function
		result <- optimizer(params)

		# If optimization succeeded, end the loop
		if (result$objective != Inf) break

		# If optimization failed, perturb the parameters and try again
		perturbation <- rnorm(n = length(initial), mean = 0, sd = 0.2)
		params <- initial * (1 + perturbation)
		attempts <- attempts + 1

	}

	# Return the optimal parameters and maximum log-likelihood
	list(
		method = if (is.null(prior)) "MLE" else "GMLE",
		params = result$par,
		mll = -result$objective
	)

}
