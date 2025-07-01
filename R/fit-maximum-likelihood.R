#' Maximum Likelihood Parameter Estimation
#'
#' @description
#' Estimates parameters of a probability distribution (`GUM`, `NOR`, `LNO`, `GEV`, `GLO`, 
#' `GNO`, `PE3`, `LP3`, or `WEI`) or non-stationary variant by maximizing the log‐likelihood.
#' Initial values are obtained through L‐moment parameter estimation, and optimization is
#' performed via \link[stats]{nlminb} with repeated perturbations if needed.
#'
#' @param data Numeric; a vector of annual maximum streamflow data.
#'
#' @param years Numeric; a vector of years with the same length as `data`.
#'
#' @param model Character (1); three character distribution code. Must be one of: 
#'   `"GUM"`, `"NOR"`, `"LNO"`, `"GEV"`, `"GLO"`, `"GNO"`, `"PE3"`, `"LP3"`, or `"WEI"`.
#'
#' @param trend List; information about non-stationary trend(s) to use:
#' - `location` Logical (1); if TRUE, there is a trend in the location parameter.
#' - `scale` Logical (1); if TRUE, there is a trend in the scale parameter.
#'
#' @param prior Numeric (2); optional vector of parameters \eqn{(p, q)} that specifies 
#'  the parameters of a Beta prior on \eqn{\kappa}. Only works with distribution `"GEV"`
#'
#' @return List; results of parameter estimation:
#' - `params`: Numeric vector of estimated parameters.
#' - `mll`: Maximum log‐likelihood value.
#'
#' @details
#' 1. Calls \link{fit_lmom_fast} on `data` to obtain initial parameter estimates.
#' 2. Initializes trend parameters to zero if necessary.  
#' 3. For `WEI` models, sets the location parameter to zero to ensure support.  
#' 4. Defines an objective function as the negative of the \link{llvxxx} function.  
#' 5. Runs \link[stats]{nlminb} with box constraints. Attempts optimization
#'    up to 100 times.  
#'
#' @note Although the more modern \link[stats]{optim} function is preferred over 
#' \link[stats]{nlminb}, we use \link[stats]{nlminb} because it supports non-finite
#' values of the likelihood function. 
#'
#' @seealso \link{llvxxx}, \link{general_loglik_fast}, \link{fit_lmom_fast}, \link[stats]{nlminb}
#'
#' @examples
#' data <- rnorm(n = 100, mean = 100, sd = 10)
#' years <- seq(from = 1901, to = 2000)
#' fit_maximum_likelihood(data, years, "GNO100")
#' 
#' @importFrom stats optim rnorm
#' @export
fit_maximum_likelihood <- function(data, years, model, trend, prior = NULL) {

	# Run parameter validation (see helpers.R)
	validate.data(data)
	validate.years(years)
	validate.model(model)
	validate.trend(trend)

	# Assume stationarity. Determine the initial parameters using L-moments.
	p <- fit_lmom_fast(model, data)

	# Set the bounds and initial values for the location parameter(s)
	if (trend$location) {
		initial <- c(p[1], 0)
		lower <- c(-Inf, -Inf)
		upper <- c( Inf,  Inf)
	} else {
		initial <- c(p[1])
		lower <- c(-Inf)
		upper <- c( Inf)
	}

	# Set the bounds and initial values for the scale parameter(s)
	if (trend$scale) {
		initial <- c(initial, p[2], 0)	
		lower <- c(lower, 1e-8, -Inf)
		upper <- c(upper,  Inf, Inf)
	} else {
		initial <- c(initial, p[2])
		lower <- c(lower, 1e-8)
		upper <- c(upper,  Inf)
	} 

	# Set the bounds and initial values for the shape parameter (if necessary)
	info <- model.info(model)
	if (info$n.params == 3) {
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

	# Initialize location parameter to 0 for Weibull model to ensure support
	if (model == "WEI") initial[1] <- 0

	# Maximize the log-likelihood by minimizing the negative log-likelihood
	objective <- function(theta) {
		if (!is.null(prior)) {
			0 - general_loglik_fast(model, signature, data, theta, prior, years)
		} else {
			0 - llvxxx(model, signature, data, theta, years) 
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
		perturbation <- rnorm(n = length(initial_params), mean = 0, sd = 0.2)
		params <- initial * (1 + perturbation)
		attempts <- attempts + 1

	}

	# Return the optimal parameters and maximum log-likelihood
	list(params = result$par, mll = -result$objective)

}
