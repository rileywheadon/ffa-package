#' Regula-Falsi Confidence Intervals for Flood Quantile Estimates
#'
#' @description
#' Calculates estimates and confidence intervals for return levels at standard 
#' return periods (2, 5, 10, 20, 50, and 100 years) using the profile likelihood 
#' and Regula-Falsi root‐finding method.
#'
#' @inheritParams param-data
#' @inheritParams param-model
#' @inheritParams param-prior
#' @inheritParams param-years
#' @inheritParams param-trend
#' @inheritParams param-slice
#' @inheritParams param-alpha
#' 
#' @param eps Numeric scalar. The log-likelihood tolerance for the Regula-Falsi 
#' convergence (default is 0.01).
#'
#' @return A list contianing the return levels and confidence intervals containing: 
#' - `estimates`: Estimated quantiles for each return period.
#' - `ci_lower`: Lower bound of the confidence interval for each return period.
#' - `ci_upper`: Upper bound of the confidence interval for each return period.
#' - `t`: Vector of return periods `c(2, 5, 10, 20, 50, 100)` in years.
#' - `slice`: The value of `slice` argument.
#' - `trend`: The value of `trend` argument.
#'
#' @details
#' 1. Fits the model using \link{fit_maximum_likelihood} to obtain parameter 
#'    estimates and log‐likelihood.  
#' 2. Defines an objective function \eqn{f(y_p, p)} based on the chi-squared 
#'    distribution.
#' 3. Iteratively brackets the root by scaling initial guesses by 0.05 until 
#'    \eqn{f(y_p, p)} changes sign.  
#' 4. Uses the Regula Falsi method to solve \eqn{f(y_p, p) = 0} for each 
#'    return-period probability.  
#' 5. Returns lower and upper confidence bounds at significance level `alpha` 
#'    given the return level estimates.
#'
#' @note Although the more modern \link[stats]{optim} function is preferred over 
#' \link[stats]{nlminb}, we use \link[stats]{nlminb} because it supports non-finite
#' values of the likelihood function. 
#'
#' @seealso \link{quantile_fast}, \link{uncertainty_bootstrap}, 
#'   \link{plot_uncertainty}
#'
#' @examples
#' data <- rnorm(n = 100, mean = 100, sd = 10)
#' uncertainty_rfpl(data, "GLO")
#'
#' @importFrom stats qchisq nlminb
#' @export

uncertainty_rfpl <- function(
    data,
    model,
    prior = NULL,
    years = NULL,
    trend = NULL,
    slice = NULL,
    alpha = 0.05,
    eps = 1e-2
) {

	data <- validate_data(data)
	model <- validate_model(model)
	prior <- validate_prior(prior)
	years <- validate_years(years, data)
	trend <- validate_trend(trend)
	slice <- validate_slice(slice)
	alpha <- validate_alpha(alpha)

	# Helper function for computing the profile likelihood
	profile_likelihood <- function(yp, p, initial, prior = NULL) {

		# Remove the parameter we are using for reparameterization on yp
		if (model != "WEI") {
			initial <- initial[-1]
		} else if (model == "WEI" && !trend$location) {
			initial <- initial[-2]
		} else {
			initial <- initial[-3]
		} 

		# Set the default bounds based on the trend and model
		if (trend$location) {
			lower <- c(-Inf)
			upper <- c( Inf)
		} else {
			lower <- numeric(0)
			upper <- numeric(0)
		}

		if (trend$scale) {
			lower <- c(lower, 1e-8, -Inf)
			upper <- c(upper,  Inf,  Inf)
		} else {
			lower <- c(lower, 1e-8)
			upper <- c(upper,  Inf)
		}

		# The Weibull distribution has different parameter bounds
		if (model == "WEI") { 
			if (trend$location) {
				lower <- c(-Inf, -Inf)
				upper <- c( Inf,  Inf)
			} else {
				lower <- c(-Inf)
				upper <- c( Inf)
			}

			if (trend$scale) {
				lower <- c(lower, -Inf)
				upper <- c(upper,  Inf)
			}
		} 

		# Add bounds for the shape parameter if necessary
		info <- model_info(model)
		if (info$n.params == 3) {

			# Set finite bounds on the shape parameter for RFGPL
			if (is.null(prior)) {
				lower <- c(lower, -Inf)
				upper <- c(upper,  Inf)
			} else {
				lower <- c(lower, -0.5 + 1e-8)
				upper <- c(upper,  0.5 - 1e-8)
			}
		}

		# Maximize the log-likelihood by minimizing the negative log-likelihood
		objective <- function(theta) {

			# Reparameterize on the location for non-Weibull distributions
			if (model != "WEI") {

				# Get the quantile at probability p with location 0
				qp <- quantile_fast(p, model, c(0, theta), slice, trend)

				# Log-transform yp, qp if necessary
				if (model %in% c("LNO", "LP3")) {
					yp <- log(yp)
					qp <- log(qp)
				}

				# Reparameterize in terms of the quantiles
				theta <- c(yp - qp, theta)

			} 

			# Use a different reparameterization for the Weibull distributions
			else {

				# i and j are used to shift location in the parameter vector
				i <- trend$location
				j <- trend$scale

				# Get the quantiles for the Wei(0, 1, shape) distribution
				qp <- quantile_wei(p, c(0, 1, theta[2 + i + j]))

				# Reparameterize on the scale parameter sigma
				covariate <- get_covariates(slice)

				if (trend$location) {
					sigma <- (yp - theta[1] - (theta[2] * covariate)) / qp
				} else {
					sigma <- (yp - theta[1]) / qp
				}

				if (trend$scale) {
					sigma <- sigma - (theta[2 + i] * covariate)
				}

				# Reassemble the parameter vector 
				theta <- c(theta[1:(1 + i)], sigma, theta[(2 + i):(2 + i + j)])

			}

			# Use the correct likelihood function based on whether there is a prior
			if (!is.null(prior)) {
				0 - general_loglik_fast(data, model, theta, prior, years, trend)
			} else {
				0 - loglik_fast(data, model, theta, years, trend)
			}

		} 

		# Helper function for running parameter optimization
		optimizer <- function(theta) {
			nlminb(theta, objective, lower = lower, upper = upper)
		}

		# Repeatedly attempt optimization using the optim() function
		attempts <- 1
		params <- initial

		while (attempts <= 100) {
			
			# Attempt to run the optimize() function
			result <- tryCatch(optimizer(params), error = function(e) NULL)

			# If optimization succeeded, end the loop
			if (!is.null(result)) break

			# If optimization failed, perturb the parameters and try again
			perturbation <- rnorm(n = length(params), mean = 0, sd = 0.2)
			params <- initial * (1 + perturbation)
			attempts <- attempts + 1

		}

		# Flip the sign because we optimized the negative log-likelihood earlier
		0 - result$objective

	} 

	# Define the non-exceedance probabilities for the target return periods
	t <- c(2, 5, 10, 20, 50, 100)
	returns <- 1 - (1 / t)

	# Get the results of maximum likelihood estimation
	mle <- fit_maximum_likelihood(data, model, prior, years, trend) 
	lp_hat <- mle$mll
	yp_hat <- quantile_fast(returns, model, mle$params, slice, trend)

	# We want to find the roots of the function f defined below:
	f <- function(yp, p) {
		lp_theta <- profile_likelihood(yp, p, mle$params, prior)
		lp_theta - (lp_hat - (qchisq(1 - alpha, 1) / 2))
	}

	# Define a helper function for regula-falsi iteration
	regula.falsi <- function(a, b, p) { 

		# Compute c from a and b
		c <- ((a * f(b, p)) - (b * f(a, p))) / (f(b, p) - f(a, p))
		
		# If f(c) is small, we are done
		if (abs(f(c, p)) < eps) return (c) 

		# Otherwise, update a, b and then call regula.falsi again
		if (f(c, p) < 0) { a <- c } else { b <- c }
		regula.falsi(a, b, p)

	}

	# Run uncertainty quantification
	ci_lower <- numeric(length(yp_hat))
	ci_upper <- numeric(length(yp_hat))

	# Iterate through the return periods 
	for (i in 1:length(yp_hat)) {

		# Find initial lower bound for mu
		yp_minus <- yp_hat[i] * 0.95
		while (f(yp_minus, returns[i]) > 0) yp_minus <- yp_minus * 0.95

		# Run the iteration algorithm to find the lower confidence interval
		yp_lower <- regula.falsi(yp_minus, yp_hat[i], returns[i])
		ci_lower[i] <- yp_lower

		# Find initial upper bound for mu
		yp_plus <- yp_hat[i] * 1.05
		while (f(yp_plus, returns[i]) > 0) yp_plus <- yp_plus * 1.05

		# Run the iteration algorithm to find the upper confidence interval
		yp_upper <- regula.falsi(yp_hat[i], yp_plus, returns[i])
		ci_upper[i] <- yp_upper

	}

	list(
		t = t,
		ci_lower = ci_lower,
		estimates = yp_hat ,
		ci_upper = ci_upper,
		slice = slice,
		trend = trend
	)

}
