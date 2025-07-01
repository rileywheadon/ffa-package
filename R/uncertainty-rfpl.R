#' Regula-Falsi Confidence Intervals for Flood Quantile Estimates
#'
#' @description
#' Calculates estimates and confidence intervals for return levels at standard 
#' return periods (2, 5, 10, 20, 50, and 100 years) using the profile likelihood 
#' and Regula-Falsi root‐finding method.
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
#' @param slices Character (1) or Numeric; specifies the years at which
#'   to compute the estimates and confidence intervals (default is "last").
#'   - `"all"`: returns estimates for all values in `years`.
#'   - `"first"`: returns estimates for first year in the dataset.
#'   - `"last"`: returns estimates for last year in the dataset.
#'   - Passing a numeric vector to `slices` allows for custom values.
#'
#'   If the chosen model is stationary, the results will be the same for all slices.
#'
#' @param alpha Numeric (1); the significance level (default is 0.05).
#'
#' @param eps Numeric (1); tolerance for the Regula-Falsi convergence (default is 0.01).
#'
#' @param prior Numeric (2); optional vector of parameters \eqn{(p, q)} that specifies 
#'  the parameters of a Beta prior on \eqn{\kappa}. Only works with models `GEV`, 
#'  `GEV100`, and `GEV110`.
#'
#' @return List; quantiles and confidence intervals. Each year maps to a sub-list:
#' - `estimates`: Estimated quantiles for each return period.
#' - `ci_lower`: Lower bound of the confidence interval for each return period.
#' - `ci_upper`: Upper bound of the confidence interval for each return period.
#' - `t`: Vector of return periods; `c(2, 5, 10, 20, 50, 100)`.
#'
#' @details
#' 1. Fits the model using \link{fit_maximum_likelihood} to obtain parameter estimates and log‐likelihood.  
#' 2. Defines an objective function \eqn{f(y_p, p)} based on the chi-squared distribution.
#' 3. Iteratively brackets the root by scaling initial guesses by 0.05 until f changes sign.  
#' 4. Uses the Regula Falsi method to solve \eqn{f(y_p, p) = 0} for each return-period probability.  
#' 5. Returns lower and upper confidence bounds at level alpha and the quantile estimates.
#'
#' @note Although the more modern \link[stats]{optim} function is preferred over 
#' \link[stats]{nlminb}, we use \link[stats]{nlminb} because it supports non-finite
#' values of the likelihood function. 
#'
#' @seealso \link{qntxxx}, \link{sb.uncertainty}, \link[stats]{nlminb}
#'
#' @examples
#' data <- rnorm(n = 100, mean = 100, sd = 10)
#' years <- seq(from = 1901, to = 2000)
#' rfpl.uncertainty(data, years, "GLO110")
#'
#' @importFrom stats qchisq
#' @export
rfpl.uncertainty <- function(
  data,
  years,
  model,
  trend,
  slices = "last",
  alpha = 0.05,
  eps = 1e-2,
  prior = NULL
) {

	# Run parameter validation (see helpers.R)
	validate.data(data)
	validate.years(years)
	validate.model(model)
	validate.trend(trend)
	validate.alpha(alpha)
	if (!is.null(prior)) validate.prior(prior)

	# Helper function for computing the profile likelihood
	profile.likelihood <- function(yp, p, initial, slice, prior = NULL) {

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
			lower <- c(lower, -Inf)
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
		info <- model.info(model)
		if (info$n.params == 3) {

			# Set finite bounds on the shape parameter for RFGPL
			if (!is.null(prior)) {
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
				qp <- qntxxx(model, trend, p, c(0, theta), slice)

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
				qp <- qntxxx("WEI", NULL, p, c(0, 1, theta[2 + i]))

				# Reparameterize on the scale parameter sigma
				covariate <- get.covariates(slice)

				if (trend$location) {
					sigma <- (yp - theta[1] - (theta[2] * covariate)) / qp
				} else {
					sigma <- (yp - theta[1]) / qp
				}

				if (trend$scale) {
					sigma <- sigma - (theta[2 + i] * covariate)
				}

				# Reassemble the parameter vector 
				theta <- c(theta[1:(1 + i)], sigma, theta[(2 + i):(3 + i + j)])

			}

			# Use the correct likelihood function based on whether there is a prior
			if (!is.null(prior)) {
				0 - general_loglik_fast(model, trend, data, theta, prior, years)
			} else {
				0 - llvxxx(model, trend, data, theta, years)
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

	# Determine the slices based on the "slices" argument
	if (is.character(slices)) {
		slices <- switch(
			slices,
			"all" = years,
			"first" = min(years),
			"last" = max(years)
		)
	}

	# Get the results of maximum likelihood estimation
	mle <- if (is.null(prior)) {
		fit_maximum_likelihood(data, years, model, trend) 
	} else {
		fit_maximum_likelihood(data, years, model, trend, prior) 
	} 

	lp_hat <- mle$mll
	yp_hat <- qntxxx(model, trend, returns, mle$params, slices)

	# We want to find the roots of the function f defined below:
	f <- function(yp, p, slice) {
		lp_theta <- profile.likelihood(yp, p, mle$params, slice, prior)
		lp_theta - (lp_hat - (qchisq(1 - alpha, 1) /2))
	}

	# Define a helper function for regula-falsi iteration
	regula.falsi <- function(a, b, p, slice) { 

		# Compute c from a and b
		fa <- f(a, p, slice)
		fb <- f(b, p, slice)
		c <- ((a * fb) - (b * fa)) / (fb - fa)
		
		# If f(c) is small, we are done
		fc <- f(c, p, slice)
		if (abs(fc) < eps) return (c) 

		# Otherwise, update a, b and then call regula.falsi again
		if (fc < 0) { a <- c } else { b <- c }
		regula.falsi(a, b, p, slice)

	}

	# Define a helper function for uncertainty quantification
	compute.uncertainty <- function(yp_hat, slice) {
	 
		ci_lower <- numeric(length(yp_hat))
		ci_upper <- numeric(length(yp_hat))

		# Iterate through the return periods 
		for (i in 1:length(yp_hat)) {

			# Find initial lower bound for mu
			yp_minus <- yp_hat[i] * 0.95
			while (f(yp_minus, returns[i], slice) > 0) yp_minus <- yp_minus * 0.95

			# Run the iteration algorithm to find the lower confidence interval
			yp_lower <- regula.falsi(yp_minus, yp_hat[i], returns[i], slice)
			ci_lower[i] <- yp_lower

			# Find initial upper bound for mu
			yp_plus <- yp_hat[i] * 1.05
			while (f(yp_plus, returns[i], slice) > 0) yp_plus <- yp_plus * 1.05

			# Run the iteration algorithm to find the upper confidence interval
			yp_upper <- regula.falsi(yp_hat[i], yp_plus, returns[i], slice)
			ci_upper[i] <- yp_upper

		}

		# Return the results as a list
		list(ci_lower = ci_lower, estimates = yp_hat, ci_upper = ci_upper)

	}

	# Run uncertainty quantification at each year in slices
	lapply(1:length(slices), function (i) {
		if (length(slices) == 1) {
			compute.uncertainty(yp_hat, slices[i])
		} else {
			compute.uncertainty(yp_hat[i, ], slices[i])
		}
	})

}
