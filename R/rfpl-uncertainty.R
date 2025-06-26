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
#' @param model Character (1); string specifying the probability model. The first three 
#'   letters denote the family: `GUM`, `NOR`, `LNO`, `GEV`, `GLO`, `GNO`, `PE3`, 
#'   `LP3`, or `WEI`. A trailing signature of `10` or `100` indicates a linear trend 
#'   in location; `11` or `110` indicates linear trends in both location and scale.
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
#' 1. Fits the model using \link{mle.estimation} to obtain parameter estimates and log‐likelihood.  
#' 2. Defines an objective function \eqn{f(y_p, p)} based on the chi-squared distribution.
#' 3. Iteratively brackets the root by scaling initial guesses by 0.05 until f changes sign.  
#' 4. Uses the Regula Falsi method to solve \eqn{f(y_p, p) = 0} for each return-period probability.  
#' 5. Returns lower and upper confidence bounds at level alpha and the quantile estimates.
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
  slices = "last",
  alpha = 0.05,
  eps = 1e-2,
  prior = NULL
) {

	# Get the name, signature, and covariates
	name <- substr(model, 1, 3)
	signature <- if(nchar(model) == 3) NULL else substr(model, 4, 5)
	year_covariates <- get.covariates(years)

	# Helper function for computing the profile likelihood
	profile.likelihood <- function(yp, p, params, slice, prior = NULL) {

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
			if (is.null(prior)) {
				lower <- c(lower, -Inf)
				upper <- c(upper,  Inf)
			} else {
				lower <- c(lower, -0.49)
				upper <- c(upper, 0.49)
			}
		}

		# Maximize the log-likelihood by minimizing the negative log-likelihood
		objective <- function(theta) {

			# Get the quantile at probability p with location 0
			qp <- qntxxx(name, signature, p, c(0, theta), slice)

			# Log-transform yp, qp if necessary
			name <- substr(model, 1, 3)
			if (name %in% c("LNO", "LP3")) {
				yp <- log(yp)
				qp <- log(qp)
			}

			# Reparameterize and return the log-likelihood
			theta <- c(yp - qp, theta)

			if (!is.null(prior)) {
				0 - gllxxx(name, signature, data, theta, prior, year_covariates)
			} else {
				0 - llvxxx(name, signature, data, theta, year_covariates)
			}

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

	slice_covariates <- get.covariates(slices)

	# Get the results of maximum likelihood estimation
	mle <- if (is.null(prior)) {
		mle.estimation(data, years, model) 
	} else {
		mle.estimation(data, years, model, prior) 
	} 

	lp_hat <- mle$mll
	yp_hat <- qntxxx(name, signature, returns, mle$params, slice_covariates)

	# We want to find the roots of the function f defined below:
	f <- function(yp, p, slice) {
		lp_theta <- profile.likelihood(yp, p, mle$params[-1], slice, prior)
		lp_theta - (lp_hat - (qchisq(1 - alpha, 1) /2))
	}

	# Define a helper function for regula-falsi iteration
	regula.falsi <- function(a, b, p, slice) { 

		# Compute c from a and b
		c <- ((a * f(b, p, slice)) - (b * f(a, p, slice))) / (f(b, p, slice) - f(a, p, slice))
		
		# If f(c) is small, we are done
		if (abs(f(c, p, slice)) < eps) return (c) 

		# Otherwise, update a, b and then call regula.falsi again
		if (f(c, p, slice) < 0) { a <- c } else { b <- c }
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
			compute.uncertainty(yp_hat, slice_covariates[i])
		} else {
			compute.uncertainty(yp_hat[i, ], slice_covariates[i])
		}
	})

}
