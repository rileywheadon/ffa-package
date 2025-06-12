#' Compute Uncertainty via Regula Falsi Profile Likelihood
#'
#' @description
#' Calculates point estimates and confidence intervals for return levels at
#' standard return periods (2, 5, 10, 20, 50, 100 years) using profile
#' likelihood and the Regula Falsi root‐finding method.
#'
#' @param data Numeric vector of observations. NaN values are removed internally.
#'
#' @param model Character string specifying the distribution code. The first three
#'    letters denote the family: 'GUM', 'NOR', 'LNO', 'GEV', 'GLO',
#'    'GNO', 'PE3', 'LP3', or 'WEI'. A trailing signature of '10'/'100' indicates
#'    a trend in location. '11'/'110' indicates trends in both location and scale.
#'
#' @param alpha Numeric significance level for confidence intervals (default 0.05).
#'
#' @param eps Numeric tolerance for the Regula Falsi convergence criterion (default 1e-2).
#'
#' @return
#' A list with components:
#' - ci_lower : Numeric vector of lower confidence bounds for each return period.  
#' - estimates: Numeric vector of profile‐likelihood point estimates (return levels).  
#' - ci_upper : Numeric vector of upper confidence bounds for each return period.
#'
#' @details
#' 1. Retrieves the quantile function for the specified model via get.distributions().  
#' 2. Fits the model by ordinary MLE to obtain parameter estimates and log‐likelihood.  
#' 3. Defines an objective function f(yp, pe) based on the chi-squared distribution.
#' 4. Iteratively brackets the root by scaling initial guesses by 0.05 until f changes sign.  
#' 5. Uses the Regula Falsi method to solve f(yp, pe) = 0 for each return-period probability.  
#' 6. Returns lower and upper confidence bounds at level alpha and the point estimates.
#'
#' @importFrom stats qchisq
#' @export

rfpl.uncertainty <- function(data, model, alpha = 0.05, eps = 1e-2) {

	# Get the quantile function
	distribution_list <- get.distributions()
	name <- substr(model, 1, 3)
	qfunc <- distribution_list[[name]]$quantile

	# Define the non-exceedance probabilities for the target return periods
	t <- c(2, 5, 10, 20, 50, 100)
	returns <- 1 - (1 / t)

	# Get the results of maximum likelihood estimation
	mle <- mle.estimation(data, model)
	lp_hat <- mle$mll
	yp_hat <- qfunc(returns, mle$params)

	# We want to find the roots of the function f defined below:
	f <- function(yp, pe) {
		lp_theta <- fixed.likelihood(data, model, yp, pe, mle$params[-1])
		lp_theta - (lp_hat - (qchisq(1 - alpha, df = 1) /2))
	}

	# Define a helper function for regula-falsi iteration
	regula.falsi <- function(a, b, pe) { 

		# Compute c from a and b
		c <- ((a * f(b, pe)) - (b * f(a, pe))) / (f(b, pe) - f(a, pe))
		
		# If f(c) is small, we are done
		if (abs(f(c, pe)) < eps) { return (c) }

		# Otherwise, update a, b and then call regula.falsi again
		if (f(c, pe) < 0) { a <- c } else { b <- c }
		regula.falsi(a, b, pe)

	}

	ci_lower <- numeric(length(yp_hat))
	ci_upper <- numeric(length(yp_hat))

	# Iterate through the return periods 
	for (i in 1:length(yp_hat)) {

		# Find initial lower bound for mu
		yp_minus <- yp_hat[i] * 0.95
		while (f(yp_minus, returns[i]) > 0) yp_minus <- yp_minus * 0.95

		# Run the iteration algorithm to find the lower confidence interval
		a <- yp_minus
		b <- yp_hat[i]
		yp_lower <- regula.falsi(a, b, returns[i])
		ci_lower[i] <- yp_lower

		# Find initial upper bound for mu
		yp_plus  <- yp_hat[i] * 1.05
		while (f(yp_plus, returns[i]) > 0) yp_plus <- yp_plus * 1.05

		# Run the iteration algorithm to find the upper confidence interval
		a <- yp_hat[i]
		b <- yp_plus
		yp_upper <- regula.falsi(a, b, returns[i])
		ci_upper[i] <- yp_upper

	}

	# Return the results as a list
	list(ci_lower = ci_lower, estimates = yp_hat, ci_upper = ci_upper)

}
