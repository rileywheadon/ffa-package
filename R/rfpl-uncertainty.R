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
#' @param years Character string or numeric vector specifying the years at which
#'   to compute the estimates and confidence intervals. Defaults to "last".
#'   \itemize{
#'     \item{`"all"` returns estimates for all years in the dataset.}
#'     \item{`"first"` returns estimates for first year in the dataset.}
#'     \item{`"last"` returns estimates for last year in the dataset.}
#'     \item{Passing a numeric vector to `years` allows for custom values.}
#'   }
#'   If the chosen model is stationary, the results will be the same for all years
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

rfpl.uncertainty <- function(
  df,
  model,
  years = "last",
  alpha = 0.05,
  eps = 1e-2
) {

	# Define the non-exceedance probabilities for the target return periods
	t <- c(2, 5, 10, 20, 50, 100)
	returns <- 1 - (1 / t)

	# Determine the years based on the "years" argument
	if (is.character(years)) {
		years <- switch(
			years,
			"all" = df$year,
			"first" = min(df$year),
			"last" = max(df$year)
		)
	}

	# Define the covariates 
	covariates <- get.covariates(df$year, years)

	# Get the results of maximum likelihood estimation
	mle <- mle.estimation(df, model)
	lp_hat <- mle$mll
	yp_hat <- get.quantiles(returns, model, mle$params, covariates)

	# We want to find the roots of the function f defined below:
	f <- function(yp, pe, ti) {
		lp_theta <- fixed.likelihood(df, model, yp, pe, mle$params[-1], ti)
		lp_theta - (lp_hat - (qchisq(1 - alpha, 1) /2))
	}

	# Define a helper function for regula-falsi iteration
	regula.falsi <- function(a, b, pe, ti) { 

		# Compute c from a and b
		c <- ((a * f(b, pe, ti)) - (b * f(a, pe, ti))) / (f(b, pe, ti) - f(a, pe, ti))
		
		# If f(c) is small, we are done
		if (abs(f(c, pe, ti)) < eps) { return (c) }

		# Otherwise, update a, b and then call regula.falsi again
		if (f(c, pe, ti) < 0) { a <- c } else { b <- c }
		regula.falsi(a, b, pe, ti)

	}

	# Define a helper function for uncertainty quantification
	compute.uncertainty <- function(yp_hat, ti) {
	 
		ci_lower <- numeric(length(yp_hat))
		ci_upper <- numeric(length(yp_hat))

		# Iterate through the return periods 
		for (i in 1:length(yp_hat)) {

			# Find initial lower bound for mu
			yp_minus <- yp_hat[i] * 0.95
			while (f(yp_minus, returns[i], ti) > 0) yp_minus <- yp_minus * 0.95

			# Run the iteration algorithm to find the lower confidence interval
			a <- yp_minus
			b <- yp_hat[i]
			yp_lower <- regula.falsi(a, b, returns[i], ti)
			ci_lower[i] <- yp_lower

			# Find initial upper bound for mu
			yp_plus <- yp_hat[i] * 1.05
			while (f(yp_plus, returns[i], ti) > 0) yp_plus <- yp_plus * 1.05

			# Run the iteration algorithm to find the upper confidence interval
			a <- yp_hat[i]
			b <- yp_plus
			yp_upper <- regula.falsi(a, b, returns[i], ti)
			ci_upper[i] <- yp_upper

		}

		# Return the results as a list
		list(ci_lower = ci_lower, estimates = yp_hat, ci_upper = ci_upper)

	}

	# Run uncertainty quantification at each year in years
	lapply(1:length(years), function (i) {
		compute.uncertainty(yp_hat[, i], years[i])
	})

}
