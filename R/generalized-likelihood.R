#' Compute Generalized Log‐Likelihood for GEV Models with Beta Prior
#'
#' @description
#' Calculates the log‐likelihood of a sample under a Generalized Extreme Value
#' (GEV) distribution (stationary or with covariate trends) combined with a
#' Beta(p, q) prior on the shape parameter. Supports three model forms:
#' - `'GEV'`   : stationary location, scale, and shape  
#' - `'GEV100'`: linear trend in location only  
#' - `'GEV110'`: linear trends in location and scale  
#'
#' @param data Numeric vector of observations (e.g., annual maxima).  
#'   Any `NaN` values are removed prior to likelihood computation.  
#'
#' @param model Character string specifying the model form.  
#'   One of `'GEV'`, `'GEV100'`, or `'GEV110'`.  
#'
#' @param theta Numeric vector of GEV parameters:  
#'   - For `'GEV'`    : `theta = (mu, sigma, kappa)`  
#'   - For `'GEV100'` : `theta = (mu1, mu2, sigma, kappa)`  
#'   - For `'GEV110'` : `theta = (mu1, mu2, sigma1, sigma2, kappa)`  
#'   Trends are applied as `mu = mu1 + (covariate * mu2)` and/or  
#'   `sigma = sigma1 + (covariate * sigma2)`, where `covariate` is 
#'   scaled linearly on \code{[0,1]} over the sample.  
#'
#' @param prior Numeric vector of length 2 giving the Beta prior parameters  
#'   `(p, q)` on the shape parameter kappa.  
#'
#' @return
#' A single numeric value: the sum of the pointwise log‐likelihoods of the data  
#' under the specified GEV model plus the log‐density of the Beta prior on kappa.  
#' Returns `-Inf` if likelihood is zero.  
#'
#' @details
#' 1. Removes `NaN` values from `data` and constructs a covariate vector of  
#'    the same length, linearly spaced on \code{[0,1]}.  
#' 2. Unpacks `theta` into `(mu, sigma, kappa)` or trend variants depending on `model`.  
#' 3. Computes the GEV log‐density for each data point.
#' 4. Computes the log‐density of the Beta(p, q) prior on kappa.
#' 5. Returns the total log‐likelihood (the sum of steps 3 and 4).
#'
#' @export

generalized.likelihood <- function(data, model, theta, prior) {

	# The prior is Beta(p, q)
	p <- prior[1]
	q <- prior[2]

	# Compute the covariate
	n <- length(data)
	covariate <- ((1:n) - 1) / (n - 1)
	covariate <- covariate[!is.nan(data)]

	# Clean the data
	data <- data[!is.nan(data)]

	# NOTE: Abbreviated Variable Names
	# - u: mu
	# - s: sigma
	# - k: kappa

	# Unpack the parameters based on the GEV model
	if (model == "GEV") {
		u <- theta[1]
		s <- theta[2]
		k <- theta[3]
	} else if (model == "GEV100") {
		u <- theta[1] + (covariate * theta[2])
		s <- theta[3]
		k <- theta[4]
	} else if (model == "GEV110") {
		u <- theta[1] + (covariate * theta[2])
		s <- theta[3] + (covariate * theta[4])
		k <- theta[5]
	} 

	# Compute t. Fail if any values in t are negative
	t <- 1 + k * ((data - u) / s)
	if (any(t <= 0)) return (-Inf)

	# Compute vector of likelihoods for each data point
	ll <- -log(s) - (1 + (1 / k)) * log(t) - t^(-1 / k)

	# Compute prior likelihood
	pll <- (p - 1) * log(0.5 - k) + (q - 1) * log(k + 0.5) - lbeta(p, q)

	# Return the sum of (ll + pll) over all data points
	sum(ll + pll)

}
