#' Log-Likelihood Helper Function
#'
#' A helper function used by [loglik_xxx()].
#' This function does not validate parameters and is designed for use in other methods.
#' 
#' @inheritParams param-data
#' @inheritParams param-distribution
#' @inheritParams param-params
#' @inheritParams param-years
#' @inheritParams param-structure
#'
#' @return Numeric scalar. The log-likelihood value.
#'
#' @seealso [loglik_xxx()]
#'
#' @examples
#' data <- rnorm(n = 100, mean = 100, sd = 10)
#' params <- c(0, 1, 0)
#' years <- seq(from = 1901, to = 2000)
#' structure <- list(location = FALSE, scale = FALSE)
#' loglik_fast(data, "GEV", params, years, structure)
#'
#' @export

loglik_fast <- function(data, distribution, params, years, structure) {

	# Generate the covariate from years 
	covariate <- get_covariates(years)

	# Minimal amount of error handling to prevent issues during MLE estimation
	if (any(is.nan(params))) return (-Inf)
	if (any(is.nan(data))) return (-Inf)

	# Transform nonstationary parmaeters into a vector of stationary parameters
	if (structure$location) {
		u <- params[1] + (covariate * params[2])
	} else {
		u <- params[1]
	}

	i <- structure$location
	if (structure$scale) {
		s <- params[2 + i] + (covariate * params[3 + i])
	} else {
		s <- params[2 + i]
	}

	# Get shape parameter (will remain unused for 2-parameter distributions)
	k <- params[length(params)]

	# Ensure that the scale parameter is positive
	if (any(s <= 0)) return (-Inf)

	# Distribution functions
	if (distribution == "GUM") {
		
		# Normalize the data
		z <- ((data - u) / s)
		
		# Compute the log-likelihood
		ll <- -log(s) - z - exp(-z)

	} else if (distribution == "NOR") {

		# Normalize the data
		z <- ((data - u) / s)

		# Compute the log-likelihood
		ll <- -log(s * sqrt(2 * pi)) - (z^2 / 2)

	} else if (distribution == "LNO") {

		# Normalize log(data)
		log_z <- ((log(data) - u) / s)

		# Subtract log(data) from ll because of the chain rule.
		ll <- -log(s * sqrt(2 * pi)) - (log_z^2 / 2) - log(data)

	} else if (distribution == "GEV") {

		# Get the shape-normalized data
		t <- 1 + k * ((data - u) / s)

		# Check for support and then compute the log-likelihood
		if (any(t <= 0)) return (-Inf)
		ll <- -log(s) - (1 + (1 / k)) * log(t) - t^(-1 / k)

	} else if (distribution == "GLO") {

		# Get the shape-normalized data
		t <- 1 - k * ((data - u) / s)

		# Check for support and then compute the log-likelihood
		if (any(t <= 0)) return (-Inf)
		ll <- -log(s) + ((1 / k) - 1) * log(t) - 2 * log(1 + t^(1 / k))
	} 

	else if (distribution == "GNO") {

		# Get the shape-normalized data
		t <- 1 - k * ((data - u) / s)

		# Check for support and then compute the log-likelihood
		if (any(t <= 0)) return (-Inf)
		ll <- -log(s * sqrt(2 * pi)) - log(t) - (log(t)^2 / (2 * k^2))

	} else if (distribution == "PE3") {

		# Reparameterize
		a <- 4 / (k^2)
		b <- s * abs(k) / 2
		x <- u - (2 * s) / k

		# Check for support 
		if ((k > 0 & any(data <= x)) | (k < 0 & any(data >= x))) return (-Inf)

		# Compute the log-likelihood
		t <- abs(data - x)
		ll <- (a - 1) * log(t) - (t / b) - a * log(b) - lgamma(a)

	} else if (distribution == "LP3") {

		# Reparameterize
		a <- 4 / (k^2)
		b <- s * abs(k) / 2
		x <- u - (2 * s) / k

		# Check for support
		if ((k > 0 & any(log(data) <= x)) | (k < 0 & any(log(data) >= x))) return (-Inf)

		# Compute the log-likelihood
		t <- abs(log(data) - x)
		ll <- (a - 1) * log(t) - (t / b) - a * log(b) - lgamma(a) - log(data)

	} else if (distribution == "WEI") {

		# Check for support
		if (k <= 0 | any(data <= u)) return (-Inf) 
		
		# Compute the log-likelihood
		t <- data - u
		ll <- log(k) - k * log(s) + (k - 1) * log(t) - (t / s)^k

	}

	# The sum of the log-likelihood over all data points is the total log-likelihood
	sum(ll)

}


