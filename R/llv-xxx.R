#' Log-Likelihood Helper Function
#'
#' A helper function used by \link{llv-functions}.
#' 
#' @note 
#' This function does not perform parameter validation, which improves performance
#' at the cost of potentially unpredictable behaviour. Use at your own risk.
#'
#' @param name Character (1); the name of the probability distribution.
#'
#' @param signature Character (1); the non-stationary signature (`NULL`, `"10"`, or `"11"`).
#'
#' @param data Numeric; a vector of annual maximum streamflow data.
#'
#' @param params Numeric; a vector of parameters. Must have the correct length for the model.
#'
#' @param covariate Numeric; a vector with the same length as `data`. 
#'   Required if `signature` is `"10"` or `"11"`.
#'
#' @return Numeric (1); the log-likelihood value.
#'
#' @seealso \link{llv-functions}
#'
#' @examples
#' # Initialize data and params
#' data <- rnorm(n = 100, mean = 100, sd = 10)
#' params <- c(0, 1, 0)
#'
#' # Compute the log-likelihood
#' llvxxx("GEV", NULL, data, params)
#'
#' @export
llvxxx <- function(name, signature, data, params, covariate = NULL) {

	# Minimal amount of error handling to prevent issues during MLE estimation
	if (any(is.nan(params))) return (-Inf)
	if (any(is.nan(data))) return (-Inf)

	# Parse the stationary/non-stationary signature
	# - u: mu (location parameter)
	# - s: sigma (scale parameter)
	# - k: kappa (shape parameter)

	if (is.null(signature)) {
		u <- params[1]
		s <- params[2]
	} else if (signature == "10") {
		u <- params[1] + (covariate * params[2])
		s <- params[3]
	} else if (signature == "11") {
		u <- params[1] + (covariate * params[2])
		s <- params[3] + (covariate * params[4])
	}

	if (name %in% c("GEV", "GLO", "GNO", "PE3", "LP3", "WEI")) {
		k <- params[length(params)]
	}

	# Ensure that the scale parameter is positive
	if (any(s <= 0)) return (-Inf)

	# Distribution functions
	if (name == "GUM") {
		
		# Normalize the data
		z <- ((data - u) / s)
		
		# Compute the log-likelihood
		ll <- -log(s) - z - exp(-z)

	} else if (name == "NOR") {

		# Normalize the data
		z <- ((data - u) / s)

		# Compute the log-likelihood
		ll <- -log(s * sqrt(2 * pi)) - (z^2 / 2)

	} else if (name == "LNO") {

		# Normalize log(data)
		log_z <- ((log(data) - u) / s)

		# Subtract log(data) from ll because of the chain rule.
		ll <- -log(s * sqrt(2 * pi)) - (log_z^2 / 2) - log(data)

	} else if (name == "GEV") {

		# Get the shape-normalized data
		t <- 1 + k * ((data - u) / s)

		# Check for support and then compute the log-likelihood
		if (any(t <= 0)) return (-Inf)
		ll <- -log(s) - (1 + (1 / k)) * log(t) - t^(-1 / k)

	} else if (name == "GLO") {

		# Get the shape-normalized data
		t <- 1 - k * ((data - u) / s)

		# Check for support and then compute the log-likelihood
		if (any(t <= 0)) return (-Inf)
		ll <- -log(s) + ((1 / k) - 1) * log(t) - 2 * log(1 + t^(1 / k))
	} 

	else if (name == "GNO") {

		# Get the shape-normalized data
		t <- 1 - k * ((data - u) / s)

		# Check for support and then compute the log-likelihood
		if (any(t <= 0)) return (-Inf)
		ll <- -log(s * sqrt(2 * pi)) - log(t) - (log(t)^2 / (2 * k^2))

	} else if (name == "PE3") {

		# Reparameterize
		a <- 4 / (k^2)
		b <- s * abs(k) / 2
		x <- u - (2 * s) / k

		# Check for support 
		if ((k > 0 & any(data <= x)) | (k < 0 & any(data >= x))) return (-Inf)

		# Compute the log-likelihood
		t <- abs(data - x)
		ll <- (a - 1) * log(t) - (t / b) - a * log(b) - lgamma(a)

	} else if (name == "LP3") {

		# Reparameterize
		a <- 4 / (k^2)
		b <- s * abs(k) / 2
		x <- u - (2 * s) / k

		# Check for support
		if ((k > 0 & any(log(data) <= x)) | (k < 0 & any(log(data) >= x))) return (-Inf)

		# Compute the log-likelihood
		t <- abs(log(data) - x)
		ll <- (a - 1) * log(t) - (t / b) - a * log(b) - lgamma(a) - log(data)

	} else if (name == "WEI") {

		# Check for support
		if (k <= 0 | any(data <= u)) return (-Inf) 
		
		# Compute the log-likelihood
		t <- data - u
		ll <- log(k) - k * log(s) + (k - 1) * log(t) - (t / s)^k

	}

	# The sum of the log-likelihood over all data points is the total log-likelihood
	sum(ll)

}


