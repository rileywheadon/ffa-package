#' Compute Log-Likelihood for Multiple Extreme‐Value Models
#'
#' @description
#' Calculates the total log-likelihood of a data vector under various
#' extreme-value distributions: GUM, NOR, LNO, GEV, GLO, GNO PE3, LP3, and WEI.
#' Allows for optional linear covariate trends in location and/or scale.
#'  Returns -Inf whenever parameters or data violate distribution support.
#'
#' @param data Numeric vector of observations. NaN values are removed before computation.
#'
#' @param model Character string giving the model code.
#'   The first three letters specify the distribution:
#'   ’GUM’, ’NOR’, ’LNO’, ’GEV’, ’GLO’, ’GNO’, ’PE3’, ’LP3’, or ’WEI’.
#'   A five or six-character code adds a covariate signature:
#'   ’10’/`100` = trend in location, ’11’/`110` = trends in location and scale.
#'
#' @param theta Numeric parameter vector for the specified model and signature.
#'   From the parameter vector, we determine:
#'   - u (location).
#'   - s (scale).
#'   - k (shape) for three-parameter distributions.
#'   - Additional trend coefficients if there is non-stationarity. 
#'
#' @return
#' A single numeric value: the sum of pointwise log-densities.
#' Returns -Inf if any transformation term is non-positive or parameters are NaN.
#'
#' @details
#' 1. Removes NaN values from data and builds a covariate scaled on \code{[0,1]}.
#' 2. Parses the stationary or non-stationary signature to compute u and s.
#' 3. Adds k for three-parameter families and reparameterizes PE3/LP3 when needed.
#' 4. Computes the log-density for each supported distribution, replacing invalid or NaN terms with -Inf.
#' 5. Sums the pointwise log-likelihoods for output.
#'
#' @export

likelihood <- function(data, model, theta) {

	# NOTE: Sometimes the optimizer passes NaN parameters to this function
	# If this happens, immediately return -Inf, which the optimizer can handle.
	if (any(is.nan(theta))) return (-Inf)

	# Get the name and signature for the model 
	name <- substr(model, 1, 3)
	signature <- if(nchar(model) == 3) NULL else substr(model, 4, 5)
	
	# Define the covariate
	n <- length(data)
	covariate <- ((1:n) - 1) / (n - 1)
	covariate <- covariate[!is.nan(data)]

	# Clean the data
	data <- data[!is.nan(data)]

	# NOTE: Abbreviated Variable Names
	# - u: mu
	# - s: sigma
	# - k: kappa
	# - a: alpha (PE3/LP3 only)
	# - b: beta (PE3/LP3 only)
	# - x: xi (PE3/LP3 only)

	# Parse the stationary/non-stationary signature
	if (is.null(signature)) {
		u <- theta[1]
		s <- theta[2]
	} else if (signature == "10") {
		u <- theta[1] + (covariate * theta[2])
		s <- theta[3]
	} else if (signature == "11") {
		u <- theta[1] + (covariate * theta[2])
		s <- theta[3] + (covariate * theta[4])
	} 

	# Add the Kappa parameter if the distribution has three parameters
	if (name %in% c("GEV", "GLO", "GNO", "PE3", "LP3", "WEI")) {
		k <- theta[length(theta)]
	}

	# Compute the log-likelihood
	if (name == "GUM") {

		# Compute z, the normalized data
		z <- ((data - u) / s)

		# Compute vector of likelihoods for each data point
		ll <- -log(s) - z - exp(-z)
		
	} else if (name == "NOR") {

		# Compute z, the normalized data
		z <- ((data - u) / s)

		# Compute vector of likelihoods for each data point
		ll <- -log(abs(s) * sqrt(2 * pi)) - (z^2 / 2)

	} else if (name == "LNO") {

		# Log-transform the data
		data <- log(data)

		# Compute z, the normalized data
		z <- ((data - u) / s)

		# Compute vector of likelihoods for each data point
		ll <- -log(abs(s) * sqrt(2 * pi)) - (z^2 / 2) - data

	} else if (name == "GEV") {
		
		# Compute t and replace negative values with NaN
		t <- 1 + k * ((data - u) / s)
		t[t <= 0] <- NaN

		# Compute vector of likelihoods for each data point
		ll <- -log(s) - (1 + (1 / k)) * log(t) - t^(-1 / k)

	} else if (name == "GLO") {

		# Compute t and replace negative values with NaN
		t <- 1 - k * ((data - u) / s)
		t[t <= 0] <- NaN

		# Compute vector of likelihoods for each data point
		ll <- -log(s) + ((1 / k) - 1) * log(t) - 2 * log(1 + t^(1 / k))

	} else if (name == "GNO") {

		# Compute t and replace negative values with NaN
		t <- 1 - k * ((data - u) / s)
		t[t <= 0] <- NaN

		# Compute vector of likelihoods for each data point
		ll <- -log(s * sqrt(2 * pi)) - log(t) - (log(t)^2 / (2 * k^2))

	} else if (name == "PE3") {

		# Reparameterize
		a <- 4 / (k^2)
		b <- s * abs(k) / 2
		x <- u - (2 * s) / k

		# Check for invalid values 
		if (any(is.nan(x)) | (k > 0 & any(data <= x)) | (k < 0 & any(data >= x))) {
			ll <- -Inf
		} else {
			t <- abs(data - x)
			ll <- (a - 1) * log(t) - (t / b) - a * log(b) - lgamma(a)
		}

	} else if (name == "LP3") {

		# Log-transform the data
		data <- log(data)

		# Reparameterize
		a <- 4 / (k^2)
		b <- s * abs(k) / 2
		x <- u - (2 * s) / k

		# Check for invalid values 
		if ((k > 0 & any(data <= x)) | (k < 0 & any(data >= x))) { 
			ll <- -Inf
		} else {
			t <- abs(data - x)
			ll <- (a - 1) * log(t) - (t / b) - a * log(b) - lgamma(a) - data
		}

	} else if (name == "WEI") {

		# Check for invalid data/parameters
		if (k <= 0 | any(data <= u)) { 
			ll <- -Inf
		} else {
			t <- data - u
			ll <- log(k) - k * log(s) + (k - 1) * log(t) - (t / s)^k
		}

	}

	# Replace any NaN values in ll with -Inf
	ll[is.nan(ll)] <- -Inf

	# Return the sum of ll over all data points
	sum(ll)

}
