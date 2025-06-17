# Returns the Log-likelihood for the following distributions and their non-stationary variants:
# - Gumbel (GUM)
# - Normal (NOR)
# - Log-Normal (LNO)
# - Generalized Extreme Value (GEV)
# - Generalized Logistic (GLO)
# - Generalized Normal (GNO)
# - Pearson Type III (PE3)
# - Log-Pearson Type III (LP3)
# - Weibull (WEI)
#
# If an error occurs, llvxxx will return -Inf to avoid breaking optimizers.
#
# Using quiet = FALSE will print the error message.

# Lis tof models
MODELS <- list(
	"GEV" = list(n.params = 3)
)

llvxxx <- function(model, data, params, quiet = TRUE) {

	# Initialize a variable for storing the warning state
	warn <- FALSE

	# Check that model is in the list of valid models
	if ()

	# Check that data is numeric vector
	if (!is.numeric(data)) {
		msg <- "Warning: 'data' must be a numeric vector."
		warn <- TRUE
	}

	# Check that data has no missing values
	if (any(is.nan(data)) | any(is.na(data))) {
		msg <- "Warning: 'data' must not contain missing values."
		warn <- TRUE
	}

	# Check that data is strictly positive
	if (any(data <= 0)) {
		msg <- "Warning: 'data' must be strictly positive."
		warn <- TRUE
	}

	# Check that params is a numeric vector
	if (!is.numeric(params)) {
		msg <- "Warning: 'params' must be a numeric vector."
		warn <- TRUE
	}

	# Check that all parameters are defined
	if (any(is.nan(c(u, s, k)))) return (-Inf)

	# Check that the shape parameter s is strictly positive
	if (any(s <= 0)) return (-Inf)

	# End validation by printing a message if quiet == FALSE.
	if (warn) {
		if (!quiet) message(msg)
		return (-Inf)
	}


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

llvgum <- function(data, params) {

	# Validate the data and parameters	
	validate.llv(data, params, 2)

} 



