# Helper function for computing the log-likelihood for each model
log.likelihood <- function(data, model, theta) {

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
	if (name %in% c("GEV", "GLO", "GNO", "GPA", "PE3", "LP3", "WEI")) {
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

	} else if (name == "GPA") {

		# Check for invalid values in the data vector
		if (any(data <= u) | (k < 0 & any(data >= u - (s / k)))) { 
			ll <- -Inf
		} else {
			t <- 1 + k * ((data - u) / s)
			ll <- - log(s) - (1 + (1 / k)) * log(t)
		}
		
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


mle.estimation <- function(data, model) {

	# Get the name and signature for the model 
	name <- substr(model, 1, 3)
	signature <- if(nchar(model) == 3) NULL else substr(model, 4, 5)
	
	# Determine the initial parameters using L-moments estimation
	p <- lmom.estimation(data[!is.nan(data)], name)

	# Initialize the non-stationary parameters to 0 (if necessary)
	if (is.null(signature)) {
		initial_params <- c(p[1], p[2])
		lower <- c(-Inf, 1e-8)
		upper <- c( Inf,  Inf)
	} else if (signature == "10") {
		initial_params <- c(p[1], 0, p[2])	
		lower <- c(-Inf, -Inf, 1e-8)
		upper <- c( Inf,  Inf,  Inf)
	} else if  (signature == "11") {
		initial_params <- c(p[1], 0, p[2], 0)
		lower <- c(-Inf, -Inf, 1e-8, -Inf)
		upper <- c( Inf,  Inf,  Inf,  Inf)
	} 

	# Add the kappa parameter (if necessary)
	if (name %in% c("GEV", "GLO", "GNO", "GPA", "PE3", "LP3", "WEI")) {
		initial_params <- c(initial_params, p[3])
		lower <- c(lower, -Inf)
		upper <- c(upper,  Inf)
	}

	# NOTE: The parameters for the Weibull/GPA distributions produced by lmom.estimation
	# often do not have support for smaller data points. This isn't a problem for FFA but
	# it is a problem for MLE. Therefore, we need to adjust the location parameter manually.
	if (name %in% c("WEI", "GPA")) {
		initial_params[1] <- 0
	}

	# Maximize the log-likelihood by minimizing the negative log-likelihood
	objective <- function(theta) {
		0 - log.likelihood(data, model, theta)
	} 

	# Helper function for running parameter optimization
	optimizer <- function(params) {
		nlminb(params, objective, lower = lower, upper = upper)
	}

	# Repeatedly attempt optimization using the optim() function
	attempts <- 1
	current_params <- initial_params

	while (attempts <= 100) {
		
		# Attempt to run the optimize() function
		result <- optimizer(current_params)

		# If optimization succeeded, end the loop
		if (result$objective != Inf) break

		# If optimization failed, perturb the parameters and try again
		perturbation <- rnorm(n = length(initial_params), mean = 0, sd = 0.2)
		current_params <- initial_params * (1 + perturbation)
		attempts <- attempts + 1

	}

	# Return the optimal parameters if fitting succeeded, otherwise return NULL
	list(params = result$par, mll = -result$objective)

}
