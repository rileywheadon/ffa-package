# Helper function for computing the generalized log-likelihood for GEV distribution
generalized.log.likelihood <- function(data, model, theta, prior) {

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

gmle.estimation <- function(data, model, prior) {

	p <- lmom.estimation(data[!is.nan(data)], "GEV")

	# Initialize the non-stationary parameters to 0 (if necessary)
	if (model == "GEV") {
		initial_params <- c(p[1], p[2], p[3])
		lower <- c(-Inf, 1e-8, -0.5 + 1e-8)
		upper <- c( Inf,  Inf,  0.5 - 1e-8)
	} else if (model == "GEV100") {
		initial_params <- c(p[1], 0, p[2], p[3])	
		lower <- c(-Inf, -Inf, 1e-8, -0.5 + 1e-8)
		upper <- c( Inf,  Inf,  Inf,  0.5 - 1e-8)
	} else if  (model == "GEV110") {
		initial_params <- c(p[1], 0, p[2], 0, p[3])
		lower <- c(-Inf, -Inf, 1e-8, -Inf, -0.5 + 1e-8)
		upper <- c( Inf,  Inf,  Inf,  Inf,  0.5 - 1e-8)
	} 

	# Maximize the log-likelihood by minimizing the negative log-likelihood
	objective <- function(theta) {
		0 - generalized.log.likelihood(data, model, theta, prior)
	} 

	# Run parameter optimization
	result <- nlminb(initial_params, objective, lower = lower, upper = upper)

	# Return the optimal parameters if fitting succeeded, otherwise return NULL
	list(params = result$par, mll = -result$objective)

}
