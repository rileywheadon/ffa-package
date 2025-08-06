# Helper function used by fit_mle and fit_gmle
fit_maximum_likelihood <- function(
	data,
	distribution,
	prior,
	years,
	structure
) {
		
	# Assume stationarity. Determine the initial parameters using L-moments.
	p <- fit_lmoments_fast(data, distribution)$params

	# Set the bounds and initial values for the location parameter(s)
	if (structure$location) {
		initial <- c(p[1], 0)
		lower <- c(-Inf, -Inf)
		upper <- c( Inf,  Inf)
	} else {
		initial <- c(p[1])
		lower <- c(-Inf)
		upper <- c( Inf)
	}

	# Set the bounds and initial values for the scale parameter(s)
	if (structure$scale) {
		initial <- c(initial, p[2], 0)	
		lower <- c(lower, 1e-8, -Inf)
		upper <- c(upper,  Inf, Inf)
	} else {
		initial <- c(initial, p[2])
		lower <- c(lower, 1e-8)
		upper <- c(upper,  Inf)
	} 

	# Set the bounds and initial values for the shape parameter (if necessary)
	info <- model_info(distribution)
	if (info$n_params == 3) {
		initial <- c(initial, p[3])

		# Shape parameter must between -0.5 and 0.5 if we are doing GMLE 
		if (!is.null(prior)) {
			lower <- c(lower, -0.5 + 1e-8)
			upper <- c(upper,  0.5 - 1e-8)
		} else {
			lower <- c(lower, -Inf)
			upper <- c(upper,  Inf)
		} 

	}

	# Initialize location parameter to 0 for Weibull distribution to ensure support
	if (distribution == "WEI") initial[1] <- 0

	# Maximize the log-likelihood by minimizing the negative log-likelihood
	objective <- function(theta) {
		if (!is.null(prior)) {
			0 - general_loglik_fast(data, distribution, theta, prior, years, structure)
		} else {
			0 - loglik_fast(data, distribution, theta, years, structure) 
		}
	} 

	# Helper function for running parameter optimization
	optimizer <- function(params) {
		nlminb(params, objective, lower = lower, upper = upper)
	}

	# Repeatedly attempt optimization using the nlminb() function
	attempts <- 1
	params <- initial

	while (attempts <= 100) {

		# Attempt to run the optimize() function
		result <- optimizer(params)

		# If optimization succeeded, end the loop
		if (result$objective != Inf) break

		# If optimization failed, perturb the parameters and try again
		perturbation <- rnorm(n = length(initial), mean = 0, sd = 0.2)
		params <- initial * (1 + perturbation)
		attempts <- attempts + 1

	}

	# Return the optimal parameters and maximum log-likelihood
	if (structure$location || structure$scale) {
		ns_years <- years
		ns_structure <- structure
	} else {
		ns_years <- NULL
		ns_structure <- NULL
	}

	list(
		data = data,
		distribution = distribution,
		ns_years = ns_years,
		ns_structure = ns_structure,
		prior = prior,
		method = if (is.null(prior)) "MLE" else "GMLE",
		params = result$par,
		mll = -result$objective
	)

}
