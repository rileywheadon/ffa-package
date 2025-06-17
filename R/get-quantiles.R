# Computes the quantiles given the following: 
# - "x" is a numeric vector of probabilities (must satisfy 0 < x < 1)
# - "model" is a 3, 5, or 6 character model name (i.e. 'GEV', 'GUM11', or 'WEI100')
# - "params" is a numeric vector of parameters for the specified model
# - "covariates" is a numeric vector of covariates at which to compute quantiles 
# - "collapse" if TRUE, gets the quantile of x[i] at covariates[i] and returns a 1D vector
#
# For stationary models (i.e. 'NOR'), the quantiles will be identical for all covariates 
#
# Returns a matrix with length(x) rows and length(covariates) columns unless length(x) == 1,
# in which case quantile.function returns a vector with length(covariates) entries.

get.quantiles <- function(x, model, params, covariates, collapse = FALSE) {

	# Get the covariates
	n <- length(covariates)

	# Split the model into name and signature
	name <- substr(model, 1, 3)
	signature <- if(nchar(model) == 3) NULL else substr(model, 4, 5)

	# Compute location/scale parameter at the covariates
	if (is.null(signature)) {
		mu <- rep(params[1], n)
		sigma <- rep(params[2], n)
	} else if (signature == "10") {
		mu <- params[1] + (covariates * params[2])
		sigma <- rep(params[3], n)
	} else if (signature == "11") {
		mu <- params[1] + (covariates * params[2])
		sigma <- params[3] + (covariates * params[4])
	}

	# Compute the shape parameter if the model has one
	if (name %in% c("GEV", "GEV", "GLO", "GNO", "PE3", "LP3", "WEI")) {
		kappa <- rep(params[length(params)], n)
	}  else {
		kappa <- NULL
	}

	# Create a matrix of parameters for each covariate
	params_matrix <- cbind(mu, sigma, kappa)

	# Compute a list of quantiles at each covariate
	result <- lapply(1:length(covariates), function(i) {

		# Get the parameters and the probabilities
		params <- params_matrix[i, ]
		p <- if (collapse) x[i] else x

		# Flip the sign of the third parameter of the GEV to use Hosking's notation
		if (name == "GEV") params[3] <- -params[3]

		# Apply the correct quantile function from the lmom library to each row in params
		switch(
			name,
			"GUM" = quagum(p, params),
			"NOR" = quanor(p, params),
			"LNO" = qualn3(p, c(0, params)),
			"GEV" = quagev(p, params),
			"GLO" = quaglo(p, params),
			"GNO" = quagno(p, params),
			"PE3" = quape3(p, params),
			"LP3" = exp(quape3(p, params)),
			"WEI" = quawei(p, params)
		)

	})

	# Return the results as a matrix
	do.call(cbind, result)

}
