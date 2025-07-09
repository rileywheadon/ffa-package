# Helper function for getting information about distributions
model_info <- function(model, trend = NULL) {
	info <- switch(
		model,	
		"GUM" = list(n_params = 2, log = FALSE),
		"NOR" = list(n_params = 2, log = FALSE),
		"LNO" = list(n_params = 2, log =  TRUE),
		"GEV" = list(n_params = 3, log = FALSE, k_bounds = c(-9, 9)),
		"GLO" = list(n_params = 3, log = FALSE, k_bounds = c(-0.999, 0.999)),
		"PE3" = list(n_params = 3, log = FALSE, k_bounds = c(-10, 10)),
		"LP3" = list(n_params = 3, log =  TRUE, k_bounds = c(-10, 10)),
		"GNO" = list(n_params = 3, log = FALSE, k_bounds = c(-4, 4)),
		"WEI" = list(n_params = 3, log = FALSE, k_bounds = c(-9, 9)),
		"KAP" = list(n_params = 4, log = FALSE),
	)

	# If optional trend parameter is given, update the number of parameters 
	if (!is.null(trend)) {
		info$n_params <- info$n_params + trend$location + trend$scale
	}

	info
}

# Helper function for computing covariates. A covariate is a normalized version of 
# the year where 0 corresponds to the first year and 1 is the last year. Covariates
# are used to fit parameters for non-stationary distributions.
get_covariates <- function(years) (years - 1900) / 100


