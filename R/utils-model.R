# Helper function for getting information about distributions
model.info <- function(model, trend = NULL) {
	info <- switch(
		model,	
		"GUM" = list(n.params = 2, log = FALSE),
		"NOR" = list(n.params = 2, log = FALSE),
		"LNO" = list(n.params = 2, log =  TRUE),
		"GEV" = list(n.params = 3, log = FALSE, k.bounds = c(-9, 9)),
		"GLO" = list(n.params = 3, log = FALSE, k.bounds = c(-0.999, 0.999)),
		"PE3" = list(n.params = 3, log = FALSE, k.bounds = c(-10, 10)),
		"LP3" = list(n.params = 3, log =  TRUE, k.bounds = c(-10, 10)),
		"GNO" = list(n.params = 3, log = FALSE, k.bounds = c(-4, 4)),
		"WEI" = list(n.params = 3, log = FALSE, k.bounds = c(-9, 9)),
	)

	# If optional trend parameter is given, update the number of parameters 
	if (!is.null(trend)) {
		info$n.params <- info$n.params + trend$location + trend$scale
	}

	info
}

# Helper function for computing covariates. A covariate is a normalized version of 
# the year where 0 corresponds to the first year and 1 is the last year. Covariates
# are used to fit parameters for non-stationary distributions.
get.covariates <- function(years) (years - 1900) / 100


