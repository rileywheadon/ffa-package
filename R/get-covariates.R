# Helper function for computing covariates. A covariate is a normalized version of 
# the year where 0 corresponds to the first year and 1 is the last year. Covariates
# are used to fit parameters for non-stationary distributions.
#
# Argumnets: 
# - years is an integer vector of years.
# - slices is an integer vector of years at which to compute the covariate.
#
# Returns: A numeric vector of covariates for the given slices

get.covariates <- function(years, slices) {
	min_year <- min(years)
	max_year <- max(years)
	(slices - min_year) / (max_year - min_year)	
}
