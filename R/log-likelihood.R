#' Compute Log-Likelihood for Multiple Extreme‐Value Models
#'
#' @description
#' Calculates the total log-likelihood of a data vector under various
#' extreme-value distributions: GUM, NOR, LNO, GEV, GLO, GNO PE3, LP3, and WEI.
#' Allows for optional linear covariate trends in location and/or scale.
#'  Returns -Inf whenever parameters or data violate distribution support.
#'
#' @param df Dataframe with columns "max", a vector of annual maxima observations,
#'   and "year", a vector of years corresponding to the observations in "max". Any
#'   `NaN` values are removed prior to likelihood computation.  
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

likelihood <- function(df, model, theta) {

	# Return -Inf if any parameters are NaN
	if (any(is.nan(theta))) return (-Inf)

	# Get the name and signature for the model 
	name <- substr(model, 1, 3)
	signature <- if(nchar(model) == 3) NULL else substr(model, 4, 5)
	
	# Define the covariate and remove NaN values
	covariate <- get.covariates(df$year, df$year)
	data <- df$max[!is.nan(df$max)]
	covariate <- covariate[!is.nan(df$max)]

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
	} else {
		k <- NULL	
	}

	get.ll(name, data, u, s, k)

}
