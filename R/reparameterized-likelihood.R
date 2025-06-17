#' Compute Reparameterized Log-Likelihood for Extreme-Value Models
#'
#' @description
#' Calculates the log-likelihood of data under various extreme-value distributions
#' with a fixed reference quantile yp at exceedance probability pe. Supports
#' optional linear covariate trends in location and/or scale via model signatures.
#'
#' @param df Numeric vector of observations. NaN values are removed internally.
#'
#' @param model Character string specifying the distribution code. The first three
#'    letters denote the family: 'GUM', 'NOR', 'LNO', 'GEV', 'GLO',
#'    'GNO', 'PE3', 'LP3', or 'WEI'. A trailing signature of '10'/'100' indicates
#'    a trend in location. '11'/'110' indicates trends in both location and scale.
#'
#' @param yp Numeric value of the fixed reference quantile at exceedance probability pe.
#'
#' @param pe Numeric exceedance probability corresponding to yp.
#'
#' @param theta Numeric vector of nuisance parameters to be optimized:
#'   - s (scale).
#'   - optional trend coefficients for nonstationary models.
#'   - k (shape) for three-parameter families.
#'
#' @return
#' A single numeric value: the sum of pointwise log-likelihoods under the specified
#' model with the given reparameterization. Invalid or out-of-support cases return –Inf.
#'
#' @details
#' 1. Removes NaN values from data and constructs a covariate vector on \code{[0,1]}.
#' 2. Computes the model quantile qpe at probability pe using get.distributions().  
#' 3. Determines location u and scale s from yp, qpe, and theta, applying covariate trends.  
#' 4. Includes shape parameter k for three-parameter distributions.  
#' 5. Computes the log-density for each supported family, replacing invalid values with –Inf.  
#' 6. Returns the total log-likelihood.  
#'
#' @export

reparameterized.likelihood <- function(df, model, yp, pe, theta, ti = 0) {

	# Get the name and signature for the model 
	name <- substr(model, 1, 3)
	signature <- if(nchar(model) == 3) NULL else substr(model, 4, 5)
	
	# Define the covariate and rescale ti
	covariate <- get.covariates(df$year, df$year)
	ti <- get.covariates(df$year, ti)

	# Remove NaN values from the data and the covariates
	data <- df$max[!is.nan(df$max)]
	covariate <- covariate[!is.nan(df$max)]

	# Get the quantile at exceedance probability pe
	qpe <- get.quantiles(pe, model, c(0, theta), ti)[1, 1]

	# Log-transform yp, qpe, and the data if necessary
	if (name %in% c("LNO", "LP3")) {
		yp <- log(yp)
		qpe <- log(qpe)
		data <- log(data)
	}

	# Parse the stationary/non-stationary signature
	if (is.null(signature)) {
		u <- yp - qpe
		s <- theta[1]
	} else if (signature == "10") {
		u <- yp - qpe + (theta[1] * covariate)
		s <- theta[2]
	} else if (signature == "11") {
		u <- yp - qpe + (theta[1] * covariate)
		s <- theta[2] + (theta[3] * covariate)
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
		ll <- -log(s * sqrt(2 * pi)) - (z^2 / 2)

	} else if (name == "LNO") {

		# Compute z, the normalized data
		z <- ((data - u) / s)

		# Compute vector of likelihoods for each data point
		ll <- -log(s * sqrt(2 * pi)) - (z^2 / 2) - data

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
