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


# List of models with the number of parameters
models.info <- list(
	"GUM"    = 2,
	"GUM10"  = 3,
	"GUM11"  = 4,
	"NOR"    = 2,
	"NOR10"  = 3,
	"NOR11"  = 4,
	"LNO"    = 2,
	"LNO10"  = 3,
	"LNO11"  = 4,
	"GEV"    = 3,
	"GEV100" = 4,
	"GEV110" = 5,
	"GLO"    = 3,
	"GLO100" = 4,
	"GLO110" = 5,
	"GNO"    = 3,
	"GNO100" = 4,
	"GNO110" = 5,
	"PE3"    = 3,
	"PE3100" = 4,
	"PE3110" = 5,
	"LP3"    = 3,
	"LP3100" = 4,
	"LP3110" = 5,
	"WEI"    = 3,
	"WEI100" = 4,
	"WEI110" = 5
)

llvxxx <- function(model, data, params, years = NULL) {

	# Check that data is numeric vector
	if (!is.numeric(data) | !is.vector(data)) {
		warning("Warning: 'data' is not a numeric vector.")
		return (-Inf)
	}

	# Check that data has no missing values
	if (any(is.nan(data)) | any(is.na(data))) {
		warning("Warning: 'data' contains NaN or NA values.")
		return (-Inf)
	}

	# Check that data is strictly positive
	if (any(data <= 0)) {
		warning("Warning: 'data' contains negative values.")
		return (-Inf)
	}

	# Check that params is a numeric vector
	if (!is.numeric(params) | !is.vector(data)) {
		warning("Warning: 'params' is not a numeric vector.")
		return (-Inf)
	}

	# Check that params has the correct number of entries
	if (length(params) != models.info[[model]]) {
		str <- "Warning: 'params' for model '%s' must have length %d."
		warning(sprintf(str, model, models.info[[model]]))
		return (-Inf)
	}

	# Check that all parameters are defined
	if (any(is.nan(params)) | any(is.na(params))) {
		message("Message: 'params' contains NaN or NA values.")
		return (-Inf)
	}

	# Get the name and signature for the model
	name <- substr(model, 1, 3)
	signature <- if (nchar(model) == 3) NULL else substr(model, 4, 5)

	# Validate the years argument if the model is non-stationary
	if (!is.null(signature)) {

		# Check that years is not NULL, NaN, or NA
		if (is.null(years) | any(is.nan(years)) | any(is.na(years))) {
			warning("Warning: 'years' contains NaN or NA values.")
			return (-Inf)
		}

		# Check that years has the same length as data
		if (length(years) != length(data)) {
			warning("Warning: 'years' and 'data' have different lengths.")
			return (-Inf)
		}

		# Compute the covariate
		covariate <- get.covariates(years, years)

	}

	# Parse the stationary/non-stationary signature
	# - u: mu (location parameter)
	# - s: sigma (scale parameter)
	# - k: kappa (shape parameter)

	if (is.null(signature)) {
		u <- params[1]
		s <- params[2]
	} else if (signature == "10") {
		u <- params[1] + (covariate * params[2])
		s <- params[3]
	} else if (signature == "11") {
		u <- params[1] + (covariate * params[2])
		s <- params[3] + (covariate * params[4])
	}

	if (name %in% c("GEV", "GLO", "GNO", "PE3", "LP3", "WEI")) {
		k <- params[length(params)]
	}

	# Ensure that the scale parameter is positive
	if (any(s <= 0)) {
		message("Message: scale parameter has negative values.")
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

llvgum    <- function(data, params, years = NULL) llvxxx("GUM", data, params)
llvgum10  <- function(data, params, years) llvxxx("GUM10", data, params, years)
llvgum11  <- function(data, params, years) llvxxx("GUM11", data, params, years)

llvnor    <- function(data, params, years = NULL) llvxxx("NOR", data, params)
llvnor10  <- function(data, params, years) llvxxx("NOR10", data, params, years)
llvnor11  <- function(data, params, years) llvxxx("NOR11", data, params, years)

llvlno    <- function(data, params, years = NULL) llvxxx("LNO", data, params)
llvlno10  <- function(data, params, years) llvxxx("LNO10", data, params, years)
llvlno11  <- function(data, params, years) llvxxx("LNO11", data, params, years)

llvgev    <- function(data, params, years = NULL) llvxxx("GEV", data, params)
llvgev100 <- function(data, params, years) llvxxx("GEV100", data, params, years)
llvgev110 <- function(data, params, years) llvxxx("GEV110", data, params, years)

llvglo    <- function(data, params, years = NULL) llvxxx("GLO", data, params)
llvglo100 <- function(data, params, years) llvxxx("GLO100", data, params, years)
llvglo110 <- function(data, params, years) llvxxx("GLO110", data, params, years)

llvgno    <- function(data, params, years = NULL) llvxxx("GNO", data, params)
llvgno100 <- function(data, params, years) llvxxx("GNO100", data, params, years)
llvgno110 <- function(data, params, years) llvxxx("GNO110", data, params, years)

llvpe3    <- function(data, params, years = NULL) llvxxx("PE3", data, params)
llvpe3100 <- function(data, params, years) llvxxx("PE3100", data, params, years)
llvpe3110 <- function(data, params, years) llvxxx("PE3110", data, params, years)

llvlp3    <- function(data, params, years = NULL) llvxxx("LP3", data, params)
llvlp3100 <- function(data, params, years) llvxxx("LP3100", data, params, years)
llvlp3110 <- function(data, params, years) llvxxx("LP3110", data, params, years)

llvwei    <- function(data, params, years = NULL) llvxxx("WEI", data, params)
llvwei100 <- function(data, params, years) llvxxx("WEI100", data, params, years)
llvwei110 <- function(data, params, years) llvxxx("WEI110", data, params, years)
