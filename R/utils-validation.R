# Parameter validation helper functions
validate_data <- function(data, positive = TRUE) {
	if (!is.vector(data)) stop("'data' must be a vector")
	if (!is.numeric(data)) stop("'data' must be numeric")
	if (any(is.nan(data) | is.na(data))) stop("'data' must have no missing values")
	if (positive & any(data < 0)) stop("'data' must be strictly positive")
	return (data)
}

validate_model <- function(model) {
	if (!is.character(model)) stop("'model' must be a string")
	if (length(model) != 1) stop("'model' must have length 1")
	options <- c("GUM", "NOR", "LNO", "GEV", "GLO", "GNO", "PE3", "LP3", "WEI")
	text <- paste(options, collapse = ", ")
	if (!(model %in% options)) stop(sprintf("'model' must be one of %s", text))
	return (model)
}

validate_method <- function(method) {
	if (!is.character(method)) stop("'method' must be a string")
	if (length(method) != 1) stop("'method' must have length 1")
	options <- c("L-moments", "MLE", "GMLE")
	text <- paste(options, collapse = ", ")
	if (!(method %in% options)) stop(sprintf("'method' must be one of %s", text))
	return (method)
}


validate_params <- function(params, model, trend = NULL) {
	if (!is.vector(params)) stop("'params' must be a vector")
	if (!is.numeric(params)) stop("'params' must be numeric")
	if (any(is.nan(params) | is.na(params))) stop("'params' must have no missing values")
	n <- model_info(model, trend)$n.params 
	if (length(params) != n) stop(sprintf("'params' must have length %d", n))
	return (params)
}

validate_prior <- function(prior) {

	# If prior is NULL, return immediately
	if (is.null(prior)) return (NULL)

	# If prior is not NULL, ensure it is a positive numeric vector of length 2
	if (!is.vector(prior)) stop("'prior' must be a vector")
	if (!is.numeric(prior)) stop("'prior' must be numeric")
	if (any(is.nan(prior) | is.na(prior))) stop("'prior' must have no missing values")
	if (length(prior) != 2) stop("'prior' must have length 2")
	if (any(prior <= 0)) stop("'prior' must be strictly positive")
	return (prior)
}

validate_years <- function(years, data = NULL) {

	# If years is NULL, return immediately
	if (is.null(years)) return (NULL)

	# If years is not NULL, check that it is a numeric vector with no missing values
	if (!is.vector(years)) stop("'years' must be a vector")
	if (!is.numeric(years)) stop("'years' must be numeric")
	if (any(is.nan(years) | is.na(years))) stop("'years' must have no missing values")

	# If data is not NULL, check that it has same length as years
	if (is.null(data)) return (years)
	if (length(years) != length(data)) stop("'years' and 'data' must have equal length")
	return (years)
}

validate_trend <- function(trend) {

	# If trend is NULL, return a stationary trend object
	if (is.null(trend)) return (list(location = FALSE, scale = FALSE))

	# Validate the location option
	location <- trend$location
	if (!is.logical(location)) stop("'trend$location' must be logical")
	if (length(location) != 1) stop("'trend$location' must have length 1")

	# Validate the scale option
	scale <- trend$location
	if (!is.logical(scale)) stop("'trend$scale' must be logical")
	if (length(scale) != 1) stop("'trend$scale' must have length 1")
	return (trend)

}

validate_slice <- function(slice) {
	if (is.null(slice)) return (1900)
	if (!is.vector(slice)) stop("'slice' must be a vector")
	if (!is.numeric(slice)) stop("'slice' must be numeric")
	if (length(slice) != 1) stop("'slice' must have length 1")
	return (slice)
}

validate_samples <- function(samples) {
	if (length(samples) != 1) stop("'samples' must have length 1")
	if (!is.integer(samples)) stop("'samples' must be an integer")
	return (samples)
}

validate_quiet <- function(quiet) {
	if (length(quiet) != 1) stop("'quiet' must have length 1")
	if (!is.logical(quiet)) stop("'quiet' must be TRUE or FALSE")
	return (quiet)
}

validate_probabilities <- function(p) {
	if (any(p < 0 | p > 1)) stop("'p' must be between 0 and 1 inclusive")
	return (p)
}

validate_alpha <- function(alpha) {
	if (length(alpha) != 1) stop("'alpha' must have length 1")
	if (alpha < 0.01 | alpha > 0.10) stop("'alpha' must be between 0.01 and 0.1")
	return (alpha)
}

