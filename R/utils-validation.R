# Parameter validation helper functions
validate_data <- function(data, positive = TRUE) {
	if (!is.vector(data)) stop("'data' must be a vector")
	if (!is.numeric(data)) stop("'data' must be numeric")
	if (length(data) == 0) stop("'data' must be non-empty")
	if (any(is.nan(data) | is.na(data))) stop("'data' must have no missing values")
	if (any(is.infinite(data))) stop("'data' must be finite")
	if (positive & any(data < 0)) stop("'data' must be non-negative")
	return (data)
}

validate_model <- function(model) {
	options <- c("GUM", "NOR", "LNO", "GEV", "GLO", "GNO", "PE3", "LP3", "WEI")
	text <- paste(options, collapse = ", ")

	if (!is.character(model)) stop("'model' must be a character string")
	if (length(model) != 1L) stop("'model' must have length 1")
	if (!(model %in% options)) stop(sprintf("'model' must be one of %s", text))
	return (as.vector(model))
}

validate_method <- function(method) {
	options <- c("L-moments", "MLE", "GMLE")
	text <- paste(options, collapse = ", ")

	if (!is.character(method)) stop("'method' must be a character string")
	if (length(method) != 1) stop("'method' must have length 1")
	if (!(method %in% options)) stop(sprintf("'method' must be one of %s", text))
	return (method)
}


validate_params <- function(params, model, trend = NULL) {
	n <- model_info(model, trend)$n_params 

	if (!is.vector(params)) stop("'params' must be a vector")
	if (!is.numeric(params)) stop("'params' must be numeric")
	if (length(params) != n) stop(sprintf("'params' must have length %d", n))
	if (any(is.nan(params) | is.na(params))) stop("'params' must have no missing values")
	if (any(is.infinite(params))) stop("'params' must be finite")
	return (params)
}

validate_prior <- function(prior) {

	# If prior is NULL, return immediately
	if (is.null(prior)) return (NULL)

	# If prior is not NULL, ensure it is a positive numeric vector of length 2
	if (!is.vector(prior)) stop("'prior' must be a vector")
	if (!is.numeric(prior)) stop("'prior' must be numeric")
	if (length(prior) != 2) stop("'prior' must have length 2")
	if (any(is.nan(prior) | is.na(prior))) stop("'prior' must have no missing values")
	if (any(is.infinite(prior))) stop("'prior' must be finite")
	if (any(prior <= 0)) stop("'prior' must be strictly positive")
	return (prior)
}

validate_years <- function(years, data = NULL) {

	# If years is NULL, return 
	if (is.null(years)) return (NULL)

	# If years is not NULL, check that it is a numeric vector with no missing values
	if (!is.vector(years)) stop("'years' must be a vector")
	if (!is.numeric(years)) stop("'years' must be numeric")
	if (length(years) == 0) stop("'years' must be non-empty")
	if (any(is.nan(years) | is.na(years))) stop("'years' must have no missing values")
	if (any(is.infinite(years))) stop("'years' must be finite")

	# If data is NULL, return
	if (is.null(data)) return (years)

	# If data is not NULL, check that it has same length as years
	if (length(years) != length(data)) stop("'years' and 'data' must have equal length")
	return (years)
}

validate_trend <- function(trend) {

	# If trend is NULL, return a stationary trend object
	if (is.null(trend)) return (list(location = FALSE, scale = FALSE))

	# Validate the location option
	location <- trend$location
	if (is.null(location)) stop("'trend' requires item 'location'")
	if (!is.logical(location)) stop("'trend$location' must be logical")
	if (length(location) != 1) stop("'trend$location' must have length 1")

	# Validate the scale option
	scale <- trend$scale
	if (is.null(scale)) stop("'trend' requires item 'scale'")
	if (!is.logical(scale)) stop("'trend$scale' must be logical")
	if (length(scale) != 1) stop("'trend$scale' must have length 1")
	return (trend)

}

validate_slice <- function(slice) {
	if (!is.vector(slice)) stop("'slice' must be a vector")
	if (!is.numeric(slice)) stop("'slice' must be numeric")
	if (length(slice) != 1) stop("'slice' must have length 1")
	if (any(is.nan(slice) | is.na(slice))) stop("'slice' must have no missing values")
	if (any(is.infinite(slice))) stop("'slice' must be finite")
	return (slice)
}

validate_samples <- function(samples) {
	if (!is.vector(samples)) stop("'samples' must be a vector")
	if (!is.integer(samples)) stop("'samples' must be an integer")
	if (length(samples) != 1) stop("'samples' must have length 1")
	if (any(is.nan(samples) | is.na(samples))) stop("'samples' must have no missing values")
	return (samples)
}

validate_quiet <- function(quiet) {
	if (!is.vector(quiet)) stop("'quiet' must be a vector")
	if (!is.logical(quiet)) stop("'quiet' must be logical")
	if (length(quiet) != 1) stop("'quiet' must have length 1")
	if (any(is.nan(quiet) | is.na(quiet))) stop("'quiet' must have no missing values")
	return (quiet)
}

validate_probabilities <- function(p) {
	if (!is.vector(p)) stop("'p' must be a vector")
	if (!is.numeric(p)) stop("'p' must be numeric")
	if (length(p) == 0) stop("'p' must be non-empty")
	if (any(is.nan(p) | is.na(p))) stop("'p' must have no missing values")
	if (any(p < 0 | p > 1)) stop("'p' must be between 0 and 1 inclusive")
	return (p)
}

validate_alpha <- function(alpha) {
	if (!is.vector(alpha)) stop("'alpha' must be a vector")
	if (!is.numeric(alpha)) stop("'alpha' must be numeric")
	if (length(alpha) != 1) stop("'alpha' must have length 1")
	if (any(is.nan(alpha) | is.na(alpha))) stop("'alpha' must have no missing values")
	if (alpha < 0.01 | alpha > 0.10) stop("'alpha' must be between 0.01 and 0.1")
	return (alpha)
}

