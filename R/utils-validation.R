# Parameter validation helper functions
validate.data <- function(data) {
	if (!is.vector(data)) stop("'data' must be a vector")
	if (!is.numeric(data)) stop("'data' must be numeric")
	if (any(is.nan(data) | is.na(data))) stop("'data' must have no missing values")
	if (any(data < 0)) stop("'data' must be strictly positive")
}

validate.years <- function(years, data) {
	if (!is.vector(years)) stop("'years' must be a vector")
	if (!is.numeric(years)) stop("'years' must be numeric")
	if (any(is.nan(years) | is.na(years))) stop("'years' must have no missing values")
	if (length(years) != length(data)) stop("'years' and 'data' must have equal length")
}

validate.model <- function(model) {
	if (!is.character(model)) stop("'model' must be a string.")
	if (length(model) != 1) stop("'model' must have length 1.")
	model_list <- c("GUM", "NOR", "LNO", "GEV", "GLO", "GNO", "PE3", "LP3", "WEI")
	model_text <- paste(model_list, collapse = ", ")
	if (!(model %in% model_list)) stop(sprintf("'model' must be one of %s.", model_text))
}

validate.trend <- function(trend) {

	location <- trend$location
	if (!is.logical(location)) stop("'trend$location' must be logical.")
	if (length(location) != 1) stop("'trend$location' must have length 1.")

	scale <- trend$location
	if (!is.logical(scale)) stop("'trend$scale' must be logical.")
	if (length(scale) != 1) stop("'trend$scale' must have length 1.")

}

validate.alpha <- function(alpha) {
	if (length(alpha) != 1) stop("'alpha' must have length 1.")
	if (alpha < 0.01 | alpha > 0.10) stop("'alpha' must be between 0.01 and 0.1.")
}

validate.params <- function(params, model, trend) {
	if (!is.vector(params)) stop("'params' must be a vector.")
	if (!is.numeric(params)) stop("'params' must be numeric.")
	if (any(is.nan(params) | is.na(params))) {
		stop("'params' must not contain NaN/NA values.")
	}
	info <- model.info(model, trend)
	if (length(params) != info$n.params) {
		stop(sprintf("'params' must have length %d.", info$n.params))
	}
}

validate.prior <- function(prior) {
	if (!is.vector(prior)) stop("'prior' must be a vector")
	if (!is.numeric(prior)) stop("'prior' must be numeric")
	if (any(is.nan(prior) | is.na(prior))) stop("'prior' must have no missing values")
	if (length(prior) != 2) stop("'prior' must have length 2")
	if (any(prior <= 0)) stop("'prior' must be strictly positive")
}

validate.samples <- function(samples) {
	if (length(samples) != 1) stop("'samples' must have length 1")
	if (!is.integer(samples)) stop("'samples' must be an integer")
}

validate.quiet <- function(quiet) {
	if (length(quiet) != 1) stop("'quiet' must have length 1")
	if (!is.logical(quiet)) stop("'quiet' must be TRUE or FALSE")
}
