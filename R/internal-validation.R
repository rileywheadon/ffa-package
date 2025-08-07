# Validate a numeric vector with optional bounds/size restrictions
validate_numeric <- function(name, x, optional = FALSE, bounds = NULL, size = NULL) {

	# Helper function to concisely generate error messages
	err <- function(msg, ...) stop(do.call(sprintf, list(msg, name, ...)))

	# If x is null and the argument is optional, return NULL
	if (optional && is.null(x)) return (NULL)

	# If x is not null, check that it is a numeric vector with no missing values
	if (!is.vector(x)) err("'%s' must be a vector")
	if (!is.numeric(x)) err("'%s' must be numeric")
	if (length(x) == 0) err("'%s' must be non-empty")
	if (any(is.nan(x) | is.na(x))) err("'%s' must have no missing values")
	if (any(is.infinite(x))) err("'%s' must be finite")

	# Check the bounds if they exist
	if (!is.null(bounds)) {
		if (any(x < bounds[1])) err("'%s' must be at least %f", bounds[1])
		if (any(x > bounds[2])) err("'%s' must be at most %f", bounds[2])
	} 

	# Check the size restriction if it exists
	if (!is.null(size)) {
		if (length(x) != size) err("'%s' must have length %d", size)
	}

	# If data is NULL, return
	return (x)

}

# Validate an integer scalar with optional bounds
validate_integer <- function(name, x, bounds = NULL) {

	# Helper function to concisely generate error messages
	err <- function(msg, ...) stop(do.call(sprintf, list(msg, name, ...)))

	# If x is not null, check that it is an integer scalar
	if (!is.integer(x)) err("'%s' must be an integer")
	if (length(x) != 1) err("'%s' must have length 1")
	if (is.nan(x) | is.na(x)) err("'%s' must not be missing")

	# Check the bounds if they exist
	if (!is.null(bounds)) {
		if (x < bounds[1]) err("'%s' must be at least '%f'", bounds[1])
		if (x > bounds[2]) err("'%s' must be at most '%f'", bounds[2])
	} 

	return (as.vector(x))

}

# Validate a logical scalar
validate_logical <- function(name, x) {

	# Helper function to concisely generate error messages
	err <- function(msg, ...) stop(do.call(sprintf, list(msg, name, ...)))

	# if x is not null, check that it is a numeric vector with no missing values
	if (!is.logical(x)) err("'%s' must be logical")
	if (length(x) != 1) err("'%s' must have length 1")
	if (is.nan(x) | is.na(x)) err("'%s' must not be missing")

	return (as.vector(x))

}

# Validate a floating-point scalar with optional bounds
validate_float <- function(name, x, bounds = NULL) {

	err <- function(msg, ...) stop(do.call(sprintf, list(msg, name, ...)))

	# if x is not null, check that it is a numeric vector with no missing values
	if (!is.numeric(x)) err("'%s' must be numeric")
	if (length(x) != 1) err("'%s' must have length 1")
	if (is.nan(x) | is.na(x)) err("'%s' must not be missing")
	if (is.infinite(x)) err("'%s' must be finite")

	# Check the bounds if they exist
	if (!is.null(bounds)) {
		if (x < bounds[1]) err("'%s' must be at least %f", bounds[1])
		if (x > bounds[2]) err("'%s' must be at most %f", bounds[2])
	} 

	return (as.vector(x))

}

# Validate an enumeration
validate_enum <- function(name, x) {

	# Helper function to concisely generate error messages
	err <- function(msg, ...) stop(do.call(sprintf, list(msg, name, ...)))

	# Get options based on the name
	options <- if (name == "distribution") {
		c("GUM", "NOR", "LNO", "GEV", "GLO", "GNO", "PE3", "LP3", "WEI")
	} else if (name == "method") {
		c("L-moments", "MLE", "GMLE")
	} else if (name == "pp_formula") {
		c("Weibull", "Blom", "Cunnane", "Gringorten", "Hazen")
	}

	# Check that x is in the list of enumerations
	options_text <- paste(options, collapse = ", ")
	if (!is.character(x)) err("'%s' must be a character string")
	if (length(x) != 1) err("'%s' must have length 1")
	if (!(x %in% options)) err("'%s' must be one of %s", options_text)

	return(as.vector(x))
}

# Validate a 'structure' object
validate_structure <- function(x) {

	# If structure is NULL, return a stationary structure object
	if (is.null(x)) return (list(location = FALSE, scale = FALSE))

	# Validate the location option
	location <- x$location
	if (is.null(location)) stop("'ns_structure' requires item 'location'")
	validate_logical('ns_structure$location', x$location)

	# Validate the scale option
	scale <- x$scale
	if (is.null(scale)) stop("'ns_structure' requires item 'scale'")
	validate_logical('ns_structure$scale', x$scale)

	return (x)

}

# Validate a 'params' vector
validate_params <- function(distribution, x, structure = NULL) {
	n <- model_info(distribution, structure)$n_params 
	validate_numeric("params", x, FALSE, size = n)
}
