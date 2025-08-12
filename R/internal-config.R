# Helper function to compare the user's arguments with the default
generate_config <- function(args) {

	list(
		alpha = args$alpha %||% 0.05,
		bbmk_samples = args$bbmk_samples %||% 10000L,
		window_size = args$window_size %||% 10L,
		window_step = args$window_step %||% 5L,
		selection = args$selection %||% "L-distance",
		z_samples = args$z_samples %||% 10000L,
		s_estimation = args$s_estimation %||% "L-moments",
		ns_estimation = args$ns_estimation %||% "MLE",
		gev_prior = args$gev_perior %||% c(6, 9),
		s_uncertainty = args$s_uncertainty %||% "Bootstrap",
		ns_uncertainty = args$ns_uncertainty %||% "RFPL",
		return_periods = args$return_periods %||% c(2L, 5L, 10L, 20L, 50L, 100L),
		ns_slices = args$ns_slices %||% c(1925L, 1975L, 2025L),
		bootstrap_samples = args$bootstrap_samples %||% 1000L,
		rfpl_tolerance = args$rfpl_tolerance %||% 0.01,
		pp_formula = args$pp_formula %||% "Weibull"
	)

}

# Helper function for validating a configuration list
validate_config <- function(config) {

	# Load the configuration file 
    keys <- names(config)

	# Specify the types and lengths for each option
	specification <- list(
        alpha = list(type = "numeric", size = 1),
        bbmk_samples = list(type = "integer", size = 1),
		window_size = list(type = "integer", size = 1),
		window_step = list(type = "integer", size = 1),
		selection = list(type = "character", size = 1),
        z_samples = list(type = "integer", size = 1),
        s_estimation = list(type = "character", size = 1),
        ns_estimation = list(type = "character", size = 1),
        gev_prior = list(type = "numeric", size = 2),
        s_uncertainty = list(type = "character", size = 1),
        ns_uncertainty = list(type = "character", size = 1),
        return_periods = list(type = "integer"),
        ns_slices = list(type = "integer"),
        bootstrap_samples = list(type = "integer", size = 1),
        rfpl_tolerance = list(type = "numeric", size = 1),
        pp_formula = list(type = "character", size = 1)
    )

	# Values, ranges, and logical constraints
	distributions <- c("GUM", "NOR", "LNO", "GEV", "GLO", "GNO", "PE3", "LP3", "WEI")

	constraints <- list(
		alpha = c(0.01, 0.1),
        bbmk_samples = c(0, Inf),
		window_size = c(0, Inf),
		window_step = c(0, Inf),
		selection = c("L-distance", "L-kurtosis", "Z-statistic", distributions),
		z_samples = c(0, Inf),
		s_estimation = c("L-moments", "MLE", "GMLE"),
		ns_estimation = c("MLE", "GMLE"),
		gev_prior = c(0, Inf),
		s_uncertainty = c("Bootstrap", "RFPL", "RFGPL"),
		ns_uncertainty = c("Bootstrap", "RFPL", "RFGPL"),
		return_periods = c(2, Inf),
		bootstrap_samples = c(0, Inf),
		pp_formula = c("Weibull", "Blom", "Cunnane", "Gringorten", "Hazen")
	)

    # Check for unknown fields
    unknown_keys <- setdiff(keys, names(specification))
    if (length(unknown_keys) > 0) {
		unknown_text <- paste(sQuote(unknown_keys, q = FALSE), collapse = ", ")
		stop(glue("Unknown options: {unknown_text}"))
	}

    # Validate each option in config
    for (key in names(config)) {

        value <- config[[key]]
        rule <- specification[[key]]
		constraint <- constraints[[key]]

		# Validate types
        if (!any(vapply(rule$type, function(t) inherits(value, t), logical(1)))) {
			observed <- class(value)
			expected <- paste(rule$type, collapse = '/')
        	stop(glue("Invalid type for '{key}': expected {expected} but got {observed}"))
        }

		# Validate lengths
        if (!is.null(rule$size) && all(length(value) != rule$size)) {
            stop(glue("'{key}' must have length {rule$size}, got {length(value)}"))
        }

		# Validate character constraints
		if (!is.null(constraint) && is.character(constraint)) {
			if (!all(value %in% constraint)) {
				valid_entries <- paste(sQuote(constraint, q = FALSE), collapse = ", ")
				stop(glue("'{key}' must be one of {valid_entries}"))
			}
		}

		# Validate functional constraints
        if (!is.null(constraint) && is.numeric(constraint)) {
			if (!all(value >= constraint[1]) || !all(value <= constraint[2])) {
				stop(glue("'{key}' must be within ({constraint[1]}, {constraint[2]})"))
			}
        }

    }

    # Cross-field checks for GMLE parameter estimation
    if (config$s_estimation == "GMLE" && config$selection != "GEV") {
    	stop(glue("s_estimation: 'GMLE' requires selection: 'GEV'"))
    }

    if (config$ns_estimation == "GMLE" && config$selection != "GEV") {
   	 	stop(glue("ns_estimation: 'GMLE' requires selection: 'GEV'"))
    }

	# Cross-field checks for RFPL uncertainty quantification
    if (config$s_uncertainty == "RFPL" && config$s_estimation != "MLE") {
        stop("s_uncertainty: 'RFPL' requires s_estimation: 'MLE'")
    }

	if (config$ns_uncertainty == "RFPL" && config$ns_estimation != "MLE") {
        stop("ns_uncertainty: 'RFPL' requires ns_estimation: 'MLE'")
    }

	# Cross-field checks for RFGPL uncertainty quantification
    if (config$s_uncertainty == "RFGPL" && config$s_estimation != "GMLE") {
        stop("s_uncertainty: 'RFGPL' requires s_estimation: 'GMLE'")
    }

    if (config$ns_uncertainty == "RFGPL" && config$ns_estimation != "GMLE") {
        stop("ns_uncertainty: 'RFGPL' requires ns_estimation: 'GMLE'")
    }

    config

}
