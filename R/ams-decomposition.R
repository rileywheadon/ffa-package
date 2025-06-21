#' Decompose Annual Maximum Streamflow
#'
#' @description
#' Removes trends in the means and/or variances of an annual maximum streamflow (AMS)
#' data using Sen’s slope estimator and a moving‐window variance estimator. Three
#' scenarios are supported:
#' 1. Trend in means only
#' 2. Trend in variance only
#' 3. Trends in both means and variance
#'
#' @param df A data frame containing:
#'   `max`: numeric vector of annual maximum streamflow values.
#'   `year`: any values (will be replaced internally by a scaled covariate).
#'
#' @param scenario Integer (1, 2, or 3) indicating which trend component(s) to remove:
#'   1: Remove linear trend in the mean.
#'   2: Remove trend in variance.
#'   3: Remove both mean and variance trends sequentially.
#'
#' @return
#' A numeric vector of the same length as `df`, containing the “decomposed”
#' AMS values with the specified trend(s) removed and adjusted to be strictly
#' positive.
#'
#' @details
#' Internally, the function:
#' 1. Constructs a covariate linearly spaced on \code{[0,1]} over the record length.
#' 2. For scenario 1, fits Sen’s slope (`sens.trend`) to (`max` vs. covariate)
#'    and removes the fitted linear mean trend.
#' 3. For scenario 2, computes moving‐window standard deviations
#'    (`mw.variance`), fits Sen’s slope to those deviations, and rescales the
#'    series to remove variance trends around the overall mean.
#' 4. For scenario 3, applies scenario 1 then scenario 2 sequentially.
#' 5. Ensures all returned values are great than 1 by shifting the data if any negatives occur.
#'
#' @export

ams.decomposition <- function(data, years, signature) {

	# Replace the 'year' column in df with the covariate
	covariate <- get.covariates(years)

	# Signature NULL: no trend.
	if (is.null(signature)) {
		return (data)
	}

	# Signature "10": Trend in the AMS means.
	else if (signature == "10") {
		model <- sens.trend(data, years)
		decomposed <- data - (covariate * model$sens.slope)
	}

	# Signature "11": Trend in AMS means and variance.
	else if (signature == "11") {

		# Run Sen's trend estimator and remove the trend in the means
		model_means <- sens.trend(data, years)
		decomposed <- data - (covariate * model_means$sens.slope)

		# Get the moving window SD estimates
		df_variance <- mw.variance(decomposed, years)

		# Run Sen's trend estimator and decompose the data
        model_variance <- sens.trend(df_variance$std, df_variance$year)
		c0 <- model_variance$sens.intercept
		c1 <- model_variance$sens.slope
        gt <- ((c1 * covariate) + c0) / c0
		mu <- mean(decomposed, na.rm = TRUE)
        decomposed <- mu + ((decomposed - mu) / gt)

	}

	else {
		stop("Error: 'signature' must be NULL, '10' or '11'.")
	}

	# Enforce positivity
	if (sum(decomposed < 0, na.rm = TRUE) > 0) {
		decomposed = decomposed - min(decomposed) + 1
	}

	# Return decomposed data
	decomposed

}
