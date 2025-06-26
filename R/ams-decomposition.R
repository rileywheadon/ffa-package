#' Decompose Annual Maximum Streamflow Data
#'
#' @description
#' Removes trends in the means and/or variances of annual maximum streamflow (AMS)
#' data using Sen’s trend estimator and a moving‐window estimator of the variance. 
#' Four scenarios are supported:
#'
#' 1. No trend. The data is returned unmodified.
#' 2. Trend in means only.
#' 3. Trend in variance only.
#' 4. Trends in both means and variance.
#'
#' @param data Numeric; a vector of annual maximum streamflow data.
#'
#' @param years Numeric; a vector of years with the same length as `data`.
#'
#' @param signature Character (1); Indicates trend(s) to remove.  Must be one of:
#' - `NULL`: Do not remove any trends.
#' - `"10"`: Remove linear trend in the mean.
#' - `"01"`: Remove linear trend in the variance.
#' - `"11"`: Remove trends in both the mean and variance.
#'
#' @details
#' Internally, the function does the following:
#' 1. If `signature == NULL`, returns `data` without modification.
#' 2. Constructs a `covariate` based on `years` using the formula `(years - 1900) / 100`.
#' 3. If `signature == "10"`, fits Sen’s trend estimator to `data` and `covariate`
#'    and removes the fitted linear mean trend.
#' 4. If `signature == "01"`, computes moving‐window variances using \link{mw.variance}, 
#'    fits Sen’s trend estimator to those variances, and rescales the series to remove 
#'    trends in the variance.
#' 5. If `signature == "11"`, applies (3) then (4) sequentially.
#' 6. Ensures all returned values are greater than 1 by shifting the decomposed data.
#'
#' @return Numeric; a vector with the same length as `data` and `years` containing 
#' the decomposed AMS data with the specified trend(s) removed.
#'
#' @seealso \link{mw.variance}, \link{sens.trend}
#'
#' @examples
#' data <- rnorm(n = 100, mean = 100, sd = 10)
#' years <- seq(from = 1901, to = 2000)
#' ams.decomposition(data, years, "10")
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

	# Signature "01": Trend in the AMS variance.
	else if (signature == "01") {

		# Get the moving window SD estimates
		variance <- mw.variance(data, years)

		# Run Sen's trend estimator and decompose the data
        model <- sens.trend(variance$std, variance$years)
		c0 <- model$sens.intercept
		c1 <- model$sens.slope
        gt <- ((c1 * covariate) + c0) / c0
		mu <- mean(data, na.rm = TRUE)
        decomposed <- mu + ((data - mu) / gt)

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

	# Otherwise return an error
	else {
		stop("Error: 'signature' must be NULL, '10', '01', or '11'.")
	}

	# Enforce positivity
	if (sum(decomposed < 0, na.rm = TRUE) > 0) {
		decomposed <- decomposed - min(decomposed) + 1
	}

	# Return decomposed data
	decomposed

}
