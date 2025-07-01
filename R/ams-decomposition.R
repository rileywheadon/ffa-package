#' Decompose Annual Maximum Streamflow Data
#'
#' @description
#' Removes trends in the mean and/or variance of annual maximum streamflow (AMS)
#' data using Sen’s trend estimator and/or a moving‐window estimator of the variance. 
#' Four scenarios are supported:
#'
#' 1. No trend. The data is returned unmodified.
#' 2. Trend in the mean only.
#' 3. Trend in the variance only.
#' 4. Trends in both the mean and the variance.
#'
#' @inheritParams param-data
#' @inheritParams param-years
#' @inheritParams param-trend
#'
#' @details
#' Internally, the function does the following:
#' 1. Compute `covariate = (years - 1900) / 100`.
#' 2. If there is a trend in the location, fit Sen’s trend estimator to 
#'    `data` and `covariate`. Then, remove the fitted linear trend.
#' 3. If there is a trend in the scale, compute the moving‐window variance using 
#'    \link{mw_variance}, fit Sen’s trend estimator to those variances, and then 
#'    rescales the series to remove trends in the scale.
#' 4. If necessary, shift the data so that its minimum is at least 1.
#'
#' @return Numeric vector of decomposed AMS data with the same length as `data`.
#'
#' @seealso \link{ams_mw_variance}, \link{eda_sens_trend}
#'
#' @examples
#' data <- rnorm(n = 100, mean = 100, sd = 10)
#' years <- seq(from = 1901, to = 2000)
#' trend <- list(location = TRUE, scale = FALSE)
#' ams_decomposition(data, years, trend)
#'
#' @export

ams_decomposition <- function(data, years, trend) {

	validate_data(data)
	validate_years(years, data)
	validate_trend(trend)

	covariate <- get_covariates(years)
	decomposed <- data

	# Remove trends in location first to get rid of excess variance
	if (trend$location) {
		sens <- sens_trend(data, years)
		decomposed <- data - (covariate * sens$slope)
	}

	if (trend$scale) {
		variance <- mw_variance(decomposed, years)
        sens <- sens_trend(variance$std, variance$years)
        gt <- ((sens$slope * covariate) + sens$intercept) / sens$intercept
		mu <- mean(decomposed, na.rm = TRUE)

		# To remove variance around the mean, data must have mean 0
        decomposed <- mu + ((decomposed - mu) / gt)
	}

	# Enforce positivity since negative data causes issues elsewhere
	if (any(decomposed < 1)) {
		decomposed <- decomposed - min(decomposed) + 1
	}

	decomposed

}
