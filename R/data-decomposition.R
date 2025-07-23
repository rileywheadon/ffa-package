#' Decompose Annual Maximum Series
#'
#' Decomposes the annual maxima series to derive its stationary stochastic component,
#' which can be used to identify a best-fit distribution using conventional stationary 
#' methods. The decomposition procedure follows that proposed by Vidrio-Sahagún and He 
#' (2022), which relies on the statistical representation of nonstationary stochastic 
#' processes.
#' 
#' Four scenarios are supported:
#'
#' 1. No trend (the data is returned unmodified).
#' 2. Linear trend in the mean only.
#' 3. Linear trend in the standard deviation only.
#' 4. Linear trends in both the mean and the standard deviation.
#' 
#' @inheritParams param-data
#' @inheritParams param-years
#' @inheritParams param-structure
#'
#' @details
#' Internally, the function does the following:
#' 1. Compute `covariate = (years - 1900) / 100`.
#' 2. If there is a trend in the location, fit Sen’s trend estimator to 
#'    `data` and `covariate`. Then, remove the fitted linear trend.
#' 3. If there is a trend in the scale, compute the variability using 
#'    [data_mw_variability()], fit Sen’s trend estimator to the vector of
#'    standard deviations, and then rescale the series to remove trends 
#'    in the scale.
#' 4. If necessary, shift the data so that its minimum is at least 1.
#'
#' @return Numeric vector of decomposed AMS data with the same length as `data`.
#'
#' @seealso [data_mw_variability()], [eda_sens_trend()]
#'
#' @examples
#' data <- rnorm(n = 100, mean = 100, sd = 10)
#' years <- seq(from = 1901, to = 2000)
#' structure <- list(location = TRUE, scale = FALSE)
#' data_decomposition(data, years, structure)
#'
#' @references
#' Vidrio-Sahagún, C. T., and He, J. (2022). The decomposition-based nonstationary 
#' flood frequency analysis. Journal of Hydrology, 612 (September 2022), 128186. 
#' \doi{10.1016/j.jhydrol.2022.128186}
#'
#' @export

data_decomposition <- function(data, years, structure) {

	data <- validate_numeric("data", data)
	years <- validate_numeric("years", years, size = length(data))
	structure <- validate_structure(structure)

	covariate <- get_covariates(years)
	decomposed <- data

	# Remove trends in location first to get rid of excess variance
	if (structure$location) {
		sens <- eda_sens_trend(data, years)
		decomposed <- data - (covariate * sens$slope)
	}

	if (structure$scale) {
		variance <- data_mw_variability(decomposed, years)
        sens <- eda_sens_trend(variance$std, variance$year)
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
