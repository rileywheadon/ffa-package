#' Decompose Annual Maximum Series
#'
#' @description
#' Decomposes a nonstationary annual maxima series to derive its stationary stochastic 
#' component, which can be used to identify a best-fit distribution using conventional 
#' stationary methods. The decomposition procedure follows that proposed by Vidrio-Sahagún 
#' and He (2022), which relies on the statistical representation of nonstationary stochastic 
#' processes.
#' 
#' @inheritParams param-data
#' @inheritParams param-ns-years
#' @inheritParams param-ns-structure
#'
#' @details
#' Internally, the function does the following:
#' 1. If there is a trend in the location, fit Sen’s trend estimator and subtract
#'    away the fitted trend.
#' 2. If there is a trend in the scale, estimate the variability of the data 
#'    with [data_mw_variability()], fit Sen’s trend estimator to the variability 
#'    vector, and rescale the data to remove the trend.
#' 3. If necessary, shift the data so that its minimum is at least 1.
#'
#' @return Numeric vector of decomposed data.
#'
#' @seealso [data_mw_variability()], [eda_sens_trend()]
#'
#' @examples
#' data <- rnorm(n = 100, mean = 100, sd = 10)
#' ns_years <- seq(from = 1901, to = 2000)
#' ns_structure <- list(location = TRUE, scale = FALSE)
#' data_decomposition(data, ns_years, ns_structure)
#'
#' @references
#' Vidrio-Sahagún, C. T., and He, J. (2022). The decomposition-based nonstationary 
#' flood frequency analysis. Journal of Hydrology, 612 (September 2022), 128186. 
#' \doi{10.1016/j.jhydrol.2022.128186}
#'
#' @export

data_decomposition <- function(data, ns_years, ns_structure) {

	data <- validate_numeric("data", data)
	ns_years <- validate_numeric("years", ns_years, size = length(data))
	ns_structure <- validate_structure(ns_structure)

	covariate <- get_covariates(ns_years)
	decomposed <- data

	# Remove trends in location first to get rid of excess variance
	if (ns_structure$location) {
		sens <- eda_sens_trend(data, ns_years)
		decomposed <- data - (covariate * sens$slope)
	}

	if (ns_structure$scale) {
		variance <- data_mw_variability(decomposed, ns_years)
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
