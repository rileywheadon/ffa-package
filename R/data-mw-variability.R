#' Estimate Variance for Annual Maximum Series Data 
#'
#' Generates a time series of standard deviations using a moving window algorithm,
#' which can be used to explore potential evidence of nonstationarity in the AMS 
#' variability. It returns a list that pairs each window’s mean year with its window 
#' standard deviation. The hyperparameters `size` and `step` control the behaviour 
#' of the moving window. Following the simulation findings from Vidrio-Sahagún 
#' and He (2022), the default window size and step are set to 10 and 5 years
#' respectively. However, these can be changed by the user.
#'
#' @inheritParams param-data
#' @inheritParams param-years
#'
#' @param size Integer scalar. The number of years in each moving window.
#'   Must be a positive number less than or equal to `length(data)`
#'   (default is 10). 
#'
#' @param step Integer scalar. The offset (in years) between successive 
#'   moving windows. Must be a positive number (default is 5).
#'
#' @return A list with two entries:
#' - `years`: Numeric vector containing the mean year within each window.
#' - `std`: Numeric vector of standard deviations within each window.
#'
#' @examples
#' data <- rnorm(n = 100, mean = 100, sd = 10)
#' years <- seq(from = 1901, to = 2000)
#' data_mw_variability(data, years)
#'
#' @references
#' Vidrio-Sahagún, C. T., and He, J. (2022). The decomposition-based nonstationary 
#' flood frequency analysis. Journal of Hydrology, 612 (September 2022), 128186. 
#' \doi{10.1016/j.jhydrol.2022.128186}
#'
#' @importFrom stats sd
#' @export

data_mw_variability <- function(data, years, size = 10L, step = 5L) { 

	data <- validate_numeric("data", data)
	years <- validate_numeric("years", years, size = length(data))
	size <- validate_integer("size", size, bounds = c(1, Inf))
	step <- validate_integer("step", step, bounds = c(1, size))

	# If length(data) < size, we cannot create a moving window.
	n <- length(data)
  	if (n < size) {
		stop(sprintf("'data' does not have enough entries (%d < %d)", n, size))
	}

	std_series <- c()
	year_series <- c()
	location <- min(years)

	# Once the leading edge of the moving window is past max(years) we stop.
	while (location <= max(years) - size + 1) {

		window <- which(years >= location & years < location + size)

		if (length(window) > 1) {
			std <- sd(data[window])
			std_series <- c(std_series, std)
			year <- mean(years[window])
			year_series <- c(year_series, year)
		}

		# Increment move the trailing edge of the moving window by the step
		location <- location + step
	}

	list(std = std_series, year = year_series)

}
