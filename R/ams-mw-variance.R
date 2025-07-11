#' Estimate Variance for Annual Maximum Streamflow Data 
#'
#' This function estimates the standard deviation of a vector of annual maximum 
#' streamflow (AMS) data using a moving window algorithm, returning a list that
#' pairs each window’s mean year with its computed standard deviation. The 
#' parameters `size` and `step` parameters control the behaviour of the window.
#'
#' @inheritParams param-data
#' @inheritParams param-years
#'
#' @param size Integer scalar. The number of years in each moving window.
#'   Must be a positive number less than or equal to `length(data)`
#'   (default is 10). If `length(data) < size`, an error is raised.
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
#' ams_mw_variance(data, years)
#'
#' @importFrom stats sd
#' @export

ams_mw_variance <- function(data, years, size = 10L, step = 5L) { 

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

		std <- sd(data[window])
		std_series <- c(std_series, std)

		year <- mean(years[window])
		year_series <- c(year_series, year)

		# Increment move the trailing edge of the moving window by the step
		location <- location + step
	}

	list(std = std_series, year = year_series)

}
