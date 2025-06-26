#' Estimate Variance for Annual Maximum Streamflow Data 
#'
#' This function estimates the standard deviation of a vector of annual maximum 
#' streamflow (AMS) data using a moving window algorithm, returning a list that
#' pairs each window’s mean year with its computed standard deviation. The 
#' parameters `size` and `step` parameters control the behaviour of the window.
#'
#' @param data Numeric; a vector of annual maximum streamflow data.
#'
#' @param years Numeric; a vector of years with the same length as `data`.
#'
#' @param size Integer (1); the number of consecutive indices in each moving
#'   window. Must be a positive integer less than or equal to `length(data)`
#'   (default is 10). If `length(data) < size`, an error is raised.
#'
#' @param step Integer (1); the offset (in indices) between successive moving 
#'   windows. Must be a positive integer (default is 5).
#'
#' @return A list with two entries:
#' - `years`: Numeric; the mean year within each window.
#' - `std`: Numeric; the standard deviation of the data within each window.
#'
#' @examples
#' data <- rnorm(n = 100, mean = 100, sd = 10)
#' years <- seq(from = 1901, to = 2000)
#' mw.variance(data, years)
#'
#' @importFrom stats sd
#' @export

mw.variance <- function(data, years, size = 10, step = 5) { 

	# Check that the data vector is sufficiently large
	n <- length(data)
  	if (n < size) {
    	stop(sprintf("Data frame has too few rows (%d < %d).", n, size))
  	}

	# Create df_variance, which contains the variances of the AMS data
	std_series <- c()
	year_series <- c()
	n <- length(data)
	i <- 1

	# Iterate through all the windows
	while ((i + size - 1) <= n) {

		# Get the window from the data frame, increment i
		window <- i:(i + size - 1)
		i <- i + step

		# Compute the standard deviation within the window, add it to std_series
		std <- sd(data[window], na.rm = TRUE)
		std_series <- c(std_series, std)

		# Compute the mean within the window, add it to year_series
		year <- mean(years[window], na.rm = TRUE)
		year_series <- c(year_series, year)
	}

	# Return moving window variances as a dataframe
	list(years = year_series, std = std_series)

}
