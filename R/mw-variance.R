#' Compute moving‐window standard deviations for an AMS data frame
#'
#' This function calculates the standard deviation of the annual maximum series
#' (“max”) in non‐overlapping (or partially overlapping) moving windows, returning
#' a data frame that pairs each window’s mean “year” with its computed standard
#' deviation. The window parameters control the length and step size.
#'
#' @param df A data.frame (or tibble) containing at least two numeric columns:
#'   \describe{
#'     \item{year}{Numeric or integer vector of years.}
#'     \item{max}{Numeric vector of annual maximum values.}
#'   }
#'   If either column is missing or not numeric, the function will throw an error.
#' @param window_length Integer(1). The number of consecutive rows in each moving
#'   window. Must be a positive integer less than or equal to \code{nrow(df)}.
#'   Defaults to \code{10}. If \code{nrow(df) < window_length}, an error is raised.
#' @param window_step Integer(1). The offset (in rows) between the start of
#'   successive windows. Must be a positive integer. Defaults to \code{5}.
#'
#' @return A data.frame with two columns:
#'   \describe{
#'     \item{year}{Numeric. The mean of \code{df$year} within each window.}
#'     \item{std}{Numeric. The standard deviation of \code{df$max} within each window,
#'       computed with \code{na.rm = TRUE}.}
#'   }
#'   Each row corresponds to one window. The number of rows equals
#'   \code{floor((nrow(df) - window_length) / window_step) + 1}.
#'
#' @importFrom stats sd
#' @export


mw.variance <- function(df, window_length = 10, window_step = 5) { 

	# Check that the dataframe has enough rows 
	n <- nrow(df)
  	if (n < window_length) {
    	stop(sprintf("Data frame has too few rows (%d < %d).", n, window_length))
  	}

	# Create df_variance, which contains the variances of the AMS data
	std_series <- c()
	year_series <- c()
	n <- nrow(df)
	i <- 1

	# Iterate through all the windows
	while ((i + window_length - 1) <= n) {

		# Get the window from the data frame, increment i
		window <- df[i:(i + window_length - 1), ]
		i <- i + window_step

		# Compute the standard deviation within the window, add it to std_series
		std <- sd(window$max, na.rm = TRUE)
		std_series <- c(std_series, std)

		# Compute the mean within the window, add it to year_series
		year <- mean(window$year, na.rm = TRUE)
		year_series <- c(year_series, year)
	}

	# Return moving window variances as a dataframe
	data.frame(year = year_series, std = std_series)

}
