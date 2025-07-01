#' Sen's Trend Estimator
#'
#' Computes Sen's trend estimator for a univariate time series.
#'
#' @param data Numeric; a vector of annual maximum streamflow data.
#'
#' @param years Numeric; a vector of years with the same length as `data`.
#'
#' @param quiet Logical (1); if FALSE, prints a summary of results (default is TRUE).
#'
#' @return List; the estimated trend:
#' - `sens.slope`: Median slope of all pairwise data-year combinations.
#' - `sens.intercept`: Median intercept estimate of the fitted line.
#' - `residuals`: Vector of residuals between observed and fitted values.
#' - `msg`: Character string summarizing the results.
#'
#' @details
#' Sen's slope estimator is a robust, non-parametric trend estimator computed from the median
#' of all pairwise slopes between data points. The corresponding intercept is taken as the
#' median of residual-corrected values. 
#'
#' @references
#' Sen, P.K. (1968). Estimates of the regression coefficient based on Kendall's tau.
#' \emph{Journal of the American Statistical Association}, 63(324), 1379–1389. 
#'
#' @seealso \link{eda_runs_test}
#'
#' @examples
#' data <- rnorm(n = 100, mean = 100, sd = 10)
#' years <- seq(from = 1901, to = 2000)
#' eda_sens_trend(data, years)
#'
#' @importFrom stats median
#' @export

eda_sens_trend <- function(data, years, quiet = TRUE) {

	validate.data(data)
	validate.years(years, data)

	# Get the length of data for convenience
	n <- length(data)

	# Convert the years into covariates
	covariate <- get.covariates(years)

	# Compute all pairwise slopes
	slopes <- c()
	for (i in 1:(n-1)) {
		for (j in (i+1):n) {
			m <- (data[j] - data[i]) / (covariate[j] - covariate[i])
			slopes <- c(slopes, m)
		}
	}

	# Get the estimate for the slope
	sens_slope <- median(slopes, na.rm = TRUE)

	# Get the estimate for the intercept
	intercepts <- data - (sens_slope * covariate)
	sens_intercept <- median(intercepts, na.rm =  TRUE)

	# Compute the predicted AMS values and the residuals
	predicted_data <- sens_intercept + (sens_slope * covariate)
	residuals <- data - predicted_data

	# Print the results of Sen's trend estimator
	m <- round(sens_slope, 3)
	b <- round(sens_intercept, 2)
	msg <- sprintf("Estimated trend: y = %fx + %f.", m, b)
	if (!quiet) message(msg)

	# Return the results as a list
	list(
		slope = sens_slope,
		intercept = sens_intercept,
		residuals = residuals,
		msg = msg
	)

}
