#' Sen's Slope Estimator
#'
#' Computes Sen's slope estimator and intercept for a univariate time series
#'
#' @param data Numeric vector of AMS values or variances with no missing values.
#' @param year Numeric vector of years corresponding to \code{data}, with no missing values.
#' @param quiet Logical. If FALSE, prints a summary message describing results (default is TRUE).
#'
#' @return A named list containing:
#' \describe{
#'   \item{sens.slope}{Median slope of all pairwise data-year combinations (Sen's slope).}
#'   \item{sens.intercept}{Median intercept estimate of the fitted line.}
#'   \item{residuals}{Vector of residuals between observed and fitted values.}
#'   \item{msg}{Character string summarizing the estimator.}
#' }
#'
#' @details
#' Sen's slope estimator is a robust, non-parametric trend estimator computed from the median
#' of all pairwise slopes between data points. The corresponding intercept is taken as the
#' median of residual-corrected values. 
#'
#' @references
#' Sen, P.K. (1968). Estimates of the regression coefficient based on Kendall's tau.
#' \emph{Journal of the American Statistical Association}, 63(324), 1379–1389. \cr
#'
#' @seealso \code{\link{runs.test}}, \code{\link{mk.test}}
#'
#' @importFrom stats median
#' @export

sens.trend <- function(data, year, quiet = TRUE) {

	# Get the length of data for convenience
	n <- length(data)

	# Compute all pairwise slopes
	slopes <- c()
	for (i in 1:(n-1)) {
		for (j in (i+1):n) {
			slopes <- c(slopes, (data[j] - data[i]) / (year[j] - year[i]))
		}
	}

	# Get the estimate for the slope
	sens_slope <- median(slopes, na.rm = TRUE)

	# Get the estimate for the intercept
	intercepts <- data - (sens_slope * year)
	sens_intercept <- median(intercepts, na.rm =  TRUE)

	# Compute the predicted AMS values and the residuals
	predicted_data <- sens_intercept + (sens_slope * year)
	residuals <- data - predicted_data

	# Print the results of Sen's trend estimator
	m <- round(sens_slope, 3)
	b <- round(sens_intercept, 2)
	msg <- sprintf("Estimated trend: y = %fx + %f.", m, b)
	if (!quiet) message(msg)

	# Return the results as a list
	list(
		sens.slope = sens_slope,
		sens.intercept = sens_intercept,
		residuals = residuals,
		msg = msg
	)

}
