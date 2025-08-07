#' Sen's Trend Estimator
#'
#' Computes Sen's linear trend estimator for a univariate time series. The estimated 
#' slope and y-intercept are given in terms of the data and the *covariate*, which 
#' is derived from the years using the formula \eqn{(\text{Years} - 1900) / 100}.
#'
#' @inheritParams param-data
#' @inheritParams param-years
#'
#' @return A list containing the estimated trend:
#' - `data`: The `data` argument.
#' - `years`: The `years` argument.
#' - `slope`: The estimated slope.
#' - `intercept`: The estimated y-intercept.
#' - `residuals`: Vector of differences between the predicted and observed values.
#'
#' @details
#' Sen's slope estimator is a robust, nonparametric trend estimator based on the 
#' median of all pairwise slopes between data points. The corresponding intercept 
#' is the median of each \eqn{y_i - mx_i} where \eqn{m} is the estimated slope.
#'
#' @references
#' Sen, P.K. (1968). Estimates of the regression coefficient based on Kendall's tau.
#' \emph{Journal of the American Statistical Association}, 63(324), 1379–1389. 
#'
#' @seealso [eda_runs_test()], [plot_ams_data()]
#'
#' @examples
#' data <- rnorm(n = 100, mean = 100, sd = 10)
#' years <- seq(from = 1901, to = 2000)
#' eda_sens_trend(data, years)
#'
#' @importFrom stats median
#' @export

eda_sens_trend <- function(data, years) {

	data <- validate_numeric("data", data, bounds = c(0, Inf))
	years <- validate_numeric("years", years, size = length(data))

	# Get the length of data for convenience
	n <- length(data)

	# Convert the years into covariates
	covariate <- get_covariates(years)

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

	# Compute the predicted values and the residuals
	predicted_data <- sens_intercept + (sens_slope * covariate)
	residuals <- data - predicted_data

	# Return the results as a list
	list(
		data = data,
		years = years,
		slope = sens_slope,
		intercept = sens_intercept,
		residuals = residuals
	)

}
