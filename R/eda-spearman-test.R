#' Spearman Test for Autocorrelation
#'
#' Performs the Spearman serial correlation test on annual maximum series data 
#' to check for serial correlation at various lags. Reports the smallest lag 
#' where the serial correlation is not statistically significant at the given 
#' significance level (the *least insignificant lag*).
#'
#' @inheritParams param-data
#' @inheritParams param-alpha
#'
#' @return A list containing the test results, including:
#' - `data`: The `data` argument.
#' - `alpha`: The significance level as specified in the `alpha` argument.
#' - `null_hypothesis`: A string describing the null hypothesis.
#' - `alternative_hypothesis`: A string describing the alternative hypothesis.
#' - `rho`: Numeric vector of serial correlation estimates for lags \eqn{1} to \eqn{n-3}.
#' - `least_lag`: The smallest lag at which the serial correlation is insignificant. 
#' - `reject`: If `TRUE`, the null hypothesis was rejected at significance `alpha`.
#'
#' @seealso [stats::cor.test()], [eda_bbmk_test()], [plot_spearman_test()]
#'
#' @examples
#' data <- rnorm(n = 100, mean = 100, sd = 10)
#' eda_spearman_test(data)
#'
#' @importFrom stats cor.test
#' @export

eda_spearman_test <- function(data, alpha = 0.05) {

	data <- validate_numeric("data", data, bounds = c(0, Inf))
	alpha <- validate_float("alpha", alpha, bounds = c(0.01, 0.1))

	# Assign a variable to the number of data points for convenience
	n <- length(data)

	# Compute the spearman rho serial correlation for a given lag
	rho_autocorrelation <- function(lag, data) {
		data_original <- data[(lag + 1):length(data)]
		data_lagged <- data[1:(length(data) - lag)]
		cor.test(data_original, data_lagged, method="spearman", exact=FALSE)
	}

	# Find the lowest insignificant serial correlation lag
	rho <- numeric(n - 3)
	p_values <- numeric(n - 3)

	for (i in 1:(n - 3)) {
		result <- rho_autocorrelation(i, data)
		rho[i] = result$estimate
		p_values[i] = result$p.value
	}

	least_lag <- which(p_values > alpha)[1]

	# Get a series of booleans for whether the serial correlation is significant
	sig <- (p_values <= alpha)
	reject <- (least_lag > 1)

	# Return the results as a list
	list(
		data = data,
		alpha = alpha,
		null_hypothesis = "The least insignificant lag is 1." ,
		alternative_hypothesis = "The least insignificant lag is greater than 1." ,
		rho = rho,
		least_lag = least_lag,
		reject = reject
	)

}
