#' Spearman Test for Autocorrelation
#'
#' @description
#' Performs the Spearman rank autocorrelation test on annual maximum streamflow 
#' (AMS) data to check for autocorrelation at various lags. Reports the smallest 
#' lag where the autocorrelation is not statistically significant at the given 
#' significance level.
#'
#' @inheritParams param-data
#' @inheritParams param-alpha
#' @inheritParams param-quiet
#'
#' @return A list containing the test results, including:
#' - `data`: The `data` argument.
#' - `rho`: Numeric vector of autocorrelation estimates for lags \eqn{1} to \eqn{n-3}.
#' - `sig`: Logical vector indicating which lags exhibit significant autocorrelation.
#' - `least.lag`: The smallest lag at which the autocorrelation is insignificant. 
#' - `reject`: Logical. If `TRUE`, then `least.lag > 0`.
#' - `msg`: Character string summarizing the test result, printed if `quiet = FALSE`.
#'
#' @details
#' For each lag from \eqn{1} to \eqn{n - 3}, the function computes the Spearman 
#' correlation coefficient between the AMS series and its lagged version. The 
#' first lag with an insignificant autocorrelation coefficient returned as `least.lag`.
#'
#' @seealso \link[stats]{cor.test}, \link{eda_bbmk_test}, \link{plot_spearman_test}
#'
#' @examples
#' data <- rnorm(n = 100, mean = 100, sd = 10)
#' eda_spearman_test(data)
#'
#' @importFrom stats cor.test
#' @export

eda_spearman_test <- function(data, alpha = 0.05, quiet = TRUE) {

	data <- validate_numeric("data", data, bounds = c(0, Inf))
	alpha <- validate_float("alpha", alpha, bounds = c(0.01, 0.1))
	quiet <- validate_logical("quiet", quiet)

	# Assign a variable to the number of data points for convenience
	n <- length(data)

	# Compute the spearman rho-autocorrelation for a given lag
	rho_autocorrelation <- function(lag, data) {
		data_original <- data[(lag + 1):length(data)]
		data_lagged <- data[1:(length(data) - lag)]
		cor.test(data_original, data_lagged, method="spearman", exact=FALSE)
	}

	# Find the lowest non-significant serial correlation lag
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
	result <- ifelse(reject, "evidence", "NO evidence")

	# Print the results
	l1 <- sprintf("The Spearman test found a least insignificant lag of %f.", least_lag)
	l2 <- sprintf("There is %s of serial correlation.", result)
	msg <- paste0("\n - ", c(l1, l2), collapse = "")
	if (!quiet) message(msg)

	# Return the results as a list
	list(
		data = data,
		rho = rho,
		sig = sig,
		least_lag = least_lag,
		reject = reject,
		msg = msg
	)

}
