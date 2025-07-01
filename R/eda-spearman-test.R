#' Spearman Test for Autocorrelation
#'
#' @order $1
#'
#' @description
#' Performs the Spearman rank autocorrelation test on annual maximum streamflow 
#' (AMS) data to check for autocorrelation at various lags. Reports the smallest 
#' lag where the autocorrelation is not statistically significant at the given 
#' significance level.
#'
#' @param data Numeric; a vector of annual maximum streamflow data.
#'
#' @param alpha Numeric (1); the significance level (default is 0.05).
#'
#' @param quiet Logical (1); if FALSE, prints a summary of results (default is TRUE).
#'
#' @return List; test results, including:
#' - `rho`: Vector of Spearman autocorrelation estimates for lags \eqn{1} to \eqn{n - 3}.
#' - `sig`: Logical vector indicating which lags exhibit significant autocorrelation.
#' - `least.lag`: The smallest lag at which the autocorrelation is not statistically significant.
#' - `reject`: Logical. TRUE if `least.lag > 0`.
#' - `msg`: Character string summarizing the test result (printed if `quiet = FALSE`).
#'
#' @details
#' For each lag from \eqn{1} to \eqn{n - 3}, the function computes the Spearman correlation 
#' coefficient between the AMS series and its lagged version. The first lag with an 
#' insignificant autocorrelation coefficient returned as `least.lag`.
#'
#' @seealso \link[stats]{cor.test}, \link{eda_bbmk_test}
#'
#' @examples
#' data <- rnorm(n = 100, mean = 100, sd = 10)
#' eda_spearman_test(data)
#'
#' @importFrom stats cor.test
#' @export

eda_spearman_test <- function(data, alpha = 0.05, quiet = TRUE) {

	# Run parameter validation (see helpers.R)
	validate.data(data)
	validate.alpha(alpha)

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

	least_lag <- which(p_values > alpha)[1] - 1

	# Get a series of booleans for whether the serial correlation is significant
	sig <- (p_values <= alpha)
	reject <- (least_lag > 0)

	# Print the results
	l1 <- sprintf("The Spearman test found a least insignificant lag of %f.", least_lag)
	l2 <- sprintf("There is %s evidence of serial correlation.", ifelse(reject, "", "NO"))

	msg <- paste0("\n - ", c(l1, l2), collapse = "")
	if (!quiet) message(msg)

	# Return the results as a list
	list(
		rho = rho,
		sig = sig,
		least.lag = least_lag,
		reject = reject,
		msg = msg
	)

}
