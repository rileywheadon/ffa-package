#' Spearman Test for Autocorrelation
#'
#' Performs the Spearman rank serial correlation test on annual maximum series 
#' (AMS) data to check for serial correlation at various lags. Reports the smallest 
#' lag where the serial correlation is not statistically significant at the given 
#' significance level.
#'
#' @inheritParams param-data
#' @inheritParams param-alpha
#' @inheritParams param-quiet
#'
#' @return A list containing the test results, including:
#' - `data`: The `data` argument.
#' - `rho`: Numeric vector of serial correlation estimates for lags \eqn{1} to \eqn{n-3}.
#' - `sig`: Logical vector indicating which lags exhibit significant serial correlation
#' - `least_lag`: The smallest lag at which the serial correlation is insignificant. 
#' - `reject`: Logical. If `TRUE`, then `least_lag > 0`.
#' - `msg`: Character string summarizing the test result, printed if `quiet = FALSE`.
#'
#' @details
#' For each lag from \eqn{1} to \eqn{n - 3}, the function computes the Spearman 
#' correlation coefficient between the original AMS series and the lagged series. The 
#' first lag with an insignificant serial correlation coefficient returned as `least_lag`.
#'
#' @seealso [stats::cor.test()], [eda_bbmk_test()], [plot_spearman_test()]
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
	result <- ifelse(reject, "evidence", "NO evidence")

	# Print the results
	l1 <- sprintf("The Spearman test found a least insignificant lag of %d.", least_lag)
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
