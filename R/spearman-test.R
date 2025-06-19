#' Spearman Test for Serial Correlation in Time Series
#'
#' Performs the Spearman rank autocorrelation test on annual maximum streamflow (AMS) data to
#' assess the presence of serial correlation at various lags. Reports the first lag where
#' autocorrelation is no longer statistically significant at the specified level.
#'
#' @param data Numeric vector of annual maximum streamflow data with no missing values.
#' @param alpha Numeric significance level for the test (default is 0.05).
#' @param quiet Logical. If FALSE, prints a summary message describing the result (default is TRUE).
#'
#' @return A named list containing:
#' \describe{
#'   \item{rho}{Vector of Spearman autocorrelation estimates for lags \code{1} to \code{n - 3}.}
#'   \item{sig}{Logical vector indicating which lags exhibit significant autocorrelation.}
#'   \item{least.lag}{The smallest lag at which autocorrelation is not statistically significant.}
#'   \item{reject}{Logical. TRUE if we reject the null hypothesis of no serial correlation.}
#'   \item{msg}{Character string summarizing the test result (printed if \code{quiet = FALSE}).}
#' }
#'
#' @details
#' For each lag from 1 to \code{n - 3}, the function computes the Spearman rank correlation
#' between the AMS series and its lagged version. The first lag with a non-significant
#' autocorrelation (p-value > \code{alpha}) is returned as \code{least.lag}.
#'
#' This test is useful for identifying the minimum temporal separation required to ensure
#' approximate independence, especially when constructing block-bootstrap resampling schemes.
#'
#' @seealso \code{\link[stats]{cor.test}}, \code{\link{bbmk.test}}
#'
#' @importFrom stats cor.test
#' @export

spearman.test <- function(data, alpha = 0.05, quiet = TRUE) {

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
	l2 <- sprintf("There %s evidence of serial correlation.", ifelse(reject, "is", "is NO"))

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
