#' Kwiatkowski–Phillips–Schmidt–Shin (KPSS) Unit Root Test
#'
#' Performs the KPSS test for stationarity in annual maximum streamflow (AMS) data 
#' using an implementation based on the \pkg{aTSA} package. The null hypothesis is that 
#' the time series is trend-stationary with a linear trend. The alternative hypothesis
#' is that the time series has a unit root (non-stationarity).
#'
#' @param data A numeric vector of annual maximum streamflow data. Must not contain NA values.
#' @param alpha A numeric value indicating the significance level. Must be at least 0.01 (default is 0.05).
#' @param quiet Logical. If FALSE, prints a summary message to the console (default is TRUE).
#'
#' @return A named list with the following components:
#' \describe{
#'   \item{statistic}{The KPSS test statistic.}
#'   \item{p.value}{The reported p-value from the test. See notes regarding discrete thresholds.}
#'   \item{reject}{Logical. TRUE if the null hypothesis of stationarity is rejected at \code{alpha}.}
#'   \item{msg}{Character string summarizing the test outcome, printed if \code{quiet = FALSE}.}
#' }
#'
#' @details
#' The implementation of the KPSS test is based on the \pkg{aTSA} package, which interpolates
#' a significance table from Hobjin et al. (2004). Therefore, a result of \eqn{p = 0.01} 
#' implies that \eqn{p \leq 0.01} and a result of \eqn{p = 0.10} implies that \eqn{p \geq 0.10}. 
#' This implementation uses the Type III KPSS test, which accounts for a linear trend in the data.
#'
#' @seealso \code{\link[aTSA]{kpss.test}}, \code{\link{pp.test}}
#' @export

kpss.test <- function(data, alpha = 0.05, quiet = TRUE) {

	# Construct time series yt and shifted time series yt1 for fitting the autoregressive model 
	z <- embed(data, 2)
	yt <- z[,1]
	yt1 <- z[,2]

	# Set the number of lags using the standard choice, proportional to n^(1/2)
	n <- length(yt)
	q <- max(1, floor(3 * sqrt(n) / 13))

	# Create an autoregressive model with drift and trend (yt as a function of yt1 and t) 
	t <- 1:n
    model <- lm(yt ~ yt1 + t)

	# Get the residuals and their cumulative sum of squares
	residuals <- resid(model)
	Sk <- cumsum(residuals)^2

	# Compute the sample autocovariances
	gamma <- numeric(q + 1)
	for (i in 1:(q + 1)) {
		u <- embed(residuals,i)
		gamma[i] = sum(u[,1] * u[,i]) / n
	}

	# Compute an estimator of the long-run variance 
	lambda_hat <- gamma[1] + 2*sum((1 - 1:q/(q + 1))*gamma[-1])
	statistic <- sum(Sk) / (lambda_hat * n^2)

	# Define table of quantiles and test statistic values from Hobjin et al. (2004)
	quantiles <- c(0.10, 0.05, 0.025, 0.01)
    table <- c(0.119, 0.146, 0.176, 0.216)

	# Interpolate the p-value
	p_value <- approx(table, quantiles, statistic, rule = 2)$y

	# Determine whether we reject or fail to reject based on p_value and alpha
	reject <- if (alpha == 0.10) p_value < alpha else p_value <= alpha

	# Set the P-value text to inform the user of the limited significance thresholds
	p_text <- if (p_value == 0.10) {
		"*at least* 0.10"
	} else if (p_value == 0.01) {
		"*at most* 0.01"
	} else {
		round(p_value, 3)
	}

	# Print the results of the test
	msg <- test_message(
		"KPSS",
		reject,
		p_value,
		alpha,
		"NO evidence of a unit root.",
		"evidence of a unit root."
	)

	if (!quiet) message(msg)

	# Return the results
	list(
		statistic = statistic,
		p.value = p_value,
		reject = reject,
		msg = msg
	)

}

