#' Kwiatkowski–Phillips–Schmidt–Shin (KPSS) Unit Root Test
#'
#' Performs the KPSS unit root test on annual maximum streamflow (AMS) data. 
#' The null hypothesis is that the time series is trend-stationary with a linear 
#' trend and constant drift. The alternative hypothesis is that the time series 
#' has a unit root and is non-stationary.
#'
#' @inheritParams param-data
#' @inheritParams param-alpha
#' @inheritParams param-quiet
#'
#' @return A list containing the test results, including:
#' - `data`: The `data` argument.
#' - `statistic`: The KPSS test statistic.
#' - `p_value`: The interpolated p-value. See note regarding discrete thresholds.
#' - `reject`: Logical scalar. If, TRUE the null hypothesis is rejected at `alpha`.
#' - `msg`: Character string summarizing the test outcome, printed if `quiet = FALSE`.
#'
#' @details
#' The implementation of the KPSS test is based on the \pkg{aTSA} package, which 
#' interpolates a significance table from Hobjin et al. (2004). Therefore, a result 
#' of \eqn{p = 0.01} implies that \eqn{p \leq 0.01} and a result of \eqn{p = 0.10} 
#' implies that \eqn{p \geq 0.10}. This implementation uses the Type III KPSS test,
#' which accounts for a linear trend in the data.
#'
#' @seealso \link{eda_pp_test}
#'
#' @references
#' Hobijn, B., Franses, P.H. and Ooms, M. (2004), Generalizations of the KPSS-test 
#' for stationarity. Statistica Neerlandica, 58: 483-502. 
#'
#' @examples
#' data <- rnorm(n = 100, mean = 100, sd = 10)
#' eda_kpss_test(data)
#'
#' @importFrom stats embed
#' @export

eda_kpss_test <- function(data, alpha = 0.05, quiet = TRUE) {

	data <- validate_numeric("data", data, bounds = c(0, Inf))
	alpha <- validate_float("alpha", alpha, bounds = c(0.01, 0.1))
	quiet <- validate_logical("quiet", quiet)

	# Construct time series yt/yt1 for fitting the autoregressive model 
	z <- embed(data, 2)
	yt <- z[,1]
	yt1 <- z[,2]

	# Set the number of lags using the standard choice, proportional to sqrt(n)
	n <- length(yt)
	q <- max(1, floor(3 * sqrt(n) / 13))

	# Create an autoregressive model with drift + trend
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
	lambda_hat <- gamma[1] + 2 * sum((1 - 1:q/(q + 1))*gamma[-1])
	statistic <- sum(Sk) / (lambda_hat * n^2)

	# Table of quantiles and test statistic values from Hobjin et al. (2004)
	quantiles <- c(0.10, 0.05, 0.025, 0.01)
    table <- c(0.119, 0.146, 0.176, 0.216)

	# Interpolate the p-value using the table above
	p_value <- approx(table, quantiles, statistic, rule = 2)$y

	reject <- if (alpha == 0.10) p_value < alpha else p_value <= alpha

	p_text <- if (p_value == 0.10) {
		"at least 0.10"
	} else if (p_value == 0.01) {
		"at most 0.01"
	} else {
		round(p_value, 3)
	}

	msg <- stats_message(
		"KPSS",
		reject,
		p_value,
		alpha,
		"NO evidence of a unit root.",
		"evidence of a unit root."
	)

	if (!quiet) message(msg)

	list(
		data = data,
		statistic = statistic,
		p_value = p_value,
		reject = reject,
		msg = msg
	)

}

