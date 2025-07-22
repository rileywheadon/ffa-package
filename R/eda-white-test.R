#' White Test for Heteroskedasticity
#'
#' Performs the White test for heteroskedasticity by regressing the squared residuals 
#' of a linear model on the original regressors and their squared terms. The null 
#' hypothesis is homoskedasticity.
#'
#' @inheritParams param-data
#' @inheritParams param-years
#' @inheritParams param-alpha
#' @inheritParams param-quiet
#'
#' @return A list containing the results of the White test:
#' - `data`: The `data` argument.
#' - `years`: The `years` argument.
#' - `r_squared`: Coefficient of determination from the auxiliary regression.
#' - `statistic`: White test statistic based on sample size and `r_squared`.
#' - `p_value`: The p-value derived from a Chi-squared distribution with `df = 2`.
#' - `reject`: Logical. If `TRUE`, the null hypothesis is rejected at `alpha`.
#' - `msg`: Character string summarizing the test result, printed if `quiet = FALSE`.
#'
#' @details
#' The White test regresses the squared residuals from a primary linear model 
#' `lm(data ~ years)` against both the original regressor and its square. 
#' The test statistic is calculated as \eqn{nR^2}, where \eqn{R^2} is the 
#' coefficient of determination from the auxiliary regression. Under the null 
#' hypothesis, the test statistic has the \eqn{\chi^2} distribution. 
#'
#' @references 
#' White, H. (1980). A heteroskedasticity-consistent covariance matrix estimator and 
#' a direct test for heteroskedasticity. \emph{Econometrica}, 48(4), 817–838.
#'
#' @seealso [stats::lm()], [stats::pchisq()]
#'
#' @examples
#' data <- rnorm(n = 100, mean = 100, sd = 10)
#' years <- seq(from = 1901, to = 2000)
#' eda_white_test(data, years)
#'
#' @importFrom stats lm pchisq resid
#' @export

eda_white_test <- function(data, years, alpha = 0.05, quiet = TRUE) {

	data <- validate_numeric("data", data, bounds = c(0, Inf))
	years <- validate_numeric("years", years, size = length(data))
	alpha <- validate_float("alpha", alpha, bounds = c(0.01, 0.1))
	quiet <- validate_logical("quiet", quiet)

	# Do a linear regression of data against years, get the squared residuals
	primary_model <- lm(data ~ years)
	squared_residuals <- resid(primary_model)^2

	# Fit an auxillary model to the squared residuals, get the R^2 statistic
	auxillary_model <- lm(squared_residuals ~ years + I(years^2))
	r_squared <- summary(auxillary_model)$r.squared

	# Compute the test statistic and p-value
	statistic <- length(data) * r_squared
	p_value <- 1 - pchisq(statistic, df = 2)

	# Determine whether we reject or fail to reject based on p_value and alpha
	reject <- (p_value <= alpha)

	# Print the results of the test
	msg <- stats_message(
		"White",
		reject,
		p_value,
		alpha,
		"NO evidence of heteroskedasticity",
		"evidence of heteroskedasticity"
	)
	
	if (!quiet) message(msg)

	# Return the results of the test
	list(
		data = data,
		years = years,
		r_squared = r_squared,
		statistic = statistic,
		p_value = p_value,
		reject = reject,
		msg = msg
	)

}
