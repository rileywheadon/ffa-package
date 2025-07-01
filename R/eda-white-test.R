#' White Test for Heteroskedasticity
#'
#' @order $1
#'
#' @description
#' Performs the White test for heteroskedasticity by regressing the squared residuals of a 
#' linear model on the original regressors and their squared terms. The null hypothesis 
#' is homoskedasticity.
#'
#' @param data Numeric; a vector of annual maximum streamflow data.
#'
#' @param years Numeric; a vector of years with the same length as `data`.
#'
#' @param alpha Numeric (1); the significance level (default is 0.05).
#'
#' @param quiet Logical (1); if FALSE, prints a summary of results (default is TRUE).
#'
#' @return List; results of the White test:
#' - `r.squared`: Coefficient of determination from the auxiliary regression.
#' - `statistic`: White test statistic based on sample size and `r.squared`.
#' - `p.value`: P-value derived from a Chi-squared distribution with 2 degrees of freedom.
#' - `reject`: Logical. TRUE if the null hypothesis is rejected at significance `alpha`.
#' - `msg`: Character string summarizing the test result (printed if `quiet = FALSE`).
#'
#' @details
#' The White test regresses the squared residuals from a primary linear model 
#' `lm(data ~ years)` against both the original regressor and its square. 
#' The test statistic is calculated as \eqn{nR^2}, where \eqn{R^2} is the 
#' coefficient of determination from the auxiliary regression. Under the null hypothesis,
#' this statistic follows a \eqn{\chi^2} distribution with 2 degrees of freedom.
#' Rejection of the null hypothesis suggests heteroskedasticity in the residuals.
#'
#' @references White, H. (1980). A heteroskedasticity-consistent covariance matrix 
#' estimator and a direct test for heteroskedasticity. \emph{Econometrica}, 48(4), 817–838.
#'
#' @seealso \link[stats]{lm}, \link[stats]{pchisq}
#'
#' @examples
#' data <- rnorm(n = 100, mean = 100, sd = 10)
#' years <- seq(from = 1901, to = 2000)
#' eda_white_test(data, years)
#'
#' @importFrom stats lm pchisq resid
#' @export

eda_white_test <- function(data, years, alpha = 0.05, quiet = TRUE) {

	# Run parameter validation (see helpers.R)
	validate.data(data)
	validate.years(years)
	validate.alpha(alpha)

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
	msg <- stats.message(
		"White",
		reject,
		p_value,
		alpha,
		"evidence of heteroskedasticity",
		"evidence of homoskedasticity"
	)
	
	if (!quiet) message(msg)

	# Return the results of the test
	list(
		r.squared = r_squared,
		statistic = statistic,
		p.value = p_value,
		reject = reject,
		msg = msg
	)

}
