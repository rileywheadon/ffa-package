#' White Test for Heteroskedasticity in Annual Maximum Streamflow
#'
#' Performs the White test for heteroskedasticity by regressing the squared residuals of a linear
#' model on the original regressors and their squared terms. The null hypothesis is homoskedasticity.
#'
#' @param data Numeric vector of annual maximum streamflow values with no missing values.
#' @param years Numeric vector of years corresponding to \code{data}, with no missing values.
#' @param alpha Numeric significance level for the test (default is 0.05).
#' @param quiet Logical. If FALSE, prints a summary message to the console (default is TRUE).
#'
#' @return A named list containing:
#' \describe{
#'   \item{r.squared}{Coefficient of determination from the auxiliary regression.}
#'   \item{statistic}{White test statistic based on sample size and auxiliary \code{R^2}.}
#'   \item{p.value}{P-value computed from the Chi-squared distribution with 2 degrees of freedom.}
#'   \item{reject}{Logical. TRUE if the null hypothesis is rejected at \code{alpha}.}
#'   \item{msg}{Character string summarizing the test result (printed if \code{quiet = FALSE}).}
#' }
#'
#' @details
#' The White test regresses the squared residuals from a primary linear model \code{lm(data ~ years)}
#' against both the original regressor and its square. The test statistic is calculated as
#' \code{n * R^2}, where \code{R^2} is from the auxiliary regression. Under the null hypothesis,
#' this statistic follows a \eqn{\chi^2} distribution with 2 degrees of freedom.
#'
#' Rejection of the null hypothesis suggests the presence of heteroskedasticity in the residuals.
#'
#' @references White, H. (1980). A heteroskedasticity-consistent covariance matrix estimator and a
#' direct test for heteroskedasticity. \emph{Econometrica}, 48(4), 817–838.
#'
#' @seealso \code{\link[stats]{lm}}, \code{\link[stats]{pchisq}}
#'
#' @importFrom stats lm pchisq resid
#' @export

white.test <- function(data, years, alpha = 0.05, quiet = TRUE) {

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
	msg <- test_message(
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
