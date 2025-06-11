#' Evaluate Goodness-of-Fit and Uncertainty Coverage for Fitted Flood Models
#'
#' Computes multiple performance metrics and diagnostic indicators to assess the quality of
#' a fitted flood frequency distribution. This includes residual statistics, information criteria,
#' and coverage-based metrics using bootstrapped confidence intervals.
#'
#' @param ams Numeric vector of annual maximum streamflow values (no missing values).
#' @param distribution The three letter code for a probability distribution.
#' @param params Numeric vector of fitted distribution parameters.
#' @param uncertainty A list containing bootstrap confidence interval estimates, typically the output
#'   from \code{\link{sb.uncertainty}}. Must include \code{t}, \code{ci_lower}, and \code{ci_upper}.
#' @param plotting_position Character string specifying the plotting position formula.
#'   One of \code{"Weibull"}, \code{"Blom"}, \code{"Cunnane"}, \code{"Gringorten"}, or \code{"Hazen"}.
#' @param alpha Significance level for interval-based metrics (default is 0.05).
#'
#' @return A named list of model assessment metrics:
#' \describe{
#'   \item{estimates}{Quantile estimates for empirical return periods.}
#'   \item{R2}{Coefficient of determination comparing estimates vs observed AMS.}
#'   \item{RMSE}{Root mean squared error of quantile estimates.}
#'   \item{Bias}{Mean bias of quantile estimates.}
#'   \item{AIC}{Akaike Information Criterion.}
#'   \item{BIC}{Bayesian Information Criterion.}
#'   \item{AW}{Average width of bootstrap confidence intervals.}
#'   \item{POC}{Percent of observations covered by the confidence intervals.}
#'   \item{CWI}{Confidence Width Index, penalizing over/under-coverage.}
#' }
#'
#' @details
#' Empirical return periods are calculated using the specified plotting position formula.
#' Quantile estimates are generated from the fitted model. Standard residual metrics (R²,
#' RMSE, Bias) are computed. The confidence interval width (AW), empirical coverage (POC),
#' and CWI are used to assess uncertainty calibration.
#'
#' @note
#' The confidence intervals are interpolated on the log-return scale. Coverage statistics
#' exclude missing widths due to extrapolation or poor overlap.
#'
#' @seealso \code{\link{sb.uncertainty}}, \code{\link[stats]{lm}}, \code{\link[stats]{approx}}
#'
#' @importFrom stats approx
#' @export

model.assessment <- function(
  ams,
  distribution,
  params,
  uncertainty,
  plotting_position = "Weibull",
  alpha = 0.05
) {

	n <- length(ams)                          
	x <- ams[order(ams, decreasing = TRUE)]  

	# Determine empirical exceedance probabilities using the plotting position
	p_empirical <- switch(
		plotting_position, 
		Weibull = (1:n) / (n + 1),
		Blom =  ((1:n) - 0.375) / (n + 0.25),
		Cunnane = ((1:n) - 0.4) / (n + 0.2),
		Gringorten = ((1:n) - 0.44) / (n + 0.12),
		Hazen = ((1:n) - 0.5) / n
	)

	t_return <- 1 / p_empirical               

	# Load information about the distribution
	distribution_list <- get.distributions()
	qfunc <- distribution_list[[distribution]]$quantile
	n_params <- distribution_list[[distribution]]$n_params

	# Compute the R2, RMSE, and Bias
	estimates <- qfunc(1 - p_empirical, params)
	R2 <- summary(lm(estimates ~ x))$r.squared
	RMSE <- sqrt(mean((estimates - x)^2))
	bias <- mean(estimates - x)

	# Compute the AIC and BIC
	AIC <- n * log(RMSE) + (2 * n_params)
	BIC <- n * log(RMSE) + (log(n) * n_params)

	# Interpolate confidence intervals at empirical return periods
	ci_lower <- approx(log(uncertainty$t), uncertainty$ci_lower, log(t_return))
	ci_upper <- approx(log(uncertainty$t), uncertainty$ci_upper, log(t_return))
	w <- ci_upper$y - ci_lower$y

	# Compute the AW, POC, and CWI
	AW <- mean(w, na.rm = TRUE)
	POC <- 100 * sum(x < ci_upper$y & x > ci_lower$y, na.rm = TRUE) / sum(!is.na(w))
	CWI <- AW * exp((1 - alpha) - (POC / 100))^2

	# Return assessment results in a list
	list(
		estimates = estimates,
		R2 = R2,
		RMSE = RMSE,
		bias = bias,
		AIC = AIC,	
		BIC = BIC,
		AW = AW,
		POC = POC,
		CWI = CWI
	)

}
