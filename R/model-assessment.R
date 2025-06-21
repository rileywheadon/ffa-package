#' Evaluate Goodness-of-Fit and Uncertainty Coverage for Fitted Flood Models
#'
#' Computes multiple performance metrics and diagnostic indicators to assess the quality of
#' a fitted flood frequency model This includes residual statistics, information criteria,
#' and coverage-based metrics using bootstrapped confidence intervals.
#'
#' @param data Numeric vector of annual maximum streamflow values (no missing values).
#' @param model The 3, 5, or 6 letter code for a probability model
#' @param params Numeric vector of fitted model parameters.
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
#' Quantile estimates are generated from the fitted model. Standard residual metrics (\eqn{R^2},
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
  data,
  years,
  model,
  params,
  uncertainty,
  plotting_position = "Weibull",
  alpha = 0.05
) {

	# Get the name and signature for the model 
	name <- substr(model, 1, 3)
	signature <- if(nchar(model) == 3) NULL else substr(model, 4, 5)

	# Sort the data vector
	n <- length(data)                          
	data_sorted <- data[order(data, decreasing = TRUE)]  

	# Sort the years on the decomposed data (this gets the "true" plotting positions)
	data_decomposed <- ams.decomposition(data, years, signature)
	years_sorted <- years[order(data_decomposed, decreasing = TRUE)]

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

	# Compute the estimates, using the years as a covariate if necessary
	estimates <- sapply(1:n, function(i) {
		qntxxx(model, 1 - p_empirical[i], params, years_sorted[i])
	})

	# Run linear regression (stationary models only)
	R2 <- summary(lm(estimates ~ data_sorted))$r.squared
	RMSE <- sqrt(mean((estimates - data_sorted)^2))
	bias <- mean(estimates - data_sorted)

	# Compute the AIC and BIC
	info <- models.info(model)
	AIC <- n * log(RMSE) + (2 * info$n.params)
	BIC <- n * log(RMSE) + (log(n) * info$n.params)

	# Compute the MLL AIC and BIC
	MLL = mle.estimation(data, model, years)$mll
	AIC_MLL = (2 * info$n.params) - (2 * MLL)
	BIC_MLL = (info$n.params * log(n)) - (2 * MLL)

	# Filter t_return and x to indices where t_return is between 2 and 100
	idx <- (t_return > 2 & t_return < 100)
	t_return <- t_return[idx]
	data_sorted <- data_sorted[idx]
		
	# Define w, the width of the confidence interval at each year
	w <- numeric(length(idx))

	# Define covers, the count of data points within the confidence interval
	covers <- 0

	# Interpolate confidence intervals at empirical return periods
	if (nchar(model == 3)) {

		# Use any of the confidence intervals for a stationary model
		uncertainty <- uncertainty[[1]]
		ci_lower <- approx(log(uncertainty$t), uncertainty$ci_lower, log(t_return))
		ci_upper <- approx(log(uncertainty$t), uncertainty$ci_upper, log(t_return))
		w <- ci_upper$y - ci_lower$y
		covers <- sum(data_sorted < ci_upper$y & data_sorted > ci_lower$y)

	} else {

		# Iterate through the confidence intervals for the non-stationary models 
		for (i in 1:length(w)) {

			uncertainty <- uncertainty[[i]]
			ci_lower <- approx(log(uncertainty$t), uncertainty$ci_lower, log(t_return[i]))
			ci_upper <- approx(log(uncertainty$t), uncertainty$ci_upper, log(t_return[i]))
			w[i] <- ci_upper$y - ci_lower$y

			# Add 1 to covers if data point i is within its confidence interval
			if (data_sorted[i] < ci_upper$y & data_sorted[i] > ci_lower$y) {
				covers <- covers + 1
			}
		}
	}

	# Compute the AW, POC, and CWI
	AW <- mean(w)
	POC <- 100 * covers / length(w)
	CWI <- AW * exp((1 - alpha) - (POC / 100))^2

	# Return assessment results in a list
	list(
		estimates = estimates,
		R2 = R2,
		RMSE = RMSE,
		bias = bias,
		AIC = AIC,	
		BIC = BIC,
		AIC_MLL = AIC_MLL,
		BIC_MLL = BIC_MLL,
		AW = AW,
		POC = POC,
		CWI = CWI
	)

}
