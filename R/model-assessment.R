#' Model Assessment 
#'
#' @description
#' Computes various metrics for assessing the quality of a fitted flood frequency model. 
#' Metrics include accuracy (residual statistics), fitting efficiency (information criteria), 
#' and uncertainty (coverage based metrics using confidence intervals).
#'
#' @inheritParams param-data
#' @inheritParams param-distribution
#' @inheritParams param-params
#' @inheritParams param-ns-years
#' @inheritParams param-ns-structure
#' @inheritParams param-alpha
#'
#' @param pp_formula Character string specifying the plotting position formula. 
#' Must `"Weibull"` (default), `"Blom"`, `"Cunnane"`, `"Gringorten"`, or `"Hazen"`.
#'
#' @param ci Dataframe containing return periods (in the column `periods`) and confidence 
#' intervals (in the columns `ci_lower` and `ci_upper`). Dataframes in this format can be
#' generated with [uncertainty_bootstrap()], [uncertainty_rfpl()], or [uncertainty_rfgpl()].
#'
#' @return List containing the results of model assessment:
#' - `data`: The `data` argument.
#' - `estimates`: Return level estimates based on plotting positions (S-FFA only).
#' - `metrics`: A list of model assessment metrics (see details).
#' 
#' @details
#' These metrics are are computed for both *stationary* and *nonstationary* models:
#'
#' - `AIC_MLL`: Akaike Information Criterion, computed using the maximum log-likelihood.
#' - `BIC_MLL`: Bayesian Information Criterion, computed using the maximum log-likelihood.
#'
#' These metrics are *always* computed for *stationary models*:
#'
#' - `R2`: Coefficient of determination from linear regression of estimates vs. data.
#' - `RMSE`: Root mean squared error of quantile estimates.
#' - `bias`: Mean bias of quantile estimates.
#' - `AIC_RMSE`: Akaike Information Criterion, computed using the RMSE.
#' - `BIC_RMSE`: Bayesian Information Criterion, computed using the RMSE.
#'
#' These metrics are computed for stationary models *if the `ci` argument is provided*:
#'
#' - `AW`: Average width of the confidence interval(s).
#' - `POC`: Percent of observations covered by the confidence interval(s).
#' - `CWI`: Confidence width index, a metric that combines `AW` and `POC`.
#'
#' @seealso [uncertainty_bootstrap()], [uncertainty_rfpl()], [uncertainty_rfgpl()] 
#'
#' @examples 
#' # Initialize example data and params
#' data <- rnorm(n = 100, mean = 100, sd = 10)
#' params <- c(100, 10)
#'
#' # Perform uncertainty analysis
#' ci <- uncertainty_bootstrap(data, "NOR", "L-moments")$results
#'
#' # Evaluate model diagnostics
#' model_assessment(data, "NOR", params, ci = ci)
#'
#' @importFrom stats approx
#' @export

model_assessment <- function(
  data,
  distribution,
  params,
  ns_years = NULL,
  ns_structure = NULL,
  alpha = 0.05,
  pp_formula = "Weibull",
  ci = NULL
) {

	data <- validate_numeric("data", data)
	distribution <- validate_enum("distribution", distribution)
	params <- validate_params(distribution, params, ns_structure)

	years <- validate_numeric("ns_years", ns_years, optional = TRUE, size = length(data))
	structure <- validate_structure(ns_structure)
	pp_formula <- validate_enum("pp_formula", pp_formula)

	# Get the number of data points 
	n <- length(data)                          

	# Get information about the distribution
	info <- model_info(distribution, structure)

	# Initialize a list to store the metrics 
	estimates <- NULL
	metrics <- list()

	# Compute the AIC and BIC using the MLL (S-FFA and NS-FFA)
	MLL = fit_mle(
		data,
		distribution,
		ns_years = ns_years,
		ns_structure = ns_structure
	)$mll

	metrics$AIC_MLL = (2 * info$n_params) - (2 * MLL)
	metrics$BIC_MLL = (info$n_params * log(n)) - (2 * MLL)

	# Compare with the method of plotting positions (S-FFA only)
	if (!structure$location && !structure$scale) {

		# Sort the data vector
		data_sorted <- data[order(data, decreasing = TRUE)]

		# Determine empirical exceedance probabilities using the plotting position 
		p_empirical <- switch(
			pp_formula, 
			Weibull = (1:n) / (n + 1),
			Blom =  ((1:n) - 0.375) / (n + 0.25),
			Cunnane = ((1:n) - 0.4) / (n + 0.2),
			Gringorten = ((1:n) - 0.44) / (n + 0.12),
			Hazen = ((1:n) - 0.5) / n,
			stop("Unknown plotting position formula.")
		)

		returns <- 1 / p_empirical               
		estimates <- quantiles_fast(1 - p_empirical, distribution, params, 0, structure)

		# Run linear regression against the sorted data
		metrics$R2 <- summary(lm(estimates ~ data_sorted))$r.squared
		metrics$RMSE <- sqrt(mean((estimates - data_sorted)^2))
		metrics$bias <- mean(estimates - data_sorted)

		# Compute the AIC and BIC
		metrics$AIC_RMSE <- n * log(metrics$RMSE) + (2 * info$n_params)
		metrics$BIC_RMSE <- n * log(metrics$RMSE) + (log(n) * info$n_params)

		# Compute uncertainty metrics if 'ci' argument is provided
		if (!is.null(ci)) {

			# Filter returns and x to indices where returns is between 2 and 100
			idx <- which(returns > 2 & returns < 100)
			returns <- returns[idx]
			data_sorted <- data_sorted[idx]

			# Use log-linear interpolation to get confidence intervals for each return period
			ci_lower <- approx(log(ci$periods), ci$ci_lower, log(returns))
			ci_upper <- approx(log(ci$periods), ci$ci_upper, log(returns))

			# Compute the width and coverage of the confidence intervals
			widths <- ci_upper$y - ci_lower$y
			covers <- sum(data_sorted < ci_upper$y & data_sorted > ci_lower$y)

			# Compute the AW, POC, and CWI
			metrics$AW <- mean(widths)
			metrics$POC <- 100 * covers / length(widths)
			metrics$CWI <- metrics$AW * exp((1 - alpha) - (metrics$POC / 100))^2

		}

	} 

	# Return assessment results in a list
	list(data = data, estimates = estimates, metrics = metrics)

}
