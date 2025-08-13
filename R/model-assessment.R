#' Model Assessment 
#'
#' @description
#' Computes various metrics for assessing the quality of a fitted flood frequency model. 
#' Metrics include accuracy (residual statistics), fitting efficiency (information 
#' criteria), and uncertainty (coverage based metrics using confidence intervals).
#'
#' **For NS-FFA**: The metrics are computed from the normalized empirical/theoretical
#' quantiles, which are determined based on the selected distribution family. Therefore, 
#' metrics from stationary and nonstationary models should not be compared.
#'
#' @inheritParams param-data
#' @inheritParams param-distribution
#' @inheritParams param-method
#' @inheritParams param-prior
#' @inheritParams param-ns-years
#' @inheritParams param-ns-structure
#' @inheritParams param-alpha
#'
#' @param pp_formula Character string specifying the plotting position formula. 
#' Must `"Weibull"` (default), `"Blom"`, `"Cunnane"`, `"Gringorten"`, or `"Hazen"`.
#'
#' @param ci **For S-FFA only**. Dataframe containing return periods (in the column  
#' `periods`) and confidence intervals (in the columns `lower` and `upper`). Dataframes 
#' in this format can be generated with [uncertainty_bootstrap()], [uncertainty_rfpl()], 
#' or [uncertainty_rfgpl()].  
#'
#' @return List containing the results of model assessment:
#' - `data`: The `data` argument.
#' - `q_theoretical`: The theoretical return level estimates based on the plotting 
#'   positions. Normalized for nonstationary models.
#' - `q_empirical`: The empirical return levels. Normalized for nonstationary models. 
#' - `metrics`: A list of model assessment metrics (see details).
#' 
#' @details
#' These metrics are are computed for all models:
#'
#' - `AIC_MLL`: Akaike Information Criterion, computed using the maximum log-likelihood.
#' - `BIC_MLL`: Bayesian Information Criterion, computed using the maximum log-likelihood.
#' - `R2`: Coefficient of determination from linear regression of estimates vs. data.
#' - `RMSE`: Root mean squared error of quantile estimates.
#' - `bias`: Mean bias of quantile estimates.
#' - `AIC_RMSE`: Akaike Information Criterion, computed using the RMSE.
#' - `BIC_RMSE`: Bayesian Information Criterion, computed using the RMSE.
#'
#' These metrics are only computed  *if the `ci` argument is provided*:
#'
#' - `AW`: Average width of the confidence interval(s).
#' - `POC`: Percent of observations covered by the confidence interval(s).
#' - `CWI`: Confidence width index, a metric that combines `AW` and `POC`.
#'
#' @seealso [uncertainty_bootstrap()], [uncertainty_rfpl()], [uncertainty_rfgpl()],
#' [plot_sffa_assessment()], [plot_nsffa_assessment()]
#'
#' @examples 
#' # Initialize example data and params
#' data <- rnorm(n = 100, mean = 100, sd = 10)
#' params <- c(100, 10)
#'
#' # Perform uncertainty analysis
#' ci <- uncertainty_bootstrap(data, "NOR", "L-moments")$ci
#'
#' # Run model assessment
#' model_assessment(data, "NOR", "L-moments", ci = ci)
#'
#' @importFrom stats approx
#' @export

model_assessment <- function(
  data,
  distribution,
  method,
  prior = NULL,
  ns_years = NULL,
  ns_structure = NULL,
  alpha = 0.05,
  pp_formula = "Weibull",
  ci = NULL
) {

	data <- validate_numeric("data", data)
	distribution <- validate_enum("distribution", distribution)
	method <- validate_enum("method", method )
	prior <- validate_numeric("prior", prior, TRUE, c(0, Inf), 2)
	years <- validate_numeric("ns_years", ns_years, optional = TRUE, size = length(data))
	structure <- validate_structure(ns_structure)
	alpha <- validate_float("alpha", alpha, bounds = c(0.01, 0.1))
	pp_formula <- validate_enum("pp_formula", pp_formula)

	# Estimate the parameters
	params <- if (method == "L-moments") {
		fit_lmoments_fast(data, distribution)$params
	} else {
		fit_maximum_likelihood(data, distribution, prior, years, structure)$params
	}

	# Initialize a list to store the metrics 
	metrics <- list()

	# Compute the AIC and BIC using the MLL (S-FFA and NS-FFA)
	MLL = fit_mle(
		data,
		distribution,
		ns_years = years,
		ns_structure = structure
	)$mll

	n <- length(data)                          
	info <- model_info(distribution, structure)
	metrics$AIC_MLL = (2 * info$n_params) - (2 * MLL)
	metrics$BIC_MLL = (info$n_params * log(n)) - (2 * MLL)

	# Determine exceedance probabilities using the plotting position formula
	p_exceedance <- switch(
		pp_formula, 
		Weibull = (1:n) / (n + 1),
		Blom =  ((1:n) - 0.375) / (n + 0.25),
		Cunnane = ((1:n) - 0.4) / (n + 0.2),
		Gringorten = ((1:n) - 0.44) / (n + 0.12),
		Hazen = ((1:n) - 0.5) / n,
		stop("Unknown plotting position formula.")
	)

	# Get the empirical and theoretical quantiles
	if (!structure$location && !structure$scale) {

		# The theoretical are computed at the plotting positions
		q_theoretical <- quantiles_fast(
			1 - p_exceedance,
			distribution,
			params,
			0,
			structure
		)

		# The empirical quantiles for S-FFA are just the sorted data
		q_empirical <- data[order(data, decreasing = TRUE)]

	} else {

		# Get the normalized theoretical quantiles from the plotting positions
		q_theoretical <- qnorm(1 - p_exceedance)

		# Get the normalized empirical quantiles
		p_empirical <- sapply(seq_along(data), function(i) {
			utils_cdf(data[[i]], distribution, params, years[[i]], structure)
		})

		q_empirical <- sort(qnorm(p_empirical), decreasing = TRUE)
		
	}

	# Run linear regression against the empirical quantiles
	metrics$R2 <- summary(lm(q_theoretical ~ q_empirical))$r.squared
	metrics$RMSE <- sqrt(mean((q_theoretical - q_empirical)^2))
	metrics$bias <- mean(q_theoretical - q_empirical)

	# Compute the AIC and BIC
	metrics$AIC_RMSE <- n * log(metrics$RMSE) + (2 * info$n_params)
	metrics$BIC_RMSE <- n * log(metrics$RMSE) + (log(n) * info$n_params)

	# Compute uncertainty metrics if 'ci' argument is provided (S-FFA only)
	if (!is.null(ci)) {

		# Filter returns and x to indices where returns is between 2 and 100
		returns <- 1 / p_exceedance
		idx <- which(returns > 2 & returns < 100)
		returns <- returns[idx]
		q_filtered <- q_empirical[idx]

		# Use log-linear interpolation to get CIs for each return period
		ci_lower <- approx(log(ci$periods), ci$lower, log(returns))
		ci_upper <- approx(log(ci$periods), ci$upper, log(returns))

		# Compute the width and coverage of the confidence intervals
		widths <- ci_upper$y - ci_lower$y
		covers <- sum(q_filtered < ci_upper$y & q_filtered > ci_lower$y)

		# Compute the AW, POC, and CWI
		metrics$AW <- mean(widths)
		metrics$POC <- 100 * covers / length(widths)
		metrics$CWI <- metrics$AW * exp((1 - alpha) - (metrics$POC / 100))^2

	}

	# Return assessment results in a list
	list(
		data = data,
		q_theoretical = q_theoretical,
		q_empirical = q_empirical,
		metrics = metrics
	)

}
