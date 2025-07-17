#' Parametric Bootstrap Confidence Intervals for Flood Quantile Estimates
#'
#' @description
#' Computes estimates and confidence intervals for return levels at return periods
#' 2, 5, 10, 20, 50, and 100 years using the parametric bootstrap method. This function 
#' supports a variety of probability models and parameter estimation methods.
#'
#' @inheritParams param-data
#' @inheritParams param-model
#' @inheritParams param-method
#' @inheritParams param-prior
#' @inheritParams param-years
#' @inheritParams param-trend
#' @inheritParams param-slices
#' @inheritParams param-alpha
#' @inheritParams param-samples
#' @inheritParams param-periods
#'
#' @return A list of lists containing the return levels and confidence 
#' intervals for each slice. Each sub-list contains: 
#' - `method`: "Bootstrap"
#' - `estimates`: Estimated quantiles for each return period.
#' - `ci_lower`: Lower bound of the confidence interval for each return period.
#' - `ci_upper`: Upper bound of the confidence interval for each return period.
#' - `t`: Vector of return periods; `c(2, 5, 10, 20, 50, 100)`.
#' - `slice`: The value of `slice` argument.
#' - `trend`: The value of `trend` argument.
#'
#' @details
#' The bootstrap procedure samples from the fitted distribution via inverse 
#' transform sampling. For each bootstrapped sample, the parameters are re-estimated 
#' using the specified `method`. Then, the bootstrapped parameters are used to compute 
#' a new set of bootstrapped quantiles. Confidence intervals are obtained from the 
#' empirical nonexceedance probabilities of the bootstrapped quantiles.
#'
#' @note
#' The parametric bootstrap is known to give unreasonably wide confidence intervals 
#' for small datasets. If this function detects a confidence interval that is 5+ times 
#' wider than the return levels themselves, it will return an error and recommend 
#' RFPL uncertainty quantification (with `uncertainty_rfpl`).
#'
#' @seealso \link{fit_lmom_fast}, \link{fit_maximum_likelihood}, \link{lmom_sample},
#'   \link{quantile_fast}, \link{plot_sffa}, \link{plot_nsffa}
#'
#' @examples
#' data <- rnorm(n = 100, mean = 100, sd = 10)
#' years <- seq(from = 1901, to = 2000)
#' uncertainty_bootstrap(data, "WEI", "L-moments")
#'
#' @importFrom stats runif
#' @export

uncertainty_bootstrap <- function(
    data,
    model,
    method,
    prior = NULL,
    years = NULL,
    trend = NULL,
    slices = 1900,
    alpha = 0.05,
    samples = 10000L,
	periods = c(2, 5, 10, 20, 50, 100)
) {

	data <- validate_numeric("data", data, FALSE)
	model <- validate_enum("model", model)
	method <- validate_enum("method", method)
	prior <- validate_numeric("prior", prior, size = 2, bounds = c(0, Inf))
	years <- validate_numeric("years", years, size = length(data))
	trend <- validate_trend(trend)
	slices <- validate_numeric("slices", slices, FALSE)
	alpha <- validate_float("alpha", alpha, bounds = c(0.01, 0.1))
	samples <- validate_integer("samples", samples, bounds = c(1, Inf))
	periods <- validate_numeric("periods", periods, FALSE, bounds = c(1, Inf))

	# Return a list of lists
	lapply(slices, function(slice) {
		uncertainty_bootstrap_helper(
			data,
			model,
			method,
			prior,
			years,
			trend,
			slice,
			alpha,
			samples,
			periods
		)
	})

}

uncertainty_bootstrap_helper <- function(
    data,
    model,
    method,
    prior,
    years,
    trend,
    slice,
    alpha,
    samples,
	periods
) {

	# Set return periods and their quantiles
    probabilities <- 1 - (1 / periods)
    n <- length(data)

	# Define the parameter estimation function
	fit <- function(data, years) {
		if (method == "L-moments") {
			return (fit_lmom_fast(data, model)$params)
		} else {
			return (fit_maximum_likelihood(data, model, prior, years, trend)$params)
		}
	}

	# Get the estimated quantiles
	params <- fit(data, years)
	estimates <- quantile_fast(probabilities, model, params, slice, trend)

	# Generate the bootstrapped quantiles 
	quantiles <- sapply(1:n, function(i) {
		u <- runif(samples)
		quantile_fast(u, model, params, slice, trend)
	})

	# Vectorized bootstrap function 
	bootstrap <- sapply(1:samples, function(i) {
		bootstrap_params <- fit(quantiles[i, ], years)
		quantile_fast(probabilities, model, bootstrap_params, slice, trend)
	})

	# Compute confidence intervals
	probs <- c(alpha / 2, 1 - (alpha / 2))
	ci <- apply(bootstrap, 1, quantile, probs = probs)

	# Check for unreasonably large confidence intervals (5x the estimates)
	if (any(ci[2, ] - ci[1, ] >= 5 * estimates)) {
		stop("Bootstrap uncertainty quantification failed to converge. Try RFPL instead.")
	}

	# Generate the results as a list
	list(
		method = "Bootstrap",
		periods = periods,
		ci_lower = ci[1, ],
		estimates = estimates,
		ci_upper = ci[2, ],
		slice = slice,
		trend = trend
	)

}
