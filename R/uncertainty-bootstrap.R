#' Sample Bootstrap Confidence Intervals for Flood Quantile Estimates
#'
#' @description
#' Computes estimates and confidence intervals for return levels at return periods
#' 2, 5, 10, 20, 50, and 100 years using the sample bootstrap method. This function 
#' supports a variety of probability models and parameter estimation methods.
#'
#' @inheritParams param-data
#' @inheritParams param-model
#' @inheritParams param-method
#' @inheritParams param-prior
#' @inheritParams param-years
#' @inheritParams param-trend
#' @inheritParams param-slice
#' @inheritParams param-alpha
#' @inheritParams param-samples
#'
#' @return A list contianing the return levels and confidence intervals containing: 
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
#' empirical non-exceedance probabilities of the bootstrapped quantiles.
#'
#' @seealso \link{fit_lmom_fast}, \link{fit_maximum_likelihood}, \link{lmom_sample},
#'   \link{quantile_fast}, \link{plot_uncertainty}
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
    slice = 1900,
    alpha = 0.05,
    samples = 10000L
) {

	data <- validate_data(data)
	model <- validate_model(model)
	method <- validate_method(method)
	prior <- validate_prior(prior)
	years <- validate_years(years, data)
	trend <- validate_trend(trend)
	slice <- validate_slice(slice)
	alpha <- validate_alpha(alpha)
	samples <- validate_samples(samples)

	# Set return periods and their quantiles
	t = c(2, 5, 10, 20, 50, 100)
    returns <- 1 - (1 / t)
    n <- length(data)

	# Define the parameter estimation function
	fit <- function(data, years) {
		if (method == "L-moments") {
			return (fit_lmom_fast(data, model))
		} else {
			return (fit_maximum_likelihood(data, model, prior, years, trend)$params)
		}
	}

	# Get the estimated quantiles
	params <- fit(data, years)
	estimates <- quantile_fast(returns, model, params, slice, trend)

	# Generate the bootstrapped quantiles 
	quantiles <- sapply(1:n, function(i) {
		u <- runif(samples)
		quantile_fast(u, model, params, slice, trend)
	})

	# Some distributions generate negative values. Adjust these to epsilon.
	quantiles[quantiles < 0] = 1e-8

	# Vectorized bootstrap function 
	bootstrap <- sapply(1:samples, function(i) {
		bootstrap_params <- fit(quantiles[i, ], years)
		quantile_fast(returns, model, bootstrap_params, slice, trend)
	})

	# Compute confidence intervals
	probs <- c(alpha / 2, 1 - (alpha / 2))
	ci <- apply(bootstrap, 1, quantile, probs = probs)

	# Generate the results as a list
	list(
		t = t,
		ci_lower = ci[1, ],
		estimates = estimates,
		ci_upper = ci[2, ],
		slice = slice,
		trend = trend
	)

}
