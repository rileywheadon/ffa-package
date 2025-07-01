#' Sample Bootstrap Confidence Intervals for Flood Quantile Estimates
#'
#' @description
#' Computes estimates and confidence intervals for return levels at standard return periods
#' (2, 5, 10, 20, 50, and 100 years) using the sample bootstrap method. This function 
#' supports a variety of probability models and parameter estimation methods.
#'
#' @param data Numeric; a vector of annual maximum streamflow data.
#'
#' @param years Numeric; a vector of years with the same length as `data`.
#'
#' @param model Character (1); three character distribution code. Must be one of: 
#'   `"GUM"`, `"NOR"`, `"LNO"`, `"GEV"`, `"GLO"`, `"GNO"`, `"PE3"`, `"LP3"`, or `"WEI"`.
#'
#' @param trend List; information about non-stationary trend(s) to use:
#' - `location` Logical (1); if TRUE, there is a trend in the location parameter.
#' - `scale` Logical (1); if TRUE, there is a trend in the scale parameter.
#'
#' @param method Character (1); string specifying the estimation method. 
#'   Must be `"L-moments"`, `"MLE"`, or `"GMLE"`.
#' 
#' @param slices Character (1) or Numeric; specifies the years at which
#'   to compute the estimates and confidence intervals (default is "last").
#'   - `"all"`: returns estimates for all values in `years`.
#'   - `"first"`: returns estimates for first year in the dataset.
#'   - `"last"`: returns estimates for last year in the dataset.
#'   - Passing a numeric vector to `slices` allows for custom values.
#'
#'   If the `trend$location` and `trend$scale` are FALSE, the results will 
#'   be the same for all slices.
#'
#' @param samples Integer (1); the number of bootstrap samples (default is 10000).
#'
#' @param alpha Numeric (1); the significance level (default is 0.05).
#'
#' @param prior Numeric (2); optional vector of parameters \eqn{(p, q)} that specifies 
#'  the parameters of a Beta prior on \eqn{\kappa}. Only works with distribution `"GEV"`.
#'
#' @return List; quantiles and confidence intervals. Each year maps to a sub-list:
#' - `estimates`: Estimated quantiles for each return period.
#' - `ci_lower`: Lower bound of the confidence interval for each return period.
#' - `ci_upper`: Upper bound of the confidence interval for each return period.
#' - `t`: Vector of return periods; `c(2, 5, 10, 20, 50, 100)`.
#'
#' @details
#' The bootstrap procedure samples from the fitted distribution via inverse 
#' transform sampling. For each bootstrapped sample, the parameters are re-estimated 
#' using the specified `method`. Then, the bootstrapped parameters are used to compute 
#' a new set of bootstrapped quantiles. Confidence intervals are obtained from the 
#' empirical non-exceedance probabilities of the bootstrapped quantiles.
#'
#' @seealso \link{fit_lmom_fast}, \link{fit_maximum_likelihood}, \link{lmom.sample}, \link[stats]{quantile}
#'
#' @examples
#' data <- rnorm(n = 100, mean = 100, sd = 10)
#' years <- seq(from = 1901, to = 2000)
#' sb.uncertainty(data, years, "WEI", "L-moments")
#'
#' @importFrom stats runif
#' @export

sb.uncertainty <- function(
  data,
  years,
  model,
  trend,
  method,
  slices = "last",
  samples = 10000,
  alpha = 0.05,
  prior = NULL
) {

	# Check if the method is invalid
	if (!(method %in% c("L-moments", "MLE", "GMLE"))) {
		stop("Unsupported method: ", method)
	}

	# Set return periods and their quantiles
    t <- c(2, 5, 10, 20, 50, 100)
    returns <- 1 - (1 / t)
    n <- length(data)

	# Determine the slices based on the "years" argument
	if (is.character(slices)) {
		slices <- switch(
			slices,
			"all" = years,
			"first" = min(years),
			"last" = max(years)
		)
	}

	# Get the estimation function
	if (method == "L-moments") {
		efunc <- function(data, years) {
			fit_lmom_fast(model, data)
		}
	} else if (method == "MLE") {
		efunc <- function(data, years) {
			fit_maximum_likelihood(data, years, model, trend)$params
		}
	} else if (method == "GMLE") {
		efunc <- function(data, years) {
			fit_maximum_likelihood(data, years, model, trend, prior)$params
		}
	}	

	# Get the estimated quantiles
	params <- efunc(data, years)
	estimates <- qntxxx(model, trend, returns, params, slices)

	# Generate the bootstrapped quantiles in parallel, one year at a time
	quantiles <- sapply(1:n, function(i) {
		u <- runif(samples)
		qntxxx(model, trend, u, params, years[i])
	})

	# Some distributions generate negative values. Adjust these to epsilon.
	quantiles[quantiles < 0] = 1e-8

	# Vectorized, parallel bootstrap function 
	bootstrap_list <- lapply(1:samples, function(i) {
		bootstrap_params <- efunc(quantiles[i, ], years)
		qntxxx(model, trend, returns, bootstrap_params, slices)
	})

	# Create a 3D array with dimensions (1: slices), (2: periods), (3: samples)
	bootstrap_array <- array(unlist(bootstrap_list), dim = c(length(slices), 6, samples))

	# Compute confidence intervals
	probs <- c(alpha / 2, 1 - (alpha / 2))
	ci <- apply(bootstrap_array, c(2, 1), quantile, probs = probs)

	# Generate the results as a list
	results <- lapply(1:length(slices), function(k) {
		ye <- if (length(slices) == 1) estimates else estimates[k, ]
		list(t = t, ci_lower = ci[1, , k], estimates = ye, ci_upper = ci[2, , k])
	})

	names(results) <- slices
	return(results)

}
