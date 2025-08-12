#' Parametric Bootstrap Uncertainty Quantification
#'
#' @description
#' Computes return level estimates and confidence intervals at the specified return 
#' periods (defaults to 2, 5, 10, 20, 50, and 100 years) using the parametric bootstrap.
#' This function supports many probability models and parameter estimation methods.
#'
#' **For NS-FFA**: To perform uncertainty quantification for a nonstationary model, 
#' include the observation years (`ns_years`), the nonstationary model structure 
#' (`ns_structure`), and a list of years at which to compute the return level estimates
#' and confidence intervals (`ns_slices`).
#'
#' @inheritParams param-data
#' @inheritParams param-distribution
#' @inheritParams param-method
#' @inheritParams param-prior
#' @inheritParams param-ns-years
#' @inheritParams param-ns-structure
#' @inheritParams param-ns-slices
#' @inheritParams param-alpha
#' @inheritParams param-samples
#' @inheritParams param-periods
#'
#' @return A list containing the following six items:
#' - `method`: "Bootstrap"
#' - `distribution`: The `distribution` argument.
#' - `params`: The fitted parameters.
#' - `ns_structure`: The `ns_structure` argument, if given.
#' - `ns_slices`: The `ns_slices` argument, if given.
#' - `ci`: A dataframe containing confidence intervals (S-FFA only)
#' - `ci_list`: A list of dataframes containing confidence intervals (NS-FFA only).
#'
#' The dataframe(s) in `ci` and `ci_list` have four columns:
#' - `estimates`: Estimated quantiles for each return period.
#' - `lower`: Lower bound of the confidence interval for each return period.
#' - `upper`: Upper bound of the confidence interval for each return period.
#' - `periods`: The `periods` argument. 
#'
#' @details
#' Bootstrap samples are obtained from the fitted distribution via inverse transform 
#' sampling. For each bootstrapped sample, the parameters are re-estimated based on the 
#' `method` argument. Then, the bootstrapped parameters are used to compute a new set of 
#' bootstrapped quantiles. Confidence intervals are obtained from the empirical 
#' nonexceedance probabilities of the bootstrapped quantiles.
#'
#' @note
#' The parametric bootstrap is known to give unreasonably wide confidence intervals 
#' for small datasets. If this method yields a confidence interval that is at least 
#' 5 times greater than the magnitude of the return levels, it will return an error 
#' and recommend [uncertainty_rfpl()] or [uncertainty_rfgpl()] as alternatives.
#'
#' @references
#' Vidrio-Sahagún, C.T., He, J. Enhanced profile likelihood method for the nonstationary 
#' hydrological frequency analysis, Advances in Water Resources 161, 10451 (2022). 
#' \doi{10.1016/j.advwatres.2022.104151}
#' 
#' @seealso [fit_lmoments()], [fit_mle()], [fit_gmle()], [utils_sample_lmoments()]
#' [utils_quantiles()], [plot_sffa_estimates()], [plot_nsffa_estimates()]
#'
#' @examples
#' data <- rnorm(n = 100, mean = 100, sd = 10)
#' uncertainty_bootstrap(data, "WEI", "L-moments")
#'
#' @importFrom stats runif
#' @export

uncertainty_bootstrap <- function(
    data,
    distribution,
    method,
    prior = NULL,
    ns_years = NULL,
    ns_structure = NULL,
    ns_slices = NULL,
    alpha = 0.05,
    samples = 10000L,
	periods = c(2, 5, 10, 20, 50, 100)
) {

	data <- validate_numeric("data", data)
	distribution <- validate_enum("distribution", distribution)
	method <- validate_enum("method", method)
	prior <- validate_numeric("prior", prior, TRUE, c(0, Inf), 2)
	years <- validate_numeric("ns_years", ns_years, TRUE, size = length(data))
	structure <- validate_structure(ns_structure)
	slices <- validate_numeric("ns_slices", ns_slices, TRUE)
	alpha <- validate_float("alpha", alpha, bounds = c(0.01, 0.1))
	samples <- validate_integer("samples", samples, bounds = c(1, Inf))
	periods <- validate_numeric("periods", periods, FALSE, bounds = c(1, Inf))

	# Set return periods and their quantiles
	p <- 1 - (1 / periods)
	n <- length(data)

	# Estimate the parameters
	fit <- function(data, years) {
		if (method == "L-moments") {
			fit_lmoments_fast(data, distribution)$params
		} else {
			fit_maximum_likelihood(data, distribution, prior, years, structure)$params
		}
	}

	params <- fit(data, years)

	# Helper function for running the bootstrap
	bootstrap_helper <- function(slice) {

		# Compute return level estimates for the slice
		estimates <- quantiles_fast(p, distribution, params, slice, structure)

		# Generate the bootstrapped quantiles 
		quantiles <- sapply(1:n, function(i) {
			u <- runif(samples)
			quantiles_fast(u, distribution, params, slice, structure)
		})

		# Vectorized bootstrap function 
		bootstrap <- sapply(1:samples, function(i) {
			bootstrap_params <- fit(quantiles[i, ], years)
			quantiles_fast(p, distribution, bootstrap_params, slice, structure)
		})

		# Compute confidence intervals
		probs <- c(alpha / 2, 1 - (alpha / 2))
		ci <- apply(bootstrap, 1, quantile, probs = probs)

		# Check for unreasonably large confidence intervals (5x the estimates)
		if (any(ci[2, ] - ci[1, ] >= 5 * estimates)) {
			stop("Bootstrap uncertainty quantification failed to converge. Try RFPL instead.")
		}

		# Generate the results as a data frame
		data.frame(
			periods = periods,
			estimates = estimates,
			lower = ci[1, ],
			upper = ci[2, ]
		)

	}

	# Initialize results list
	results <- list(
		method = "Bootstrap",
		distribution = distribution,
		params = params,
		ns_structure = ns_structure,
		ns_slices = ns_slices
	)

	# Add a list of dataframes (NS-FFA) or a single dataframe (S-FFA)
	if (structure$location || structure$scale) {
		ci_list <- lapply(slices, bootstrap_helper)		
		names(ci_list) <- slices
		results$ci_list <- ci_list
	} else {
		results$ci <- bootstrap_helper(0)
	}

	# Return results list
	results
}
