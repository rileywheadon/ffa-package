#' Sample Bootstrap Confidence Intervals for Flood Quantile Estimates
#'
#' Computes confidence intervals for flood quantile estimates using the nonparametric
#' sample bootstrap method, based on L-moment parameter estimation. This function supports
#' uncertainty quantification for return period estimates derived from a fitted distribution.
#'
#' @param df Dataframe with columns "max", a vector of annual maxima observations,
#'   and "year", a vector of years corresponding to the observations in "max". 
#'
#' @param model Character string specifying the distribution code. The first three letters 
#'   denote the family: 'GUM', 'NOR', 'LNO', 'GEV', 'GLO', 'GNO', 'PE3', 'LP3', or 'WEI'. 
#'   A trailing signature of '10' or '100' indicates a linear trend in location; '11' or 
#'   '110' indicates linear trends in both location and scale.
#'
#' @param method Character string specifying the estimation method. 
#'   Currently supports \code{"L-moments"}, \code{"MLE"}, and \code{"GMLE"}.
#' 
#' @param years Character string or numeric vector specifying the years at which
#'   to compute the estimates and confidence intervals. Defaults to "last".
#'   \itemize{
#'     \item{`"all"` returns estimates for all years in the dataset.}
#'     \item{`"first"` returns estimates for first year in the dataset.}
#'     \item{`"last"` returns estimates for last year in the dataset.}
#'     \item{Passing a numeric vector to `years` allows for custom values.}
#'   }
#'   If the chosen model is stationary, the results will be the same for all years
#'
#' @param n_sim Integer number of bootstrap simulations (default is 100000).
#'
#' @param alpha Numeric significance level for the confidence intervals (default is 0.05).
#'
#' @param parallel Logical. If TRUE, runs the bootstrap in parallel (default is FALSE).
#'
#' @return A named list containing a list of years. Each year maps to a sublist:
#' \describe{
#'   \item{estimates}{Vector of estimated quantiles for return periods 2, 5, 10, 20, 50, and 100.}
#'   \item{ci_lower}{Lower bound of the confidence interval for each return period.}
#'   \item{ci_upper}{Upper bound of the confidence interval for each return period.}
#'   \item{t}{Vector of return periods (2, 5, 10, 20, 50, and 100).}
#' }
#'
#' @details
#' The bootstrap procedure simulates resamples from the fitted distribution via inverse transform
#' sampling using the estimated parameters. For each resample, L-moment parameters are re-estimated
#' and used to compute quantiles. Confidence intervals are obtained by applying empirical quantiles
#' to the resulting distribution of estimates.
#'
#' Using \code{parallel = TRUE} can reduce computation time by approximately 50%.
#' However, using this option will nullify any calls to \code{set.seed()}, 
#' so your results may not be reproducible.
#'
#' @seealso \code{\link[lmom]{samlmu}}, \code{\link[stats]{quantile}}
#'
#' @importFrom parallel mclapply
#' @importFrom stats runif
#' @export

sb.uncertainty <- function(
  data,
  years,
  model,
  method,
  slices = "last",
  n_sim = 10000,
  alpha = 0.05,
  parallel = FALSE,
  prior = NULL
) {

	# Check if the method is invalid
	if (!(method %in% c("L-moments", "MLE", "GMLE"))) {
		stop("Unsupported method: ", method)
	}

	# Split the model into name and signature
	name <- substr(model, 1, 3)
	signature <- if (nchar(model) == 3) NULL else substr(model, 4, 5)

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
		efunc <- function(data, years) pelxxx(name, data)
	} else if (method == "MLE") {
		efunc <- function(data, years) mle.estimation(data, model, years)$params
	} else if (method == "GMLE") {
		efunc <- function(data, years) mle.estimation(data, model, years, prior)$params
	}	

	# Get the estimated quantiles
	params <- efunc(data, years)
	estimates <- qntxxx(model, returns, params, slices)

	# Generate the bootstrapped quantiles in parallel, one year at a time
	quantiles <- sapply(1:n, function(i) {
		u <- runif(n_sim)
		qntxxx(model, u, params, years[i])
	})

	# Some distributions generate negative values. Adjust these to epsilon.
	quantiles[quantiles < 0] = 1e-8

	# Define the apply() function based on 'parallel' parameter 
	# afunc <- if (parallel) { parallel::mclapply } else { lapply }

	# Vectorized, parallel bootstrap function 
	bootstrap_list <- lapply(1:n_sim, function(i) {
		bootstrap_params <- efunc(quantiles[i, ], years)
		qntxxx(model, returns, bootstrap_params, slices)
	})

	# Create a 3D array with dimensions (1: slices), (2: periods), (3: simulations)
	bootstrap_array <- array(unlist(bootstrap_list), dim = c(length(slices), 6, n_sim))

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
