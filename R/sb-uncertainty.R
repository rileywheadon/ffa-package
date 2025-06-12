#' Sample Bootstrap Confidence Intervals for Flood Quantile Estimates
#'
#' Computes confidence intervals for flood quantile estimates using the nonparametric
#' sample bootstrap method, based on L-moment parameter estimation. This function supports
#' uncertainty quantification for return period estimates derived from a fitted distribution.
#'
#' @param ams Numeric vector of annual maximum streamflow values (no missing values).
#' @param distribution Name of the selected distribution
#' @param method Character string specifying the estimation method. 
#'   Currently supports \code{"L-moments"}, \code{"MLE"}, and \code{"GMLE"}.
#' @param n_sim Integer number of bootstrap simulations (default is 100000).
#' @param alpha Numeric significance level for the confidence intervals (default is 0.05).
#' @param parallel Logical. If TRUE, runs the bootstrap in parallel (default is FALSE).
#'
#' @return A named list containing:
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

sb.uncertainty <- function(ams, distribution, method, n_sim = 10000, alpha = 0.05, parallel = FALSE) {

	# Check if the method is invalid
	if (!(method %in% c("L-moments", "MLE"))) {
		stop("Unsupported method: ", method)
	}

	# Get the estimation function
	if (method == "L-moments") {
		efunc <- function(ams, distribution) lmom.estimation(ams, distribution)
	} else if (method == "MLE") {
		efunc <- function(ams, distribution) mle.estimation(ams, distribution)$params
	} else if (method == "GMLE") {
		efunc <- function(ams, distribution) gmle.estimation(ams, distribution)$params
	}

	# Set return periods and their quantiles
    t <- c(2, 5, 10, 20, 50, 100)
    returns <- 1 - (1 / t)
    n <- length(ams)

	# Get the distribution list
	distribution_list <- get.distributions()

    # Get the quantile function and parameter estimates
	qfunc <- distribution_list[[distribution]]$quantile
    params <- efunc(ams, distribution)
    estimates <- qfunc(returns, params)

	# Define the apply() function based on 'parallel' parameter 
	afunc <- if(parallel) { parallel::mclapply } else { lapply }

	# Vectorized, parallel bootstrap function 
	bootstrap_list <- afunc(1:n_sim, function(i) {
		X <- runif(n)
		SQ <- qfunc(X, params)
		SQ_params <- efunc(SQ, distribution)
		as.numeric(qfunc(returns, SQ_params))
	})

	# Create matrix
	bootstrap_results <- do.call(rbind, bootstrap_list)

	# Compute and return confidence intervals
	probs <- c(alpha / 2, 1 - (alpha / 2))
	ci <- apply(bootstrap_results, 2, quantile, probs = probs)

	list(
		estimates = estimates,
		ci_lower = ci[1, ],
		ci_upper = ci[2, ],
		t = t
	)

}
