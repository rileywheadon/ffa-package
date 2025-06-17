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
  df,
  model,
  method,
  years = "last",
  n_sim = 10000,
  alpha = 0.05,
  parallel = FALSE
) {

	# Check if the method is invalid
	if (!(method %in% c("L-moments", "MLE", "GMLE"))) {
		stop("Unsupported method: ", method)
	}

	# Split the model into name and signature
	name <- substr(model, 1, 3)
	signature <- if(nchar(model) == 3) NULL else substr(model, 4, 5)

	# Set return periods and their quantiles
    t <- c(2, 5, 10, 20, 50, 100)
    returns <- 1 - (1 / t)
    n <- length(df$max)

	# Determine the years based on the "years" argument
	if (is.character(years)) {
		years <- switch(
			years,
			"all" = df$year,
			"first" = min(df$year),
			"last" = max(df$year)
		)
	}

	# Get the quantile function
	distribution_list <- get.distributions()
	qfunc <- distribution_list[[name]]$quantile

	# Get the estimation function
	if (method == "L-moments") {
		efunc <- function(df, model) lmom.estimation(df$max, name)
	} else if (method == "MLE") {
		efunc <- function(df, model) mle.estimation(df, model)$params
	} else if (method == "GMLE") {
		efunc <- function(df, model) gmle.estimation(df, model)$params
	}	

	# Get the estimated quantiles
	params <- efunc(df, model)
	covariates <- get.covariates(df$year, years)
	estimates <- get.quantiles(returns, model, params, covariates)

	# Define the apply() function based on 'parallel' parameter 
	afunc <- if(parallel) { parallel::mclapply } else { lapply }

	# Generate the bootstrapped quantiles in parallel, one year at a time
	quantiles <- sapply(1:n, function(i) {
		x <- runif(n_sim)
		covariate <- get.covariates(df$year, df$year[i])
		get.quantiles(x, model, params, covariate)
	})

	# Vectorized, parallel bootstrap function 
	bootstrap_list <- afunc(1:n_sim, function(i) {
		quantiles_df <- data.frame(max = quantiles[i, ], year = df$year)
		bootstrap_params <- efunc(quantiles_df, model)
		get.quantiles(returns, model, bootstrap_params, covariates)
	})

	# Create a 3D array with dimensions (1: periods), (2: years), (3: simulations)
	bootstrap_array <- array(unlist(bootstrap_list), dim = c(6, length(years), n_sim))

	# Compute confidence intervals
	probs <- c(alpha / 2, 1 - (alpha / 2))
	ci <- apply(bootstrap_array, c(1, 2), quantile, probs = probs)

	# Generate the results as a list
	results <- lapply(1:length(years), function(k) {
		list(
			ci_lower = ci[1, , k], 
			estimates = estimates[, k],
			ci_upper = ci[2, , k]
		)
	})

	names(results) <- years
	return(results)

}
