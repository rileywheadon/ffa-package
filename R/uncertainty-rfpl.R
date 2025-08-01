#' (Generalized) Regula-Falsi Confidence Intervals for Flood Quantile Estimates
#'
#' Calculates return level estimates and confidence intervals at specified return
#' periods (defaults to 2, 5, 10, 20, 50, and 100 years) using the Regula-Falsi profile
#' likelihood or Regula-Falsi generalized profile likelihood root‐finding methods.
#'
#' @inheritParams param-data
#' @inheritParams param-distribution
#' @inheritParams param-prior
#' @inheritParams param-years
#' @inheritParams param-structure
#' @inheritParams param-slices
#' @inheritParams param-alpha
#' @inheritParams param-periods
#' 
#' @param eps Numeric scalar. The log-likelihood tolerance for the Regula-Falsi 
#' convergence (default is 0.01).
#'
#' @return A list containing the following three items:
#' - `method`: "Bootstrap"
#' - `structure`: The value of the `structure` argument.
#' - `slices`: A list of lists containing the results for each slice.
#'
#' Each element of `slices` is a list with the following five items:
#' - `estimates`: Estimated quantiles for each return period.
#' - `ci_lower`: Lower bound of the confidence interval for each return period.
#' - `ci_upper`: Upper bound of the confidence interval for each return period.
#' - `periods`: Vector of return periods; defaults to `c(2, 5, 10, 20, 50, 100)`.
#' - `year`: The year at which the estimates were computed (nonstationary models only).
#'
#' @details
#' 1. Fits the distribution using [fit_maximum_likelihood()] to obtain parameter 
#'    estimates and log‐likelihood.  
#' 2. Defines an objective function \eqn{f(y_p, p)} using the reparameterized
#'    log-likelihood function.
#' 3. Iteratively brackets the root by rescaling initial guesses by 0.05 until 
#'    \eqn{f(y_p, p)} changes sign.  
#' 4. Uses the Regula Falsi method to solve \eqn{f(y_p, p) = 0} for each 
#'    return-period probability.  
#' 5. Returns lower and upper confidence bounds at significance level `alpha` 
#'    given the return level estimates.
#'
#' @note Although the more modern [stats::optim()] function is preferred over 
#' [stats::nlminb()], we use [stats::nlminb()] because it supports infinite
#' values of the likelihood function. 
#'
#' RFPL uncertainty quantification can be numerically unstable for some datasets. 
#' If this function encounters an issue, it will return an error and recommend 
#' using the parametric bootstrap method (`uncertainty_bootstrap`) instead.
#'
#' @seealso [quantile_fast()], [uncertainty_bootstrap()], 
#'   [plot_sffa()], [plot_nsffa()]
#'
#' @examples
#' data <- rnorm(n = 100, mean = 100, sd = 10)
#' uncertainty_rfpl(data, "GLO")
#'
#' @references
#' Vidrio-Sahagún, C.T., He, J. Enhanced profile likelihood method for the nonstationary 
#' hydrological frequency analysis, Advances in Water Resources 161, 10451 (2022). 
#' \doi{10.1016/j.advwatres.2022.104151}
#' 
#' Vidrio-Sahagún, C.T., He, J. & Pietroniro, A. Multi-distribution regula-falsi profile 
#' likelihood method for nonstationary hydrological frequency analysis. Stoch Environ Res 
#' Risk Assess 38, 843–867 (2024). \doi{10.1007/s00477-023-02603-0}
#'
#' @importFrom stats qchisq nlminb
#' @export

uncertainty_rfpl <- function(
    data,
    distribution,
    prior = NULL,
    years = NULL,
    structure = NULL,
    slices = 1900,
    alpha = 0.05,
    eps = 1e-2,
	periods = c(2, 5, 10, 20, 50, 100)
) {

	data <- validate_numeric("data", data, FALSE)
	distribution <- validate_enum("distribution", distribution)
	prior <- validate_numeric("prior", prior, size = 2, bounds = c(0, Inf))
	years <- validate_numeric("years", years, size = length(data))
	structure <- validate_structure(structure)
	slices <- validate_numeric("slices", slices, FALSE)
	alpha <- validate_float("alpha", alpha, bounds = c(0.01, 0.1))
	eps <- validate_float("eps", eps, bounds = c(0, 1))
	periods <- validate_numeric("periods", periods, FALSE, bounds = c(1, Inf))

	# Return a list of lists
	results <- lapply(slices, function(slice) {
		uncertainty_rfpl_helper(
			data,
			distribution,
			prior,
			years,
			structure,
			slice,
			alpha,
			eps,
			periods
		)
	})

	list(
		method = if (is.null(prior)) "RFPL" else "RFGPL",
		structure = structure,
		slices = results
	)
}

uncertainty_rfpl_helper <- function(
    data,
    distribution,
    prior,
    years,
    structure,
    slice,
    alpha,
    eps,
	periods
) {

	# Failure message
	msg <- "RFPL uncertainty quantification failed to converge. Try bootstrap instead."

	# Helper function for computing the profile likelihood
	profile_likelihood <- function(yp, p, initial, prior = NULL) {

		# Remove the parameter we are using for reparameterization on yp
		if (distribution != "WEI") {
			initial <- initial[-1]
		} else if (distribution == "WEI" && !structure$location) {
			initial <- initial[-2]
		} else {
			initial <- initial[-3]
		} 

		# Set the default bounds based on the structure and distribution
		if (structure$location) {
			lower <- c(-Inf)
			upper <- c( Inf)
		} else {
			lower <- numeric(0)
			upper <- numeric(0)
		}

		if (structure$scale) {
			lower <- c(lower, 1e-8, -Inf)
			upper <- c(upper,  Inf,  Inf)
		} else {
			lower <- c(lower, 1e-8)
			upper <- c(upper,  Inf)
		}

		# The Weibull distribution has different parameter bounds
		if (distribution == "WEI") { 
			if (structure$location) {
				lower <- c(-Inf, -Inf)
				upper <- c( Inf,  Inf)
			} else {
				lower <- c(-Inf)
				upper <- c( Inf)
			}

			if (structure$scale) {
				lower <- c(lower, -Inf)
				upper <- c(upper,  Inf)
			}
		} 

		# Add bounds for the shape parameter if necessary
		info <- model_info(distribution)
		if (info$n_params == 3) {

			# Set finite bounds on the shape parameter for RFGPL
			if (is.null(prior)) {
				lower <- c(lower, -Inf)
				upper <- c(upper,  Inf)
			} else {
				lower <- c(lower, -0.5 + 1e-8)
				upper <- c(upper,  0.5 - 1e-8)
			}
		}

		# Maximize the log-likelihood by minimizing the negative log-likelihood
		objective <- function(theta) {

			# Reparameterize on the location for non-Weibull distributions
			if (distribution != "WEI") {

				# Get the quantile at probability p with location 0
				qp <- quantile_fast(p, distribution, c(0, theta), slice, structure)

				# Log-transform yp, qp if necessary
				if (distribution %in% c("LNO", "LP3")) {
					yp <- log(yp)
					qp <- log(qp)
				}

				# Reparameterize in terms of the quantiles
				theta <- c(yp - qp, theta)

			} 

			# Use a different reparameterization for the Weibull distributions
			else {

				# i and j are used to shift location in the parameter vector
				i <- structure$location
				j <- structure$scale

				# Get the quantiles for the Wei(0, 1, shape) distribution
				qp <- quantile_wei(p, c(0, 1, theta[2 + i + j]))

				# Reparameterize on the scale parameter sigma
				covariate <- get_covariates(slice)

				if (structure$location) {
					sigma <- (yp - theta[1] - (theta[2] * covariate)) / qp
				} else {
					sigma <- (yp - theta[1]) / qp
				}

				if (structure$scale) {
					sigma <- sigma - (theta[2 + i] * covariate)
				}

				# Reassemble the parameter vector 
				theta <- c(theta[1:(1 + i)], sigma, theta[(2 + i):(2 + i + j)])

			}

			# Use the correct likelihood function based on whether there is a prior
			if (!is.null(prior)) {
				0 - general_loglik_fast(data, distribution, theta, prior, years, structure)
			} else {
				0 - loglik_fast(data, distribution, theta, years, structure)
			}

		} 

		# Helper function for running parameter optimization
		optimizer <- function(theta) {
			nlminb(theta, objective, lower = lower, upper = upper)
		}

		# Repeatedly attempt optimization using the nlminb() function
		attempts <- 1
		params <- initial

		while (attempts < 100) {

			# Attempt to run the optimize() function
			result <- tryCatch(optimizer(params), error = function(e) NULL)

			# If optimization succeeded, end the loop
			if (!is.null(result) && result$convergence == 0) break

			# If optimization failed, perturb the parameters and try again
			perturbation <- rnorm(n = length(params), mean = 0, sd = 0.2)
			params <- initial * (1 + perturbation)
			attempts <- attempts + 1

		}

		# Throw an error if optimization failed
		if (attempts == 100) stop(msg)

		# Flip the sign because we optimized the negative log-likelihood earlier
		0 - result$objective

	} 

	# Define the nonexceedance probabilities for the target return periods
	probabilities <- 1 - (1 / periods)

	# Get the results of maximum likelihood estimation
	mle <- fit_maximum_likelihood(data, distribution, prior, years, structure) 
	lp_hat <- mle$mll
	yp_hat <- quantile_fast(probabilities, distribution, mle$params, slice, structure)

	# We want to find the roots of the function f defined below:
	f <- function(yp, p) {
		lp_theta <- profile_likelihood(yp, p, mle$params, prior)
		lp_theta - (lp_hat - (qchisq(1 - alpha, 1) / 2))
	}

	# Define a helper function for regula-falsi iteration
	regula.falsi <- function(a, b, p) { 

		# Compute c from a and b
		c <- ((a * f(b, p)) - (b * f(a, p))) / (f(b, p) - f(a, p))
		
		# If f(c) is small, we are done
		if (abs(f(c, p)) < eps) return (c) 

		# Otherwise, update a, b and then call regula.falsi again
		if (f(c, p) < 0) { a <- c } else { b <- c }
		regula.falsi(a, b, p)

	}

	# Run uncertainty quantification
	ci_lower <- numeric(length(yp_hat))
	ci_upper <- numeric(length(yp_hat))

	# Iterate through the return periods 
	for (i in 1:length(yp_hat)) {

		# Find initial lower bound for mu
		yp_minus <- yp_hat[i] * 0.95
		while (f(yp_minus, probabilities[i]) > 0) {
			if (yp_minus < 1) stop(msg)
			yp_minus <- yp_minus * 0.95
		}

		# Run the iteration algorithm to find the lower confidence interval
		yp_lower <- regula.falsi(yp_minus, yp_hat[i], probabilities[i])
		ci_lower[i] <- yp_lower

		# Find initial upper bound for mu
		yp_plus <- yp_hat[i] * 1.05
		while (f(yp_plus, probabilities[i]) > 0) {
			if (yp_plus > 1e9) stop(msg)
			yp_plus <- yp_plus * 1.05
		}

		# Run the iteration algorithm to find the upper confidence interval
		yp_upper <- regula.falsi(yp_hat[i], yp_plus, probabilities[i])
		ci_upper[i] <- yp_upper

	}

	list(
		estimates = yp_hat ,
		ci_lower = ci_lower,
		ci_upper = ci_upper,
		periods = periods,
		year = if (structure$location || structure$scale) slice else NULL
	)

}
