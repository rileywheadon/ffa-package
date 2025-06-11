#' Z-Statistic Method for Distribution Selection
#'
#' Selects the best-fit distribution by computing a bias-corrected Z-statistic for the sample
#' \eqn{\tau_4} (L-kurtosis) against theoretical L-moment surfaces for a set of candidate
#' distributions. The distribution with the smallest absolute Z-score is selected.
#'
#' @param ams Numeric vector of annual maximum streamflow values (no missing values).
#' @param n_sim Number of bootstrap samples to generate (default = 100000).
#'
#' @return A list containing:
#' \describe{
#'   \item{params}{Kappa parameters fitted to the raw AMS data.}
#'   \item{log_params}{Kappa parameters fitted to the log-transformed AMS data.}
#'   \item{bootstrap}{List of bootstrap estimates of bias and standard deviation for \eqn{\tau_4}.}
#'   \item{distance}{List of computed Z-statistics for each candidate distribution.}
#'   \item{recommendation}{Name of the best-fit distribution based on the smallest Z-statistic.}
#' }
#'
#' @details
#' The method evaluates both raw and log-transformed data. Raw-data distributions include GEV,
#' GLO, PE3, GNO, WEI, and GPA. Log-data distributions include LP3. A Kappa distribution is
#' fitted to each and used to simulate bootstrapped L-moments. The observed \eqn{\tau_4} is then
#' compared to each theoretical distribution using the Z-statistic framework.
#'
#' @seealso \code{\link{ld.selection}}, \code{\link{lk.selection}}, 
#'   \code{\link[lmom]{pelkap}}, \code{\link[lmom]{quakap}}
#'
#' @importFrom lmom pelkap quakap
#' @importFrom stats runif optim
#' @importFrom parallel mclapply
#' @export

z.selection <- function(ams, n_sim = 100000) {

	# Helper function that attempts to fit a Kappa distribution and draw a bootstrap
	get_bootstrap <- function(data) {

		# Get the sample L-moments for the data
		sample_t3 <- unname(samlmu(data))[3]
		sample_t4 <- unname(samlmu(data))[4]

		# Attempt to fit the Kappa distribution
		params <- unname(pelkap(samlmu(data)))

		# Generate a bootstrap from this Kappa distribution
		t4_list <- mclapply(1:n_sim, function(i) {
			u <- runif(length(ams))	
			unname(samlmu(quakap(u, params)))[4]
		})

		t4 <- as.numeric(t4_list)

		# Compute bias and standard deviation of t4 estimates
		bias_t4 <- mean(t4 - sample_t4)
		sd_t4 <- sqrt(mean((t4 - sample_t4)^2) - bias_t4^2)

		# Return the results as a list
		list(
			params = params,
			sample_t3 = sample_t3,
			sample_t4 = sample_t4,
			t4 = t4,
			bias_t4 = bias_t4,
			sd_t4 = sd_t4
		)

	}

	# Attempt to fit kappa distribution to the data
	reg_bootstrap <- tryCatch(
		get_bootstrap(ams),
		error = function(e) { 
			message("A Kappa distribution could not be fit to the data.")
			NULL
		}
	)

	# Attempt to fit kappa distribution to log(data)
	log_bootstrap <- tryCatch(
		get_bootstrap(log(ams)),
		error = function(e) { 
			message("A Kappa distribution could not be fit to log(data).")
			NULL
		}
	)

	# Initialize list of metrics
	metrics <- list()

	# Iterate through the list of distributions
	distribution_list <- get.distributions()
	for (distribution in distribution_list) {

		# Get the correct parameters, L-moments, and bootstrap
		if (distribution$log & !is.null(log_bootstrap)) {
			b <- log_bootstrap
		} else if (!distribution$log & !is.null(reg_bootstrap)) {
			b <- reg_bootstrap
		} else {
			next
		}

		# The z-statistic metric does not exist for two-parameter distributions 
		if (distribution$n_params == 2) {
			next
		}

		# Find the shape parameter with the same L-skewness as the data
		objective <- function(i) {
			distribution_t3 <- distribution$lmr_function(c(0, 1, i))[3]
			abs(distribution_t3 - b$sample_t3)
		}

		# Run optimization for three parameter distributions
		result <- optim(
			par = (distribution$kappa_lower + distribution$kappa_upper) / 2,
			fn = objective,
			method = "Brent",
			lower = distribution$kappa_lower,
			upper = distribution$kappa_upper
		)

		# Get the parameters of the fitted distribution and compute the z-score
		distribution_t4 <- distribution$lmr_function(c(0, 1, result$par))[4]
		z <- (distribution_t4 - b$sample_t4 + b$bias_t4) / b$sd_t4
		metrics[[distribution$name]] <- z

	}

	# Determine the recommendation (distribution with lowest L-distance)
	index <- which.min(abs(unlist(metrics)))
	recommendation <- names(metrics)[[ index ]]

	# Return the results as a list
	list(
		params = reg_bootstrap$params,
		log_params = log_bootstrap$params,
		bootstrap = list(
			bias_t4 = reg_bootstrap$bias_t4,
			sd_t4 = reg_bootstrap$sd_t4,
			log_bias_t4 = log_bootstrap$bias_t4,
			log_sd_t4 = log_bootstrap$sd_t4
		),
		metrics = metrics,
		recommendation = recommendation
	)

}
