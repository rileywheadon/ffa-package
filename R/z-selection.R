#' Z-Statistic Method for Distribution Selection
#'
#' Selects the best-fit distribution by computing a bias-corrected Z-statistic for the sample
#' \eqn{\tau_4} (L-kurtosis) against theoretical L-moment surfaces for a set of candidate
#' distributions. The distribution with the smallest absolute Z-score is selected.
#'
#' @param data Numeric vector of annual maximum streamflow values (no missing values).
#' @param n_sim Number of bootstrap samples to generate (default = 100000).
#' @param parallel Logical. If TRUE, runs the bootstrap in parallel (default is FALSE).
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
#' GLO, PE3, GNO, and WEI. Log-data distributions include LP3. A Kappa distribution is
#' fitted to each and used to simulate bootstrapped L-moments. The observed \eqn{\tau_4} is then
#' compared to each theoretical distribution using the Z-statistic framework.
#'
#' Using \code{parallel = TRUE} can reduce computation time by approximately 50%.
#' However, using this option will nullify any calls to \code{set.seed()}, 
#' so your results may not be reproducible.
#'
#' @seealso \code{\link{ld.selection}}, \code{\link{lk.selection}}, 
#'   \code{\link[lmom]{pelkap}}, \code{\link[lmom]{quakap}}
#'
#' @importFrom lmom pelkap quakap
#' @importFrom stats runif optim
#' @importFrom parallel mclapply
#' @export

z.selection <- function(data, n_sim = 100000, parallel = FALSE) {

	# Helper function that attempts to fit a Kappa distribution and draw a bootstrap
	get_bootstrap <- function(data) {

		# Get the sample L-moments for the data
		moments <- lmom.sample(data)
		sample_l1 <- moments[1]
		sample_l2 <- moments[2]
		sample_t3 <- moments[3]
		sample_t4 <- moments[4]

		# Check that the smaple L-moments are viable
		if (sample_t4 > (1 + 5 * sample_t3^2) / 6) {
			return (NULL)
		}

		# Attempt to fit the Kappa distribution
		params <- pelkap(sample_l1, sample_l2, sample_t3, sample_t4)

		# Define the apply() function based on 'parallel' parameter 
		afunc <- if(parallel) { parallel::mclapply } else { lapply }

		# Generate a bootstrap from this Kappa distribution
		t4_list <- afunc(1:n_sim, function(i) {
			u <- runif(length(data))	
			lmom.sample(qntkap(u, params))[4]
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
	reg_bootstrap <- get_bootstrap(data)

	# Attempt to fit kappa distribution to log(data)
	log_bootstrap <- get_bootstrap(log(data))

	# Initialize list of metrics
	metrics <- list()

	# Iterate through the list of three parameter distributions
	for (model in c("GEV", "GLO", "GNO", "PE3", "LP3", "WEI")) {

		# Get distribution information
		info <- models.info(model)

		# Get the correct bootstrap
		if (info$log & !is.null(log_bootstrap)) {
			bootstrap <- log_bootstrap
		} else if (!info$log & !is.null(reg_bootstrap)) {
			bootstrap <- reg_bootstrap
		} else {
			next
		}

		# Find the shape parameter with the same L-skewness as the data
		objective <- function(i) {
			distribution_t3 <- lmrxxx(model, c(0, 1, i))[3]
			abs(distribution_t3 - bootstrap$sample_t3)
		}

		# Run optimization for three parameter distributions
		result <- optim(
			par = (info$k.bounds[1] + info$k.bounds[2]) / 2,
			fn = objective,
			method = "Brent",
			lower = info$k.bounds[1],
			upper = info$k.bounds[2]
		)

		# Get the parameters of the fitted distribution and compute the z-score
		distribution_t4 <- lmrxxx(model, c(0, 1, result$par))[4]
		z <- (distribution_t4 - bootstrap$sample_t4 + bootstrap$bias_t4) / bootstrap$sd_t4
		metrics[[model]] <- z

	}

	# Determine the recommendation (distribution with lowest L-distance)
	index <- which.min(abs(unlist(metrics)))
	recommendation <- names(metrics)[index]

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
