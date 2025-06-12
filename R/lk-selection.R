#' L-Kurtosis Method for Distribution Selection Using L-Moment Ratios
#'
#' Selects a best-fit probability distribution by minimizing the absolute vertical
#' distance (in \eqn{\tau_4}) between the sample L-moment ratios and the theoretical
#' L-moment curves. For 3-parameter distributions, we use the shape parameter that best
#' replicates the L-skewness of the data.
#'
#' @param ams A numeric vector containing the AMS data without NaN values
#'
#' @return A named list containing:
#' \describe{
#'   \item{distance}{A list of interpolated L-moment matches and kurtosis-based metrics for each distribution.}
#'   \item{recommendation}{Name of the distribution with the smallest L-kurtosis deviation.}
#' }
#'
#' @details
#' This method computes the vertical distance in \eqn{\tau_4} (L-kurtosis) between the sample
#' and theoretical L-moment ratio diagrams at fixed \eqn{\tau_3} (L-skewness). The interpolated
#' \eqn{\tau_4} and \eqn{\kappa} values are derived using \code{\link[stats]{approx}}.
#'
#' Only 3-parameter distributions are considered in this method. Specifically, it evaluates
#' GEV, GLO, PE3, LP3, GNO, and WEI. For more information, see the FFA framework website.
#'
#' @seealso \code{\link{ld.selection}}, \code{\link{z.selection}}
#'
#' @importFrom stats optim
#' @export

lk.selection <- function(ams) {

	# Get the sample L-moments for ams and log(ams)
	reg_sample_t3_t4 <- unname(samlmu(ams))[3:4]
	log_sample_t3_t4 <- unname(samlmu(log(ams)))[3:4]

	# Initialize list of metrics
	metrics <- list()

	# Iterate through the list of distributions
	distribution_list <- get.distributions()
	for (distribution in distribution_list) {

		# Determine the sample L-moments (regular or log)
		sample_t3_t4 <- if (distribution$log) {
			log_sample_t3_t4 
		} else {
			reg_sample_t3_t4
		}

		# Compute the L-kurtosis metric directly for two-parameter distributions 
		if (distribution$n_params == 2) {
			metrics[[distribution$name]] <- abs(distribution$t3_t4[2] - sample_t3_t4[2])
			next
		}

		# Find the shape parameter with the same L-skewness as the data
		objective <- function(i) {
			distribution_t3_t4 <- distribution$lmr_function(c(0, 1, i))[3:4]
			abs(distribution_t3_t4[1] - sample_t3_t4[1])
		}

		# Run optimization for three parameter distributions
		result <- optim(
			par = (distribution$kappa_lower + distribution$kappa_upper) / 2,
			fn = objective,
			method = "Brent",
			lower = distribution$kappa_lower,
			upper = distribution$kappa_upper
		)

		# Get the parameters of the fitted distribution
		distribution_t3_t4 <- distribution$lmr_function(c(0, 1, result$par))[3:4]
		metrics[[distribution$name]] <- abs(distribution_t3_t4[2] - sample_t3_t4[2])

	}

	# Determine the recommendation (distribution with lowest L-distance)
	recommendation <- distribution_list[[which.min(metrics)]]$name

	# Return the results as a list
	list(metrics = metrics, recommendation = recommendation)	

}
