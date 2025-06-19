#' L-Distance Method for Distribution Selection Using L-Moment Ratios
#'
#' Selects the best-fit distribution from a candidate set by minimizing the Euclidean distance
#' between theoretical and sample L-moment ratios (\eqn{\tau_3}, \eqn{\tau_4}). This method
#' quantifies goodness-of-fit in the L-moment ratio space and returns the closest matching
#' distribution.
#'
#' @param data A numeric vector containing the AMS data without NaN values
#'
#' @return A named list containing:
#' \describe{
#'   \item{distance}{A list of fitted moment points for each candidate distribution with
#'     associated L-distance metrics.}
#'   \item{recommendation}{The name of the distribution with the smallest L-distance.}
#' }
#'
#' @details
#' For each candidate distribution, the method computes the Euclidean distance between
#' sample L-moment ratios (\eqn{\tau_3}, \eqn{\tau_4}) and the closest point on the
#' theoretical distribution's L-moment surface. The distribution with the minimum distance
#' is selected.
#'
#' If a distribution is flagged as requiring log-transformed data, the \code{log_lm}
#' component is used for matching.
#'
#' @seealso \code{\link{z.selection}}, \code{\link{lk.selection}}
#'
#' @importFrom lmom samlmu
#' @importFrom stats optim
#' @export

ld.selection <- function(data) {

	# Get the sample L-moments for data and log(data)
	reg_sample_t3_t4 <- lmom.sample(data)[3:4]
	log_sample_t3_t4 <- lmom.sample(log(data))[3:4]

	# Helper function to get distance between two points
	distance <- function(p1, p2) sqrt((p1[1] - p2[1])^2 + (p1[2] - p2[2])^2)

	# Initialize list of metrics
	metrics <- list()

	# Iterate through the list of distributions
	for (model in c("GUM", "NOR", "LNO", "GEV", "GLO", "GNO", "PE3", "LP3", "WEI")) {

		# Get distribution information
		info <- models.info(model)

		# Determine the sample L-moments (regular or log)
		sample_t3_t4 <- if (info$log) {
			log_sample_t3_t4 
		} else {
			reg_sample_t3_t4
		}

		# Compute the L-distance metric directly for two-parameter distributions
		if (info$n.params == 2) {
			distribution_t3_t4 <- lmrxxx(model, c(0, 1))[3:4]
			metrics[[model]] <- distance(distribution_t3_t4, sample_t3_t4)
			next
		}

		# Define objective function for three parameter distributions
		objective <- function(i) {
			distribution_t3_t4 <- lmrxxx(model, c(0, 1, i))[3:4]
			distance(distribution_t3_t4, sample_t3_t4)
		}

		# Run optimization for three parameter distributions
		result <- optim(
			par = (info$k.bounds[1] + info$k.bounds[2]) / 2,
			fn = objective,
			method = "Brent",
			lower = info$k.bounds[1],
			upper = info$k.bounds[2]
		)

		metrics[[model]] <- result$value

	}

	# Determine the recommendation (distribution with lowest L-distance)
	recommendation <- names(metrics)[which.min(metrics)]

	# Return the results as a list
	list(metrics = metrics, recommendation = recommendation)	

}
