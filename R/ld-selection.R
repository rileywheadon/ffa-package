#' L-Distance Method for Distribution Selection
#'
#' @description
#' Selects a distribution from a set of candidate distributions by minimizing the 
#' Euclidean distance between the theoretical L-moment ratios \eqn{(\tau_3, \tau_4)} 
#' and the sample L-moment ratios \eqn{(t_3, t_4)}.
#'
#' @param data Numeric; a vector of annual maximum streamflow data.
#'
#' @return List; results of distribution selection:
#' - `method`: `"L-distance"`
#' - `metrics`: A list of L-distance metrics for each candidate distribution.
#' - `recommendation`: The name of the distribution with the smallest L-distance.
#'
#' @details
#' For each candidate distribution, the method computes the Euclidean distance between
#' sample L-moment ratios (\eqn{\tau_3}, \eqn{\tau_4}) and the closest point on the
#' theoretical distribution's L-moment curve. For two-parameter distributions (Gumbel,
#' Normal, Log-Normal), the theoretical L-moment ratios are compared directly with
#' the sample L-moment ratios. The distribution with the minimum distance is selected.
#' If a distribution is fit to log-transformed data (Log-Normal or Log-Pearson Type III), 
#' the L-moment ratios for the log-transformed sample are used instead.
#'
#' @seealso \link{lmom.sample}, \link{lk.selection}, \link{z.selection}, \link[stats]{optim}
#'
#' @examples
#' data <- rnorm(n = 100, mean = 100, sd = 10)
#' ld.selection(data)
#'
#' @importFrom stats optim
#' @export

ld.selection <- function(data) {

	# Get the sample L-moments for data and log(data)
	reg_t3_t4 <- lmom.sample(data)[3:4]
	log_t3_t4 <- lmom.sample(log(data))[3:4]

	# Helper function to get distance between two points
	distance <- function(p1, p2) sqrt((p1[1] - p2[1])^2 + (p1[2] - p2[2])^2)

	# Initialize list of metrics
	metrics <- list()

	# Iterate through the list of distributions
	for (model in c("GUM", "NOR", "LNO", "GEV", "GLO", "GNO", "PE3", "LP3", "WEI")) {

		# Get distribution information
		info <- models.info(model)

		# Determine the sample L-moments (regular or log)
		t3_t4 <- if (info$log) log_t3_t4 else reg_t3_t4

		# Compute the L-distance metric directly for two-parameter distributions
		if (info$n.params == 2) {
			tau3_tau4 <- lmrxxx(model, c(0, 1))[3:4]
			metrics[[model]] <- distance(tau3_tau4, t3_t4)
			next
		}

		# Define objective function for three parameter distributions
		objective <- function(i) {
			tau3_tau4 <- lmrxxx(model, c(0, 1, i))[3:4]
			distance(tau3_tau4, t3_t4)
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
	list(
		method = "L-distance",
		metrics = metrics,
		recommendation = recommendation
	)	

}
