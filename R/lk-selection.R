#' L-Kurtosis Method for Distribution Selection
#'
#' Selects a probability distribution by minimizing the absolute distance
#' between the theoretical L-kurtosis (\eqn{\tau_4}) and the sample L-kurtosis 
#' (\eqn{t_4}). For 3-parameter distributions, we use the shape parameter that 
#' best replicates the sample L-skewness (\eqn{t_3}) of the data.
#'
#' @param data Numeric; a vector of annual maximum streamflow data.
#'
#' @return List; results of distribution selection:
#' - `method`: `"L-kurtosis"`
#' - `metrics`: A list of L-kurtosis metrics for each distribution.
#' - `recommendation`: Name of the distribution with the smallest L-kurtosis metric
#'
#' @details
#' This method computes the distance between the sample and theoretical L-kurtosis values at 
#' a fixed L-skewness. For three parameter distributions, the shape parameter that best 
#' replicates the sample L-skewness is derived using \link[stats]{optim}.
#'
#' @seealso \link{lmom.sample}, \link{ld.selection}, \link{z.selection}, \link[stats]{optim}
#'
#' @examples
#' data <- rnorm(n = 100, mean = 100, sd = 10)
#' lk.selection(data)
#'
#' @importFrom stats optim
#' @export

lk.selection <- function(data) {

	# Get the sample L-moments for data and log(data)
	reg_t3_t4 <- lmom.sample(data)[3:4]
	log_t3_t4 <- lmom.sample(log(data))[3:4]

	# Initialize list of metrics
	metrics <- list()

	# Iterate through the list of distributions
	for (model in c("GUM", "NOR", "LNO", "GEV", "GLO", "GNO", "PE3", "LP3", "WEI")) {

		# Get distribution information
		info <- models.info(model)

		# Determine the sample L-moments (regular or log)
		t3_t4 <- if (info$log) log_t3_t4 else reg_t3_t4

		# Compute the L-kurtosis metric directly for two-parameter distributions 
		if (info$n.params == 2) {
			tau3_tau4 <- lmrxxx(model, c(0, 1))[3:4]
			metrics[[model]] <- abs(tau3_tau4[2] - t3_t4[2])
			next
		}

		# Find the shape parameter with the same L-skewness as the data
		objective <- function(i) {
			tau3_tau4 <- lmrxxx(model, c(0, 1, i))[3:4]
			abs(tau3_tau4[1] - t3_t4[1])
		}

		# Run optimization for three parameter distributions
		result <- optim(
			par = (info$k.bounds[1] + info$k.bounds[2]) / 2,
			fn = objective,
			method = "Brent",
			lower = info$k.bounds[1],
			upper = info$k.bounds[2]
		)

		# Get the parameters of the fitted distribution
		tau3_tau4 <- lmrxxx(model, c(0, 1, result$par))[3:4]
		metrics[[model]] <- abs(tau3_tau4[2] - t3_t4[2])

	}

	# Determine the recommendation (distribution with lowest L-distance)
	recommendation <- names(metrics)[which.min(metrics)]

	# Return the results as a list
	list(
		method = "L-kurtosis",
		metrics = metrics, 
		recommendation = recommendation
	)	

}
