#' L-Kurtosis Method for Distribution Selection
#'
#' @description
#' Selects a probability distribution by minimizing the absolute distance
#' between the theoretical L-kurtosis (\eqn{\tau_4}) and the sample L-kurtosis 
#' (\eqn{t_4}). Only supports 3-parameter distributions. 
#'
#' **For NS-FFA**: To select a distribution for a nonstationary model, include the 
#' observation years (`ns_years`) and the nonstationary model structure 
#' (`ns_structure`). Then, this method will detrend the original, nonstationary data 
#' internally using the [data_decomposition()] function prior to distribution selection.
#'
#' @inheritParams param-data
#' @inheritParams param-ns-years
#' @inheritParams param-ns-structure
#'
#' @return A list with the results of distribution selection:
#' - `method`: `"L-kurtosis"`.
#' - `decomposed_data`: The detrended dataset used to compute the L-moments. For S-FFA, 
#'    this is the `data` argument. For NS-FFA, it is output of [data_decomposition()].
#' - `metrics`: A list of L-kurtosis metrics for each distribution.
#' - `recommendation`: Name of the distribution with the smallest L-kurtosis metric.
#'
#' @details
#' This method computes the distance between the sample and theoretical L-kurtosis 
#' values at a fixed L-skewness. For three parameter distributions, the shape parameter 
#' that best replicates the sample L-skewness is determined using [stats::optim()].
#'
#' @seealso [utils_sample_lmoments()], [select_ldistance()], [select_zstatistic()], 
#'   [plot_lmom_diagram()]
#'
#' @examples
#' data <- rnorm(n = 100, mean = 100, sd = 10)
#' select_lkurtosis(data)
#'
#' @references
#' Hosking, J.R.M. & Wallis, J.R., 1997. Regional frequency analysis: an approach based 
#' on L-Moments. Cambridge University Press, New York, USA.
#'
#' @importFrom stats optim
#' @export

select_lkurtosis <- function(data, ns_years = NULL, ns_structure = NULL) {

	data <- validate_numeric("data", data, optional = FALSE)

	if (!is.null(ns_years) && !is.null(ns_structure)) {
		ns_years <- validate_numeric("ns_years", ns_years, size = length(data))
		ns_structure <- validate_structure(ns_structure)
		data <- data_decomposition(data, ns_years, ns_structure)
	}

	# Get the sample L-moments for data and log(data)
	reg_t3_t4 <- utils_sample_lmoments(data)[3:4]
	log_t3_t4 <- utils_sample_lmoments(log(data))[3:4]

	# Initialize list of metrics
	metrics <- list()

	# Iterate through the list of distributions
	for (distribution in c("GEV", "GLO", "GNO", "PE3", "LP3", "WEI")) {

		# Get distribution information
		info <- model_info(distribution)

		# Determine the sample L-moments (regular or log)
		t3_t4 <- if (info$log) log_t3_t4 else reg_t3_t4

		# Find the shape parameter with the same L-skewness as the data
		objective <- function(i) {
			tau3_tau4 <- theoretical_lmoments_fast(distribution, c(0, 1, i))[3:4]
			abs(tau3_tau4[1] - t3_t4[1])
		}

		# Run optimization for three parameter distributions
		result <- optim(
			par = (info$k_bounds[1] + info$k_bounds[2]) / 2,
			fn = objective,
			method = "Brent",
			lower = info$k_bounds[1],
			upper = info$k_bounds[2]
		)

		# Get the parameters of the fitted distribution
		tau3_tau4 <- theoretical_lmoments_fast(distribution, c(0, 1, result$par))[3:4]
		metrics[[distribution]] <- abs(tau3_tau4[2] - t3_t4[2])

	}

	# Determine the recommendation (distribution with lowest L-distance)
	recommendation <- names(metrics)[which.min(metrics)]

	# Return the results as a list
	list(
		method = "L-kurtosis",
		decomposed_data = data,
		metrics = metrics, 
		recommendation = recommendation
	)	

}
