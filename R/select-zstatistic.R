#' Z-Statistic Method for Distribution Selection
#'
#' Selects the best-fit distribution by comparing a bias-corrected Z-statistic for 
#' the sample L-kurtosis (\eqn{\tau_4}) against the theoretical L-moments for a set 
#' of candidate distributions. The distribution with the smallest absolute Z-statistic 
#' is selected.
#'
#' @inheritParams param-data
#' @inheritParams param-samples
#'
#' @return A list with the results of distribution selection:
#' - `method`: `"Z-selection"`.
#' - `data`: The `data` argument.
#' - `metrics`: List of computed Z-statistics for each candidate distribution.
#' - `recommendation`: Name of the distribution with the smallest Z-statistic.
#' - `reg_params`: Kappa distribution parameters for the AMS data.
#' - `reg_bias_t4`: Bias of the L-kurtosis from the AMS bootstrap.
#' - `reg_std_t4`: Standard deviation of the L-kurtosis from the AMS bootstrap.
#' - `log_params`: Kappa distribution parameters for the log-AMS data.
#' - `log_bias_t4`: Bias of the L-kurtosis from the log-AMS bootstrap.
#' - `log_std_t4`: Standard deviation of the L-kurtosis from the log-AMS bootstrap.
#'
#' @details
#' The method performs distribution selection using both raw and log-transformed data. The 
#' distributions which use the raw AMS data are GEV, GLO, PE3, GNO, and WEI. The LP3
#' distribution uses log-transformed data. 
#'
#' The Z-statistic is determined by fitting a four-parameter Kappa distribution to the 
#' raw and log-transformed data. Then, bootstrapped samples from this Kappa distribution
#' The L-moments of these bootstrapped samples are used to estimate the Z-statistic 
#' for each distribution.
#'
#' @seealso [select_ldistance()], [select_lkurtosis()], [fit_lmom_kappa()],
#'   [quantile_fast()], [plot_lmom_diagram()]
#'
#' @examples
#' data <- rnorm(n = 100, mean = 100, sd = 10)
#' select_zstatistic(data)
#'
#' @references
#' Hosking, J.R.M. & Wallis, J.R., 1997. Regional frequency analysis: an approach based 
#' on L-Moments. Cambridge University Press, New York, USA.
#'
#' @importFrom stats runif optim
#' @export

select_zstatistic <- function(data, samples = 10000L) {

	data <- validate_numeric("data", data, optional = FALSE)
	samples <- validate_integer("samples", samples, bounds = c(1, Inf))

	# Helper function that attempts to fit a Kappa distribution and draw a bootstrap
	get_bootstrap <- function(data) {

		# Get the sample L-moments for the data
		moments <- lmom_sample(data)
		sample_l1 <- moments[1]
		sample_l2 <- moments[2]
		sample_t3 <- moments[3]
		sample_t4 <- moments[4]

		# Check that the smaple L-moments are viable
		if (sample_t4 > (1 + 5 * sample_t3^2) / 6) {
			return (NULL)
		}

		# Attempt to fit the Kappa distribution
		params <- fit_lmom_kappa(data)$params

		# Generate a bootstrap from this Kappa distribution
		t4_list <- lapply(1:samples, function(i) {
			u <- runif(length(data))	
			lmom_sample(quantile_kap(u, params))[4]
		})

		t4 <- as.numeric(t4_list)

		# Compute bias and standard deviation of t4 estimates
		bias_t4 <- mean(t4 - sample_t4)
		std_t4 <- sqrt(mean((t4 - sample_t4)^2) - bias_t4^2)

		# Return the results as a list
		list(
			params = params,
			sample_t3 = sample_t3,
			sample_t4 = sample_t4,
			t4 = t4,
			bias_t4 = bias_t4,
			std_t4 = std_t4
		)

	}

	# Attempt to fit kappa distribution to the data
	reg_bootstrap <- get_bootstrap(data)

	# Attempt to fit kappa distribution to log(data)
	log_bootstrap <- get_bootstrap(log(data))

	# Initialize list of metrics
	metrics <- list()

	# Iterate through the list of three parameter distributions
	for (distribution in c("GEV", "GLO", "GNO", "PE3", "LP3", "WEI")) {

		# Get distribution information
		info <- model_info(distribution)

		# Get the correct bootstrap
		if (info$log & !is.null(log_bootstrap)) {
			b <- log_bootstrap		
		} else if (!info$log & !is.null(reg_bootstrap)) {
			b <- reg_bootstrap
		} else {
			next
		}

		# Find the shape parameter with the same L-skewness as the data
		objective <- function(i) {
			distribution_t3 <- lmom_fast(distribution, c(0, 1, i))[3]
			abs(distribution_t3 - b$sample_t3)
		}

		# Run optimization for three parameter distributions
		result <- optim(
			par = (info$k_bounds[1] + info$k_bounds[2]) / 2,
			fn = objective,
			method = "Brent",
			lower = info$k_bounds[1],
			upper = info$k_bounds[2]
		)

		# Get the parameters of the fitted distribution and compute the z-score
		distribution_t4 <- lmom_fast(distribution, c(0, 1, result$par))[4]
		z <- (distribution_t4 - b$sample_t4 + b$bias_t4) / b$std_t4
		metrics[[distribution]] <- z

	}

	# Determine the recommendation (distribution with lowest L-distance)
	index <- which.min(abs(unlist(metrics)))
	recommendation <- names(metrics)[index]

	# Return the results as a list
	list(
		method = "Z-statistic",
		data = data,
		metrics = metrics,
		recommendation = recommendation,
		reg_params = reg_bootstrap$params,
		reg_bias_t4 = reg_bootstrap$bias_t4,
		reg_std_t4 = reg_bootstrap$std_t4,
		log_params = log_bootstrap$params,
		log_bias_t4 = log_bootstrap$bias_t4,
		log_std_t4 = log_bootstrap$std_t4
	)

}
