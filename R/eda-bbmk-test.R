#' Block-Bootstrap Mann-Kendall Test for Trend Detection
#'
#' Performs a bootstrapped version of the Mann-Kendall trend test to account
#' for serial correlation in annual maximum streamflow (AMS) data. The procedure
#' uses Spearman’s autocorrelation test to estimate the least insignificant lag, 
#' then applies a bootstrap procedure to obtain the empirical p-value and confidence 
#' bounds for the Mann-Kendall S-statistic.
#'
#' @inheritParams param-data
#' @inheritParams param-alpha
#' @inheritParams param-samples
#' @inheritParams param-quiet
#'
#' @return List; the results of the test, including:
#' - `data`: The `data` argument.
#' - `s_bootstrap`: Vector of bootstrapped test statistics used for plotting.
#' - `s_statistic`: The Mann-Kendall test statistic computed on the original series.
#' - `p_value`: Empirical two-sided p-value computed from the bootstrap distribution.
#' - `bounds`: Confidence interval bounds for the null distribution of the statistic.
#' - `reject`: Logical. If `TRUE`, the null hypothesis was rejected at `alpha`.
#' - `msg`: Character string summarizing the test result (printed if `quiet = FALSE`).
#'
#' @details
#' The block size for the bootstrap is selected as `least_lag + 1`, where `least_lag` 
#' is estimated using \link{eda_spearman_test}. Each bootstrap sample is generated by 
#' resampling blocks of the original data  (without replacement) and computing the 
#' Mann-Kendall S-statistic. This procedure adjusts for autocorrelation in the data. 
#'
#' @seealso \link{plot_bbmk_test}, \link{eda_mk_test}, \link{eda_spearman_test}
#'
#' @examples
#' data <- rnorm(n = 100, mean = 100, sd = 10)
#' eda_bbmk_test(data, samples = 1000L)
#'
#' @importFrom stats quantile
#' @export

eda_bbmk_test <- function(data, alpha = 0.05, samples = 10000L, quiet = TRUE) {

	data <- validate_numeric("data", data, bounds = c(0, Inf))
	alpha <- validate_float("alpha", alpha, bounds = c(0.01, 0.1))
	samples <- validate_integer("samples", samples, bounds = c(1, Inf))
	quiet <- validate_logical("quiet", quiet)

	least_lag <- eda_spearman_test(data, alpha)$least_lag
	s_statistic  <- eda_mk_test(data, alpha)$s_statistic

	# Blocks have size of least_lag to remove autocorrelation 
	n <- length(data)
	size <- least_lag
	n_blocks <- ceiling(n / size)
	blocks <- split(data[1:(n_blocks * size)], rep(1:n_blocks, each = size))

	bootstrap_results <- sapply(1:samples, function(i) { 

		# Sample blocks without replacement
		sampled_blocks <- sample(blocks, n_blocks, replace = FALSE)
		sampled_data <- unlist(sampled_blocks, use.names = FALSE)

		# Compute the Mann-Kendall statistic for the bootstrap
		m <- outer(sampled_data, sampled_data, `-`)                   
  		sum(sign(m[lower.tri(m)]), na.rm = TRUE)

	})

	# The p-value is computed empirically using the bootstrap distribution
	p_value <- ifelse(
		s_statistic < 0, 
		2 * mean(s_statistic >= bootstrap_results),
		2 * mean(s_statistic <= bootstrap_results)
	)

	# The confidence interval bounds are determined empirically from the bootstrap
	bounds <- quantile(bootstrap_results, c(alpha / 2, 1 - (alpha / 2)))

	reject <- (p_value <= alpha)

	msg <- stats_message(
		"BBMK",
		reject,
		p_value,
		alpha,
		"evidence of a trend given the serial correlation.",
		"NO evidence of a trend given the serial correlation."
	)

	if (!quiet) message(msg)

	list(
		data = data,
		s_bootstrap = bootstrap_results,
		s_statistic = s_statistic,
		p_value = p_value,
		bounds = bounds,
		reject = reject,
		msg = msg
	)

}

