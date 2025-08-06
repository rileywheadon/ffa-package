#' Wald–Wolfowitz Runs Test for Randomness
#'
#' Applies the Wald–Wolfowitz runs test to a numeric vector in order to assess 
#' whether they behave as a random sequence. The test statistic and p-value 
#' is computed using the number of runs (sequences of values above or below the
#' median). Under the null hypothesis, the data is random.
#' 
#' @param values A numeric vector of values to check for randomness. 
#' @inheritParams param-alpha
#'
#' @return A list containing the test results, including:
#' - `values`: The `values` argument.
#' - `alpha`: The significance level as specified in the `alpha` argument.
#' - `null_hypothesis`: A string describing the null hypothesis.
#' - `alternative_hypothesis`: A string describing the alternative hypothesis.
#' - `n`: The length of the input vector after removing the median.
#' - `runs`: The number of runs in the transformed sequence of residuals.
#' - `statistic`: The runs test statistic, computed using `runs` and `n`.
#' - `p_value`: The p-value derived from the normally distributed test statistic.
#' - `reject`: If `TRUE`, the null hypothesis was rejected at significance `alpha`.
#'
#' @references
#' Wald, A. and Wolfowitz, J. (1940). On a test whether two samples are from the 
#' same population. Annals of Mathematical Statistics, 11(2), 147–162.
#'
#' @seealso [plot_runs_test()], [eda_sens_trend()]
#'
#' @examples
#' data <- rnorm(n = 100, mean = 100, sd = 10)
#' years <- seq(from = 1901, to = 2000)
#' sens <- eda_sens_trend(data, years)
#' eda_runs_test(sens)
#'
#' @export

eda_runs_test <- function(values, alpha = 0.05) {

	values <- validate_numeric('values', values)
	alpha <- validate_float("alpha", alpha, bounds = c(0.01, 0.1))

	# Remove values that are equal to the median
	filtered_values <- values[values != median(values)]

	# Transform the values into a boolean vector (TRUE if >median, FALSE if <median)	
	boolean_values <- (filtered_values > median(filtered_values))

	# Count the number of data points in each category
	n <- length(boolean_values)
	np <- sum(boolean_values == TRUE)
	nm <- sum(boolean_values == FALSE)

	# Determine the number of "runs" (contiguous blocks of + or -) in the data
	runs <- length(rle(boolean_values)$values)
	
	# Compute the distribution parameters under the null hypothesis
	mu <- (2 * np * nm / n) + 1
	sigma <- sqrt((2 * np * nm * (2 * np * nm - n)) / ((n^2) * (n - 1)))
	
	# Compute the p-value of the two-sided runs test
	z <- (runs - mu) / sigma
	p_value <- 2 * (1 - pnorm(abs(z)))

	# Determine whether we reject or fail to reject based on p_value and alpha
	reject <- (p_value <= alpha)

	# Return the results as a list
	list(
		values = values,
		alpha = alpha,
		null_hypothesis = "The input vector is random.",
		alternative_hypothesis = "The input vector is not random.",
		n = n,
		runs = runs,
		statistic = z,
		p_value = p_value,
		reject = reject
	)

}

