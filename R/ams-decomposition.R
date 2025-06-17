#' Decompose Annual Maximum Streamflow
#'
#' @description
#' Removes trends in the means and/or variances of an annual maximum streamflow (AMS)
#' data using Sen’s slope estimator and a moving‐window variance estimator. Three
#' scenarios are supported:
#' 1. Trend in means only  
#' 2. Trend in variance only  
#' 3. Trends in both means and variance  
#'
#' @param df A data frame containing:  
#'   `max`: numeric vector of annual maximum streamflow values.  
#'   `year`: any values (will be replaced internally by a scaled covariate).  
#'
#' @param scenario Integer (1, 2, or 3) indicating which trend component(s) to remove:  
#'   1: Remove linear trend in the mean.
#'   2: Remove trend in variance.
#'   3: Remove both mean and variance trends sequentially.  
#'
#' @return
#' A numeric vector of the same length as `df`, containing the “decomposed”
#' AMS values with the specified trend(s) removed and adjusted to be strictly
#' positive.  
#'
#' @details
#' Internally, the function:  
#' 1. Constructs a covariate linearly spaced on \code{[0,1]} over the record length.  
#' 2. For scenario 1, fits Sen’s slope (`sens.trend`) to (`max` vs. covariate)
#'    and removes the fitted linear mean trend.  
#' 3. For scenario 2, computes moving‐window standard deviations
#'    (`mw.variance`), fits Sen’s slope to those deviations, and rescales the
#'    series to remove variance trends around the overall mean.  
#' 4. For scenario 3, applies scenario 1 then scenario 2 sequentially.  
#' 5. Ensures all returned values are great than 1 by shifting the data if any negatives occur.  
#'
#' @export

ams.decomposition <- function(df, scenario) {

	# Replace the 'year' column in df with the covariate
	df$year <- get.covariates(df, df$year)

	# Scenario 1: Trend in the AMS means. 
	if (scenario == 1) {
		model <- sens.trend(df$max, df$year)
		decomposed <- df$max - (df$year * model$sens.slope)
	}

	# Scenario 2: Trend in the AMS variance.
	else if (scenario == 2) {

		# Get the moving window SD estimates
		df_variance <- mw.variance(df)

		# Run Sen's trend estimator and decompose the data
        model <- sens.trend(df_variance$std, df_variance$year)
		c0 <- model$sens.intercept
		c1 <- model$sens.slope
        gt <- ((c1 * df$year) + c0) / c0
		mu <- mean(df$max, na.rm = TRUE)
        decomposed <- mu + ((df$max - mu) / gt)
		
	}

	# Scenario 3: Trend in AMS means and variance.
	else if (scenario == 3) {

		# Run Sen's trend estimator and remove the trend in the means
		model_means <- sens.trend(df$max, df$year)
		decomposed <- df$max - (df$year * model_means$sens.slope)

		# Get the moving window SD estimates
		df_decomposed <- data.frame(max = decomposed, year = df$year)
		df_variance <- mw.variance(df_decomposed)

		# Run Sen's trend estimator and decompose the data
        model_variance <- sens.trend(df_variance$std, df_variance$year)
		c0 <- model_variance$sens.intercept
		c1 <- model_variance$sens.slope
        gt <- ((c1 * df$year) + c0) / c0
		mu <- mean(decomposed, na.rm = TRUE)
        decomposed <- mu + ((decomposed - mu) / gt)

	} 

	# Return an error if scenario is not 1, 2, or 3
	else {
		stop("Error: 'scenario' must be 1, 2, or 3.")
	}

	# Enforce positivity
	if (sum(decomposed < 0, na.rm = TRUE) > 0) {
		decomposed = decomposed - min(decomposed) + 1
	}

	# Return decomposed data
	decomposed

}
