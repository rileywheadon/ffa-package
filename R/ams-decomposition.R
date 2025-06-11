ams.decomposition <- function(df, scenario) {

	# Compute the covariate
	n <- nrow(df)
	covariate <- ((1:n) - 1) / (n - 1)

	# Replace the 'year' column in df with the covariate
	df$year <- covariate

	# Scenario 1: Trend in the AMS means. 
	if (scenario == 1) {
		model <- sens.trend(df$max, df$year)
		decomposed <- df$max - (covariate * model$sens.slope)
	}

	# Scenario 2: Trend in the AMS variance.
	else if (scenario == 2) {

		# Get the moving window SD estimates
		df_variance <- mw.variance(df)

		# Run Sen's trend estimator and decompose the data
        model <- sens.trend(df_variance$std, df_variance$year)
		c0 <- model$sens.intercept
		c1 <- model$sens.slope
        gt <- ((c1 * covariate) + c0) / c0
		mu <- mean(df$max, na.rm = TRUE)
        decomposed <- mu + ((df$max - mu) / gt)
		
	}

	# Scenario 3: Trend in AMS means and variance.
	else if (scenario == 3) {

		# Run Sen's trend estimator and remove the trend in the means
		model_means <- sens.trend(df$max, df$year)
		decomposed <- df$max - (covariate * model_means$sens.slope)

		# Get the moving window SD estimates
		df_decomposed <- data.frame(max = decomposed, year = covariate)
		df_variance <- mw.variance(df_decomposed)

		# Run Sen's trend estimator and decompose the data
        model_variance <- sens.trend(df_variance$std, df_variance$year)
		c0 <- model_variance$sens.intercept
		c1 <- model_variance$sens.slope
        gt <- ((c1 * covariate) + c0) / c0
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
