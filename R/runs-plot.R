#' Plot Results from the Runs Test

runs.plot <- function(df, results, name, show_trend = TRUE) {

	# Set labels and data based on the name
	if (name == "sens-variance") {
		data <- df$std
		title <- "Sen's Estimator Residuals (AMS Variance)"
	} else {
		data <- df$max
		title <- "Sen's Estimator Residuals (AMS Mean)"
	}

	# Generate dataframes for the trend estimate, data, and residuals
	df_residuals <- data.frame(x = df$year, y = results$residuals)

	# First subplot: Plot of residuals
	p1 <- ggplot(df_residuals, aes(x = x, y = y)) +
		geom_point(color = "black", size = 2.25) + 
	    geom_hline(yintercept = 0, color = "red", linetype = "dashed", linewidth = 1.2) +
		labs(title = "Residual Plot", x = "Year", y = "Residual Value") 

	# Add p-value annotation
	runs_label <- sprintf("Runs p-value: %.3f", results$p.value)
	p1 <- add_annotation(p1, runs_label)

	# Return the plot with added theme
	add_theme(add_scales(p1))

}

 
