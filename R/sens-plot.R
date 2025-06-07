#' Plot Results from Sen's Trend Estimator

sens.plot <- function(df, results, name, show_trend = TRUE) {

	# Load the results of the test into the environment
	m <- results$sens.slope
	b <- results$sens.intercept

	# Set labels and data based on the name
	if (name == "sens-variance") {
		data <- df$std
		ams_label <- expression(AMS ~ Variance ~ m^3/s)
		title <- "Sen's Trend Estimator (AMS Variance)"
	} else {
		data <- df$max
		ams_label <- expression(AMS ~ m^3/s)
		title <- "Sen's Trend Estimator (AMS Mean)"
	}

	# Generate dataframes for the trend estimate, data
	df_line <- data.frame(x = df$year, y = m * (df$year) + b)
	df_data <- data.frame(x = df$year, y = data)

	# Generate the equation and runs test label 
	eq_label <- sprintf("y = %.3fx + %.2f", m, b)

	# First subplot: Plot of Sen's trend estimator
	p1 <- ggplot(df_data, aes(x = x, y = y)) +
		geom_point(aes(color = "black"), size = 2.25) + 
		(if (show_trend) geom_line(color = "black", linewidth = 1.1) else NULL) +
		geom_line(data = df_line, aes(x = x, y = y, color = "blue"), linewidth = 1.2) + 
		labs(title = title, x = "Year", y = ams_label, color = "Legend") + 
		scale_color_manual(
			values = c("blue" = "blue", "black" = "black"),
			breaks = c("blue", "black"),
			labels = c("Estimated Trend", ams_label),
		) 

	# Add equation annotation, scales, and theme
	p1 <- add_annotation(p1, eq_label)
	add_theme(add_scales(p1))

}

 
