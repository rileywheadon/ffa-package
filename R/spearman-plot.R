#' Plot Results of the Spearman Test

spearman.plot <- function(df, results, show_trend = TRUE) {

	# Load the results of the test into the environment
	list2env(results, envir = environment())

	# Create dataframe for generating the results
	rho_df <- data.frame(
		lag = 1:length(rho),
		rho = rho,
		sig = sig
	)

	# First subplot: Spearman's Rho Autocorrelation
	p1 <- ggplot(rho_df, aes(x = lag, y = rho)) +
		geom_segment(aes(x = lag, xend = lag, y = 0, yend = rho)) +
		geom_point(
			aes(fill = sig),
			shape = 21,
			size = 3,
			stroke = 1.2
		) +
		labs(
			title = "Spearman's \u03c1 Autocorrelation",
			x = "Lag",
			y = "Spearman's \u03c1",
			fill = "Legend"
		) + 
		scale_fill_manual(
			values = c(`TRUE` = "black", `FALSE` = "white"),
			labels = c("No Serial Correlation", "Serial Correlation"),
		)

	# Return the plot with added theme
	add_theme(add_scales(p1))

}

 
