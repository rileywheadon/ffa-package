#' Plot Results from the BB-MK Test

bbmk.plot <- function(results) {

	p1 <- ggplot() +
		geom_histogram(
			aes(x = results$s.bootstrap, color = "gray"), 
			fill = "lightgray",
			bins = 30
		)  +
		geom_vline(aes(xintercept = results$bounds, color = "red"), linewidth = 1.2) + 
		geom_vline(aes(xintercept = results$s.statistic, color = "black"), linewidth = 1.2) + 
		labs(
			title = "Block-Bootstrap Mann-Kendall Test",
			x = "S-Statistic",
			y = "Frequency",
			color = "Legend"
		) + 
		scale_color_manual(
			values = c("gray" = "gray", "black" = "black", "red" = "red"),
			breaks = c("gray", "black", "red"),
			labels = c("Bootstrapped Statistics", "S-Statistic", "Confidence Bounds"),
		)

	# Return the plot with added theme
	add_theme(p1)

}
