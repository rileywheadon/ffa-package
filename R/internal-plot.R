# Adds sensible axis scales to a plot
add_scales <- function(p) {
	p + 
	scale_x_continuous(breaks = function(limits) pretty(limits, n = 10)) +
	scale_y_continuous(breaks = function(limits) pretty(limits, n = 10))
}


# Adds sensible styling to a plot
add_theme <- function(p) {
	p +
	theme_minimal() +
	theme(
		plot.background = element_rect(fill = "white", color = NA),
		plot.title = element_text(size = 20, hjust = 0.5),
		plot.margin = margin(5, 15, 5, 15),
		axis.title = element_text(size = 16),
		axis.text = element_text(size = 12),
		panel.grid.minor = element_blank(),
		legend.title = element_text(hjust = 0.5),
		legend.background = element_rect(fill = "white", color = "black"),
		legend.box.background = element_rect(color = "black"),
		legend.direction = "vertical"
	)
}


# Adds a nicely formatted annotation to the top right corner of a plot
add_annotation <- function(p, label) {

	grob <- textGrob(label)
	rect_w <- 0.22
	rect_h <- 0.04

	p + annotation_custom(
		grob = grobTree(
			rectGrob(
				x = 0.95 - (rect_w / 2), y = 0.95,
				hjust = 0.5, vjust = 0.5,
				width = rect_w,
				height = rect_h,
				gp = gpar(fill = "white", col = "black")
			),
			textGrob(
				label,
				x = 0.95 - (rect_w / 2), y = 0.95,
				hjust = 0.5, vjust = 0.5 ,
				gp = gpar(col = "black", fontsize = 10)
			)
		),
		xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf
	)

}


