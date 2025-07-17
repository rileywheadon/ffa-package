#' Plot Annual Maximum Series (AMS) Data
#'
#' @description
#' Generates a plot of annual maximum series data with an optional trend line.
#'
#' @inheritParams param-data
#' @inheritParams param-years
#'
#' @param show_line If `TRUE` (default), draw a line through the AMS data.
#'
#' @param ... Optional named arguments: 'title', 'xlabel', and 'ylabel'.
#'
#' @return \code{ggplot}; A plot containing:
#'   - Black dots at each (data, year) pair.
#'   - An optional line thorugh the data points (if `show_line == TRUE`)
#'
#' @examples
#' data <- rnorm(n = 100, mean = 100, sd = 10)
#' years <- seq(from = 1901, to = 2000)
#' plot_ams_data(data, years)
#'
#' @import ggplot2
#' @export
plot_ams_data <- function(data, years, show_line = TRUE, ...) {

	# Capture optional arguments
	args <- list(...)

	# Set labels and title
	title <- args$title %||% "Annual Maximum Series (AMS)"
	xlabel <- args$xlabel %||% "Years"
	ylabel <- args$ylabel %||% expression(AMS ~ m^3/s)

	# Generate dataframe for the data
	df <- data.frame(x = years, y = data)

	# Create a plot of the data
	p <- ggplot(df, aes(x = .data$x, y = .data$y)) +
		geom_point(color = "dimgray", size = 2.25) + 
		(if (show_line) geom_line(color = "dimgray", linewidth = 1) else NULL) +
		labs(title = title, x = xlabel, y = ylabel)

	add_theme(add_scales(p))

}

