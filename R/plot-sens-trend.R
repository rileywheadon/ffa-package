#' Plot Sen’s Trend Estimator
#'
#' Produces a scatterplot of the annual maximum series (AMS) data or its variance
#' against time, optionally overlaid with Sen’s trend estimator of the mean and/or
#' variability.
#'
#' @inheritParams param-data
#' @inheritParams param-years
#'
#' @param mean_trend Trend in the mean estimated by by [eda_sens_trend()].
#'
#' @param variability_trend Trend in variability estimated by [eda_sens_trend()].
#'
#' @param show_line If `TRUE` (default), draw a fitted line through the AMS data.
#'
#' @param ... Optional named arguments: 'title', 'xlabel', and 'ylabel'.
#'
#' @return `ggplot`; a plot containing:
#' - Gray points for each year’s AMS value.
#' - Optional gray line connecting the AMS data if `show_line = TRUE`.
#' - A solid black line representing a constant mean, if `mean_trend == NULL`.
#' - A solid blue line representing a trend in the mean, if `mean_trend != NULL`.
#' - A dashed black line representing constant variability, if `variability_trend == NULL`.
#' - A dashed blue line representing a trend in variability, if `variability_trend != NULL`.
#' - The equation for the trend in the mean, written in the form \eqn{mx + b}.
#'
#' @seealso [eda_sens_trend()]
#'
#' @examples
#' data <- rnorm(n = 100, mean = 100, sd = 10)
#' years <- seq(from = 1901, to = 2000)
#' mean_trend <- eda_sens_trend(data, years)
#' plot_sens_trend(data, years, meant_trend = mean_trend)
#'
#' @import ggplot2
#' @importFrom grid textGrob
#' @export

plot_sens_trend <- function(
	data,
	years,
	mean_trend = NULL,
	variability_trend = NULL,
	show_line = TRUE, 
	...
) {

	# Get the equation for the trend in the mean
	if (!is.null(mean_trend)) {
		m1 <- mean_trend$slope
		m2 <- mean_trend$intercept
	} else {
		m1 <- 0
		m2 <- mean(data)
	} 

	equation <- sprintf("Mean: %.3fx + %.2f", m1, m2)

	# Get the equation for the trend in the variance
	if (!is.null(variability_trend)) {
		v1 <- variability_trend$slope
		v2 <- variability_trend$intercept
	} else {
		v1 <- 0
		v2 <- sd(data)
	} 

	# Capture optional arguments
	args <- list(...)

	# Set labels and title 
	title <- args$title %||% "Sen's Trend Estimator"
	xlabel <- args$xlabel %||% "Covariate"
	ylabel <- args$ylabel %||% expression(AMS ~ m^3/s)

	# Generate dataframes for the trend estimate, data
	x <- get_covariates(years)

	df <- data.frame(
		x = x,
		y_data = data,
		y_upper = (m1 * x) + m2 + ((v1 * x) + v2),
		y_trend = (m1 * x) + m2,
		y_lower = (m1 * x) + m2 - ((v1 * x) + v2)
	)

	# Plot of Sen's trend estimator
	p <- ggplot(df, aes(x = .data$x)) +
		geom_point(aes(y = .data$y_data), color = "dimgray", size = 2.25) + 
		geom_line(
			aes(y = .data$y_data),
			color = "dimgray",
			linewidth = if (show_line) 1 else 0
		) +
		geom_line(
			aes(y = .data$y_trend),
			color = if (is.null(mean_trend)) "black" else "blue",
			linewidth = 1.2
		) +
		geom_line(
			aes(y = .data$y_upper),
			color = if (is.null(variability_trend)) "black" else "blue",
			linetype = "dashed",
			linewidth = 1.2
		) + geom_line(
			aes(y = .data$y_lower),
			color = if (is.null(variability_trend)) "black" else "blue",
			linetype = "dashed",
			linewidth = 1.2
		) +
		labs(title = title, x = xlabel, y = ylabel)

	# Add equation annotation, scales, and theme
	p <- add_annotation(p, equation)
	add_theme(add_scales(p))

}

 
