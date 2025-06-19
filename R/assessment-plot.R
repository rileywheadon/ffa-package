#' Plot Model vs. Observed Quantiles for Assessment Results
#'
#' @description
#' Creates a quantile–quantile plot comparing observed annual maximum series (AMS)
#' to model‐derived quantile estimates. The 1:1 line is drawn in black, and the
#' model estimates are overplotted as semi‐transparent red points.
#'
#' @param data Numeric vector of observed annual maximum series.
#' @param assessment List containing assessment results. See \code{\link{model.assessment}}.
#'
#' @return A \code{ggplot} object showing:
#'   \itemize{
#'     \item A black line for the theoretical 1:1 relationship between observed and model quantiles.
#'     \item Red points marking the model’s estimated quantiles against the observed quantiles.
#'   }
#'
#' @details
#' The function orders \code{data} in decreasing order to define the observed quantiles,
#' then binds the corresponding \code{assessment$estimates} into a data frame. Axis labels
#' are rendered with expression notation (cubic meters per second). 
#'
#' @seealso
#' \code{\link[ggplot2]{geom_line}}, \code{\link[ggplot2]{geom_point}}
#'
#' @import ggplot2
#' @export

assessment.plot <- function(data, assessment) {

	# Create a dataframe for the plot
	x <- data[order(data, decreasing = TRUE)]   
	df <- data.frame(x = x, y = x, estimates = assessment$estimates) 

	# Get labels for the plot
	x_label <- expression(Model ~ Quantiles ~ m^3/s)
	y_label <- expression(Observed ~ Quantiles ~ m^3/s)

	# Generate the plot
	p1 <- ggplot(data = df) +
		geom_line(aes(x = x, y = .data$y), linewidth = 1.1) + 
		geom_point(aes(x = x, y = .data$estimates), color = "red", size = 3, alpha = 0.5) + 
		labs(x = x_label, y = y_label)

	# Add theme and scales and return
	add_theme(add_scales(p1))

}
