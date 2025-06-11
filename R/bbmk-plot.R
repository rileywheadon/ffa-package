#' Plot Block‐Bootstrap Mann–Kendall Test Results
#'
#' @description
#' Generates a histogram of block‐bootstrap Mann–Kendall S‐statistics with vertical
#' lines indicating the empirical S‐statistic and confidence bounds.
#'
#' @param results A list containing the BB‐MK test output. See \code{\link{bbmk.test}}.
#'
#' @return A \code{ggplot} object showing:
#'   \itemize{
#'     \item A gray histogram of the distribution of bootstrapped S‐statistics.
#'     \item A red vertical line at each confidence bound.
#'     \item A black vertical line at the observed S‐statistic.
#'   }
#'
#' @details
#' The histogram uses 30 bins by default, with bootstrapped values in light gray.
#' The legend is custom‐mapped to distinguish “Bootstrapped Statistics” (gray),
#' “S-Statistic” (black), and “Confidence Bounds” (red). The plot title and axis
#' labels are set for clarity.
#'
#' @seealso
#' \code{\link[ggplot2]{geom_histogram}},
#' \code{\link[ggplot2]{geom_vline}}, 
#' \code{\link[ggplot2]{scale_color_manual}}
#'
#' @import ggplot2
#' @export

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
