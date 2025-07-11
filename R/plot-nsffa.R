#' Plot Return Levels and Confidence Intervals for NS-FFA
#'
#' Constructs a time–series plot of estimated return levels and their confidence 
#' intervals for up to five slices. Each slice is displayed in a distinct color.
#' Confidence bounds are shown as semi-transparent ribbons, and the point estimates 
#' are overlaid as solid lines. Return periods are shown on a logarithmic scale.
#'
#' @param results A list of up to five result objects, each a list containing 
#' `periods`, `estimates`, `ci_lower`, `ci_upper`, and a `slice` year.
#' 
#' @param ... Optional named arguments: 'title', 'xlabel', and 'ylabel'.
#'
#' @return `ggplot`; a plot with one line and ribbon per slice.
#'
#'
#' @examples
#'
#' data <- rnorm(n = 100, mean = 100, sd = 10)
#' years <- seq(from = 1901, to = 2000)
#'
#' # Run the uncertainty bootstrap at slices 1920, 1960, 2000
#' results <- uncertainty_bootstrap(
#' 	   data,
#'	   "GEV",
#'	   "MLE",
#'	   years = years,
#'	   trend = list(location = TRUE, scale = FALSE),
#'	   slices = c(1920, 1960, 2000),
#'	   samples = 1000L
#' )
#'
#' # Generate the plot
#' plot_nsffa(results)
#'
#' @import ggplot2
#' @export

plot_nsffa <- function(results, ...) {

    # Capture optional arguments
    args <- list(...)
    title  <- args$title  %||% "NS-FFA Results"
    xlabel <- args$xlabel %||% "Effective Return Period (Years)"
    ylabel <- args$ylabel %||% expression("AMS (" * m^3/s * ")")
  
    # Define color palette
    palette <- c("#541352", "#3A5E8C", "#2F9AA0", "#10A53D", "#FFCF20")
  
    # Build combined data frame with an identifier for each slice
    df_list <- lapply(seq_along(results), function(i) {
        r <- results[[i]]
        data.frame(
            periods   = r$periods,
            estimates = r$estimates,
            ci_lower  = r$ci_lower,
            ci_upper  = r$ci_upper,
            slice     = as.character(r$slice)
        )
    })
  
    df_all <- do.call(rbind, df_list)
  
    # Create plot
    p <- ggplot(df_all, aes(x = .data$periods)) +
        geom_ribbon(
			aes(ymin = .data$ci_lower, ymax = .data$ci_upper, fill = .data$slice),
			alpha = 0.2
		) +
        geom_line(
			aes(y = .data$estimates, color = .data$slice), 
			linewidth = 1
		) +
        scale_color_manual(values = palette[seq_along(results)]) +
        scale_fill_manual(values = palette[seq_along(results)]) +
        scale_x_log10(breaks = unique(df_all$periods)) +
        labs(title = title, x = xlabel, y = ylabel, color = "Return Levels") +
  		guides(fill = "none")
  
    # Add theme and return
    add_theme(p)
}

