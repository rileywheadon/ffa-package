#' Plot L-Moment Ratio Diagram
#'
#' @description
#' Generates a plot of L-moment ratios with the L-skewness on the x-axis and L-kurtosis
#' on the y-axis. Plots the sample and log-sample L-moment ratios alongside the theoretical 
#' L-moment ratios for a set of candidate distributions. If the selection method is 
#' \link{ld.selection} or \link{lk.selection}, the plot will include a small inset around 
#' the L-moment ratios of the recommended distribution.
#'
#' @param data Numeric; a vector of annual maximum streamflow data.
#'
#' @param results List; output of \link{ld.selection}, \link{lk.selection}, 
#'   or \link{z.selection}.
#'
#' @return \code{ggplot}; plot object containing the L-moment ratio diagram, with:
#' - L-moment ratio curves for each 3-parameter distribution.
#' - Points for the L-moment ratios of each 2-parameter distribution.
#' - Sample and log-sample L-moment ratio \eqn{(t_3, t_4)} points.
#'
#' @seealso \link{ld.selection}, \link{lk.selection}, \link{z.selection}
#'
#' @examples
#' data <- rnorm(n = 100, mean = 100, sd = 10)
#' results <- ld.selection(data)
#' lmom.plot(data, results)
#'
#' @import ggplot2
#' @importFrom grDevices pdf
#' @export

lmom.plot <- function(data, results) {

	# Get method name from the results
	method <- results$method

	# Create dataframes for the sample L-moments
	reg_sample_t3 = lmom.sample(data)[3]
	reg_sample_t4 = lmom.sample(data)[4]
	log_sample_t3 = lmom.sample(log(data))[3]
	log_sample_t4 = lmom.sample(log(data))[4]

	reg_lm <- data.frame(x = reg_sample_t3, y = reg_sample_t4)
	log_lm <- data.frame(x = log_sample_t4, y = log_sample_t4)

	# Generate a dataframe with the moments for each distribution
	dlm <- list()

	for (model in c("GUM", "NOR", "LNO", "GEV", "GLO", "GNO", "PE3", "LP3", "WEI")) {

		info <- models.info(model)

		if (info$n.params == 2) {
			dm <- lmrxxx(model, c(0, 1))
			dlm[[model]] = data.frame(x = dm[3], y = dm[4])
		}

		else {

			# Generate a sequence of parameter sets to pass to a 3-parameter distribution
			k_seq <- seq(info$k.bounds[1], info$k.bounds[2], 0.001)
			params <- lapply(k_seq, function(i) c(0, 1, i))

			# Get a matrix of likelihood moment ratios for each parameter set in params
			lmr <- lapply(params, function(p) suppressWarnings(lmrxxx(model, p)))
			lmr <- do.call(rbind, lmr)

			# Return the t3 and t4 values as a dataframe
			df = data.frame(x = lmr[, 3], y = lmr[, 4]) 
			dlm[[model]] <- df[(!is.na(df$x) & !is.na(df$y)), ]

		}

	}

	# Initilaize the legend information
	labels <- c("Sample", "Log-Sample", "GEV/GUM", "GLO", "GNO/NOR/LNO", "PE3/LP3", "WEI")
	colors <- c("#000000", "#000000", "#e69f00", "#56b4e9", "#009e73", "#0072b2", "#d55e00")
	shapes <- c(24, 15, 16, NA, 16, NA, NA)

	# Define labels for the plot
	x_label <- expression("L-skewness (" * tau[3] * ")")
	y_label <- expression("L-kurtosis (" * tau[4] * ")")
	title <- paste(method, "Model Selection", sep = " ")

	# Generate the plot
	p1 <- ggplot(mapping = aes(x = .data$x, y = .data$y)) +
		geom_line(data = dlm$GEV, aes(color = "3"), linewidth = 1) +
		geom_line(data = dlm$GLO, aes(color = "4"), linewidth = 1) +
		geom_line(data = dlm$GNO, aes(color = "5"), linewidth = 1) +
		geom_line(data = dlm$PE3, aes(color = "6"), linewidth = 1) +
		geom_line(data = dlm$WEI, aes(color = "7"), linewidth = 1) +
		geom_point(data = dlm$GUM, aes(color = "3"), shape = 16, size = 4) +
		geom_point(data = dlm$NOR, aes(color = "5"), shape = 16, size = 4) +
		geom_point(data = reg_lm, aes(color = "1"), shape = 24, size = 5, fill = "black") +
		geom_point(data = log_lm, aes(color = "2"), shape = 15, size = 5) +
		scale_color_manual(labels = labels, values = colors) +
		guides(color = guide_legend(override.aes = list(shape = shapes))) +
		labs(x = x_label, y = y_label, color = "Legend", title = title) +
		coord_fixed(ratio = 2, xlim = c(-0.7, 0.7), ylim = c(0, 0.7))

	# Add the theme
	p1 <- add_theme(add_scales(p1))

	# If the method is l-distance or l-kurtosis, draw a line to show the distance
	if (method == "L-distance" | method == "L-kurtosis") {

		# Get the sample point with the shortest distance from a distribution
		point <- if (results$recommendation %in% c("LNO", "LP3")) log_lm else reg_lm

		# Get the recommended distribbution
		df <- dlm[[results$recommendation]]

		# Get the closest point on the (t3, t4) curve (L-distance)
		if (method == "L-distance") { 
			df$distance <- sqrt((df$x - point$x)^2 + (df$y - point$y)^2)
			df <- df[which.min(df$distance), ]
		}

		# Or get the point with the same t3 value (L-kurtosis)
		else {
			df <- df[which.min(abs(df$x - point$x)), ]
			df$distance <- abs(df$y - point$y)
		}

		# Dynamically set the radius of the magnified section
		r <- df$distance * 3

		# Draw the inset plot: same layers, zoomed to [x +/- r, y +/- r]
		p2 <- p1 +
			geom_segment(data = point, aes(xend = df$x, yend = df$y)) +
			coord_cartesian(
				xlim = c(point$x - r, point$x + r),
				ylim = c(point$y - r, point$y + r)
		    ) +
			theme_void() + 
			theme(
				legend.position = "none",
				panel.border = element_rect(colour = "black", fill = NA, linewidth = 1),
				plot.background = element_rect(colour = NA, fill = "white")
			)

		# Turn the inset plot into a grob
		pdf(nullfile())
		p2_grob <- ggplotGrob(p2)

		# Place inset on main plot using annotation_custom()
		p1 <- p1 +
			geom_segment(data = point, aes(xend = 0.25, yend = 0.35)) +
			geom_segment(data = point, aes(xend = -0.25, yend = 0.35)) +
		    annotation_custom(
				grob = p2_grob,
				xmin = -0.25, 
				xmax = 0.25,
				ymin = 0.35, 
				ymax = 0.60
			)

	}

	# Return the plot
	p1

}
