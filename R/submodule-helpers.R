# Convert 'splits' argument into (start, end) periods
splits_helper <- function(splits, years) {
	starts <- c(min(years), as.numeric(splits))
	ends <- c(as.numeric(splits) - 1, max(years))
	Map(c, starts, ends)
}


# Convert 'structures' argument from NULL into list of stationary structures
structures_helper <- function(structures, periods) {
	if (is.null(structures)) {
		stationary <- list(location = FALSE, scale = FALSE)
		structures <- replicate(length(periods), stationary, simplify = FALSE)	
	}

	structures
}

# Write plot to file (for CLI-like behaviour)
write_plot <- function(plot, path, name, period) {
	buffer <- paste0(path, "/", name, "_", period[1], "_", period[2], ".png")
	ggsave(buffer, plot = plot, height = 8, width = 10, dpi = 300)
}

# Serialize plot (for the web app)
serialize_plot <- function(plot) {
	buffer <- tempfile(fileext = ".png")
	ggsave(buffer, plot = plot, height = 8, width = 10, dpi = 300)
	base64enc::dataURI(file = buffer, mime = "image/png")
}

