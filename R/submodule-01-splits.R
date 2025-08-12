# Helper function for running change point detection
submodule_01 <- function(
	data,
	years,
	options,
	path = NULL,
	serialize = FALSE
) {

	# Change point detection has no split points
    data <- as.numeric(unlist(data))
    years <- as.integer(unlist(years))
	period <- c(min(years), max(years))

	# Run the Pettitt and MKS tests
	pettitt <- eda_pettitt_test(data, years, options$alpha)
	mks <- eda_mks_test(data, years, options$alpha)

	# Save the plots (if a path is provided)
	if (!is.null(path)) {
		pettitt_plot <- plot_pettitt_test(pettitt)
		mks_plot <- plot_mks_test(mks)
		write_plot(pettitt_plot, path, "pettitt", period)
		write_plot(mks_plot, path, "mks", period)
	}

	# Serialize the plots (for the web app)
	if (serialize) {
		pettitt$plot <- serialize_plot(plot_pettitt_test(pettitt))
		mks$plot <- serialize_plot(plot_mks_test(mks))
	}

	# Return the results as a list of lists
	list(
		list(
			name = "Change Point Detection",
			start = period[1],
			end = period[2],
			tests = list(pettitt = pettitt, mks = mks)
		)
	)

}
