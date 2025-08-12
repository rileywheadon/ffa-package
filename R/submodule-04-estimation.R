# Helper function for trend detection
submodule_04 <- function(
	data,
	years,
	distributions,
	options,
	splits,
	structures,
	path = NULL,
	serialize = FALSE
) {

	# Get the homogeneous periods as (start, end) pairs
	periods <- splits_helper(splits, years)

	# If structures is NULL, convert it to a list of stationary structures
	structures <- structures_helper(structures, periods)

	# Return the results as a list of lists (one for each period)
	lapply(seq_along(periods), function(i) { 

		# Get the current period, structure, and distribution
		period <- periods[[i]]
		distribution <- distributions[[i]]
		structure <- structures[[i]]

		# Subset data and years based on period
		idx <- which(years >= period[1] & years <= period[2])
		data_subset <- data[idx]
		years_subset <- years[idx]

		# Run parameter estimation
		if (!structure$location && !structure$scale) {
			estimation_method <- options$s_estimation
		} else {
			estimation_method <- options$ns_estimation
		}

		results <- if (estimation_method == "L-moments") {
			fit_lmoments(data_subset, distribution)
		} else if (estimation_method == "MLE") {
			fit_mle(data_subset, distribution, years_subset, structure)
		} else if (estimation_method == "GMLE") {
			fit_gmle(data_subset, options$gev_prior, years_subset, structure)
		}

		# Generate the plot and optionally write and/or serialize it
		if (!structure$location && !structure$scale) {
			plot <- plot_sffa_fit(results)
		} else {
			plot <- plot_nsffa_fit(results)
		}

		if (!is.null(path)) {
			write_plot(plot, path, "estimation", period)
		}

		if (serialize) {
			results$plot <- serialize_plot(plot) 
		}

		# Return results as a list
		list(
			name = "Parameter Estimation",
			start = period[1], 
			end = period[2], 
			estimation = results
		)

	})
}
