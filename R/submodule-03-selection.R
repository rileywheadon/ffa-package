# Helper function for distribution selection
submodule_03 <- function(
	data,
	years,
	options,
	splits,
	structures,
	path = NULL,
	serialize = FALSE
) {

	# Get the homogeneous periods as (start, end) pairs
	periods <- splits_helper(splits, years)

	# If 'structures' is NULL, convert it to a list of stationary structures
	structures <- structures_helper(structures, periods)

	# Return the results as a list of lists (one for each period)
	lapply(seq_along(periods), function(i) { 
		
		# Get the current period and structure
		period <- periods[[i]]
		structure <- structures[[i]]

		# Subset data and years based on period
		idx <- which(years >= period[1] & years <= period[2])
		data_subset <- data[idx]
		years_subset <- years[idx]

		# Run distribution selection
		results <- if (options$selection == "L-distance") {
			select_ldistance(data_subset, years_subset, structure)
		} else if (options$selection == "L-kurtosis") {
			select_lkurtosis(data_subset, years_subset, structure)
		} else if (options$selection == "Z-statistic") {
			select_zstatistic(data_subset, years_subset, structure, options$z_samples)
		} 

		# Preset distribution case
		else {
			results <- select_ldistance(data_subset, years_subset, structure)
			results$method <- "Preset"
			results$recommendation <- options$selection
			results
		}

		# Generate the plot and optionally write and/or serialize it
		pdf(nullfile())
		plot <- plot_lmom_diagram(results)

		if (!is.null(path)) {
			write_plot(plot, path, "selection", period)
		}

		if (serialize) {
			results$plot <- serialize_plot(plot) 
		}

		# Return results as a list
		list(
			name = "Distribution Selection",
			start = period[1], 
			end = period[2], 
			selection = results
		)

	})
}
