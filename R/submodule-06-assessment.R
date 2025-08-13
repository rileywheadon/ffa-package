# Helper function for trend detection
submodule_06 <- function(
	data,
	years,
	distributions,
	intervals,
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

		# Get the current period, structure, confidence interval, and distribution
		period <- periods[[i]]
		distribution <- distributions[[i]]
		interval <- intervals[[i]]
		structure <- structures[[i]]

		# Subset data and years based on period
		idx <- which(years >= period[1] & years <= period[2])
		data_subset <- data[idx]
		years_subset <- years[idx]

		# Get the parameter estimation function
		if (!structure$location && !structure$scale) {
			estimation_method <- options$s_estimation
		} else {
			estimation_method <- options$ns_estimation
		}

		# Run model assessment
		results <- model_assessment(
			data_subset,
			distribution,
			estimation_method,
			prior = if (estimation_method == "GMLE") options$gev_prior else NULL,
			ns_years = years_subset,
			ns_structure = structure,
			alpha = options$alpha,
			pp_formula = options$pp_formula,
			ci = interval
		)

		# Generate model assessment plot
		if (!structure$location && !structure$scale) {
			plot <- plot_sffa_assessment(results)

			if (!is.null(path)) {
				write_plot(plot, path, "assessment", period)
			}

			if (serialize) {
				results$plot <- serialize_plot(plot) 
			}
		} 

		# Return results as a list
		list(
			name = "Model Assessment",
			start = period[1],
			end = period[2],
			assessment = results
		)

	})
}
