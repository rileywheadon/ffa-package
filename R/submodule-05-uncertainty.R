# Helper function for trend detection
submodule_05 <- function(
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

		# Set nonstationary slices and uncertainty quantification method
		if (!structure$location && !structure$scale) {
			slices <- 0
			uncertainty_method <- options$s_uncertainty
			estimation_method <- options$s_estimation
		} else {
			slices <- as.integer(options$ns_slices)
			slices <- slices[slices >= period[1] & slices <= period[2]]
			uncertainty_method <- options$ns_uncertainty
			estimation_method <- options$ns_estimation
		}

		# Run uncertainty quantification 
		results <- if (uncertainty_method == "Bootstrap") {
			uncertainty_bootstrap(
				data_subset,
				distribution,
				estimation_method,
				prior = if (estimation_method == "GMLE") options$gev_prior else NULL,
				ns_years = years_subset,
				ns_structure = structure,
				ns_slices = slices,
				alpha = options$significance_level,
				samples = options$bootstrap_samples,
				periods = options$return_periods
			)
		} else if (uncertainty_method == "RFPL") {
			uncertainty_rfpl(
				data_subset,
				distribution,
				ns_years = years_subset,
				ns_structure = structure,
				ns_slices = slices,
				alpha = options$significance_level,
				tolerance = options$rfpl_tolerance,
				periods = options$return_periods
			)
		} else if (uncertainty_method == "RFGPL") {
			uncertainty_rfgpl(
				data_subset,
				prior = options$gev_prior,
				ns_years = years_subset,
				ns_structure = structure,
				ns_slices = slices,
				alpha = options$significance_level,
				tolerance = options$rfpl_tolerance,
				periods = options$return_periods
			)		
		}

		# Generate uncertainty quantification plot
		if (!structure$location && !structure$scale) {
			plot <- plot_sffa_estimates(results)
		} else {
			plot <- plot_nsffa_estimates(results)
		}

		if (!is.null(path)) {
			write_plot(plot, path, "uncertainty", period)
		}

		if (serialize) {
			results$plot <- serialize_plot(plot) 
		}

		# Return results as a list
		list(
			name = "Uncertainty Quantification",
			start = period[1],
			end = period[2],
			uncertainty = results
		)

	})
}
