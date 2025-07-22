#' Flood Frequency Analysis Framework
#'
#' Runs the entire flood frequency analysis framework using the exploratory
#' data analysis ([module_eda()]) and flood frequency analysis ([module_ffa()]) modules.
#' Returns a comprehensive and reproducible summary of the results.
#'
#' @details The behaviour of the function when `automatic = FALSE` can be confusing. If either
#' of `splits` or `structures` is `NULL`, the user will be prompted to select them. However, 
#' if R is running in non-interactive mode, this is not possible, so the function will return
#' all the results up to the current block along with an error message. 
#'
#' To avoid this issue, the user has three options:
#' 1. Set `splits` and `structures` manually.
#' 2. Run R in interactive mode (using RStudio, for example).
#' 3. Set `automatic = TRUE`.
#'
#' @inheritParams param-data
#' @inheritParams param-years
#'
#' @param splits An integer vector of years at which to split the data (default is `NULL`).
#' A split point is the *first* year in a subperiod.
#'
#' @param structures An list of structure objects of size `length(splits) + 1` (default is `NULL`).
#' Each structure object is a list with boolean items `location` and `scale` indicating a trend 
#' in the mean/variability respectively.
#' 
#' @param automatic If `TRUE`, the split points and nonstationary structures are chosen
#' automatically using the results of EDA (default is `FALSE`). This argument is ignored
#' if both `splits` and `structures` are not `NULL`.
#' 
#' @param ... Additional arguments to be passed to the statistical tests and frequency
#' analysis functions. See the details of [module_eda()] and [module_ffa()] for a complete
#' list.
#'
#' @return
#' `summary`: A list containing high-level information about the results:
#' - `splits`: The vector of years used to split the data into subperiods.
#' - `structures`: A list of nonstationary structure objects with size `length(splits) + 1`.
#'
#' `blocks`: A list of results for each module. Each block is a list containing:
#' - `name`: Either `"Change Points"`, `"Trend Detection"`, or `"Frequency Analysis"`.
#' - `start`: The first year of the subperiod.
#' - `end`: The last year of the subperiod.
#' - Additional items depending on the module.
#'
#' @export

ffaframework <- function(
	data,
	years,
	splits = NULL,
	structures = NULL,
	automatic = FALSE,
	...
) {
	NULL
}

# library(tools)
# library(glue)
# library(jsonlite)

# # Load ffaframework library
# suppressMessages(library(ffaframework))

# # Source all helper functions
# files <- list.files("source/helpers/", full.names = TRUE)
# for (f in files) { source(f) }

# # Load the options from the configuration file
# options <- validate_config("config.yml")


# ### CHANGE POINTS ###


# # Helper function for automatic splitting
# split_points_automatic <- function(change_results) {

# 	pettitt <- change_results$pettitt
# 	mks <- change_results$mks

# 	# If there are no change points return empty vector
# 	if (!pettitt$reject && !mks$reject) {
# 		return (numeric(0))	
# 	} 

# 	# Only the Pettitt test found change points
# 	else if (pettitt$reject && !mks$reject) {
# 		return (pettitt$change_year)
# 	} 

# 	# Only the MKS test found change points
# 	else if (!pettitt$reject && mks$reject) {
# 		return (mks$change_df$year) 
# 	}

# 	# Otherwise prioritize based on p-value
# 	if (pettitt$p_value < mks$p_value) {
# 		return (pettitt$change_year)
# 	} else {
# 		return (mks$change_df$year) 
# 	}

# }

# # Helper function for manual splitting
# split_points_manual <- function(change_results) {

# 	pettitt <- change_results$pettitt
# 	mks <- change_results$mks

# 	# If there are no change points return empty vector
# 	if (!pettitt$reject && !mks$reject) {
# 		cat("\nNo change points found.\n")
# 		return (numeric(0))	
# 	} 

# 	cat("\nPettitt Test Results:")
# 	cat(pettitt$msg)
# 	cat("\n\nMKS Test Results:")
# 	cat(mks$msg)

# 	# Helper function to get user input
# 	get_user_input <- function(n) {

# 		cat("\n\nEnter a number: ")
# 		option <- readLines(file("stdin"), 1)
# 		regex <- paste0("[1-", as.character(n), "]$")

# 		if (!grepl(regex, option)) {
# 			cat("\nInvalid input. Please try again.")
# 			get_user_input(n)
# 		} 

# 		option

# 	}

# 	# Print options for the user
# 	cat("\n\nPlease select an option:")
# 	cat("\n 1. Reject change points.")

# 	# Only the Pettitt test found change points
# 	if (pettitt$reject && !mks$reject) {
# 		cat("\n 2. Accept change points from the Pettitt test.")
# 		option <- get_user_input(2)
# 		if (option == 2) return (pettitt$change_year)
# 	} 

# 	# Only the MKS test found change points
# 	else if (!pettitt$reject && mks$reject) {
# 		cat("\n 2. Accept change points from the MKS test.")
# 		option <- get_user_input(2)
# 		if (option == 2) return (mks$change_df$year)
# 	}

# 	# Both tests found chang epoints
# 	else {
# 		cat("\n 2. Accept change points from the Pettitt test.")
# 		cat("\n 3. Accept change points from the MKS test.")
# 		option <- get_user_input(3)
# 		if (option == 2) return (pettitt$change_year)
# 		if (option == 3) return (mks$change_df$year)
# 	}

# 	numeric(0)

# }

# run_change_points <- function(data, years, output) {

# 	# Run change point detection if necessary
# 	if (options$split_selection != "Preset") {

# 		# Get change points
# 		change_results <- change_points(data, years, options)
# 		output[[length(output) + 1]] <- change_results

# 		# Call the correct handler
# 		if (options$split_selection == "Automatic") {
# 			splits <- split_points_automatic(change_results)
# 		} else if (options$split_selection == "Manual") {
# 			splits <- split_points_manual(change_results)
# 		} 

# 	} 

# 	# If split points are preset, simpy assign them.
# 	else {
# 		splits <- options$split_points
# 	} 

# 	# Generate the partitions as list of tuples (start, end)
# 	starts <- c(min(years), splits)
# 	ends <- c(splits - 1, max(years))
# 	partitions <- Map(c, starts, ends)

# 	# Update the summary object
# 	output$summary$splits <- splits
# 	output$summary$partitions <- partitions

# 	# Return the updated output object
# 	output

# }


# ### TREND DETECTION ###


# # Helper function for automatic trend selection
# trends_automatic <- function(trend_results) {
# 	trend <- list()
# 	trend$location <- ("sens_mean" %in% names(trend_results))
# 	trend$scale <- ("sens_variance" %in% names(trend_results))
# 	trend
# }

# # Helper function for manual trend selection
# trends_manual <- function(trend_results) {

# 	# Set the recommended option
# 	rec <- 1 
# 	if ("sens_mean" %in% names(trend_results)) rec <- rec + 1
# 	if ("sens_variance" %in% names(trend_results)) rec <- rec + 2

# 	partition <- glue("{trend_results$start}-{trend_results$end}")
# 	cat(glue("\n\nChoose a trend for partition {partition}:"))
# 	cat("\n 1. No trend.")
# 	cat("\n 2. Trend in mean.")
# 	cat("\n 3. Trend in variability.")
# 	cat("\n 4. Trend in mean and variability.")

# 	# Helper function to get user input
# 	get_user_input <- function() {

# 		cat(glue("\n\n\nSelect an option ({rec} recommended): "))
# 		option <- readLines(file("stdin"), 1)

# 		if (!grepl("[1-4]$", option)) {
# 			cat("\nInvalid input. Please try again.")
# 			get_user_input()
# 		} 

# 		option

# 	}

# 	option <- get_user_input()

# 	# Return the trend corresponding to the selected option
# 	switch(
# 		option,
# 		"1" = list(location = FALSE, scale = FALSE),
# 		"2" = list(location = TRUE, scale = FALSE),
# 		"3" = list(location = FALSE, scale = TRUE),
# 		"4" = list(location = TRUE, scale = TRUE)
# 	)

# }

# run_trend_detection <- function(data, years, output) {

# 	# Get the partitions from output
# 	partitions <- output$summary$partitions

# 	# Initialize a list of signatures for each partition
# 	signatures <- list()

# 	# Iterate through the homogeneous partitions
# 	for (partition in partitions) {

# 		# Run trend detection and add to results
# 		trend_results <- trend_detection(
# 			data,
# 			years,
# 			options,
# 			partition[1],
# 			partition[2]
# 		)

# 		output[[length(output) + 1]] <- trend_results

# 		# Add the signature for this partition to the list
# 		if (options$trend_selection == "Manual") {
# 			trend <- trends_manual(trend_results)	
# 		} else if (options$trend_selection == "Automatic") {
# 			trend <- trends_automatic(trend_results)	
# 		}

# 		signatures[[length(signatures) + 1]] <- trend

# 	}

# 	# Print informational message
# 	cat("\nEDA Complete. Selected trends: \n")
# 	cat("\n| Partition | Trend in Mean | Trend in Variance |")
# 	cat("\n| --------- | ------------- | ----------------- |")

# 	for (i in seq_along(partitions)) {

# 		partition <- partitions[[i]]
# 		signature <- signatures[[i]]

# 		msg <- sprintf(
# 			"\n| %d-%d | %s | %s |",
# 			partition[1],
# 			partition[2],
# 			format(signature$location, width = 13, justify = "left"),
# 			format(signature$scale, width = 17, justify = "left")
# 		)

# 		cat(msg)
# 	} 

# 	# Add the signatures to the summary object
# 	output$summary$signatures <- signatures

# 	# Return the updated output object
# 	output

# }


# ### FLOOD FRQUENCY ANALYSIS ###


# run_frequency_analysis <- function(data, years, output) {

# 	# Get the partitions and signatures from output
# 	partitions <- output$summary$partitions
# 	signatures <- output$summary$signatures

# 	# Run frequency analysis on each homogeneous period
# 	for (i in seq_along(partitions)) {

# 		# Run frequency analysis and add to results
# 		ffa_results <- tryCatch(
# 			frequency_analysis(
# 				data, 
# 				years,
# 				options,
# 				signatures[[i]],
# 				partitions[[i]][1],
# 				partitions[[i]][2]
# 			),
# 			error = function(e) {
# 				cat("\n\n")
# 				cat(conditionMessage(e))
# 				q()
# 			}
# 		)

# 		# Append the FFA results to the output object
# 		output[[length(output) + 1]] <- ffa_results 

# 	}

# 	# Print informational message
# 	cat("\n\nFFA Complete.\n")

# 	# Return the updated output object
# 	output

# }


# ### REPORT GENERATION ###


# report_generation <- function(output, dataset) {

# 	# Create the report directory if it doesn't exist
# 	report_dir <- glue("reports/{dataset}")
# 	if (!dir.exists(report_dir)) dir.create(report_dir)

# 	# Create the image directory if it doesn't exist
# 	img_dir <- glue("{report_dir}/img")
# 	if (!dir.exists(img_dir)) dir.create(img_dir)

# 	# Helper function to plot the selection
# 	plot_selection <- function(result) {
# 		if (result$method != "preset") {
# 			plot_lmom_diagram(result) 
# 		} else {
# 			NULL
# 		}
# 	}

# 	# Helper function to plot the correct uncertainty quantification
# 	plot_uncertainty <- function(result) {
# 		trend <- result[[1]]$trend

# 		if (!trend$location && !trend$scale) {
# 			plot_sffa(result)
# 		} else {
# 			plot_nsffa(result)
# 		}
# 	}

# 	# Iterate through the results and generate images
# 	for (entry in output) {

# 		for (name in names(entry)) {

# 			result <- entry[[name]]
# 			show_line <- options$show_line

# 			# Pass result to plotting function
# 			plot <- switch(
# 				name,
# 				"assessment" = plot_model_diagnostics(result),
# 				"bbmk" = plot_bbmk_test(result),
# 				"mks" = plot_mks_test(result, show_line),
# 				"pettitt" = plot_pettitt_test(result, show_line),
# 				"runs_mean" = plot_runs_test(
# 					result,
# 					title = "Runs Test (AMS Mean)"
# 				),
# 				"runs_variance" = plot_runs_test(
# 					result,
# 					title = "Runs Test (AMS Variability)"
# 				),
# 				"selection" = plot_selection(result),
# 				"sens_mean" = plot_sens_trend(
# 					result$data,
# 					result$years,
# 					mean_trend = result,
# 					variability_trend = entry[["sens_variance"]],
# 					show_line = show_line,
# 					title = "Sen's Trend Estimator (AMS Mean)"
# 				),
# 				"sens_variance" = plot_sens_trend(
# 					result$data,
# 					result$years,
# 					mean_trend = result,
# 					show_line = show_line,
# 					title = "Sen's Trend Estimator (AMS Variance)"
# 				),
# 				"spearman" = plot_spearman_test(result),
# 				"uncertainty" = plot_uncertainty(result),
# 				NULL
# 			)

# 			# Save plot to a file
# 			if (!is.null(plot)) {
# 				img_name <- glue("{name}_{entry$start}_{entry$end}.png")
# 				img_path <- glue("{img_dir}/{img_name}")
# 				ggsave(img_path, plot = plot, height = 8, width = 10, dpi = 300)
# 			}

# 		}
# 	}

# 	# Iterate through report options
# 	for (format in options$report_formats) {
# 		if (format == "json") {
# 			write_json(
# 				output, 
# 				path = glue("{report_dir}/report.json"),
# 				pretty = TRUE,
# 				auto_unbox = TRUE
# 			)
# 		} else {
# 			rmarkdown::render(
# 				"source/templates/_master.Rmd",
# 				params = list(output = output, dataset = dataset),
# 				output_format = glue("{format}_document"),
# 				output_dir = report_dir,
# 				output_file = "report",
# 				quiet = TRUE
# 			)
# 		} 
# 	}

# }


# ### FRAMEWORK ORCHESTRATION ###


# ffa_framework <- function(data, years, dataset) {

# 	cat(glue("\n\n\n::: {dataset} :::\n\n\n"))

# 	# Initialize a big list of output
# 	output <- list(summary = list())

# 	# Run the helper functions defined above
# 	output <- run_change_points(data, years, output)
# 	output <- run_trend_detection(data, years, output)
# 	output <- run_frequency_analysis(data, years, output)
# 	report_generation(output, dataset)

# }

# # Run the framework on every dataset
# if (options$data_source == "Local") {
# 	data <- load_data("Local", csv_files = options$csv_files)
# 	sources <- tools::file_path_sans_ext(options$csv_files)
# } else {
# 	data <- load_data("GeoMet", station_ids = options$station_ids)
# 	sources <- options$station_ids
# }

# for (i in seq_along(data)) {
# 	df <- data[[i]]
# 	ffa_framework(df$max, df$year, sources[[i]])
# }
