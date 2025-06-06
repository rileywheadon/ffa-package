# Helper function to load a data file
load_data <- function(file_name, window_length = 10, window_step = 5) {

	# Read the data file
	csv_path <- system.file("extdata", file_name, package = "ffaframework")
	df <- read.csv(csv_path)

	# Strip NA values and return
	df[which(!is.na(df$max)), ]

}
