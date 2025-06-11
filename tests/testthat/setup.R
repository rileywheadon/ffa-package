# Set the seed
set.seed(1)

# Helper function to load a data file
load_data <- function(file_name, clean = TRUE) {

	# Read the data file
	csv_path <- system.file("extdata", file_name, package = "ffaframework")
	df <- read.csv(csv_path)

	# Remove leading and trailing NaN values
	idx <- which(!is.na(df$max))
	df <- df[min(idx):max(idx), ]

	# If clean = FALSE, return the new dataframe
	if (!clean) return (df)
	
	# Otherwise remove NA values and return
	df[which(!is.na(df$max)), ]

}
