# Helper function to load a data file, remove NA values, and return as a list
load_data <- function(file_name) {
	csv_path <- system.file("extdata", file_name, package = "ffaframework")
	df <- read.csv(csv_path, comment.char = "#")
	df[which(!is.na(df$max)), ]
}

# Possible trends to use for unit tests
trend_00 <- list(location = F, scale = F)
trend_10 <- list(location = T, scale = F)
trend_01 <- list(location = F, scale = T)
trend_11 <- list(location = T, scale = T)
