#' Fetch Local Package Data
#'
#' Fetch annual maximum series data for a hydrological monitoring station
#' from the package data directory.
#'
#' @param csv_file A character scalar containing the file name of a local dataset in 
#' `/inst/extdata`. Must be one of:
#' - `"CAN-05BA001.csv"`: BOW RIVER AT LAKE LOUISE
#' - `"CAN-05BB001.csv"`: BOW RIVER AT BANFF
#' - `"CAN-07BE001.csv"`: ATHABASCA RIVER AT ATHABASCA
#' - `"CAN-08MH016.csv"`: CHILLIWACK RIVER AT CHILLIWACK LAKE
#' - `"CAN-08NH021.csv"`: KOOTENAI RIVER AT PORTHILL
#' - `"CAN-08NM050.csv"`: OKANAGAN RIVER AT PENTICTON
#' - `"CAN-08NM116.csv"`: MISSION CREEK NEAR EAST KELOWNA
#'
#' @return A dataframe with two columns:
#' - `max`: A float, the annual maximum series observation, in m\eqn{^3}{^3}/s.
#' - `year`: An integer, the corresponding year.
#'
#' @seealso [data_geomet()]
#'
#' @examples
#' # Get data for the BOW RIVER AT BANFF (05BB001)
#' df <- data_local("CAN-05BB001.csv")
#'
#' @importFrom utils read.csv
#' @export
data_local <- function(csv_file) {

	# Argument validation
	if (!is.character(csv_file) || length(csv_file) != 1 || nchar(csv_file) == 0) {
    	stop("'csv_file' must be a non-empty character string")
  	}

	# Get the path to the data file and check if it exists
	csv_path <- system.file("extdata", csv_file, package = "ffaframework")
	if (!file.exists(csv_path)) {
		stop(sprintf("File '%s' does not exist", csv_file))
	}

	# Load the data, remove NA values, return
	df <- read.csv(csv_path, comment.char = "#")
	df[!is.na(df$max), ]

}
