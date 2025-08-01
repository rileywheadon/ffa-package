#' Fetch Data from MSC GeoMet API
#'
#' Gets annual maximum series data for a hydrological monitoring 
#' station from the MSC GeoMet API.
#'
#' @param station_id A character scalar containing the ID of a hydrological monitoring station.
#' You can search for station IDs by name, province, drainage basin, and location 
#' \href{https://wateroffice.ec.gc.ca/search/real_time_e.html}{here}.
#'
#' @return A dataframe with two columns:
#' - `max`: A float, the annual maximum series observation, in m\eqn{^3}{^3}/s.
#' - `year`: An integer, the corresponding year.
#'
#' @seealso [data_local()]
#'
#' @examples
#' # Get data for the BOW RIVER AT BANFF (05BB001)
#' df <- data_geomet("05BB001")
#'
#' @export
data_geomet <- function(station_id) {

	# Set GeoMet API URL
	url <- "https://api.weather.gc.ca/collections/hydrometric-annual-statistics/items"

	# Set query parameters (safely limit to 200 because data is annual)
	params <- list(
		limit = 200,
		skipGeometry = TRUE,
		DATA_TYPE_EN = "Discharge",
		STATION_NUMBER = station_id
	)

	# Make a GET request and parse the content as JSON
	response <- httr::GET(url, query = params)
	content <- httr::content(response, as = "parsed", type = "application/json")
	if (content$numberMatched == 0) {
		stop(sprintf("No data exists for station with ID '%s'", station_id))
	}

	# Helper function for extracting data from API response
	extract_ams <- function(x) {
		value <- x$properties$MAX_VALUE
		if (is.null(value)) NA_real_ else value
	}

	# Extract the streamflow data and years
	ams <- unlist(sapply(content$features, function(x) x$properties$MAX_VALUE))
	dates <- unlist(sapply(content$features, function(x) x$properties$MAX_DATE))
	years <- as.integer(substr(dates, 1, 4))			

	# Create a dataframe, then remove NA values
	df <- data.frame(year = years, max = ams)
	subset(df, !is.na(years) & !is.na(max))

}
