#' Perform Data Screening
#'
#' Checks for missing entries and generates a list of summary statistics about a dataset.
#'
#' @inheritParams param-data
#' @inheritParams param-years
#'
#' @return A list with seven entries:
#' - `years_min`: The minimum value in the 'years' argument.
#' - `years_max`: The maximum value in the 'years' argument.
#' - `data_min`: The minimum value in the 'data' argument.
#' - `data_med`: The median value in the 'data' argument.
#' - `data_max`: The maximum value in the 'data' argument.
#' - `missing_years`: An integer vector of years with no data.
#' - `missing_count`: The number of missing entries in the dataset.
#'
#' @examples
#' data <- rnorm(n = 10, mean = 100, sd = 10)
#' years <- c(1900, 1902, 1903, 1904, 1905, 1907, 1909, 1911, 1912, 1914)
#' data_screening(data, years)
#'
#' @export
data_screening <- function(data, years) {

	# Parameter validation
	data <- validate_numeric("data", data)
	years <- validate_numeric("years", years, size = length(data))

    list(
		years_min = min(years),
		years_max = max(years),
		data_min = min(data),
		data_med = median(data),
		data_max = max(data),
		missing_years = setdiff(seq(min(years), max(years)), years),
		missing_count = sum(!(seq(min(years), max(years)) %in% years))
	)
}
