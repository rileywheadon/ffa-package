#' CAN-08NM050
#'
#' A dataframe of annual maximum series observations for 
#' station 08NM050, OKANAGAN RIVER AT PENTICTON in British Columbia, Canada. 
#'
#' @section Additional Information:
#' This is a *regulated* station that is not part of the RHBN. Other notable features include:
#' - The Okanagan River upstream of the station has been regulated since 1914 due to the 
#'   construction of the first dam, followed by a second dam in 1920, and a regulation system 
#'   in the early 1950s, consisting of four dams and 38 km of engineered channel.
#' - Rapid human settlement, development, and agricultural activity have occurred in the watershed.
#'
#' This dataset is used as a test case for comparison with the MATLAB implementation of the 
#' FFA framework. It is also useful for demonstrating how the framework detects and handles
#' serial correlation, trends in the mean, and trends in the variability. As noted above, 
#' this dataset is heavily influenced by reservoir operations and is intended for teaching 
#' purposes—not for design flood estimation.
#'
#' @format A dateframe with 97 rows and 2 columns spanning the period 1921-2017.
#'
#' @source Meteorological Service of Canada (MSC) GeoMet Platform
#'
#' @details Variables:
#' - `max`: Numeric; the annual maximum series observation, in m\eqn{^3}{^3}/s.
#' - `year`: Integer; the corresponding year.

"CAN_08NM050"
