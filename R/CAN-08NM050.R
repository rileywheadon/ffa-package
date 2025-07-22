#' CAN-08NM050
#'
#' A dataframe of annual maximum series observations for 
#' station 08NM050, OKANAGAN RIVER AT PENTICTON in British Columbia, Canada. 
#'
#' @section Features:
#' The streamflow at this station is heavily regulated by the Penticton dam. 
#' Therefore, the results of flood frequency analysis are **NOT** meaningful.
#' 
#' This dataset is used as a test case for comparison with the MATLAB implementation of the 
#' FFA framework. It is also useful for demonstrating how the framework detects and handles
#' serial correlation, trends in the mean, and trends in the variability. Since the station
#' is highly regulated, *this dataset should only be used as a teaching tool*.
#'
#' @format A dateframe with 97 rows and 2 columns spanning the period 1921-2017.
#'
#' @source Meteorological Service of Canada (MSC) GeoMet Platform
#'
#' @details Variables:
#' - `max`: Numeric; the annual maximum series (AMS) observation, in m\eqn{^3}{^3}/s.
#' - `year`: Integer; the corresponding year.

"CAN_08NM050"
