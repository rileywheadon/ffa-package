#' CAN-07BE001
#'
#' A dataframe of annual maximum series observations for 
#' station 07BE001, ATHABASCA RIVER AT ATHABASCA in Alberta, Canada. 
#'
#' @section Features:
#' - The MKS/Pettitt tests find no evidence of change points at the 0.05 significance level.
#' - Trend detection finds no evidence of trends in the mean or variability.
#' 
#' This dataset is used as a test case for comparison with the MATLAB implementation of the 
#' FFA framework. It is also an excellent introduction to stationary FFA for students.
#'
#' @format A dateframe with 108 rows and 2 columns spanning the period 1913-2020.
#'
#' @source Meteorological Service of Canada (MSC) GeoMet Platform
#'
#' @details Variables:
#' - `max`: Numeric; the annual maximum series (AMS) observation, in m\eqn{^3}{^3}/s.
#' - `year`: Integer; the corresponding year.

"CAN_07BE001"
