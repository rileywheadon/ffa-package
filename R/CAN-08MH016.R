#' CAN-08MH016
#'
#' A dataframe of annual maximum series observations for 
#' station 08MH016, CHILLIWACK RIVER AT CHILLIWACK LAKE in British Columbia, Canada. 
#'
#' @section Additional Information:
#' This is an *unregulated* station in the RHBN network. Other notable features include:
#' - The MKS/Pettitt tests find no evidence of change points at the 0.05 significance level.
#' - Trend detection finds evidence of an increasing trend in the variability.
#' 
#' This dataset is used as a test case for comparison with the MATLAB implementation of the 
#' FFA framework. It is also useful for demonstrating how the framework detects and handles 
#' nonstationarity in the variability of a time series.
#'
#' @format A dateframe with 95 rows and 2 columns spanning the period 1922-2016.
#'
#' @source Meteorological Service of Canada (MSC) GeoMet Platform
#'
#' @details Variables:
#' - `max`: Numeric; the annual maximum series observation, in m\eqn{^3}{^3}/s.
#' - `year`: Integer; the corresponding year.

"CAN_08MH016"
