#' CAN-08NH021
#'
#' A dataframe of annual maximum series observations for 
#' station 08NH021, KOOTENAI RIVER AT PORTHILL in British Columbia, Canada. 
#'
#' @section Additional Information:
#' This is a *regulated* station that is not in the RHBN. Other notable features include:
#' - The Libby dam was constructed upstream of this station in 1972.
#' - The Pettitt test found evidence of a change point in 1972 at the 0.05 significance level.
#' - The MKS test found evidence of change points in 1960 & 1985 at the 0.05 significance level.
#' - After splitting the data in 1972, trend detection finds evidence of an increasing, 
#'   deterministic, and linear trend in the mean for both subperiods.
#' 
#' This dataset is used as a test case for comparison with the MATLAB implementation of the 
#' FFA framework. It is also useful for demonstrating how the framework detects and handles 
#' change points in a time series.
#'
#' @format A dateframe with 91 rows and 2 columns spanning the period 1928-2018.
#'
#' @source Meteorological Service of Canada (MSC) GeoMet Platform
#'
#' @details Variables:
#' - `max`: Numeric; the annual maximum series observation, in m\eqn{^3}{^3}/s.
#' - `year`: Integer; the corresponding year.

"CAN_08NH021"
