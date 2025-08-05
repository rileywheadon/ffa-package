#' CAN-05BA001
#'
#' A dataframe of annual maximum series observations for 
#' station 05BA001, BOW RIVER AT LAKE LOUISE in Alberta, Canada. 
#'
#' @section Additional Information:
#' This is an *unregulated* station that is not in the RHBN. Other notable features include:
#' - There are no observations from 1919-1965. Missing data should be handled carefully.
#' - The MKS/Pettitt tests do not find evidence of change points at the 0.05 significance level. 
#' - Trend detection finds evidence of a trend in variability.
#' - If nonstationarity is assumed, RFPL uncertainty quantification fails on this dataset.
#'
#' This dataset is used as a test case for failure modes in RFPL and variability estimation.
#'
#' @format A dateframe with 62 rows and 2 columns spanning the period 1913-2023.
#'
#' @source Meteorological Service of Canada (MSC) GeoMet Platform
#'
#' @details Variables:
#' - `max`: Numeric; the annual maximum series observation, in m\eqn{^3}{^3}/s.
#' - `year`: Integer; the corresponding year.

"CAN_05BA001"
