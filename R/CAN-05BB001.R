#' CAN-05BB001
#'
#' A dataframe of annual maximum series observations for 
#' station 05BB001, BOW RIVER AT BANFF in Alberta, Canada. 
#'
#' @section Features:
#' - Whitfield & Pomeroy (2016) found that floods are caused separately by both rain *and* snow.
#'   Therefore, practitioners should use *extreme caution* when performing FFA on this station.
#' - Minimal human intervention in the basin means there is little evidence of change points. 
#' - Trend detection finds evidence of a decreasing trend in the mean.
#'
#' This dataset is used as a test case for comparison with the MATLAB implementation of the 
#' FFA framework. It is also an excellent introduction to nonstationary FFA for students.
#'
#' @format A dateframe with 110 rows and 2 columns spanning the period 1909-2018.
#'
#' @source Meteorological Service of Canada (MSC) GeoMet Platform
#'
#' @details Variables:
#' - `max`: Numeric; the annual maximum series (AMS) observation, in m\eqn{^3}{^3}/s.
#' - `year`: Integer; the corresponding year.
#'
#' @references
#' Whitfield P. H., and Pomeroy J. W. (2016) Changes to flood peaks of a mountain river: 
#' implications for analysis of the 2013 flood in the Upper Bow River, Canada, 
#' Hydrol. Process., 30: 4657–4673. \doi{10.1002/hyp.10957}.

"CAN_05BB001"
