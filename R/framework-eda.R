#' Orchestrate Exploratory Data Analysis
#'
#' First, this method identifies change points in the original annual maximum series 
#' data. Then, the user is given the option to split the dataset into two or more 
#' homogenous subperiods (trend-free or with monotonic trends). Finally, this method 
#' performs a collection of statistical tests for identifying monotonic nonstationarity 
#' in the mean and variability of each subperiod. The results of EDA can help guide 
#' FFA approach selection (stationary or nonstationary) and FFA model determination.
#'
#' @inheritParams param-data
#' @inheritParams param-years
#'
#' @param ns_splits An integer vector of years used to split the data into homogeneous
#' subperiods. For S-FFA, set to `NULL` (default). For NS-FFA, specify an integer vector 
#' of years (e.g., `1950L`) with physical justification for change points, or `NULL` 
#' if no such years exist.
#'
#' @param ... Additional arguments. See the "Optional Arguments" section for a 
#' complete list.
#'
#' @section Optional Arguments:
#' - `alpha`: The numeric significance level for all statistical tests (default is 0.05).
#' - `bbmk_samples`: The number of samples used in the Block-Bootstrap Mann-Kendall 
#'   (BBMK) test (default is 10000). Must be an integer.
#'
#' @return 
#' `recommendations`: A list containing the recommended FFA approach, split point(s) 
#' and nonstationary structure(s) from EDA:
#' - `approach`: Either "S-FFA", "NS-FFA" (for a single homogeneous period), or 
#'   "Piecewise NS-FFA" (for multiple homogeneous subperiods).
#' - `ns_splits`: The split point(s) identified by the change point detection test with 
#'   the the lowest statistically significant p-value, or `NULL` if no such point exists.
#' - `ns_structures`: A list of structure objects for each homogeneous subperiod. Each 
#'   structure is a list with boolean items `location` and `scale`, which represent a 
#'   linear trend in the in the mean or variability of the data, respectively. If no
#'   trends were found in any homogeneous subperiod, `ns_structures` will be `NULL`.
#'
#' `submodules`: A list of lists of statistical tests. Each list contains:
#' - `name`: Either "Change Point Detection" or "Trend Detection".
#' - `start`: The first year of the homogeneous subperiod.
#' - `end`: The last year of the homogeneous subperiod.
#' - Additional items from the statistical tests within the submodule.
#'
#' @seealso [eda_pettitt_test()], [eda_mks_test()], [eda_mk_test()], 
#' [eda_spearman_test()], [eda_bbmk_test()], [eda_pp_test()], [eda_kpss_test()], 
#' [eda_sens_trend()], [eda_runs_test()], [eda_white_test()]
#'
#' @export
framework_eda <- function(data, years, ns_splits = NULL) {
	NULL
}

