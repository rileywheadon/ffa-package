#' Orchestrate Exploratory Data Analysis
#'
#' First, this method identifies change points in annual maximum series data. Then, the 
#' user is given the option to split the dataset into two or more subperiods. Finally, 
#' this method performs a collection of statistical tests for identifying nonstationarity 
#' in the mean and variability of each subperiod.
#'
#' @details 
#' If the `ns_splits` argument is not `NULL`, it will be used to split the data.
#' To avoid splitting the data, use `splits = integer(0)`. Note that `integer(0)` is not 
#' the same as `NULL`.
#'
#' If `ns_splits` is `NULL` and `automatic` is `TRUE`, the data will be split automatically 
#' based on the change point detection test with the lowest statistically significant p-value. 
#' If neither test yielded a statistically significant p-value, the data will not be split.
#'
#' If `ns_splits` is `NULL` and `automatic` is `FALSE`, the user will be prompted to select the 
#' split points manually. This feature requires R to be running in interactive mode. Check if 
#' R is in interactive mode using the built-in `interactive()` function.
#'
#' @section Optional Arguments:
#' - `alpha`: The numeric significance level for all statistical tests (default is 0.05).
#' - `bbmk_samples`: The number of samples used in the Block-Bootstrap Mann-Kendall (BBMK) 
#'   test (default is 10000). Must be an integer.
#'
#' @inheritParams param-data
#' @inheritParams param-years
#'
#' @param ns_splits An integer vector of years at which to split the data (default is `NULL`).
#' A split point is the *first* year in a subperiod.
#'
#' @param automatic If `TRUE`, the data is split automatically using the results of the Pettitt 
#' and MKS tests (default is `FALSE`). This argument is ignored if `ns_splits` is not `NULL`.
#'
#' @param ... Additional arguments. See the "Optional Arguments" section for a complete list.
#'
#' @return 
#' `recommendations`: A list of recommended split points and nonstationary structures:
#' - `approach`: Either `"S-FFA"`, `"NS-FFA"`, or `"Piecewise NS-FFA"`.
#' - `splits`: The change point(s) identified by the change point detection test with the
#'   the lowest statistically significant p-value, or an empty vector if no such test exists.
#' - `structures`: A list of nonstationary structure objects for each subperiod. Each structure
#'   is a list with boolean items `location` and `scale` which represent a linear trend in the
#'   in the mean or variability of the data respectively.
#'
#' `submodules`: A list of lists of statistical tests. Each sublist contains:
#' - `name`: Either `"Change Points"` or `"Trend Detection"`.
#' - `start`: The first year of the subperiod.
#' - `end`: The last year of the subperiod.
#' - Additional items from the statistical tests within the submodule.
#'
#' @seealso [eda_pettitt_test()], [eda_mks_test()], [eda_mk_test()], [eda_spearman_test()]
#' [eda_bbmk_test()], [eda_pp_test()], [eda_kpss_test()], [eda_sens_trend()], 
#' [eda_runs_test()], [eda_white_test()]
#'
#' @export
framework_eda <- function(data, years, ns_splits = NULL, automatic = FALSE) {
	NULL
}

