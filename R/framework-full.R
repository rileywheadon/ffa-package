#' Orchestrate the Full FFA Framework
#'
#' Runs the entire flood frequency analysis framework using the results of exploratory 
#' data analysis (EDA) to guide approach selection (stationary or nonstationary) and 
#' perform flood frequency analysis. Returns a comprehensive and reproducible summary 
#' of the results.
#'
#' @inheritParams param-data
#' @inheritParams param-years
#'
#' @param ns_splits An integer vector of years used to split the data into homogeneous
#' subperiods. For S-FFA, set to `NULL` (default). For NS-FFA, specify an integer vector 
#' of years (e.g., `1950L`) with physical justification for change points, or `NULL` 
#' if no such years exist.
#'
#' @param ns_structures For S-FFA, set to `NULL` (default) to use a stationary model 
#' for all homogeneous subperiods. For NS-FFA, provide a list of `length(ns_splits) + 1` 
#' sublists specifying the nonstationary model structure for each homogeneous subperiod. 
#' Each sublist must contain logical elements `location` and `scale`, indicating 
#' monotonic trends in the mean and variability, respectively. 
#'
#' @param ... Additional arguments to be passed to the statistical tests and frequency
#' analysis functions. See the details of [framework_eda()] and [framework_ffa()] for a 
#' complete list.
#'
#' @return
#' `recommendations`: See [framework_eda()]. 
#'
#' `summary`: See [framework-ffa()]. 
#'
#' `submodules`: A list of lists of results. Each list contains:
#' - `name`: Either "Change Point Detection", "Trend Detection", "Distribution 
#'   Selection", "Parameter Estimation", "Uncertainty Quantification", or "Model 
#'   Assessment".
#' - `start`: The first year of the homogeneous subperiod.
#' - `end`: The last year of the homogeneous subperiod.
#' - Additional items specific to the the submodule.
#'
#' @seealso [framework_eda()], [framework_ffa()]
#'
#' @export
framework_full <- function(data, years, ns_splits = NULL, ns_structures = NULL) {
	NULL
}
