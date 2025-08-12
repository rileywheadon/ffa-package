#' Orchestrate the Full FFA Framework
#'
#' Runs the entire flood frequency analysis framework using the results of exploratory 
#' data analysis (EDA) to guide approach selection (stationary or nonstationary) and 
#' perform flood frequency analysis. Returns a comprehensive and reproducible summary 
#' of the results.
#'
#' @inheritParams param-data
#' @inheritParams param-years
#' @inheritParams param-ns-splits
#' @inheritParams param-ns-structures
#' @inheritParams param-generate-report
#' @inheritParams param-report-path
#'
#' @param ... Additional arguments to be passed to the statistical tests and frequency
#' analysis functions. See the details of [framework_eda()] and [framework_ffa()] for a 
#' complete list.
#'
#' @return
#' `recommendations`: See [framework_eda()]. 
#'
#' `summary`: See [framework_ffa()]. 
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
framework_full <- function(
	data,
	years,
	ns_splits = NULL,
	ns_structures = NULL,
	generate_report = TRUE,
	report_path = NULL
) {
	NULL
}
