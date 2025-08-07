#' Orchestrate the Full FFA Framework
#'
#' Runs the entire flood frequency analysis framework using the exploratory data analysis 
#' ([framework_eda()]) and flood frequency analysis ([framework_ffa()]) modules. Returns 
#' a comprehensive and reproducible summary of the results.
#'
#' @details The behaviour of the function when `automatic = FALSE` can be confusing. If either
#' of `ns_splits` or `ns_structures` is `NULL`, the user will be prompted to select them. However, 
#' if R is running in non-interactive mode, this is not possible, so the function will return
#' all the results up to the current block along with an error message. 
#'
#' To avoid this issue, the user has three options:
#' 1. Set `ns_splits` and `ns_structures` manually.
#' 2. Run R in interactive mode (using RStudio, for example).
#' 3. Set `automatic = TRUE`.
#'
#' @inheritParams param-data
#' @inheritParams param-years
#'
#' @param ns_splits An integer vector of years at which to split the data (default is `NULL`).
#' A split point is the *first* year in a subperiod.
#'
#' @param ns_structures An list of structure objects of size `length(ns_splits) + 1` (default 
#' is `NULL`). Each structure object is a list with boolean items `location` and `scale` 
#' indicating a trend in the mean/variability respectively.
#' 
#' @param automatic If `TRUE`, the split points and nonstationary ns_structures are chosen
#' automatically using the results of EDA (default is `FALSE`). This argument is ignored
#' if both `ns_splits` and `ns_structures` are not `NULL`.
#' 
#' @param ... Additional arguments to be passed to the statistical tests and frequency
#' analysis functions. See the details of [module_eda()] and [module_ffa()] for a complete
#' list.
#'
#' @return
#' `summary`: A list containing the nonstationary splits and structures used for the analysis.
#' If the `ns_splits` and/or `ns_structures` arguments were provided, they will be included
#' here. Otherwise, this list will contain the split points and model structures selected
#' manually by the user.
#'
#' `submodules`: A list of results for each submodule. Each submodules has a `name`, one of
#' `"Change Points"`, `"Trend Detection"`, `"Distribution Selection"`, `"Parameter Estimation"`,  
#' `"Uncertainty Quantification"`, or `"Model Assessment"`. For more information, see 
#' [framework_eda()] and [framework_ffa()].
#'
#' @seealso [framework_eda()], [framework_ffa()]
#'
#' @export
framework_full <- function(
	data,
	years,
	ns_splits = NULL,
	ns_structures = NULL,
	automatic = FALSE
) {
	NULL
}
