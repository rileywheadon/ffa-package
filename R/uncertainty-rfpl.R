#' Regula-Falsi Profile Likelihood Uncertainty Quantification
#'
#' @description
#' Calculates return level estimates and confidence intervals at specified return
#' periods (defaults to 2, 5, 10, 20, 50, and 100 years) using the regula-falsi profile
#' likelihood root‐finding method.
#'
#' **For NS-FFA**: To perform uncertainty quantification for a nonstationary model, 
#' include the observation years (`ns_years`), the nonstationary model structure 
#' (`ns_structure`), and a list of years at which to compute the return level estimates
#' and confidence intervals (`ns_slices`).
#'
#' @inheritParams param-data
#' @inheritParams param-distribution
#' @inheritParams param-ns-years
#' @inheritParams param-ns-structure
#' @inheritParams param-ns-slices
#' @inheritParams param-alpha
#' @inheritParams param-periods
#' @inheritParams param-tolerance
#'
#' @return A list containing the following four items:
#' - `method`: "RFPL"
#' - `ns_structure`: The `ns_structure` argument, if given.
#' - `ns_slices`: The `ns_slices` argument, if given.
#' - `ci`: A dataframe containing confidence intervals (S-FFA only)
#' - `ci_list`: A list of dataframes containing confidence intervals (NS-FFA only).
#'
#' The dataframe(s) in `ci` and `ci_list` have four columns:
#' - `estimates`: Estimated quantiles for each return period.
#' - `lower`: Lower bound of the confidence interval for each return period.
#' - `upper`: Upper bound of the confidence interval for each return period.
#' - `periods`: The `periods` argument. 
#'
#' @details
#' 1. Uses [fit_mle()] to obtain the maximum log‐likelihood.  
#' 2. Defines an objective function \eqn{f(y_p, p)} by reparameterizing the 
#'    log-likelihood.
#' 3. Iteratively brackets the root by rescaling initial guesses by 0.05 until 
#'    \eqn{f(y_p, p)} changes sign.  
#' 4. Uses the regula-falsi method to solve \eqn{f(y_p, p) = 0} for each 
#'    return period probability.  
#' 5. Returns lower and upper confidence bounds at significance level `alpha`.
#'
#' @note RFPL uncertainty quantification can be numerically unstable for some datasets. 
#' If this function encounters an issue, it will return an error and recommend 
#' using the parametric bootstrap method [uncertainty_bootstrap()] instead.
#'
#' @seealso [utils_quantiles()], [uncertainty_bootstrap()], [uncertainty_rfgpl()],
#' [plot_sffa_estimates()], [plot_nsffa_estimates()]
#'
#' @examples
#' data <- rnorm(n = 100, mean = 100, sd = 10)
#' uncertainty_rfpl(data, "GLO")
#'
#' @references
#' Vidrio-Sahagún, C.T., He, J. Enhanced profile likelihood method for the nonstationary 
#' hydrological frequency analysis, Advances in Water Resources 161, 10451 (2022). 
#' \doi{10.1016/j.advwatres.2022.104151}
#' 
#' Vidrio-Sahagún, C.T., He, J. & Pietroniro, A. Multi-distribution regula-falsi profile 
#' likelihood method for nonstationary hydrological frequency analysis. Stochastic Environmental 
#' Research and Risk Assessment 38, 843–867 (2024). \doi{10.1007/s00477-023-02603-0}
#'
#' @importFrom stats qchisq nlminb
#' @export

uncertainty_rfpl <- function(
    data,
    distribution,
    ns_years = NULL,
    ns_structure = NULL,
    ns_slices = NULL,
    alpha = 0.05,
	periods = c(2, 5, 10, 20, 50, 100),
    tolerance = 1e-2
) {

	data <- validate_numeric("data", data)
	distribution <- validate_enum("distribution", distribution)
	years <- validate_numeric("ns_years", ns_years, TRUE, size = length(data))
	structure <- validate_structure(ns_structure)
	slices <- validate_numeric("ns_slices", ns_slices, TRUE)
	alpha <- validate_float("alpha", alpha, bounds = c(0.01, 0.1))
	periods <- validate_numeric("periods", periods, bounds = c(1, Inf))
	tolerance <- validate_float("tolerance", tolerance, bounds = c(0, 1))

	# Call the general helper method
	results <- uncertainty_regula_falsi(
		data,
		distribution,
		NULL,
		years,
		structure,
		slices,
		alpha,
		periods,
		tolerance
	)

	# Initialize the output
	output <- list(
		method = "RFPL",
		ns_structure = ns_structure,
		ns_slices = ns_slices
	)

	# Add the confidence interval list or confidence interval dataframe
	if (structure$location || structure$scale) {
		output$ci_list <- results
	} else {
		output$ci <- results
	}

	# Return results
	output	
}

