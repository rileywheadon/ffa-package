#' Regula-Falsi Generalized Profile Likelihood Uncertainty Quantification
#'
#' @description
#' Calculates return level estimates and confidence intervals at specified return
#' periods (defaults to 2, 5, 10, 20, 50, and 100 years) using the regula-falsi 
#' generalized profile likelihood root‐finding method for the GEV distribution.
#'
#' **NS-FFA**: To perform uncertainty quantification for a nonstationary model, 
#' include the observation years (`ns_years`), the nonstationary model structure 
#' (`ns_structure`), and a list of years at which to compute the return level estimates
#' and confidence intervals (`ns_slices`).
#'
#' @inheritParams param-data
#' @inheritParams param-prior
#' @inheritParams param-ns-years
#' @inheritParams param-ns-structure
#' @inheritParams param-ns-slices
#' @inheritParams param-alpha
#' @inheritParams param-periods
#' @inheritParams param-tolerance
#'
#' @return A list containing the following four items:
#' - `method`: "RFGPL"
#' - `ns_structure`: The `ns_structure` argument, if given.
#' - `ns_slices`: The `ns_slices` argument, if given.
#' - `results`: A dataframe (S-FFA) or list of dataframes (NS-FFA).
#'
#' Each dataframe within `results` has four columns:
#' - `estimates`: Estimated quantiles for each return period.
#' - `ci_lower`: Lower bound of the confidence interval for each return period.
#' - `ci_upper`: Upper bound of the confidence interval for each return period.
#' - `periods`: The `periods` argument. 
#'
#' @details
#' 1. Uses [fit_gmle()] to obtain the maximum generalized log‐likelihood.  
#' 2. Defines an objective function \eqn{f(y_p, p)} by reparameterizing the 
#'    generalized log-likelihood.
#' 3. Iteratively brackets the root by rescaling initial guesses by 0.05 until 
#'    \eqn{f(y_p, p)} changes sign.  
#' 4. Uses the regula-falsi method to solve \eqn{f(y_p, p) = 0} for each 
#'    return period probability.  
#' 5. Returns lower and upper confidence bounds at significance level `alpha`.
#'
#' @note RFGPL uncertainty quantification can be numerically unstable for some datasets. 
#' If this function encounters an issue, it will return an error and recommend 
#' [uncertainty_bootstrap()] instead.
#'
#' @seealso [quantile_fast()], [uncertainty_bootstrap()], [plot_sffa()], [plot_nsffa()]
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
#' likelihood method for nonstationary hydrological frequency analysis. Stoch Environ Res 
#' Risk Assess 38, 843–867 (2024). \doi{10.1007/s00477-023-02603-0}
#'
#' @importFrom stats qchisq nlminb
#' @export

uncertainty_rfgpl <- function(
    data,
	prior,
    ns_years = NULL,
    ns_structure = NULL,
    ns_slices = NULL,
    alpha = 0.05,
	periods = c(2, 5, 10, 20, 50, 100),
    tolerance = 1e-2
) {

	data <- validate_numeric("data", data)
	prior <- validate_numeric("prior", prior, size = 2, bounds = c(0, Inf))
	years <- validate_numeric("ns_years", ns_years, size = length(data))
	structure <- validate_structure(ns_structure)
	slices <- validate_numeric("ns_slices", ns_slices)
	alpha <- validate_float("alpha", alpha, bounds = c(0.01, 0.1))
	periods <- validate_numeric("periods", periods, FALSE, bounds = c(1, Inf))
	tolerance <- validate_float("tolerance", tolerance, bounds = c(0, 1))

	# Call the more general helper method
	results <- uncertainty_regula_falsi(
		data,
		"GEV",
		prior,
		years,
		structure,
		slices,
		alpha,
		periods,
		tolerance
	)

	list(
		method = "RFGPL",
		ns_structure = ns_structure,
		ns_slices = ns_slices,
		results = results
	)
}

