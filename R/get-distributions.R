#' Retrieve Candidate Distribution Specifications
#'
#' @description
#' Returns a named list of distribution specifications used for flood-frequency analysis.
#' Each element is itself a list describing one distribution’s:
#' \itemize{
#'   \item \code{name}: three-letter acronym.
#'   \item \code{quantile}: function(ams, params) → quantiles.
#'   \item \code{estimate}: function(ams) → parameter estimates.
#'   \item \code{n_params}: integer number of distribution parameters.
#'   \item \code{log}: logical; \code{TRUE} if fit on \code{log(ams)}, \code{FALSE} otherwise.
#' }
#'
#' For two-parameter distributions, the element also contains:
#' \itemize{
#'   \item \code{t3_t4}: numeric \eqn{(L\;skewness,\;L\;kurtosis)} values.
#' }
#'
#' For three-parameter distributions, the element also contains:
#' \itemize{
#'   \item \code{lmr_function}: function(params) → vector of the first four L-moment ratios.
#'   \item \code{kappa_lower}, \code{kappa_upper}: numeric shape-parameter bounds.
#' }
#'
#' @return A named \code{list} of distribution specifications. Valid names:
#' \code{GEV}, \code{GUM}, \code{NOR}, \code{LNO}, \code{GLO}, \code{PE3}, \code{LP3},
#' \code{GNO}, \code{WEI}, \code{GPA}.  Each element is a list as described above.
#'
#' @import lmom
#' @export

get.distributions <- function() {

	# Custom lmr function for the Weibull distribution based on lmrgev
	lmrwei <- function(params) {
		result <- unname(lmrgev(params, nmom = 4))
		result[3] <- -result[3]
		result
	}

	list(
		GEV = list(
			name = "GEV",
			quantile = function(ams, params) quagev(ams, params),
			estimate = function(ams) unname(pelgev(samlmu(ams))),
			lmr_function = function(params) unname(lmrgev(params, nmom = 4)),
			kappa_lower = -0.999,
			kappa_upper = 9,
			n_params = 3,
			log = FALSE
		),
		GUM = list(
			name = "GUM",
			quantile = function(ams, params) quagum(ams, params),
			estimate = function(ams) unname(pelgum(samlmu(ams))),
			t3_t4 = c(0.1699, 0.1504),
			n_params = 2,
			log = FALSE
		),
		NOR = list(
			name = "NOR",
			quantile = function(ams, params) quanor(ams, params),
			estimate = function(ams) unname(pelnor(samlmu(ams))),
			t3_t4 = c(0, 0.1226),
			n_params = 2,
			log = FALSE
		),
		LNO = list(
			name = "LNO",
			quantile = function(ams, params) qualn3(ams, params),
			estimate = function(ams) unname(pelln3(samlmu(ams), bound = 0)),
			t3_t4 = c(0, 0.1226),
			n_params = 2,
			log = TRUE
		),
		GLO = list(
			name = "GLO",
			quantile = function(ams, params) quaglo(ams, params),
			estimate = function(ams) unname(pelglo(samlmu(ams))),
			lmr_function = function(params) unname(lmrglo(params, nmom = 4)),
			kappa_lower = -0.999, 
			kappa_upper = 0.999,
			n_params = 3,
			log = FALSE
		),
		PE3 = list(
			name = "PE3",
			quantile = function(ams, params) quape3(ams, params),
			estimate = function(ams) unname(pelpe3(samlmu(ams))),
			lmr_function = function(params) unname(lmrpe3(params, nmom = 4)),
			kappa_lower = -10,
			kappa_upper = 10,
			n_params = 3,
			log = FALSE
		),
		LP3 = list(
			name = "LP3",
			quantile = function(ams, params) exp(quape3(ams, params)),
			estimate = function(ams) unname(pelpe3(samlmu(log(ams)))),
			lmr_function = function(params) unname(lmrpe3(params, nmom = 4)),
			kappa_lower = -10,
			kappa_upper = 10,
			n_params = 3,
			log = TRUE
		),
		GNO = list(
			name = "GNO",
			quantile = function(ams, params) quagno(ams, params),
			estimate = function(ams) unname(pelgno(samlmu(ams))),
			lmr_function = function(params) unname(lmrgno(params, nmom = 4)),
			kappa_lower = -4,
			kappa_upper = 4,
			n_params = 3,
			log = FALSE
		),
		WEI = list(
			name = "WEI",
			quantile = function(ams, params) quawei(ams, params),
			estimate = function(ams) unname(pelwei(samlmu(ams))),
			lmr_function = function(params) lmrwei(params),
			kappa_lower = -0.999,
			kappa_upper = 9,
			n_params = 3,
			log = FALSE
		),
		GPA = list(
			name = "GPA",
			quantile = function(ams, params) quagpa(ams, params),
			estimate = function(ams) unname(pelgpa(samlmu(ams))),
			lmr_function = function(params) unname(lmrgpa(params, nmom = 4)),
			kappa_lower = -1,
			kappa_upper = 45,
			n_params = 3,
			log = FALSE
		)
	)
}
