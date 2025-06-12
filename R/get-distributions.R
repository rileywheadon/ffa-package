#' Retrieve Candidate Distribution Specifications
#'
#' @description
#' Returns a named list of distribution specifications used for flood-frequency analysis.
#' Each element is itself a list describing one distribution’s:
#' \itemize{
#'   \item \code{name}: three-letter acronym.
#'   \item \code{quantile}: takes (periods, params) and returns quantiles.
#'   \item \code{estimate}: takes (ams) and returns parameter estimates.
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
#' \code{GNO}, \code{WEI}.  Each element is a list as described above.
#'
#' @import lmom
#' @export

get.distributions <- function() {

	# --- CUSTOM QUANTILE, PARAMETER ESTIMATION, AND LMR FUNCTIONS --- #

	# Custom quantile function for GEV that flips the third parameter
	gev.qua <- function(periods, params) {
		params[3] <- -params[3]
		quagev(periods, params)
	}

	# Custom parameter estimation for GEV that flips the third parameter
	gev.pel <- function(ams) {
		result <- unname(pelgev(samlmu(ams)))
		result[3] <- -result[3]
		result
	}

	# Custom LMR function for GEV that flips the third parameter
	gev.lmr <- function(params) {
		params[3] <- -params[3]
		unname(lmrgev(params, nmom = 4))
	}

	# Custom pel function for LNO that removes the bound
	lno.pel <- function(ams) {
		params <- unname(pelln3(samlmu(ams), bound = 0))
		params[-1]
	}

	# Custom lmr function for the Weibull distribution based on lmrgev
	wei.lmr <- function(params) {
		result <- unname(lmrgev(params, nmom = 4))
		result[3] <- -result[3]
		result
	}

	list(
		GUM = list(
			name = "GUM",
			quantile = function(periods, params) quagum(periods, params),
			estimate = function(ams) unname(pelgum(samlmu(ams))),
			t3_t4 = c(0.1699, 0.1504),
			n_params = 2,
			log = FALSE
		),
		NOR = list(
			name = "NOR",
			quantile = function(periods, params) quanor(periods, params),
			estimate = function(ams) unname(pelnor(samlmu(ams))),
			t3_t4 = c(0, 0.1226),
			n_params = 2,
			log = FALSE
		),
		LNO = list(
			name = "LNO",
			quantile = function(periods, params) qualn3(periods, c(0, params)),
			estimate = function(ams) lno.pel(ams),
			t3_t4 = c(0, 0.1226),
			n_params = 2,
			log = TRUE
		),
		GEV = list(
			name = "GEV",
			quantile = function(periods, params) gev.qua(periods, params),
			estimate = function(ams) gev.pel(ams),
			lmr_function = function(params) gev.lmr(params),
			kappa_lower = -9,
			kappa_upper = 0.999,
			n_params = 3,
			log = FALSE
		),
		GLO = list(
			name = "GLO",
			quantile = function(periods, params) quaglo(periods, params),
			estimate = function(ams) unname(pelglo(samlmu(ams))),
			lmr_function = function(params) unname(lmrglo(params, nmom = 4)),
			kappa_lower = -0.999, 
			kappa_upper = 0.999,
			n_params = 3,
			log = FALSE
		),
		PE3 = list(
			name = "PE3",
			quantile = function(periods, params) quape3(periods, params),
			estimate = function(ams) unname(pelpe3(samlmu(ams))),
			lmr_function = function(params) unname(lmrpe3(params, nmom = 4)),
			kappa_lower = -10,
			kappa_upper = 10,
			n_params = 3,
			log = FALSE
		),
		LP3 = list(
			name = "LP3",
			quantile = function(periods, params) exp(quape3(periods, params)),
			estimate = function(ams) unname(pelpe3(samlmu(log(ams)))),
			lmr_function = function(params) unname(lmrpe3(params, nmom = 4)),
			kappa_lower = -10,
			kappa_upper = 10,
			n_params = 3,
			log = TRUE
		),
		GNO = list(
			name = "GNO",
			quantile = function(periods, params) quagno(periods, params),
			estimate = function(ams) unname(pelgno(samlmu(ams))),
			lmr_function = function(params) unname(lmrgno(params, nmom = 4)),
			kappa_lower = -4,
			kappa_upper = 4,
			n_params = 3,
			log = FALSE
		),
		WEI = list(
			name = "WEI",
			quantile = function(periods, params) quawei(periods, params),
			estimate = function(ams) unname(pelwei(samlmu(ams))),
			lmr_function = function(params) wei.lmr(params),
			kappa_lower = -0.999,
			kappa_upper = 9,
			n_params = 3,
			log = FALSE
		)
	)
}
