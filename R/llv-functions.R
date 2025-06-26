#' Log-Likelihood Functions for Probability Models
#'
#' Compute the log-likelihood value for stationary and non-stationary variants 
#' of nine different distributions (`GUM`, `NOR`, `LNO`, `GEV`, `GLO`, `GNO`, 
#' `PE3`, `LP3`, and `WEI`).
#' 
#' @details 
#' The log-likelihood is the logarithm of the probability density function.
#' For two-parameter distributions (`GUM`, `NOR`, `LNO`), there are three 
#' different `llv` functions:
#' 
#' - `llv...()`: Stationary location and scale, 2 parameters.
#' - `llv...10()`: Time-varying location, stationary scale, 3 parameters.
#' - `llv...11()`: Time-varying location and scale, 4 parameters.
#'
#' For three-parameter distributions (`GEV`, `GLO`, `GNO`, `PE3`, `LP3`, `WEI`), 
#' there are also three different `llv` functions:
#'
#' - `llv...()`: Stationary location and scale, 3 parameters.
#' - `llv...100()`: Time-varying location, stationary scale, 4 parameters.
#' - `llv...110()`: Time-varying location and scale, 5 parameters.
#'
#' @note
#' The `llv...` functions perform extensive parameter validation, which can be slow. 
#' If you plan to make calls these methods often, it is recommended to use the \link{llvxxx} 
#' helper function instead.
#'
#' @param data Numeric; a vector of annual maximum streamflow data.
#'
#' @param params Numeric; a vector of parameters. Must have the correct length for the model.
#'
#' @param years Numeric; a vector of years with the same length as `data`.
#'   Required for non-stationary models, which end in `10`, `11`, `100`, or `110`.
#'
#' @return Numeric (1); the log-likelihood value.
#'
#' @seealso \link{llvxxx}
#'
#' @examples
#' # Initialize data, years, and params
#' data <- rnorm(n = 100, mean = 100, sd = 10)
#' years <- seq(from = 1901, to = 2000)
#' params <- c(0, 1, 1, 0)
#'
#' # Compute the log-likelihood
#' llvgno100(data, params, years)
#'
#' @name llv-functions
NULL

llvvalidate <- function(n, data, params, years = NULL) {

	# Validate the data vector
	if (!is.numeric(data) | !is.vector(data)) stop("'data' must be a numeric vector.")
	if (any(is.nan(data)) | any(is.na(data))) stop("'data' must not contain NaN/NA values.")
	if (any(data <= 0)) stop("'data' must not contain negative values.")

	# Validate the parameter vector
	if (!is.numeric(params) | !is.vector(params)) stop("'params' must be a numeric vector.")
	if (any(is.nan(params)) | any(is.na(params))) stop("'params' must not contain NaN/NA values.")
	if (length(params) != n) stop(sprintf("'params' must have length %d.", n))

	# Validate the years vector
	if (!is.null(years)) {
		if (!is.numeric(years) | !is.vector(years)) stop("'years' must be a numeric vector.")
		if (any(is.nan(years)) | any(is.na(years))) stop("'years' must not contain NaN/NA values.")
		if (length(years) != length(data)) stop("'years' must have the same length as 'data'.")
	}

}

#' @rdname llv-functions
#' @export
llvgum    <- function(data, params, years = NULL) {
	llvvalidate(2, data, params, years)
	llvxxx("GUM", NULL, data, params, NULL)
}

#' @rdname llv-functions
#' @export
llvgum10  <- function(data, params, years) {
	llvvalidate(3, data, params, years)
	covariate <- get.covariates(years)
	llvxxx("GUM", "10", data, params, covariate)
}

#' @rdname llv-functions
#' @export
llvgum11  <- function(data, params, years) {
	llvvalidate(4, data, params, years)
	covariate <- get.covariates(years)
	llvxxx("GUM", "11", data, params, covariate)
}


#' @rdname llv-functions
#' @export
llvnor    <- function(data, params, years = NULL) {
	llvvalidate(2, data, params, years)
	llvxxx("NOR", NULL, data, params, NULL)
}

#' @rdname llv-functions
#' @export
llvnor10  <- function(data, params, years) {
	llvvalidate(3, data, params, years)
	covariate <- get.covariates(years)
	llvxxx("NOR", "10", data, params, covariate)
}

#' @rdname llv-functions
#' @export
llvnor11  <- function(data, params, years) {
	llvvalidate(4, data, params, years)
	covariate <- get.covariates(years)
	llvxxx("NOR", "11", data, params, covariate)
}


#' @rdname llv-functions
#' @export
llvlno    <- function(data, params, years = NULL) {
	llvvalidate(2, data, params, years)
	llvxxx("LNO", NULL, data, params, NULL)
}

#' @rdname llv-functions
#' @export
llvlno10  <- function(data, params, years) {
	llvvalidate(3, data, params, years)
	covariate <- get.covariates(years)
	llvxxx("LNO", "10", data, params, covariate)
}

#' @rdname llv-functions
#' @export
llvlno11  <- function(data, params, years) {
	llvvalidate(4, data, params, years)
	covariate <- get.covariates(years)
	llvxxx("LNO", "11", data, params, covariate)
}


#' @rdname llv-functions
#' @export
llvgev    <- function(data, params, years = NULL) {
	llvvalidate(3, data, params, years)
	llvxxx("GEV", NULL, data, params, NULL)
}

#' @rdname llv-functions
#' @export
llvgev100 <- function(data, params, years) {
	llvvalidate(4, data, params, years)
	covariate <- get.covariates(years)
	llvxxx("GEV", "10", data, params, covariate)
}

#' @rdname llv-functions
#' @export
llvgev110 <- function(data, params, years) {
	llvvalidate(5, data, params, years)
	covariate <- get.covariates(years)
	llvxxx("GEV", "11", data, params, covariate)
}


#' @rdname llv-functions
#' @export
llvglo    <- function(data, params, years = NULL) {
	llvvalidate(3, data, params, years)
	llvxxx("GLO", NULL, data, params, NULL)
}

#' @rdname llv-functions
#' @export
llvglo100 <- function(data, params, years) {
	llvvalidate(4, data, params, years)
	covariate <- get.covariates(years)
	llvxxx("GLO", "10", data, params, covariate)
}

#' @rdname llv-functions
#' @export
llvglo110 <- function(data, params, years) {
	llvvalidate(5, data, params, years)
	covariate <- get.covariates(years)
	llvxxx("GLO", "11", data, params, covariate)
}


#' @rdname llv-functions
#' @export
llvgno    <- function(data, params, years = NULL) {
	llvvalidate(3, data, params, years)
	llvxxx("GNO", NULL, data, params, NULL)
}

#' @rdname llv-functions
#' @export
llvgno100 <- function(data, params, years) {
	llvvalidate(4, data, params, years)
	covariate <- get.covariates(years)
	llvxxx("GNO", "10", data, params, covariate)
}

#' @rdname llv-functions
#' @export
llvgno110 <- function(data, params, years) {
	llvvalidate(5, data, params, years)
	covariate <- get.covariates(years)
	llvxxx("GNO", "11", data, params, covariate)
}


#' @rdname llv-functions
#' @export
llvpe3    <- function(data, params, years = NULL) {
	llvvalidate(3, data, params, years)
	llvxxx("PE3", NULL, data, params, NULL)
}

#' @rdname llv-functions
#' @export
llvpe3100 <- function(data, params, years) {
	llvvalidate(4, data, params, years)
	covariate <- get.covariates(years)
	llvxxx("PE3", "10", data, params, covariate)
}

#' @rdname llv-functions
#' @export
llvpe3110 <- function(data, params, years) {
	llvvalidate(5, data, params, years)
	covariate <- get.covariates(years)
	llvxxx("PE3", "11", data, params, covariate)
}


#' @rdname llv-functions
#' @export
llvlp3    <- function(data, params, years = NULL) {
	llvvalidate(3, data, params, years)
	llvxxx("LP3", NULL, data, params, NULL)
}

#' @rdname llv-functions
#' @export
llvlp3100 <- function(data, params, years) {
	llvvalidate(4, data, params, years)
	covariate <- get.covariates(years)
	llvxxx("LP3", "10", data, params, covariate)
}

#' @rdname llv-functions
#' @export
llvlp3110 <- function(data, params, years) {
	llvvalidate(5, data, params, years)
	covariate <- get.covariates(years)
	llvxxx("LP3", "11", data, params, covariate)
}


#' @rdname llv-functions
#' @export
llvwei    <- function(data, params, years = NULL) {
	llvvalidate(3, data, params, years)
	llvxxx("WEI", NULL, data, params, NULL)
}

#' @rdname llv-functions
#' @export
llvwei100 <- function(data, params, years) {
	llvvalidate(4, data, params, years)
	covariate <- get.covariates(years)
	llvxxx("WEI", "10", data, params, covariate)
}

#' @rdname llv-functions
#' @export
llvwei110 <- function(data, params, years) {
	llvvalidate(5, data, params, years)
	covariate <- get.covariates(years)
	llvxxx("WEI", "11", data, params, covariate)
}
