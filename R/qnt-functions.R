#' Quantile Functions for Probability Models
#'
#' Compute the quantiles for stationary and non-stationary variants 
#' of nine different distributions (`GUM`, `NOR`, `LNO`, `GEV`, `GLO`, 
#' `GNO`, `PE3`, `LP3`, and `WEI`).
#' 
#' @details 
#' The quantile function is the inverse of the cumulative distribution function.
#' For the two-parameter distributions (`GUM`, `NOR`, `LNO`), there are three 
#' different `qnt` functions:
#' 
#' - `qnt...()`: Stationary location and scale, 2 parameters.
#' - `qnt...10()`: Time-varying location, stationary scale, 3 parameters.
#' - `qnt...11()`: Time-varying location and scale, 4 parameters.
#'
#' For three-parameter distributions (`GEV`, `GLO`, `GNO`, `PE3`, `LP3`, `WEI`), 
#' there are also three `qnt` functions:
#'
#' - `qnt...()`: Stationary location and scale, 3 parameters.
#' - `qnt...100()`: Time-varying location, stationary scale, 4 parameters.
#' - `qnt...110()`: Time-varying location and scale, 5 parameters.
#'
#' @note
#' The `qnt...`, functions perform extensive parameter validation, which can be slow. 
#' If you plan to make many calls to these methods, it is recommended to use 
#' the \link{qntxxx} helper function instead.
#'
#' @param p Numeric; a vector of probabilities between 0 and 1.
#'
#' @param params Numeric; a vector of parameters. Must have the correct length for the model.
#'
#' @param years Numeric; a vector of years with the same length as `data`.
#'   Required for non-stationary models, which end in `10`, `11`, `100`, or `110`.
#'
#' @return If `p` or `years` is a scalar, returns a numeric vector. Otherwise, returns a matrix.
#'
#' @seealso \link{qntxxx}
#'
#' @examples
#' # Initialize p, years, and params
#' p <- runif(n = 10)
#' years <- seq(from = 1901, to = 2000)
#' params <- c(0, 1, 1, 1, 0)
#'
#' # Compute the quantiles
#' qntwei110(p, params, years)
#'
#' @name qnt-functions
NULL

qntvalidate <- function(n, p, params, years = NULL) {

	# Validate the data vector
	if (!is.numeric(p) | !is.vector(p)) stop("'p' must be a numeric vector.")
	if (any(is.nan(p)) | any(is.na(p))) stop("'p' must not contain NaN/NA values.")
	if (any(p < 0) | any(p > 1)) stop("'p' must be between 0 and 1 inclusive.")

	# Validate the parameter vector
	if (!is.numeric(params) | !is.vector(params)) stop("'params' must be a numeric vector.")
	if (any(is.nan(params)) | any(is.na(params))) stop("'params' must not contain NaN/NA values.")
	if (length(params) != n) stop(sprintf("'params' must have length %d.", n))

	# Validate the years vector
	if (!is.null(years)) {
		if (!is.numeric(years) | !is.vector(years)) stop("'years' must be a numeric vector.")
		if (any(is.nan(years)) | any(is.na(years))) stop("'years' must not contain NaN/NA values.")
	}

}

#' @rdname qnt-functions
#' @export
qntgum    <- function(p, params, years = NULL) {
	qntvalidate(2, p, params, years)
	qntxxx("GUM", NULL, p, params)
}

#' @rdname qnt-functions
#' @export
qntgum10  <- function(p, params, years) {
	qntvalidate(3, p, params, years)
	covariates <- get.covariates(years)
	qntxxx("GUM", "10", p, params, covariates)
}

#' @rdname qnt-functions
#' @export
qntgum11  <- function(p, params, years) {
	qntvalidate(4, p, params, years)
	covariates <- get.covariates(years)
	qntxxx("GUM", "11", p, params, covariates)
}


#' @rdname qnt-functions
#' @export
qntnor    <- function(p, params, years = NULL) {
	qntvalidate(2, p, params, years)
	qntxxx("NOR", NULL, p, params)
}

#' @rdname qnt-functions
#' @export
qntnor10  <- function(p, params, years) {
	qntvalidate(3, p, params, years)
	covariates <- get.covariates(years)
	qntxxx("NOR", "10", p, params, covariates)
}

#' @rdname qnt-functions
#' @export
qntnor11  <- function(p, params, years) {
	qntvalidate(4, p, params, years)
	covariates <- get.covariates(years)
	qntxxx("NOR", "11", p, params, covariates)
}


#' @rdname qnt-functions
#' @export
qntlno    <- function(p, params, years = NULL) {
	qntvalidate(2, p, params, years)
	qntxxx("LNO", NULL, p, params)
}

#' @rdname qnt-functions
#' @export
qntlno10  <- function(p, params, years) {
	qntvalidate(3, p, params, years)
	covariates <- get.covariates(years)
	qntxxx("LNO", "10", p, params, covariates)
}

#' @rdname qnt-functions
#' @export
qntlno11  <- function(p, params, years) {
	qntvalidate(4, p, params, years)
	covariates <- get.covariates(years)
	qntxxx("LNO", "11", p, params, covariates)
}


#' @rdname qnt-functions
#' @export
qntgev    <- function(p, params, years = NULL) {
	qntvalidate(3, p, params, years)
	qntxxx("GEV", NULL,  p, params)
}

#' @rdname qnt-functions
#' @export
qntgev100 <- function(p, params, years) {
	qntvalidate(4, p, params, years)
	covariates <- get.covariates(years)
	qntxxx("GEV", "10", p, params, covariates)
}

#' @rdname qnt-functions
#' @export
qntgev110 <- function(p, params, years) {
	qntvalidate(5, p, params, years)
	covariates <- get.covariates(years)
	qntxxx("GEV", "11", p, params, covariates)
}


#' @rdname qnt-functions
#' @export
qntglo    <- function(p, params, years = NULL) {
	qntvalidate(3, p, params, years)
	qntxxx("GLO", NULL, p, params)
}

#' @rdname qnt-functions
#' @export
qntglo100 <- function(p, params, years) {
	qntvalidate(4, p, params, years)
	covariates <- get.covariates(years)
	qntxxx("GLO", "10", p, params, covariates)
}

#' @rdname qnt-functions
#' @export
qntglo110 <- function(p, params, years) {
	qntvalidate(5, p, params, years)
	covariates <- get.covariates(years)
	qntxxx("GLO", "11", p, params, covariates)
}


#' @rdname qnt-functions
#' @export
qntgno    <- function(p, params, years = NULL) {
	qntvalidate(3, p, params, years)
	qntxxx("GNO", NULL, p, params)
}

#' @rdname qnt-functions
#' @export
qntgno100 <- function(p, params, years) {
	qntvalidate(4, p, params, years)
	covariates <- get.covariates(years)
	qntxxx("GNO", "10", p, params, covariates)
}

#' @rdname qnt-functions
#' @export
qntgno110 <- function(p, params, years) {
	qntvalidate(5, p, params, years)
	covariates <- get.covariates(years)
	qntxxx("GNO", "11", p, params, covariates)
}


#' @rdname qnt-functions
#' @export
qntpe3    <- function(p, params, years = NULL) {
	qntvalidate(3, p, params, years)
	qntxxx("PE3", NULL, p, params)
}

#' @rdname qnt-functions
#' @export
qntpe3100 <- function(p, params, years) {
	qntvalidate(4, p, params, years)
	covariates <- get.covariates(years)
	qntxxx("PE3", "10", p, params, covariates)
}

#' @rdname qnt-functions
#' @export
qntpe3110 <- function(p, params, years) {
	qntvalidate(5, p, params, years)
	covariates <- get.covariates(years)
	qntxxx("PE3", "11", p, params, covariates)
}


#' @rdname qnt-functions
#' @export
qntlp3    <- function(p, params, years = NULL) {
	qntvalidate(3, p, params, years)
	qntxxx("LP3", NULL, p, params)
}

#' @rdname qnt-functions
#' @export
qntlp3100 <- function(p, params, years) {
	qntvalidate(4, p, params, years)
	covariates <- get.covariates(years)
	qntxxx("LP3", "10", p, params, covariates)
}

#' @rdname qnt-functions
#' @export
qntlp3110 <- function(p, params, years) {
	qntvalidate(5, p, params, years)
	covariates <- get.covariates(years)
	qntxxx("LP3", "11", p, params, covariates)
}


#' @rdname qnt-functions
#' @export
qntwei    <- function(p, params, years = NULL) {
	qntvalidate(3, p, params, years)
	qntxxx("WEI", NULL, p, params)
}

#' @rdname qnt-functions
#' @export
qntwei100 <- function(p, params, years) {
	qntvalidate(4, p, params, years)
	covariates <- get.covariates(years)
	qntxxx("WEI", "10", p, params, covariates)
}

#' @rdname qnt-functions
#' @export
qntwei110 <- function(p, params, years) {
	qntvalidate(5, p, params, years)
	covariates <- get.covariates(years)
	qntxxx("WEI", "11", p, params, covariates)
}


#' @rdname qnt-functions
#' @export
qntkap    <- function(p, params, years = NULL) {
	qntvalidate(4, p, params, years)
	qntxxx("KAP", NULL, p, params)
}

