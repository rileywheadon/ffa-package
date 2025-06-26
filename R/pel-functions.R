#' Parameter Estimation with L-Moments
#'
#' Estimate the parameters of one of nine different distributions (`GUM`, `NOR`, 
#' `LNO`, `GEV`, `GLO`, `GNO`, `PE3`, `LP3`, and `WEI`) using the method of L-moments. 
#'
#' @param data Numeric; a vector of annual maximum streamflow data.
#'
#' @details 
#' First, the sample L-moments of the data are computed using the \link{lmom.sample} 
#' method. Then formulas from Hosking (1997) are used to compute the parameters from the
#' L-moments. Distributions `GNO`, `PE3`, and `LP3` use a rational approximation to compute
#' the parameters.
#'
#' @return Numeric; a vector of parameters. 
#'
#' @references
#' Hosking, J.R.M. & Wallis, J.R., 1997. Regional frequency analysis: an approach based 
#' on L-Moments. Cambridge University Press, New York, USA.
#'
#' @examples
#' data <- rnorm(n = 100, mean = 100, sd = 10)
#' pellp3(data)
#'
#' @name pel-functions
NULL

#' @rdname pel-functions
#' @export
pelgum <- function(data) pelxxx("GUM", data)

#' @rdname pel-functions
#' @export
pelnor <- function(data) pelxxx("NOR", data)

#' @rdname pel-functions
#' @export
pellno <- function(data) pelxxx("LNO", data)

#' @rdname pel-functions
#' @export
pelgev <- function(data) pelxxx("GEV", data)

#' @rdname pel-functions
#' @export
pelglo <- function(data) pelxxx("GLO", data)

#' @rdname pel-functions
#' @export
pelgno <- function(data) pelxxx("GNO", data)

#' @rdname pel-functions
#' @export
pelpe3 <- function(data) pelxxx("PE3", data)

#' @rdname pel-functions
#' @export
pellp3 <- function(data) pelxxx("LP3", data)

#' @rdname pel-functions
#' @export
pelwei <- function(data) pelxxx("WEI", data)
