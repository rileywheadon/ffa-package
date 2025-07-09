#' Parameter 'data'
#'
#' @param data Numeric vector of annual maximum streamflow (AMS) values.
#'   Must be strictly positive, finite, and non-missing.
#'
#' @name param-data
#'
#' @keywords internal
NULL

#' Parameter 'model'
#'
#' @param model Character scalar. A three-character code indicating 
#'   a distribution family. Must be one of:  `"GUM"`, `"NOR"`, `"LNO"`, 
#'   `"GEV"`, `"GLO"`, `"GNO"`, `"PE3"`, `"LP3"`, or `"WEI"`.
#'
#' @name param-model
#'
#' @keywords internal
NULL

#' Parameter 'method'
#'
#' @param method Character scalar specifying the estimation method.
#'   Must be `"L-moments"`, `"MLE"`, or `"GMLE"`.
#' 
#' @name param-method
#'
#' @keywords internal
NULL

#' Parameter 'params'
#'
#' @param params Numeric vector of distribution parameters, in the order (location,
#'   scale, shape). The length must be between 2 and 5, depending on the specified
#'   `model` and `trend`.
#'
#' @name param-params
#'
#' @keywords internal
NULL

#' Parameter 'prior'
#'
#' @param prior Numeric vector of length 2. Specifies the Beta prior shape 
#'   parameters \eqn{(p, q)} for the shape parameter \eqn{\kappa}. 
#'   Only used when `model = "GEV"`.
#'
#' @name param-prior
#'
#' @keywords internal
NULL

#' Parameter 'years'
#'
#' @param years Numeric vector of observation years corresponding to `data`.
#'   Must be the same length as `data` and strictly increasing.
#'
#' @name param-years
#'
#' @keywords internal
NULL

#' Parameter 'trend'
#'
#' @param trend Named list indicating which distribution parameters are 
#'   modeled as non-stationary. Must contain:
#'   - `location`: Logical scalar. If `TRUE`, the location parameter has a trend.
#'   - `scale`: Logical scalar. If `TRUE`, the scale parameter has a trend.
#'
#' @name param-trend
#'
#' @keywords internal
NULL

#' Parameter 'slice'
#'
#' @param slice Numeric scalar specifying the year at which to evaluate the 
#' quantiles or confidence intervals of a non-stationary probability distribution. 
#' The year does not have to be an element of the `years` argument. Note that if 
#' `trend$location` and `trend$scale` are both `FALSE`, this argument will have 
#' no effect the output of the function.
#'
#' @name param-slice
#'
#' @keywords internal
NULL 

#' Parameter 'alpha'
#'
#' @param alpha Numeric scalar in \eqn{[0.01, 0.1]}. The significance 
#'  level for confidence intervals or hypothesis tests. Default is 0.05. 
#'
#' @name param-alpha
#'
#' @keywords internal
NULL

#' Parameter 'samples'
#'
#' @param samples Integer scalar. The number of bootstrap samples. Default is 10000.
#'
#' @name param-samples
#'
#' @keywords internal
NULL

#' Parameter 'quiet'
#' 
#' @param quiet Logical scalar. If `FALSE`, prints a summary of of the statistical
#'   test to the console. Default is `TRUE`.
#'
#' @name param-quiet
#'
#' @keywords internal
NULL

