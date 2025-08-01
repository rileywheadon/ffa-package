#' Parameter 'data'
#'
#' @param data Numeric vector of annual maximum series values.
#' Must be strictly positive, finite, and not missing.
#'
#' @name param-data
#'
#' @keywords internal
NULL

#' Parameter 'p'
#'
#' @param p Numeric vector of probabilities between 0 and 1 with no missing values.
#'
#' @name param-p
#'
#' @keywords internal
NULL

#' Parameter 'distribution'
#'
#' @param distribution Character scalar. A three-character code indicating 
#' a distribution family. Must be one of: `"GUM"`, `"NOR"`, `"LNO"`, 
#' `"GEV"`, `"GLO"`, `"GNO"`, `"PE3"`, `"LP3"`, or `"WEI"`.
#'
#' @name param-distribution
#'
#' @keywords internal
NULL

#' Parameter 'method'
#'
#' @param method Character scalar specifying the estimation method.
#' Must be `"L-moments"`, `"MLE"`, or `"GMLE"`.
#' 
#' @name param-method
#'
#' @keywords internal
NULL

#' Parameter 'params'
#'
#' @param params Numeric vector of distribution parameters, in the order (location,
#' scale, shape). The length must be between 2 and 5, depending on the specified
#' `distribution` and `structure`.
#'
#' @name param-params
#'
#' @keywords internal
NULL

#' Parameter 'prior'
#'
#' @param prior Numeric vector of length 2. Specifies the Beta prior shape 
#' parameters \eqn{(p, q)} for the shape parameter \eqn{\kappa}. 
#' Only used when `distribution = "GEV"`.
#'
#' @name param-prior
#'
#' @keywords internal
NULL

#' Parameter 'years'
#'
#' @param years Numeric vector of observation years corresponding to `data`.
#' Must be the same length as `data` and strictly increasing.
#'
#' @name param-years
#'
#' @keywords internal
NULL

#' Parameter 'structure'
#'
#' @param structure Named list indicating which distribution parameters are 
#' modeled as nonstationary. Must contain two logical scalars:
#' - `location`: If `TRUE`, the location parameter has a linear temporal trend.
#' - `scale`: If `TRUE`, the scale parameter has a linear temporal trend.
#'
#' @name param-structure
#'
#' @keywords internal
NULL

#' Parameter 'slice'
#'
#' @param slice Numeric scalar specifying the year at which to evaluate the 
#' quantiles of a nonstationary probability distribution. The slice does not 
#' have to be an element of the `years` argument. Note that if `structure$location` 
#' and `structure$scale` are both `FALSE`, this argument will have no effect the 
#' output of the function.
#'
#' @name param-slice
#'
#' @keywords internal
NULL 

#' Parameter 'slices'
#'
#' @param slices Numeric vector specifying the years at which to evaluate the 
#' return levels confidence intervals of a nonstationary probability distribution. 
#' The slices do not have to be elements of the `years` argument. 
#'
#' @name param-slices
#'
#' @keywords internal
NULL 


#' Parameter 'alpha'
#'
#' @param alpha Numeric scalar in \eqn{[0.01, 0.1]}. The significance 
#' level for confidence intervals or hypothesis tests. Default is 0.05. 
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
#' test to the console. Default is `TRUE`.
#'
#' @name param-quiet
#'
#' @keywords internal
NULL

#' Parameter 'periods'
#' 
#' @param periods Numeric vector used to set the return periods for FFA.
#' All entries must be greater than or equal to 1.
#'
#' @name param-periods
#'
#' @keywords internal
NULL

