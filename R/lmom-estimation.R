#' Estimate Distribution Parameters via L-Moments
#'
#' @description
#' Computes parameter estimates for a specified distribution using the method of
#' L-moments. The function looks up the distribution’s estimation function from
#' \code{\link{get.distributions}()} and applies it to the observed annual maximum
#' series (AMS).
#'
#' @param ams Numeric vector of observed annual maximum streamflow values.
#' @param distribution String giving the three-letter acronym of the target
#'   distribution. Must match one of the names returned by \code{names(get.distributions())}.
#'
#' @return A numeric vector of estimated distribution parameters, in the order
#'   expected by the corresponding quantile function. The number of components
#'   equals the distribution’s \code{n_params}.
#'
#' @details
#' Internally, \code{lmom.estimation} calls:
#' \preformatted{
#'   dist_list <- get.distributions()
#'   dist_list[[distribution]]$estimate(ams)
#' }
#' If \code{distribution} is not a valid key, this will raise an error. Users
#' should ensure that \code{ams} contains no missing values and that
#' \code{length(ams)} is sufficient for L-moment estimation.
#'
#' @seealso
#' \code{\link{get.distributions}}
#'
#' @export

lmom.estimation <- function(ams, distribution) {
	
	# Get the distribution function and call it on the data
	distribution_list <- get.distributions()	
	distribution_list[[distribution]]$estimate(ams)

}
