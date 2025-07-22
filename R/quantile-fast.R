#' Helper Function for Quantile Functions
#'
#' A helper function used by [quantile_xxx()].
#' This function does not validate parameters and is designed for use in other methods.
#'
#' @inheritParams param-p
#' @inheritParams param-distribution
#' @inheritParams param-params
#' @inheritParams param-slice
#' @inheritParams param-structure
#'
#' @return A numeric vector of quantiles with the same length as `p`.
#'
#' @seealso [quantile_xxx()]
#'
#' @examples
#' # Initialize p, params, and structure
#' p <- runif(n = 10)
#' params <- c(0, 1, 1, 0)
#' structure <- list(location = FALSE, scale = TRUE)
#'
#' # Compute the log-likelihood in the year 2000
#' quantile_fast(p, "GEV", params, 2000, structure)
#'
#' @importFrom stats qlnorm qgamma
#' @export

quantile_fast <- function(p, distribution, params, slice, structure) {

	# Get the covariate for the slice
	covariate <- get_covariates(slice)

	# Transform nonstationary parmaeters into a vector of stationary parameters
	if (structure$location) {
		u <- params[1] + (covariate * params[2])
	} else {
		u <- params[1]
	}

	i <- structure$location
	if (structure$scale) {
		s <- params[2 + i] + (covariate * params[3 + i])
	} else {
		s <- params[2 + i]
	}

	# Get shape parameter (will remain unused for 2-parameter distributions)
	k <- params[length(params)]

	# The Kappa distribution uses two shape parameters and is always stationary
	if (distribution == "KAP") {
		k <- params[3]
		h <- params[4]
	}

	# Helper function for GUM quantiles
	xfgum <- function(p, u, s) {
		u - s * log(-log(p))
	}

	# Helper function for NOR quantiles
	xfnor <- function(p, u, s) {
		qnorm(p, u, s)
	}

	# Helper function for LNO quantiles
	xflno <- function(p, u, s) {
		qlnorm(p, u, s)
	}

	# Helper function for GEV quantiles
	xfgev <- function(p, u, s, k) {
		if (k != 0) {
			u - s * (1 - (-log(p))^-k) / k
		} else {
			u - s * log(-log(p))
		}
	}

	# Helper function for GLO quantiles
	xfglo <- function(p, u, s, k) {
		if (k != 0) {
			u + s * (1 - ((1 - p) / p)^k) / k
		} else {
			u - s * log((1 - p) / p)
		}
	}

	# Helper function for GNO quantiles
	xfgno <- function(p, u, s, k) {
		if (k != 0) {
			u + s * (1 - exp(-k * qnorm(p))) / k
		} else {
			qnorm(p, u, s)
		}
	}

	# Helper function for PE3 quantiles
	xfpe3 <- function(p, u, s, k) {

		# Reparameterize in terms of a, b	
		a <- 4 / (k^2)
		b <- abs((s * k) / 2)

		# Vectorize three cases (k = 0, k > 0, k < 0)
		if (k == 0) {
			qnorm(p, u, s)
		} else if (k > 0) {
			u - (a * b) + qgamma(p, shape = a, scale = b)
		} else if (k < 0) {
			u + (a * b) - qgamma(1 - p, shape = a, scale = b)
		}
	}

	# Helper function for LP3 quantiles
	xflp3 <- function(p, u, s, k) {
		exp(xfpe3(p, u, s, k))
	}

	# Helper function for WEI quantiles
	xfwei <- function(p, u, s, k) {
		u + s * ((-log(1 - p))^(1/ k))
	}

	# Helper function for KAP quantiles
	xfkap <- function(p, u, s, k, h) {
		u + (s / k) * (1 - ((1 - p^h) / h)^k)
	}

	# Compute the result for all probabilities 
	switch(
		distribution,
		GUM = xfgum(p, u, s),
		NOR = xfnor(p, u, s),
		LNO = xflno(p, u, s),
		GEV = xfgev(p, u, s, k),
		GLO = xfglo(p, u, s, k), 
		GNO = xfgno(p, u, s, k),
		PE3 = xfpe3(p, u, s, k),
		LP3 = xflp3(p, u, s, k),
		WEI = xfwei(p, u, s, k),
		KAP = xfkap(p, u, s, k, h)
	)

}
