#' Helper Function for Quantile Functions
#'
#' A helper function used by \link{qnt-functions}.
#'
#' @param name Character (1); the name of the probability distribution.
#'
#' @param signature Character (1); the non-stationary signature (`NULL`, `"10"`, or `"11"`).
#'
#' @param p Numeric; a vector of probabilities. Must be between 0 and 1.
#'
#' @param params Numeric; a vector of parameters. Must have the correct length for the model.
#'
#' @param covariate Numeric; a vector with the same length as `data`. 
#'   Required if `signature` is `"10"` or `"11"`.
#'
#' @return If `p` or `years` is a scalar, returns a numeric vector. Otherwise, returns a matrix.
#'
#' @seealso \link{qnt-functions}
#'
#' @examples
#' # Initialize p and params
#' p <- runif(n = 10)
#' params <- c(0, 1, 0)
#'
#' # Compute the log-likelihood
#' qntxxx("GEV", NULL, p, params)
#'
#' @importFrom stats qlnorm qgamma
#' @export
qntxxx <- function(name, signature, p, params, covariate = 0) {

	# Get the length of the covariate
	n <- length(covariate)

	# Compute location/scale parameter at the covariate
	if (is.null(signature)) {
		u <- rep(params[1], n)
		s <- rep(params[2], n)
	} else if (signature == "10") {
		u <- params[1] + (covariate * params[2])
		s <- rep(params[3], n)
	} else if (signature == "11") {
		u <- params[1] + (covariate * params[2])
		s <- params[3] + (covariate * params[4])
	}

	# Compute the shape parameter if the model has one
	if (name %in% c("GEV", "GEV", "GLO", "GNO", "PE3", "LP3", "WEI")) {
		k <- rep(params[length(params)], n)
	}

	# Add two shape parameters for the Kappa distribution
	if (name == "KAP") {
		k <- rep(params[3], n)
		h <- rep(params[4], n)
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

	# General helper function for all quantiles
	xfxxx <- switch(
		name,
		GUM = function(p, i) xfgum(p, u[i], s[i]),
		NOR = function(p, i) xfnor(p, u[i], s[i]),
		LNO = function(p, i) xflno(p, u[i], s[i]),
		GEV = function(p, i) xfgev(p, u[i], s[i], k[i]),
		GLO = function(p, i) xfglo(p, u[i], s[i], k[i]), 
		GNO = function(p, i) xfgno(p, u[i], s[i], k[i]),
		PE3 = function(p, i) xfpe3(p, u[i], s[i], k[i]),
		LP3 = function(p, i) xflp3(p, u[i], s[i], k[i]),
		WEI = function(p, i) xfwei(p, u[i], s[i], k[i]),
		KAP = function(p, i) xfkap(p, u[i], s[i], k[i], h[i])
	)

	# Compute the result using apply across rows 
	result <- vapply(1:n, function(i) xfxxx(p, i), numeric(length(p)))

	# Collapse the result to a vector if possible
	if (length(p) == 1 | length(covariate) == 1) {
		return (as.vector(result))
	} else {
		return (t(result))
	}

}


