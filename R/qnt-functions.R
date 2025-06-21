qntxxx <- function(model, p, params, years = NULL) {

	# Validate the parameters
	# info <- models.info(model)
	# if (length(params) != info$n.params) {
	# 	str <- "Error: 'params' for model '%s' must have length %d."
	# 	stop(sprintf(str, model, info$n.params))
	# }

	# Check that params is a numeric vector
	# if (!is.numeric(params) | !is.vector(params)) {
	# 	stop("Error: 'params' is not a numeric vector.")
	# }

	# Check that all parameters are defined
	# if (any(is.nan(params)) | any(is.na(params))) {
	# 	stop("Error: 'params' contains NaN or NA values.")
	# }

	# Check that 0 <= p <= 1 for all entries in p.
	# if (any(p < 0) | any(p > 1)) {
	# 	stop("Error: 'p' must be between 0 and 1 inclusive.")
	# }

	# Split the model into name and signature
	name <- substr(model, 1, 3)
	signature <- if(nchar(model) == 3) NULL else substr(model, 4, 5)

	# Check that years is not NULL if the model is non-stationary
	# if (!is.null(signature) & (is.null(years) | any(is.nan(years)) | any(is.na(years)))) {
	# 	stop("Error: 'years' must not be NULL or contain NaN/NA values.")
	# }

	# Get the covariates if years is not NULL
	covariates <- if (!is.null(signature)) get.covariates(years) else 0
	n <- length(covariates)

	# Compute location/scale parameter at the covariates
	if (is.null(signature)) {
		u <- rep(params[1], n)
		s <- rep(params[2], n)
	} else if (signature == "10") {
		u <- params[1] + (covariates * params[2])
		s <- rep(params[3], n)
	} else if (signature == "11") {
		u <- params[1] + (covariates * params[2])
		s <- params[3] + (covariates * params[4])
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
	if (length(p) == 1 | length(covariates) == 1) {
		return (as.vector(result))
	} else {
		return (t(result))
	}

}

qntgum    <- function(pe, params, years = NULL) qntxxx("GUM", pe, params)
qntgum10  <- function(pe, params, years) qntxxx("GUM10", pe, params, years)
qntgum11  <- function(pe, params, years) qntxxx("GUM11", pe, params, years)

qntnor    <- function(pe, params, years = NULL) qntxxx("NOR", pe, params)
qntnor10  <- function(pe, params, years) qntxxx("NOR10", pe, params, years)
qntnor11  <- function(pe, params, years) qntxxx("NOR11", pe, params, years)

qntlno    <- function(pe, params, years = NULL) qntxxx("LNO", pe, params)
qntlno10  <- function(pe, params, years) qntxxx("LNO10", pe, params, years)
qntlno11  <- function(pe, params, years) qntxxx("LNO11", pe, params, years)

qntgev    <- function(pe, params, years = NULL) qntxxx("GEV", pe, params)
qntgev100 <- function(pe, params, years) qntxxx("GEV100", pe, params, years)
qntgev110 <- function(pe, params, years) qntxxx("GEV110", pe, params, years)

qntglo    <- function(pe, params, years = NULL) qntxxx("GLO", pe, params)
qntglo100 <- function(pe, params, years) qntxxx("GLO100", pe, params, years)
qntglo110 <- function(pe, params, years) qntxxx("GLO110", pe, params, years)

qntgno    <- function(pe, params, years = NULL) qntxxx("GNO", pe, params)
qntgno100 <- function(pe, params, years) qntxxx("GNO100", pe, params, years)
qntgno110 <- function(pe, params, years) qntxxx("GNO110", pe, params, years)

qntpe3    <- function(pe, params, years = NULL) qntxxx("PE3", pe, params)
qntpe3100 <- function(pe, params, years) qntxxx("PE3100", pe, params, years)
qntpe3110 <- function(pe, params, years) qntxxx("PE3110", pe, params, years)

qntlp3    <- function(pe, params, years = NULL) qntxxx("LP3", pe, params)
qntlp3100 <- function(pe, params, years) qntxxx("LP3100", pe, params, years)
qntlp3110 <- function(pe, params, years) qntxxx("LP3110", pe, params, years)

qntwei    <- function(pe, params, years = NULL) qntxxx("WEI", pe, params)
qntwei100 <- function(pe, params, years) qntxxx("WEI100", pe, params, years)
qntwei110 <- function(pe, params, years) qntxxx("WEI110", pe, params, years)

qntkap    <- function(pe, params, years = NULL) qntxxx("KAP", pe, params)
