qntxxx <- function(model, pe, params, years = NULL, slices = NULL) {

	# Get the covariates if years, slices are not NULL
	if (!is.null(years) & !is.null(slices)) {
		covariates <- get.covariates(years, slices)
		n <- length(covariates)
	} else {
		covariates <- NULL
		n <- 1
	}

	# Split the model into name and signature
	name <- substr(model, 1, 3)
	signature <- if(nchar(model) == 3) NULL else substr(model, 4, 5)

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

	# Helper function for GUM quantiles
	xfgum <- function(p, u, s) {
		u - s * log(-log(p))
	}

	# Helper function for GEV quantiles
	xfgev <- function(p, u, s, k) {
		if (k != 0) {
			u + s * (1 - (-log(p))^k) / k
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
		a <- 4 / k^2
		b <- abs(s * k / 2)

		if (k > 0) { 
			u - (a * b) + qgamma(p, a, b) 
		} else {
			u + (a * b) - qgamma(p, a, b)
		}
	}

	# Helper function for WEI quantiles
	xfwei <- function(p, u, s, k) {
		u + s * (-log(1 - p))^(1 / k)
	}

	switch(
		name,
		"GUM" = xfgum(1 - pe, u, s)
		"NOR" = qnorm(1 - pe, u, s),
		"LNO" = qlnorm(1 - pe, u, s),
		"GEV" = xfgev(1 - pe, u, s, k),
		"GLO" = xfglo(1 - pe, u, s, k), 
		"GNO" = xfglo(1 - pe, u, s, k),
		"PE3" = xfpe3(1 - pe, u, s, k),
		"LP3" = exp(xfpe3(1 - pe, u, s, k)),
		"WEI" = xfwei(1 - pe, u, s, k)
	)

}

qntgum    <- function(pe, params, years = NULL, slices = NULL) qntxxx("GUM", pe, params)
qntgum10  <- function(pe, params, years, slices) qntxxx("GUM10", pe, params, years, slices)
qntgum11  <- function(pe, params, years, slices) qntxxx("GUM11", pe, params, years, slices)

qntnor    <- function(pe, params, years = NULL, slices = NULL) qntxxx("NOR", pe, params)
qntnor10  <- function(pe, params, years, slices) qntxxx("NOR10", pe, params, years, slices)
qntnor11  <- function(pe, params, years, slices) qntxxx("NOR11", pe, params, years, slices)

qntlno    <- function(pe, params, years = NULL, slices = NULL) qntxxx("LNO", pe, params)
qntlno10  <- function(pe, params, years, slices) qntxxx("LNO10", pe, params, years, slices)
qntlno11  <- function(pe, params, years, slices) qntxxx("LNO11", pe, params, years, slices)

qntgev    <- function(pe, params, years = NULL, slices = NULL) qntxxx("GEV", pe, params)
qntgev100 <- function(pe, params, years, slices) qntxxx("GEV100", pe, params, years, slices)
qntgev110 <- function(pe, params, years, slices) qntxxx("GEV110", pe, params, years, slices)

qntglo    <- function(pe, params, years = NULL, slices = NULL) qntxxx("GLO", pe, params)
qntglo100 <- function(pe, params, years, slices) qntxxx("GLO100", pe, params, years, slices)
qntglo110 <- function(pe, params, years, slices) qntxxx("GLO110", pe, params, years, slices)

qntgno    <- function(pe, params, years = NULL, slices = NULL) qntxxx("GNO", pe, params)
qntgno100 <- function(pe, params, years, slices) qntxxx("GNO100", pe, params, years, slices)
qntgno110 <- function(pe, params, years, slices) qntxxx("GNO110", pe, params, years, slices)

qntpe3    <- function(pe, params, years = NULL, slices = NULL) qntxxx("PE3", pe, params)
qntpe3100 <- function(pe, params, years, slices) qntxxx("PE3100", pe, params, years, slices)
qntpe3110 <- function(pe, params, years, slices) qntxxx("PE3110", pe, params, years, slices)

qntlp3    <- function(pe, params, years = NULL, slices = NULL) qntxxx("LP3", pe, params)
qntlp3100 <- function(pe, params, years, slices) qntxxx("LP3100", pe, params, years, slices)
qntlp3110 <- function(pe, params, years, slices) qntxxx("LP3110", pe, params, years, slices)

qntwei    <- function(pe, params, years = NULL, slices = NULL) qntxxx("WEI", pe, params)
qntwei100 <- function(pe, params, years, slices) qntxxx("WEI100", pe, params, years, slices)
qntwei110 <- function(pe, params, years, slices) qntxxx("WEI110", pe, params, years, slices)
