rllxxx <- function(model, data, yp, pe, params, years, slice) {

	# Define the covariate and rescale slice
	covariate <- get.covariates(years, years)
	slice <- get.covariates(years, slice)

	# Get the quantile at exceedance probability pe
	qpe <- quaxxx(model, pe, c(0, params), years, slice)

	# Log-transform yp, qpe, and the data if necessary
	if (name %in% c("LNO", "LP3")) {
		yp <- log(yp)
		qpe <- log(qpe)
		data <- log(data)
	}

	# Get the name and signature for the model 
	name <- substr(model, 1, 3)
	signature <- if(nchar(model) == 3) NULL else substr(model, 4, 5)
	
	# Parse the stationary/non-stationary signature
	if (is.null(signature)) {
		u <- yp - qpe
		s <- params[1]
	} else if (signature == "10") {
		u <- yp - qpe + (params[1] * covariate)
		s <- params[2]
	} else if (signature == "11") {
		u <- yp - qpe + (params[1] * covariate)
		s <- params[2] + (params[3] * covariate)
	} 

	# Add the Kappa parameter if the distribution has three parameters
	if (name %in% c("GEV", "GLO", "GNO", "PE3", "LP3", "WEI")) {
		k <- params[length(params)]
	} else {
		k <- NULL
	}

	# Return the log-likelihood
	llvxxx(name, data, c(u, s, k), years)

}

rllgum    <- function(data, yp, pe, params, years = NULL, slices = NULL)  {
	rllxxx("GUM", data, yp, pe, params)
}

rllgum10  <- function(data, yp, pe, params, years, slices) {
	rllxxx("GUM10", data, yp, pe, params, years, slices)
}

rllgum11  <- function(data, yp, pe, params, years, slices)  {
	rllxxx("GUM11", data, yp, pe, params, years, slices)
}


rllnor    <- function(data, yp, pe, params, years = NULL, slices = NULL) {
	rllxxx("NOR", data, yp, pe, params)
}

rllnor10  <- function(data, yp, pe, params, years, slices) {
	rllxxx("NOR10", data, yp, pe, params, years, slices)
}

rllnor11  <- function(data, yp, pe, params, years, slices) {
	rllxxx("NOR11", data, yp, pe, params, years, slices)
}


rlllno    <- function(data, yp, pe, params, years = NULL, slices = NULL) {
	rllxxx("LNO", data, yp, pe, params)
}

rlllno10  <- function(data, yp, pe, params, years, slices) {
	rllxxx("LNO10", data, yp, pe, params, years, slices)
}

rlllno11  <- function(data, yp, pe, params, years, slices) {
	rllxxx("LNO11", data, yp, pe, params, years, slices)
}


rllgev    <- function(data, yp, pe, params, years = NULL, slices = NULL) {
	rllxxx("GEV", data, yp, pe, params)
}

rllgev100 <- function(data, yp, pe, params, years, slices) {
	rllxxx("GEV100", data, yp, pe, params, years, slices)
}

rllgev110 <- function(data, yp, pe, params, years, slices) {
	rllxxx("GEV110", data, yp, pe, params, years, slices)
}


rllglo    <- function(data, yp, pe, params, years = NULL, slices = NULL)  {
	rllxxx("GLO", data, yp, pe, params)
}

rllglo100 <- function(data, yp, pe, params, years, slices) {
	rllxxx("GLO100", data, yp, pe, params, years, slices)
}

rllglo110 <- function(data, yp, pe, params, years, slices) {
	rllxxx("GLO110", data, yp, pe, params, years, slices)
}


rllgno    <- function(data, yp, pe, params, years = NULL, slices = NULL) {
	rllxxx("GNO", data, yp, pe, params)
}

rllgno100 <- function(data, yp, pe, params, years, slices) {
	rllxxx("GNO100", data, yp, pe, params, years, slices)
}

rllgno110 <- function(data, yp, pe, params, years, slices) {
	rllxxx("GNO110", data, yp, pe, params, years, slices)
}


rllpe3    <- function(data, yp, pe, params, years = NULL, slices = NULL) {
	rllxxx("PE3", data, yp, pe, params)
}

rllpe3100 <- function(data, yp, pe, params, years, slices) {
	rllxxx("PE3100", data, yp, pe, params, years, slices)
}

rllpe3110 <- function(data, yp, pe, params, years, slices) {
	rllxxx("PE3110", data, yp, pe, params, years, slices)
}


rlllp3    <- function(data, yp, pe, params, years = NULL, slices = NULL) {
	rllxxx("LP3", data, yp, pe, params)
}

rlllp3100 <- function(data, yp, pe, params, years, slices) {
	rllxxx("LP3100", data, yp, pe, params, years, slices)
}

rlllp3110 <- function(data, yp, pe, params, years, slices) {
	rllxxx("LP3110", data, yp, pe, params, years, slices)
}


rllwei    <- function(data, yp, pe, params, years = NULL, slices = NULL) {
	rllxxx("WEI", data, yp, pe, params)
}

rllwei100 <- function(data, yp, pe, params, years, slices) {
	rllxxx("WEI100", data, yp, pe, params, years, slices)
}

rllwei110 <- function(data, yp, pe, params, years, slices) {
	rllxxx("WEI110", data, yp, pe, params, years, slices)
}


