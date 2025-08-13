# Helper Function for CDFs
cdf_fast <- function(q, distribution, params, slice, structure) {

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

	# Helper function for GUM CDF
	fgum <- function(x, u, s) {
		exp(-exp(-(x - u) / s))
	}

	# Helper function for NOR CDF
	fnor <- function(x, u, s) {
		pnorm(x, u, s)
	}

	# Helper function for LNO CDF
	flno <- function(x, u, s) {
		plnorm(x, u, s)
	}

	# Helper function for GEV CDF
	fgev <- function(x, u, s, k) {
		if (k != 0) {
			exp(-exp((-1 / k) * log(1 + k * (x - u) / s)))
		} else {
			exp(-exp(-(x - u) / s))
		}
	}

	# Helper function for GLO CDF
	fglo <- function(x, u, s, k) {
		if (k != 0) {
			1 / (1 + exp((1 / k) * log(1 - k * (x - u) / s)))
		} else {
			1 / (1 + exp(-(x - u) / s))
		}
	}

	# Helper function for GNO CDF
	fgno <- function(x, u, s, k) {
		if (k == 0) {
			pnorm(x, u, s)
		} else {
			pnorm(-(1 / k) * log(1 - k * (x - u) / s))
		}
	}

	# Helper function for PE3 CDF
	fpe3 <- function(x, u, s, k) {

		# Reparameterize in terms of a, b	
		a <- 4 / (k^2)
		b <- abs((s * k) / 2)
		xi <- u - (2 * s / k)

		# Vectorize three cases (k = 0, k > 0, k < 0)
		if (k == 0) {
			pnorm(x, u, s)
		} else if (k > 0) {
			pgamma((x - xi) / b, shape = a)
		} else if (k < 0) {
			1 - pgamma((xi - x) / b, shape = a)
		}
	}

	# Helper function for LP3 CDF
	flp3 <- function(x, u, s, k) {
		fpe3(log(x), u, s, k)
	}

	# Helper function for WEI CDF
	fwei <- function(x, u, s, k) {
		1 - exp(-((x - u) / s)^k)
	}

	# Helper function for KAP CDF
	fkap <- function(x, u, s, k, h) {
		if (k == 0 && h == 0) {
			fgum(x, u, s)
		} else if (k == 0) {
			fglo(x, u, s, h)
		} else if (h == 0) {
			fgev(x, u, s, k)
		} else {
			(1 - h * (1 - (k * (x - u) / s))^(1 / k))^(1 / h)
		}
	}

	# Compute the result for all quantiles 
	switch(
		distribution,
		GUM = fgum(q, u, s),
		NOR = fnor(q, u, s),
		LNO = flno(q, u, s),
		GEV = fgev(q, u, s, k),
		GLO = fglo(q, u, s, k), 
		GNO = fgno(q, u, s, k),
		PE3 = fpe3(q, u, s, k),
		LP3 = flp3(q, u, s, k),
		WEI = fwei(q, u, s, k),
		KAP = fkap(q, u, s, k, h)
	)

}
