gllxxx <- function(model, data, params, prior, years = NULL) {

	# The prior is Beta(p, q)
	p <- prior[1]
	q <- prior[2]

	# Compute the contribution of the prior to the likelihood
	k <- params[length(params)]
	pll <- (p - 1) * log(0.5 - k) + (q - 1) * log(0.5 + k) - lbeta(p, q)

	# Compute the likelihood
	llv <- llvxxx(model, data, params, years)

	# Return the sum of (pll, llv) over all the data points
	n <- length(data)
	sum((n * pll) + llv)

}

gllgev    <- function(data, params, prior, years = NULL) gllxxx("GEV", data, params, prior)
gllgev100 <- function(data, params, prior, years) gllxxx("GEV100", data, params, prior, years)
gllgev110 <- function(data, params, prior, years) gllxxx("GEV110", data, params, prior, years)


# Faster but less elegant version of the gll function for GMLE estimation
gllfast <- function(name, signature, data, params, prior, covariate) {

	# The prior is Beta(p, q)
	p <- prior[1]
	q <- prior[2]

	# Compute the contribution of the prior to the likelihood
	k <- params[length(params)]
	pll <- (p - 1) * log(0.5 - k) + (q - 1) * log(0.5 + k) - lbeta(p, q)

	# Compute the likelihood
	llv <- llvfast(name, signature, data, params, covariate) 

	# Return the sum of (pll, llv) over all the data points
	n <- length(data)
	sum((n * pll) + llv)

}


