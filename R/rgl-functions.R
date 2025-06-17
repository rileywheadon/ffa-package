rglxxx <- function(model, data, yp, pe, params, prior, years = NULL, slice = NULL) {

	# The prior is Beta(p, q)
	p <- prior[1]
	q <- prior[2]

	# Compute the contribution of the prior to the likelihood
	k <- params[length(params)]
	pll <- (p - 1) * log(0.5 - k) + (q - 1) * log(k + 0.5) - lbeta(p, q)

	# Compute the reparameterized likelihood
	rll <- rllxxx(model, data, yp, pe, params, years, slice)

	# Return the sum of (pll, llv) over all the data points
	n <- length(data)
	sum((n * pll) + rll)

}

rglgev    <- function(data, params, prior, years = NULL) rglxxx("GEV", data, params, prior)
rglgev100 <- function(data, params, prior, years) rglxxx("GEV100", data, params, prior, years)
rglgev110 <- function(data, params, prior, years) rglxxx("GEV110", data, params, prior, years)
