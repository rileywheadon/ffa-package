# Adapted from https://rdrr.io/cran/homtest/man/KAPPA, since 'homtest' is deprecated.
#  - pelkap.R
# A collection of homogeneity tests described in: Viglione A., Laio F., Claps P. (2007)
#  - doi: 10.1029/2006WR005095

pelkap <- function(l1, l2, t3, t4) {

    sumquad.tau3tau4 = function (k.h, t3.t4) {

		k <- k.h[1]
		h <- k.h[2]
		t3 <- t3.t4[1]
		t4 <- t3.t4[2]

		if (((k < -1) && (h >= 0)) || ((h < 0) && ((k <= -1) || (k >= -1/h)))) {
			stop("L-moments are defined if h >= 0 and k > -1, or if h < 0 and -1 < k < -1/h")
		}

		g <- c(0,0,0,0)

		# GEV Case
		if (h == 0) {
			tau3 <- 2*(1 - 3^(-k))/(1 - 2^(-k)) - 3
			tau4 <- (5*(1 - 4^(-k)) - 10*(1 - 3^(-k)) + 6*(1 - 2^(-k)))/(1 - 2^(-k))
		}

		# General Kappa Distribution
		else {
			for (r in 1:4) {
				if (h > 0) {
					g[r] <- (r*gamma(1+k)*gamma(r/h)) / (h^(1+k) *gamma(1+k+r/h))
				}
				else {
					g[r]=(r*gamma(1+k)*gamma(-k-r/h)) / ((-h)^(1+k) *gamma(1-r/h))
				}
		  	}

		    tau3 <- (-g[1] + 3*g[2] -2*g[3])/(g[1]-g[2])
		    tau4 <- -(-g[1] + 6*g[2] -10*g[3] + 5*g[4])/(g[1]-g[2])
		}

		(t3-tau3)^2 + (t4-tau4)^2

	}

    xi.alfa = function (l1, l2, k, h) {

		if (((k < -1) && (h >= 0)) || ((h < 0) && ((k <= -1) || (k >= -1/h)))) {
			stop("L-moments are defined if h>=0 and k>-1, or if h<0 and -1<k<-1/h")
		}

		g <- c(0,0)

    	# GEV
		if (h == 0) {
		    alfa <- (l2*k)/((1 - 2^(-k))*gamma(1+k))
		    xi <- l1 - alfa*(1 - gamma(1+k))/k
		}

		else {
			for (r in 1:2) {
				if (h > 0) {
					g[r] <- (r*gamma(1+k)*gamma(r/h)) / (h^(1+k) *gamma(1+k+r/h))
			  	}
			  	else {
					g[r]=(r*gamma(1+k)*gamma(-k-r/h)) / ((-h)^(1+k) *gamma(1-r/h))
			  	}
			}

  		    alfa <- (l2*k)/(g[1]-g[2])
  		    xi <- l1 - alfa*(1-g[1])/k
		}

    	list(xi = xi, alfa = alfa)

    }

    result <- optim(c(1,1), sumquad.tau3tau4, t3.t4 = c(t3, t4))

    if (result$value != -1) {
        k <- result$par[1]
        h <- result$par[2]
        pp <- xi.alfa(l1, l2, k, h)
        xi <- pp$xi
        alfa <- pp$alfa
    }

	c(xi, alfa, k, h)

}

