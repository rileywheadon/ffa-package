# Estimates the parameters of a distribution given data using the method of L-moments.
#
# Takes a distribution (stationary only) and a numeric vector of data.
#
# Returns a vector with four entries:
#  1. L-mean (l1)
#  2. L-variance (l2)
#  3. L-skewness (t3 = l3 / l2)
#  4. L-kurtosis (t4 = l4 / l2)

pelxxx <- function(model, data) {

	# Get the correct L-moments based on the model
	moments <- if (model == "LP3") {
		lmom.sample(log(data))
	} else {
		lmom.sample(data)
	}

	# Unpack the L-moments
	l1 <- moments[1]
	l2 <- moments[2]
	t3 <- moments[3]
	t4 <- moments[4]

	# NOTE: -digamma(1) is Euler's constant (~0.5772)
	if (model == "GUM") {
		s <- l2 / log(2)
		u <- l1	+ digamma(1) * s
		k <- NULL
	}

	else if (model == "NOR") {
		u <- l1
		s <- l2 * sqrt(pi)
		k <- NULL
	}

	# NOTE: Copied from MATLAB, not sure why we use this formula.
	else if (model == "LNO") {
		s <- sqrt(2) * qnorm((1 + (l2 / l1)) / 2)
        u <- log(l1) - s^2 / 2
		k <- NULL
	}

	else if (model == "GEV") {
		c <- (2 / (3 + t3)) - (log(2) / log(3))
		k <- -7.8590 * c - 2.9554 * c^2
		s <- (l2 * -k) / ((1 - 2^k) * gamma(1 - k))
		u <- l1 + s * (1 - gamma(1 - k)) / k
	}

	else if (model == "GLO") {
		k <- -t3
		s <- l2 * sin(k * pi) / (k * pi)
		u <- l1 - s * ((1 / k) - (pi / sin(k * pi)))
	}

	else if (model == "GNO") {

		# Define coefficients
     	A0 <-  0.20466534e1
 		A1 <- -0.36544371e1
	 	A2 <-  0.18396733e1
		A3 <- -0.20360244

      	B1 <- -0.20182173e1
		B2 <-  0.12420401e1
		B3 <- -0.21741801

		# Compute shape parameter
		tt <- t3^2
        k = -t3 * (A0 + tt * (A1 + tt *(A2 + tt * A3))) / (1 + tt * (B1 + tt * (B2 + tt * B3)))

		# Define the error function erf(z)
		erf <- function(z) 2 * pnorm(z * sqrt(2)) - 1

		# Compute location and scale parameters
        x = exp(k^2 / 2)
        s = l2 * k / (x * erf(k / 2))
        u = l1 + s * (x - 1) / k
  
	}

	# NOTE: Computation for PE3 and LP3 is identical, just with different sample L-moments
	else if (model == "PE3" | model == "LP3") {

		# Define constants for numerical approximation
		A1 <- 0.2906

		B1 <- 0.1882
		B2 <- 0.0442

		C1 <- 0.36067
		C2 <- 0.59567
		C3 <- 0.25361

		D1 <- 2.78861
		D2 <- 2.56096
		D3 <- 0.77045

		# Compute z and a
		a <- if (0 < abs(t3) & abs(t3) < (1 / 3)) {
			z <- 3 * pi * t3^2
			(1 + A1 * z) / (z + B1 * z^2 + B2 * z^3)
		} else {
			z <- 1 - t3
			(C1 * z - C2 * z^2 + C3 * z^3) / (1 - D1 * z + D2 * z^2 - D3 * z^3)
		}

		# Compute parameter estimates
		u <- l1
		s <- l2 * pi^0.5 * a^0.5 * exp(lgamma(a) - lgamma(a + 0.5))
		k <- 2 * a^-0.5 * sign(t3)

	}

	else if (model == "WEI") {

		# Estimate GEV parameters, flipping the sign of l1, t3
		c <- (2 / (3 - t3)) - (log(2) / log(3))
		kg <- 7.8590 * c + 2.9554 * c^2
		sg <- (l2 * kg) / ((1 - 2^-kg) * gamma(1 + kg))
		ug <- -l1 - sg * (1 - gamma(1 + kg)) / kg

		# Compute parameters
		k <- 1 / kg
		s <- sg / kg
		u <- -ug - s

	} 

	# Return results as a vector
	c(u, s, k)

}

pelgum <- function(data) pelxxx("GUM", data)
pelnor <- function(data) pelxxx("NOR", data)
pellno <- function(data) pelxxx("LNO", data)
pelgev <- function(data) pelxxx("GEV", data)
pelglo <- function(data) pelxxx("GLO", data)
pelgno <- function(data) pelxxx("GNO", data)
pelpe3 <- function(data) pelxxx("PE3", data)
pellp3 <- function(data) pelxxx("LP3", data)
pelwei <- function(data) pelxxx("WEI", data)

