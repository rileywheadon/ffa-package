#' Helper Function for L-moments Ratios
#'
#' A helper function used by \link{lmr-functions}.
#'
#' @note L-moment ratios for `NOR`/`LNO` and `PE3`/`LP3` are identical
#'   since it is assumed that the `LNO`/`LP3` L-moments will be compared
#'   to the sample L-moments of the logarithm of the data.
#'
#' @param model Character; `GUM`, `NOR`, `LNO`, `GEV`, `GLO`, `GNO`,
#'   `PE3`, `LP3`, or `WEI`.
#'
#' @param params Numeric; a vector of parameters. 
#' - Numeric (2) if `model` is `GUM`, `NOR`, or `LNO`.
#' - Numeric (3) if `model` is `GEV`, `GLO`, `GNO`, `PE3`, `LP3`, or `WEI`.
#'
#' @return A numeric vector of length 4 containing:
#' - \eqn{\lambda_1}: L-mean
#' - \eqn{\lambda_2}: L-variance
#' - \eqn{\tau_3}: L-skewness
#' - \eqn{\tau_4}: L-kurtosis
#'
#' @references
#' Hosking, J.R.M. & Wallis, J.R., 1997. Regional frequency analysis: an approach based 
#' on L-Moments. Cambridge University Press, New York, USA.
#'
#' @examples
#' lmrxxx("GLO", c(0, 1, 0))
#'
#' @export
lmrxxx <- function(model, params) {

	# Unpack the parameters
	u <- params[1]
	s <- params[2]
	if (length(params) == 3) k <- params[3]

	# NOTE: -digamma(1) is Euler's constant (~0.5772)
	if (model == "GUM") {
		l1 <- u - (s * digamma(1))
		l2 <- s * log(2)
		t3 <- log(9 / 8) / log(2)
		t4 <- (16 * log(2) - 10 * log(3)) / log(2)
	}

	# NOTE: We call lmrxxx("LNO", params) internally to compare the L-moments of log(data) 
	# to the L-moments for NOR. We can do this because Data ~ LNO <=> log(Data) ~ NOR.
	else if (model == "NOR" | model == "LNO") {
		l1 <- u
		l2 <- s * pi^-0.5
		t3 <- 0
		t4 <- (30 * pi^-1 * atan(sqrt(2))) - 9
	}

	else if (model == "GEV") {
		l1 <- u + s * (1 - gamma(1 + k)) / k
		l2 <- s * (1 - 2^-k) * gamma(1 + k) / k
		t3 <- 2 * (1 - 3^-k) / (1 - 2^-k) - 3
		t4 <- (5 * (1 - 4^-k) - 10 * (1 - 3^-k) + 6 * (1 - 2^-k)) / (1 - 2^-k)
	}

	else if (model == "GLO") {
		l1 <- u + s * ((1 / k) - (pi / sin(k * pi)))
		l2 <- (s * k * pi) / sin(k * pi)
		t3 <- -k
		t4 <- (1 + (5 * k^2)) / 6
	}

	else if (model == "GNO") {

		# Constants for numerical approximation
		Tau04 <- 0.12260172

		A0 <-  0.4886025
		A1 <-  4.4493076e-3
		A2 <-  8.8027039e-4

		A3 <-  1.1507084e-6
		B1 <-  6.4662924e-2
		B2 <-  3.3090406e-3
		B3 <-  7.4290680e-5

		C0 <-  0.1875659
		C1 <- -2.5352147e-3
		C2 <-  2.6995102e-4
		C3 <- -1.8446680e-6

		D1 <-  8.2325617e-2
		D2 <-  4.2681448e-3
		D3 <-  1.1653690e-4

		# Compute L-moments
		l1 <- u + s * (1 - exp(k^2 / 2)) / k
		l2 <- (s / k) * exp(k^2 / 2) * (1 - 2 * pnorm(-k / sqrt(2)))

		t3a <- A0 + A1 * k^2 + A2 * k^4 + A3 * k^6
		t3b <- 1 + B1 * k^2 + B2 * k^4 + B3 * k^6
		t3 <- -k * (t3a / t3b)

		t4a <- C0 + C1 * k^2 + C2 * k^4 + C3 * k^6
		t4b <- 1 + D1 * k^2 + D2 * k^4 + D3 * k^6
		t4 <- Tau04 + k^2 * (t4a / t4b)
	
	}

	# NOTE: We call lmrxxx("LP3", params) internally to compare the L-moments of log(data) 
	# to the L-moments for PE3 We can do this because Data ~ LP3 <=> log(Data) ~ PE3.
	else if (model == "PE3" | model == "LP3") {
		
		# Coefficients used for approximation
		A0 <-  3.2573501e-1
		A1 <-  1.6869150e-1
		A2 <-  7.8327243e-2
		A3 <- -2.9120539e-3

		B1 <-  4.6697102e-1
		B2 <-  2.4255406e-1

		C0 <-  1.2260172e-1
		C1 <-  5.3730130e-2
		C2 <-  4.3384378e-2
		C3 <-  1.1101277e-2

		D1 <-  1.8324466e-1
		D2 <-  2.0166036e-1

		E1 <-  2.3807576
		E2 <-  1.5931792
		E3 <-  1.1618371e-1

		F1 <-  5.1533299
		F2 <-  7.1425260
		F3 <-  1.9745056

		G1 <-  2.1235833
		G2 <-  4.1670213
		G3 <-  3.1925299

		H1 <-  9.0551443
		H2 <-  2.6649995e1
		H3 <-  2.6193668e1

		# Reparameterize the distribution
		a <- 4 / k^2
		b <- s * abs(k) / 2
		x <- u - (2 * s / abs(k))

		# Compute the L-moments
		l1 <- x + (a * b)
		l2 <- (pi^-0.5) * b * exp(lgamma(a + 0.5) - lgamma(a))

		if (a >= 1) {
			t3 <- a^-0.5 * (A0 + A1 * a^-1 + A2 * a^-2 + A3 * a^-3) / (1 + B1 * a^-1 + B2 * a^-2)
			t4 <- (C0 + C1 * a^-1 + C2 * a^-2 + C3 * a^-3) / (1 + D1 * a^-1 + D2 * a^-2)
		} else {
			t3 <- (1 + E1 * a + E2 * a^2 + E3 * a^3) / (1 + F1 * a + F2 * a^2 + F3 * a^3)
			t4 <- (1 + G1 * a + G2 * a^2 + G3 * a^3) / (1 + H1 * a + H2 * a^2 + H3 * a^3)
		}

		# Flip the sign of t3 if k < 0
		if (k < 0) t3 <- -t3

	}

	else if (model == "WEI") {

		# Reparameterize
		ug <- 0
		sg <- s / k
		kg <- 1 / k

		# Compute L-moments for GEV
		l1g <- ug + sg * (1 - gamma(1 + kg)) / kg
		l2g <- sg * (1 - 2^-kg) * gamma(1 + kg) / kg
		t3g <- 2 * (1 - 3^-kg) / (1 - 2^-kg) - 3
		t4g <- (5 * (1 - 4^-kg) - 10 * (1 - 3^-kg) + 6 * (1 - 2^-kg)) / (1 - 2^-kg)

		# Compute L-moments for Weibull
		l1 <- u + s - l1g
		l2 <- l2g
		t3 <- -t3g
		t4 <- t4g

	}

	# Return results as a vector
	c(l1, l2, t3, t4)

}
