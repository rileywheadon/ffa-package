# Compute sample L-moments. 
#
# Takes a numeric vector of data.
#
# Returns a vector with four entries:
#  1. L-mean (l1)
#  2. L-variance (l2)
#  3. L-skewness (t3 = l3 / l2)
#  4. L-kurtosis (t4 = l4 / l2)

lmom.sample <- function(data) {

	# Sort the data in increasing order
	x <- sort(data)
	n <- length(x)

	# Helper function for the probability weighted moments
	pwm <- function(r) mean(x * choose(0:(n-1), r) / choose(n - 1, r))

	# Compute the probability weighted moments
	b0 <- pwm(0)
	b1 <- pwm(1)
	b2 <- pwm(2)
	b3 <- pwm(3)

	# Compute the first four sample L-moments
	l1 <- b0
	l2 <- (2 * b1) - b0
	l3 <- (6 * b2) - (6 * b1) + b0
	l4 <- (20 * b3) - (30 * b2) + (12 * b1) - b0

	# Return the results (l1, l2, t3, t4)
	c(l1 ,l2, l3 / l2, l4 / l2)

}
