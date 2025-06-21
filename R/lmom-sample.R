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
	x <- sort_cpp(data)
	n <- length(x)
	i <- 1:n

	# Recursively compute the probability weighted moments
	vb0 <- x / n
	vb1 <- vb0 * (i - 1) / (n - 1)
	vb2 <- vb1 * (i - 2) / (n - 2)
	vb3 <- vb2 * (i - 3) / (n - 3)

	b0 <- sum(vb0)
	b1 <- sum(vb1)
	b2 <- sum(vb2)
	b3 <- sum(vb3)

	# Compute the first four sample L-moments
	l1 <- b0
	l2 <- (2 * b1) - b0
	l3 <- (6 * b2) - (6 * b1) + b0
	l4 <- (20 * b3) - (30 * b2) + (12 * b1) - b0

	# Return the results (l1, l2, t3, t4)
	c(l1 ,l2, l3 / l2, l4 / l2)

}
