test_that("CDF functions produce the same results as 'lmom' package.", {

	# Gumbel (GUM) Distribution
	result <- utils_cdf(c(-0.6681, 1.7331, 5.501), "GUM", c(1, 2))
	expect_equal(result, c(0.1, 0.5, 0.9), tol = 1e-4)

	# Normal (NOR) Distribution
	result <- utils_cdf(c(-1.5631, 1.0000, 3.5631), "NOR", c(1, 2))
	expect_equal(result, c(0.1, 0.5, 0.9), tol = 1e-4)

	# Log-Normal (LNO) Distribution
	result <- utils_cdf(c(0.2095, 2.7183, 35.2725), "LNO", c(1, 2))
	expect_equal(result, c(0.1, 0.5, 0.9), tol = 1e-4)

	# Generalized Extreme Value (GEV) Distribution (k = 0) 
	result <- utils_cdf(c(-0.8340, 0.3665, 2.2504), "GEV", c(0, 1, 0))
	expect_equal(result, c(0.1, 0.5, 0.9), tol = 1e-4)

	# Generalized Extreme Value (GEV) Distribution (k != 0) 
	result <- utils_cdf(c(-1.3026, 0.3069, 0.8946), "GEV", c(0, 1, -1))
	expect_equal(result, c(0.1, 0.5, 0.9), tol = 1e-4)

	# Generalized Logistic (GLO) Distribution (k = 0) 
	result <- utils_cdf(c(-2.1972, 0, 2.1972), "GLO", c(0, 1, 0))
	expect_equal(result, c(0.1, 0.5, 0.9), tol = 1e-4)

	# Generalized Logistic (GLO) Distribution (k != 0) 
	result <- utils_cdf(c(-8.0000, 0.0000, 0.8889), "GLO", c(0, 1, 1))
	expect_equal(result, c(0.1, 0.5, 0.9), tol = 1e-4)

	# Generalized Normal (GNO) Distribution (k = 0) 
	result <- utils_cdf(c(-1.5631, 1.0000, 3.5631), "GNO", c(1, 2, 0))
	expect_equal(result, c(0.1, 0.5, 0.9), tol = 1e-4)

	# Generalized Normal (GNO) Distribution (k != 0) 
	result <- utils_cdf(c(-2.6022, 0.0000, 0.7224), "GNO", c(0, 1, 1))
	expect_equal(result, c(0.1, 0.5, 0.9), tol = 1e-4)

	# Pearson Type III (PE3) Distribution (k = 0) 
	result <- utils_cdf(c(-1.2815, 0.0000, 1.2816), "PE3", c(0, 1, 0))
	expect_equal(result, c(0.1, 0.5, 0.9), tol = 1e-4)

	# Pearson Type III (PE3) Distribution (k > 0) 
	result <- utils_cdf(c(-1.1276, -0.1640, 1.3404), "PE3", c(0, 1, 1))
	expect_equal(result, c(0.1, 0.5, 0.9), tol = 1e-4)

	# Pearson Type III (PE3) Distribution (k < 0) 
	result <- utils_cdf(c(-1.3404, 0.1640, 1.1276), "PE3", c(0, 1, -1))
	expect_equal(result, c(0.1, 0.5, 0.9), tol = 1e-4)

	# Log-Pearson Type III (LP3) Distribution (k = 0) 
	result <- utils_cdf(c(0.2776, 1.000, 3.6022), "LP3", c(0, 1, 0))
	expect_equal(result, c(0.1, 0.5, 0.9), tol = 1e-4)

	# Log-Pearson Type III (LP3) Distribution (k > 0) 
	result <- utils_cdf(c(0.3238, 0.8488, 3.8205), "LP3", c(0, 1, 1))
	expect_equal(result, c(0.1, 0.5, 0.9), tol = 1e-4)

	# Log-Pearson Type III (LP3) Distribution (k < 0) 
	result <- utils_cdf(c(0.2617, 1.1782, 3.0883), "LP3", c(0, 1, -1))
	expect_equal(result, c(0.1, 0.5, 0.9), tol = 1e-4)

	# Weibull (WEI) Distribution 
	result <- utils_cdf(c(0.1054, 0.6932, 2.3026), "WEI", c(0, 1, 1))
	expect_equal(result, c(0.1, 0.5, 0.9), tol = 1e-4)

})

test_that("CDF functions have the correct support.", {

	# Log-Normal (LNO) Distribution
	result <- utils_cdf(0, "LNO", c(0, 1))
	expect_equal(result, 0)

	# Generalized Extreme Value (GEV) Distribution (k > 0)
	result <- utils_cdf(-1, "GEV", c(0, 1, 1))
	expect_equal(result, 0)

	# Generalized Extreme Value (GEV) Distribution (k < 0)
	result <- utils_cdf(1, "GEV", c(0, 1, -1))
	expect_equal(result, 1)

	# Generalized Logistic (GLO) Distribution (k > 0)
	result <- utils_cdf(1, "GLO", c(0, 1, 1))
	expect_equal(result, 1)

	# Generalized Logistic (GLO) Distribution (k < 0)
	result <- utils_cdf(-1, "GLO", c(0, 1, -1))
	expect_equal(result, 0)

	# Pearson Type III (PE3) Distribution (k > 0)
	result <- utils_cdf(-2, "PE3", c(0, 1, 1))
	expect_equal(result, 0)

	# Pearson Type III (PE3) Distribution (k < 0)
	result <- utils_cdf(2, "PE3", c(0, 1, -1))
	expect_equal(result, 1)

	# Log-Pearson Type III (PE3) Distribution (k = 0)
	result <- utils_cdf(0, "LP3", c(0, 1, 0))
	expect_equal(result, 0)

	# Log-Pearson Type III (PE3) Distribution (k > 0)
	result <- utils_cdf(exp(-2), "LP3", c(0, 1, 1))
	expect_equal(result, 0)

	# Weibull (WEI) Distribution 
	result <- utils_cdf(0, "WEI", c(0, 1, 1))
	expect_equal(result, 0)

})

