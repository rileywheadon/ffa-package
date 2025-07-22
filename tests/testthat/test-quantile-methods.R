test_that("quantile functions produce the same results as 'lmom' package.", {

	# Gumbel (GUM) Distribution
	result <- quantile_gum(c(0.1, 0.5, 0.9), c(1, 2))
	expect_equal(result, c(-0.6681, 1.7331, 5.501), tol = 1e-4)

	# Normal (NOR) Distribution
	result <- quantile_nor(c(0.1, 0.5, 0.9), c(1, 2))
	expect_equal(result, c(-1.5631, 1.0000, 3.5631), tol = 1e-4)

	# Log-Normal (LNO) Distribution
	result <- quantile_lno(c(0.1, 0.5, 0.9), c(1, 2))
	expect_equal(result, c(0.2095, 2.7183, 35.2725), tol = 1e-4)

	# Generalized Extreme Value (GEV) Distribution (k = 0) 
	result <- quantile_gev(c(0.1, 0.5, 0.9), c(0, 1, 0))
	expect_equal(result, c(-0.8340, 0.3665, 2.2504), tol = 1e-4)

	# Generalized Extreme Value (GEV) Distribution (k != 0) 
	result <- quantile_gev(c(0.1, 0.5, 0.9), c(0, 1, -1))
	expect_equal(result, c(-1.3026, 0.3069, 0.8946), tol = 1e-4)

	# Generalized Logistic (GLO) Distribution (k = 0) 
	result <- quantile_glo(c(0.1, 0.5, 0.9), c(0, 1, 0))
	expect_equal(result, c(-2.1972, 0, 2.1972), tol = 1e-4)

	# Generalized Logistic (GLO) Distribution (k != 0) 
	result <- quantile_glo(c(0.1, 0.5, 0.9), c(0, 1, 1))
	expect_equal(result, c(-8.0000, 0.0000, 0.8889), tol = 1e-4)

	# Generalized Normal (GNO) Distribution (k = 0) 
	result <- quantile_gno(c(0.1, 0.5, 0.9), c(1, 2, 0))
	expect_equal(result, c(-1.5631, 1.0000, 3.5631), tol = 1e-4)

	# Generalized Normal (GNO) Distribution (k != 0) 
	result <- quantile_gno(c(0.1, 0.5, 0.9), c(0, 1, 1))
	expect_equal(result, c(-2.6022, 0.0000, 0.7224), tol = 1e-4)

	# Pearson Type III (PE3) Distribution (k = 0) 
	result <- quantile_pe3(c(0.1, 0.5, 0.9), c(0, 1, 0))
	expect_equal(result, c(-1.2815, 0.0000, 1.2816), tol = 1e-4)

	# Pearson Type III (PE3) Distribution (k > 0) 
	result <- quantile_pe3(c(0.1, 0.5, 0.9), c(0, 1, 1))
	expect_equal(result, c(-1.1276, -0.1640, 1.3404), tol = 1e-4)

	# Pearson Type III (PE3) Distribution (k < 0) 
	result <- quantile_pe3(c(0.1, 0.5, 0.9), c(0, 1, -1))
	expect_equal(result, c(-1.3404, 0.1640, 1.1276), tol = 1e-4)

	# Log-Pearson Type III (LP3) Distribution (k = 0) 
	result <- quantile_lp3(c(0.1, 0.5, 0.9), c(0, 1, 0))
	expect_equal(result, c(0.2776, 1.000, 3.6022), tol = 1e-4)

	# Log-Pearson Type III (LP3) Distribution (k > 0) 
	result <- quantile_lp3(c(0.1, 0.5, 0.9), c(0, 1, 1))
	expect_equal(result, c(0.3238, 0.8488, 3.8205), tol = 1e-4)

	# Log-Pearson Type III (LP3) Distribution (k < 0) 
	result <- quantile_lp3(c(0.1, 0.5, 0.9), c(0, 1, -1))
	expect_equal(result, c(0.2617, 1.1782, 3.0883), tol = 1e-4)

	# Weibull (WEI) Distribution 
	result <- quantile_wei(c(0.1, 0.5, 0.9), c(0, 1, 1))
	expect_equal(result, c(0.1054, 0.6932, 2.3026), tol = 1e-4)

})

test_that("quantile functions have the correct support.", {

	# Gumbel (GUM) Distribution
	result <- quantile_gum(c(0, 1), c(0, 1))
	expect_equal(result, c(-Inf, Inf))

	# Normal (NOR) Distribution
	result <- quantile_nor(c(0, 1), c(0, 1))
	expect_equal(result, c(-Inf, Inf))

	# Log-Normal (LNO) Distribution
	result <- quantile_lno(c(0, 1), c(0, 1))
	expect_equal(result, c(0, Inf))

	# Generalized Extreme Value (GEV) Distribution (k = 0)
	result <- quantile_gev(c(0, 1), c(0, 1, 0))
	expect_equal(result, c(-Inf, Inf))

	# Generalized Extreme Value (GEV) Distribution (k > 0)
	result <- quantile_gev(c(0, 1), c(0, 1, 1))
	expect_equal(result, c(-1, Inf))

	# Generalized Extreme Value (GEV) Distribution (k < 0)
	result <- quantile_gev(c(0, 1), c(0, 1, -1))
	expect_equal(result, c(-Inf, 1))

	# Generalized Logistic (GLO) Distribution (k = 0)
	result <- quantile_glo(c(0, 1), c(0, 1, 0))
	expect_equal(result, c(-Inf, Inf))

	# Generalized Logistic (GLO) Distribution (k > 0)
	result <- quantile_glo(c(0, 1), c(0, 1, 1))
	expect_equal(result, c(-Inf, 1))

	# Generalized Logistic (GLO) Distribution (k < 0)
	result <- quantile_glo(c(0, 1), c(0, 1, -1))
	expect_equal(result, c(-1, Inf))

	# Pearson Type III (PE3) Distribution (k = 0)
	result <- quantile_pe3(c(0, 1), c(0, 1, 0))
	expect_equal(result, c(-Inf, Inf))

	# Pearson Type III (PE3) Distribution (k > 0)
	result <- quantile_pe3(c(0, 1), c(0, 1, 1))
	expect_equal(result, c(-2, Inf))

	# Pearson Type III (PE3) Distribution (k < 0)
	result <- quantile_pe3(c(0, 1), c(0, 1, -1))
	expect_equal(result, c(-Inf, 2))

	# Log-Pearson Type III (PE3) Distribution (k = 0)
	result <- quantile_lp3(c(0, 1), c(0, 1, 0))
	expect_equal(result, c(0, Inf))

	# Log-Pearson Type III (PE3) Distribution (k > 0)
	result <- quantile_lp3(c(0, 1), c(0, 1, 1))
	expect_equal(result, c(exp(-2), Inf))

	# Weibull (WEI) Distribution 
	result <- quantile_wei(c(0, 1), c(0, 1, 1))
	expect_equal(result, c(0, Inf))

})

