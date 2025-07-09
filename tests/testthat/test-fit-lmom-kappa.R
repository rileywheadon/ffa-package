test_that("fit_lmom_kappa works on data set #1", {

	# Load dataset and run L-moments estimation
	df <- load_data("Application_1.csv")
	KAP <- fit_lmom_kappa(df$max)
	expect_equal(KAP[1], 1656.6531, tol = 1e-3)
	expect_equal(KAP[2],  567.3419, tol = 1e-3)
	expect_equal(KAP[3],   -0.1544, tol = 1e-3)
	expect_equal(KAP[4],   -0.1704, tol = 1e-3)

})

test_that("fit_lmom_kappa works on data set #2", {

	# Load dataset and run L-moments estimation
	df <- load_data("Application_2.csv")
	KAP <- fit_lmom_kappa(df$max)
	expect_equal(KAP[1],  744.3404, tol = 1e-3)
	expect_equal(KAP[2], 1479.1514, tol = 1e-3)
	expect_equal(KAP[3],    0.5351, tol = 1e-3)
	expect_equal(KAP[4],    0.9253, tol = 1e-3)

})

test_that("fit_lmom_kappa works on data set #3.1", {

	# Load dataset and run L-moments estimation
	df <- load_data("Application_3.1.csv")
	KAP <- fit_lmom_kappa(df$max)
	expect_equal(KAP[1], 174.3491, tol = 1e-3)
	expect_equal(KAP[2],  60.5105, tol = 1e-3)
	expect_equal(KAP[3],   0.1064, tol = 1e-3)
	expect_equal(KAP[4],   0.2880, tol = 1e-3)

})

test_that("fit_lmom_kappa works on data set #3.2", {

	# Load dataset and run L-moments estimation
	df <- load_data("Application_3.2.csv")
	KAP <- fit_lmom_kappa(df$max)
	expect_equal(KAP[1], 61.7462, tol = 1e-3)
	expect_equal(KAP[2], 17.7799, tol = 1e-3)
	expect_equal(KAP[3],  0.0001, tol = 1e-3)
	expect_equal(KAP[4],  0.0747, tol = 1e-3)

})

test_that("fit_lmom_kappa works on data set #3.3", {

	# Load dataset and run L-moments estimation
	df <- load_data("Application_3.3.csv")
	KAP <- fit_lmom_kappa(df$max)
	expect_equal(KAP[1],  5.8948, tol = 1e-3)
	expect_equal(KAP[2], 51.3148, tol = 1e-3)
	expect_equal(KAP[3],  0.6732, tol = 1e-3)
	expect_equal(KAP[4],  1.1215, tol = 1e-3)

})

test_that("sumquad.tau3tau4 handles GUM case.", {
	result <- sumquad.tau3tau4(c(0, 0), c(0, 0))
	expected <- 0.1699^2 + 0.1504^2
	expect_equal(result, expected, tol = 1e-4)
})

test_that("sumquad.tau3tau4 handles GEV case.", {
	result <- sumquad.tau3tau4(c(0.5, 0), c(0, 0))
	expected <- 0.1140^2 + 0.1054^2
	expect_equal(result, expected, tol = 1e-4)
})

test_that("sumquad.tau3tau4 handles invalid parameters.", {
	expect_error(sumquad.tau3tau4(c(-2, 0), c(0, 0)), "invalid parameters")
	expect_error(sumquad.tau3tau4(c(-1, -1), c(0, 0)), "invalid parameters")
	expect_error(sumquad.tau3tau4(c(2, -1), c(0, 0)), "invalid parameters")
})

test_that("mu.sigma handles GUM case.", {
	result <- mu.sigma(0, 1, 0, 0)
	expect_equal(result$mu, -0.5772 / log(2), tol = 1e-4)
	expect_equal(result$sigma, 1 / log(2), tol = 1e-4)
})

test_that("mu.sigma handles GEV case.", {
	result <- mu.sigma(0, 1, 0.5, 0)
	expect_equal(result$mu, -0.4383, tol = 1e-4)
	expect_equal(result$sigma, 1.9263, tol = 1e-4)
})

test_that("mu.sigma handles invalid parameters.", {
	expect_error(mu.sigma(0, 1, -2,  0), "invalid parameters")
	expect_error(mu.sigma(0, 1, -1, -1), "invalid parameters")
	expect_error(mu.sigma(0, 1,  2, -1), "invalid parameters")
})
