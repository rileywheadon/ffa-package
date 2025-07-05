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



