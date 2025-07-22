test_that("eda-kpss-test.R works on ATHABASCA RIVER (07BE001)", {

	# Run the KPSS test
	df <- data_local("CAN-07BE001.csv")
	test <- eda_kpss_test(df$max)

	# Test the results against aTSA:kpss.test
	expect_equal(test$statistic, 0.0813, tol = 1e-4)
	expect_equal(test$p_value, 0.10, tol = 1e-4)
	expect_equal(test$reject, FALSE)

})

test_that("eda-kpss-test.R works on KOOTENAI RIVER (08NH021)", {

	# Run the KPSS test
	df <- data_local("CAN-08NH021.csv")
	test <- eda_kpss_test(df$max)

	# Test the results against aTSA:kpss.test
	expect_equal(test$statistic, 0.1786, tol = 1e-4)
	expect_equal(test$p_value, 0.02401, tol = 1e-4)
	expect_equal(test$reject, TRUE)

})

test_that("eda-kpss-test.R works on BOW RIVER (05BB001)", {

	# Run the KPSS test
	df <- data_local("CAN-05BB001.csv")
	test <- eda_kpss_test(df$max)

	# Test the results against aTSA:kpss.test
	expect_equal(test$statistic, 0.0364, tol = 1e-4)
	expect_equal(test$p_value, 0.10, tol = 1e-4)
	expect_equal(test$reject, FALSE)

})

test_that("eda-kpss-test.R works on CHILLIWACK RIVER (08MH016)", {

	# Run the KPSS test
	df <- data_local("CAN-08MH016.csv")
	test <- eda_kpss_test(df$max)

	# Test the results against aTSA:kpss.test
	expect_equal(test$statistic, 0.0415, tol = 1e-4)
	expect_equal(test$p_value, 0.10, tol = 1e-4)
	expect_equal(test$reject, FALSE)

})

test_that("eda-kpss-test.R works on OKANAGAN RIVER (08NM050)", {

	# Run the KPSS test
	df <- data_local("CAN-08NM050.csv")
	test <- eda_kpss_test(df$max)

	# Test the results against aTSA:kpss.test
	expect_equal(test$statistic, 0.0381, tol = 1e-4)
	expect_equal(test$p_value, 0.10, tol = 1e-4)
	expect_equal(test$reject, FALSE)

})

test_that("eda-kpss-test.R rejects the null when the data has a change in mean", {
	set.seed(1)

	n <- 500
	epsilon <- rnorm(n)
	data <- c(rep(0, n / 2), rep(2, n / 2)) + epsilon - min(epsilon)

	# Run the KPSS test on the synthetic data
	test <- eda_kpss_test(data)
	expect_equal(test$p_value, 0.01, tol = 1e-4)
	expect_equal(test$reject, TRUE)

})


