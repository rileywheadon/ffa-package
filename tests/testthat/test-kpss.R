test_that("Test kpss-test.R on data set #1", {

	# Run the KPSS test
	df <- load_data("Application_1.csv")
	test <- kpss.test(df$max)

	# Test the results against aTSA:kpss.test
	expect_equal(test$statistic, 0.0813, tol = 1e-4)
	expect_equal(test$p.value, 0.10, tol = 1e-4)

})

test_that("Test kpss-test.R on data set #2", {

	# Run the KPSS test
	df <- load_data("Application_2.csv")
	test <- kpss.test(df$max)

	# Test the results against aTSA:kpss.test
	expect_equal(test$statistic, 0.1786, tol = 1e-4)
	expect_equal(test$p.value, 0.02401, tol = 1e-4)

})

test_that("Test kpss-test.R on data set #3.1", {

	# Run the KPSS test
	df <- load_data("Application_3.1.csv")
	test <- kpss.test(df$max)

	# Test the results against aTSA:kpss.test
	expect_equal(test$statistic, 0.0364, tol = 1e-4)
	expect_equal(test$p.value, 0.10, tol = 1e-4)

})

test_that("Test kpss-test.R on data set #3.2", {

	# Run the KPSS test
	df <- load_data("Application_3.2.csv")
	test <- kpss.test(df$max)

	# Test the results against aTSA:kpss.test
	expect_equal(test$statistic, 0.0415, tol = 1e-4)
	expect_equal(test$p.value, 0.10, tol = 1e-4)

})

test_that("Test kpss-test.R on data set #3.3", {

	# Run the KPSS test
	df <- load_data("Application_3.3.csv")
	test <- kpss.test(df$max)

	# Test the results against aTSA:kpss.test
	expect_equal(test$statistic, 0.0381, tol = 1e-4)
	expect_equal(test$p.value, 0.10, tol = 1e-4)

})
