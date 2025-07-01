test_that("Test mk-test.R on data set #1", {

	# Load dataset and run MK test
	df <- load_data("Application_1.csv")
	test <- mk.test(df$max)

	# Ensure the test results are the same as MATLAB
	expect_equal(test$s.statistic, 165)
	expect_equal(test$s.variance, 1.1959e5, tol = 1e-4)
	expect_equal(test$p.value, 0.6353, tol = 1e-4)
	expect_equal(test$reject, FALSE)

})

test_that("Test mk-test.R on data set #2", {

	# Load dataset and run MK test
	df <- load_data("Application_2.csv")
	test <- mk.test(df$max)

	# Ensure the test results are the same as MATLAB
	expect_equal(test$s.statistic, -1398)
	expect_equal(test$s.variance, 8.5071e4, tol = 1e-4)
	expect_equal(test$p.value, 0, tol = 1e-4)
	expect_equal(test$reject, TRUE)

})

test_that("Test mk-test.R on data set #3.1", {

	# Load dataset and run MK test
	df <- load_data("Application_3.1.csv")
	test <- mk.test(df$max)

	# Ensure the test results are the same as MATLAB
	expect_equal(test$s.statistic, -1035)
	expect_equal(test$s.variance, 145817, tol = 1e-4)
	expect_equal(test$p.value, 0.0068, tol = 1e-4)
	expect_equal(test$reject, TRUE)

})

test_that("Test mk-test.R on data set #3.2", {

	# Load dataset and run MK test
	df <- load_data("Application_3.2.csv")
	test <- mk.test(df$max)

	# Ensure the test results are the same as MATLAB
	expect_equal(test$s.statistic, 518)
	expect_equal(test$s.variance, 7.9617e4, tol = 1e-4)
	expect_equal(test$p.value, 0.0669, tol = 1e-4)
	expect_equal(test$reject, FALSE)

})

test_that("Test mk-test.R on data set #3.3", {

	# Load dataset and run MK test
	df <- load_data("Application_3.3.csv")
	test <- mk.test(df$max)

	# Ensure the test results are the same as MATLAB
	expect_equal(test$s.statistic, 1822)
	expect_equal(test$s.variance, 102933, tol = 1e-4)
	expect_equal(test$p.value, 0, tol = 1e-4)
	expect_equal(test$reject, TRUE)

})
