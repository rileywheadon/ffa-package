test_that("Test eda-mk-test.R on data set #1", {

	# Load dataset and run MK test
	df <- load_data("Application_1.csv")
	test <- eda_mk_test(df$max)

	# Ensure the test results are the same as MATLAB
	expect_equal(test$s_statistic, 165)
	expect_equal(test$s_variance, 1.1959e5, tol = 1e-4)
	expect_equal(test$p_value, 0.6353, tol = 1e-4)
	expect_equal(test$reject, FALSE)

})

test_that("Test eda-mk-test.R on data set #2", {

	# Load dataset and run MK test
	df <- load_data("Application_2.csv")
	test <- eda_mk_test(df$max)

	# Ensure the test results are the same as MATLAB
	expect_equal(test$s_statistic, -1398)
	expect_equal(test$s_variance, 8.5071e4, tol = 1e-4)
	expect_equal(test$p_value, 0, tol = 1e-4)
	expect_equal(test$reject, TRUE)

})

test_that("Test eda-mk-test.R on data set #3.1", {

	# Load dataset and run MK test
	df <- load_data("Application_3.1.csv")
	test <- eda_mk_test(df$max)

	# Ensure the test results are the same as MATLAB
	expect_equal(test$s_statistic, -1035)
	expect_equal(test$s_variance, 145817, tol = 1e-4)
	expect_equal(test$p_value, 0.0068, tol = 1e-4)
	expect_equal(test$reject, TRUE)

})

test_that("Test eda-mk-test.R on data set #3.2", {

	# Load dataset and run MK test
	df <- load_data("Application_3.2.csv")
	test <- eda_mk_test(df$max)

	# Ensure the test results are the same as MATLAB
	expect_equal(test$s_statistic, 518)
	expect_equal(test$s_variance, 7.9617e4, tol = 1e-4)
	expect_equal(test$p_value, 0.0669, tol = 1e-4)
	expect_equal(test$reject, FALSE)

})

test_that("Test eda-mk-test.R on data set #3.3", {

	# Load dataset and run MK test
	df <- load_data("Application_3.3.csv")
	test <- eda_mk_test(df$max)

	# Ensure the test results are the same as MATLAB
	expect_equal(test$s_statistic, 1822)
	expect_equal(test$s_variance, 102933, tol = 1e-4)
	expect_equal(test$p_value, 0, tol = 1e-4)
	expect_equal(test$reject, TRUE)

})

test_that("eda-mk-test.R behaves correctly when S = 0", {

	data <- rep(1, 100)
	test <- eda_mk_test(data)

	expect_equal(test$s_statistic, 0)
	expect_equal(test$s_variance, 0)
	expect_equal(test$p_value, 1)
	expect_equal(test$reject, FALSE)

})
