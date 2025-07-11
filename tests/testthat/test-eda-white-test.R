test_that("Test eda-white-test.R on data set #1", {

	# Load dataset and run White test
	df <- load_data("Application_1.csv")
	result <- eda_white_test(df$max, df$year)

	# Ensure the test results are the same as MATLAB
	expect_equal(result$r_squared, 0.0110, tol = 1e-4)
	expect_equal(result$statistic, 1.1197, tol = 1e-4)
	expect_equal(result$p_value, 0.5713, tol = 1e-4)

})

test_that("Test eda-white-test.R on data set #2", {

	# Load dataset and run White test
	df <- load_data("Application_2.csv")
	result <- eda_white_test(df$max, df$year)

	# Ensure the test results are the same as MATLAB
	expect_equal(result$r_squared, 0.1019, tol = 1e-4)
	expect_equal(result$statistic, 9.2726, tol = 1e-4)
	expect_equal(result$p_value, 0.0097, tol = 1e-4)

})

test_that("Test eda-white-test.R on data set #3.1", {

	# Load dataset and run White test
	df <- load_data("Application_3.1.csv")
	result <- eda_white_test(df$max, df$year)

	# Ensure the test results are the same as MATLAB
	expect_equal(result$r_squared, 0.0447, tol = 1e-4)
	expect_equal(result$statistic, 4.8747, tol = 1e-4)
	expect_equal(result$p_value, 0.0874, tol = 1e-4)

})

test_that("Test eda-white-test.R on data set #3.2", {

	# Load dataset and run White test
	df <- load_data("Application_3.2.csv")
	result <- eda_white_test(df$max, df$year)

	# Ensure the test results are the same as MATLAB
	expect_equal(result$r_squared, 0.0481, tol = 1e-4)
	expect_equal(result$statistic, 4.2810, tol = 1e-4)
	expect_equal(result$p_value, 0.1176, tol = 1e-4)

})

test_that("Test eda-white-test.R on data set #3.3", {

	# Load dataset and run White test
	df <- load_data("Application_3.3.csv")
	result <- eda_white_test(df$max, df$year)

	# Ensure the test results are the same as MATLAB
	expect_equal(result$r_squared, 0.0415, tol = 1e-4)
	expect_equal(result$statistic, 4.0246, tol = 1e-4)
	expect_equal(result$p_value, 0.1337, tol = 1e-4)

})
