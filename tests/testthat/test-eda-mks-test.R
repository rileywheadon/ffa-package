test_that("Test eda-mks-test.R on data set #1", {

	# Load dataset and run MKS test
	df <- load_data("Application_1.csv")
	result <- eda_mks_test(df$max, df$year)

	# Ensure the test results are the same as MATLAB
	expect_equal(length(result$s_progressive), 102)
	expect_equal(length(result$s_regressive), 102)
	expect_equal(nrow(result$crossing_df), 6)
	expect_equal(nrow(result$change_df), 0)
	expect_equal(result$p_value, 0.237, tol = 1e-4)

	# Test the crossing indices
	crosses <- c(7, 37, 39, 42, 43, 55)
	expect_equal(result$crossing_df$cross, crosses)

	# Test the interpolated crossing locations
	statistics <- c(-1.1826, 0.362, 0.2429, 0.5384, 0.4428, 0.7226)
	expect_equal(result$crossing_df$statistic, statistics, tol = 1e-4)

	# Test the bound (this dataset only)
	expect_equal(result$bound, 1.960, tol = 1e-4)

})

test_that("Test eda-mks-test.R on data set #2", {

	# Load dataset and run MKS test
	df <- load_data("Application_2.csv")
	result <- eda_mks_test(df$max, df$year)

	# Ensure the test results are the same as MATLAB
	expect_equal(length(result$s_progressive), 91)
	expect_equal(length(result$s_regressive), 91)
	expect_equal(nrow(result$crossing_df), 2)
	expect_equal(nrow(result$change_df), 2)
	expect_equal(result$p_value, 0.015, tol = 1e-4)

	# Test the crossing indices
	crosses <- c(33, 58)
	expect_equal(result$crossing_df$cross, crosses)

	# Test the interpolated crossing locations
	statistics <- c(2.1805, -2.4335)
	expect_equal(result$crossing_df$statistic, statistics, tol = 1e-4)

})

test_that("Test eda-mks-test.R on data set #3.1", {

	# Load dataset and run MKS test
	df <- load_data("Application_3.1.csv")
	result <- eda_mks_test(df$max, df$year)

	# Ensure the test results are the same as MATLAB
	expect_equal(length(result$s_progressive), 109)
	expect_equal(length(result$s_regressive), 109)
	expect_equal(nrow(result$crossing_df), 0)
	expect_equal(nrow(result$change_df), 0)
	expect_equal(result$p_value, 1)

})

test_that("Test eda-mks-test.R on data set #3.2", {

	# Load dataset and run MKS test
	df <- load_data("Application_3.2.csv")
	result <- eda_mks_test(df$max, df$year)

	# Ensure the test results are the same as MATLAB
	expect_equal(length(result$s_progressive), 89)
	expect_equal(length(result$s_regressive), 89)
	expect_equal(nrow(result$crossing_df), 6)
	expect_equal(nrow(result$change_df), 0)
	expect_equal(result$p_value, 0.1557, tol = 1e-4)

	# Test the crossing indices
	crosses <- c(9, 10, 23, 26, 38, 39)
	expect_equal(result$crossing_df$cross, crosses)

	# Test the interpolated crossing locations
	statistics <- c(-1.4196, -1.1132, -0.9080, -0.3328, -0.0659, 0.2299)
	expect_equal(result$crossing_df$statistic, statistics, tol = 1e-4)

})

test_that("Test eda-mks-test.R on data set #3.3", {

	# Load dataset and run MKS test
	df <- load_data("Application_3.3.csv")
	result <- eda_mks_test(df$max, df$year)

	# Ensure the test results are the same as MATLAB
	expect_equal(length(result$s_progressive), 97)
	expect_equal(length(result$s_regressive), 97)
	expect_equal(nrow(result$crossing_df), 0)
	expect_equal(nrow(result$change_df), 0)
	expect_equal(result$p_value, 1)

})
