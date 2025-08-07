test_that("eda-mks-test.R works on ATHABASCA RIVER (07BE001)", {

	# Load dataset and run MKS test
	df <- data_local("CAN-07BE001.csv")
	result <- eda_mks_test(df$max, df$year)

	# Ensure the test results are the same as MATLAB
	expect_equal(length(result$progressive_series), 102)
	expect_equal(length(result$regressive_series), 102)
	expect_equal(nrow(result$change_points), 0)
	expect_equal(result$p_value, 0.237, tol = 1e-4)
	expect_equal(result$bound, 1.960, tol = 1e-4)

})

test_that("eda-mks-test.R works on KOOTENAI RIVER (08NH021)", {

	# Load dataset and run MKS test
	df <- data_local("CAN-08NH021.csv")
	result <- eda_mks_test(df$max, df$year)

	# Ensure the test results are the same as MATLAB
	expect_equal(length(result$progressive_series), 91)
	expect_equal(length(result$regressive_series), 91)
	expect_equal(nrow(result$change_points), 2)
	expect_equal(result$p_value, 0.015, tol = 1e-4)

	# Test the crossing indices
	expect_equal(result$change_points$index[1], 33)
	expect_equal(result$change_points$index[2], 58)

	# Test the interpolated crossing locations
	expect_equal(result$change_points$statistic[1],  2.1805, tol = 1e-4)
	expect_equal(result$change_points$statistic[2], -2.4335, tol = 1e-4)

})

test_that("eda-mks-test.R works on BOW RIVER (05BB001)", {

	# Load dataset and run MKS test
	df <- data_local("CAN-05BB001.csv")
	result <- eda_mks_test(df$max, df$year)

	# Ensure the test results are the same as MATLAB
	expect_equal(length(result$progressive_series), 109)
	expect_equal(length(result$regressive_series), 109)
	expect_equal(nrow(result$change_points), 0)
	expect_equal(result$p_value, 1)

})

test_that("eda-mks-test.R works on CHILLIWACK RIVER (08MH016)", {

	# Load dataset and run MKS test
	df <- data_local("CAN-08MH016.csv")
	result <- eda_mks_test(df$max, df$year)

	# Ensure the test results are the same as MATLAB
	expect_equal(length(result$progressive_series), 89)
	expect_equal(length(result$regressive_series), 89)
	expect_equal(nrow(result$change_points), 0)
	expect_equal(result$p_value, 0.1557, tol = 1e-4)

})

test_that("eda-mks-test.R works on OKANAGAN RIVER (08NM050)", {

	# Load dataset and run MKS test
	df <- data_local("CAN-08NM050.csv")
	result <- eda_mks_test(df$max, df$year)

	# Ensure the test results are the same as MATLAB
	expect_equal(length(result$progressive_series), 97)
	expect_equal(length(result$regressive_series), 97)
	expect_equal(nrow(result$change_points), 0)
	expect_equal(result$p_value, 1)

})
