test_that("eda-white-test.R works on ATHABASCA RIVER (07BE001)", {

	# Load dataset and run White test
	df <- data_local("CAN-07BE001.csv")
	result <- eda_white_test(df$max, df$year)

	# Ensure the test results are the same as MATLAB
	expect_equal(result$statistic, 1.1197, tol = 1e-4)
	expect_equal(result$p_value, 0.5713, tol = 1e-4)

})

test_that("eda-white-test.R works on KOOTENAI RIVER (08NH021)", {

	# Load dataset and run White test
	df <- data_local("CAN-08NH021.csv")
	result <- eda_white_test(df$max, df$year)

	# Ensure the test results are the same as MATLAB
	expect_equal(result$statistic, 9.2726, tol = 1e-4)
	expect_equal(result$p_value, 0.0097, tol = 1e-4)

})

test_that("eda-white-test.R works on BOW RIVER (05BB001)", {

	# Load dataset and run White test
	df <- data_local("CAN-05BB001.csv")
	result <- eda_white_test(df$max, df$year)

	# Ensure the test results are the same as MATLAB
	expect_equal(result$statistic, 4.8747, tol = 1e-4)
	expect_equal(result$p_value, 0.0874, tol = 1e-4)

})

test_that("eda-white-test.R works on CHILLIWACK RIVER (08MH016)", {

	# Load dataset and run White test
	df <- data_local("CAN-08MH016.csv")
	result <- eda_white_test(df$max, df$year)

	# Ensure the test results are the same as MATLAB
	expect_equal(result$statistic, 4.2810, tol = 1e-4)
	expect_equal(result$p_value, 0.1176, tol = 1e-4)

})

test_that("eda-white-test.R works on OKANAGAN RIVER (08NM050)", {

	# Load dataset and run White test
	df <- data_local("CAN-08NM050.csv")
	result <- eda_white_test(df$max, df$year)

	# Ensure the test results are the same as MATLAB
	expect_equal(result$statistic, 4.0246, tol = 1e-4)
	expect_equal(result$p_value, 0.1337, tol = 1e-4)

})
