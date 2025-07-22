test_that("pettitt-test.R works on ATHABASCA RIVER (07BE001)", {

	# Load dataset and run Pettitt test
	df <- data_local("CAN-07BE001.csv")
	result <- eda_pettitt_test(df$max, df$year)

	# Ensure the test results are the same as MATLAB
	expect_equal(length(result$u_t), 102)
  	expect_equal(result$k_statistic, 372)
	expect_equal(result$k_critical, 731.467, tol = 1e-4)
	expect_equal(result$p_value, 0.461, tol = 1e-4)
	expect_equal(result$change_index, 0)
	expect_equal(result$change_year, 0)

})

test_that("pettitt-test.R works on KOOTENAI RIVER (08NH021)", {

	# Load dataset and run Pettitt test
	df <- data_local("CAN-08NH021.csv")
	result <- eda_pettitt_test(df$max, df$year)

	# Ensure the test results are the same as MATLAB
	expect_equal(length(result$u_t), 91)
  	expect_equal(result$k_statistic, 1871)
	expect_equal(result$k_critical, 616.753, tol = 1e-4)
	expect_equal(result$p_value, 0, tol = 1e-4)
	expect_equal(result$change_index, 45)
	expect_equal(result$change_year, 1972)

})

test_that("pettitt-test.R works on BOW RIVER (05BB001)", {

	# Load dataset and run Pettitt test
	df <- data_local("CAN-05BB001.csv")
	result <- eda_pettitt_test(df$max, df$year)

	# Ensure the test results are the same as MATLAB
	expect_equal(length(result$u_t), 109)
  	expect_equal(result$k_statistic, 914)
	expect_equal(result$k_critical, 807.790, tol = 1e-4)
	expect_equal(result$p_value, 0.022, tol = 1e-4)
	expect_equal(result$change_index, 66)
	expect_equal(result$change_year, 1974)

})

test_that("pettitt-test.R works on CHILLIWACK RIVER (08MH016)", {

	# Load dataset and run Pettitt test
	df <- data_local("CAN-08MH016.csv")
	result <- eda_pettitt_test(df$max, df$year)

	# Ensure the test results are the same as MATLAB
	expect_equal(length(result$u_t), 89)
  	expect_equal(result$k_statistic, 561)
	expect_equal(result$k_critical, 596.605, tol = 1e-4)
	expect_equal(result$p_value, 0.071, tol = 1e-4)
	expect_equal(result$change_index, 0)
	expect_equal(result$change_year, 0)

})

test_that("pettitt-test.R works on OKANAGAN RIVER (08NM050)", {

	# Load dataset and run Pettitt test
	df <- data_local("CAN-08NM050.csv")
	result <- eda_pettitt_test(df$max, df$year)

	# Ensure the test results are the same as MATLAB
	expect_equal(length(result$u_t), 97)
  	expect_equal(result$k_statistic, 1381)
	expect_equal(result$k_critical, 678.517, tol = 1e-4)
	expect_equal(result$p_value, 0, tol = 1e-4)
	expect_equal(result$change_index, 46)
	expect_equal(result$change_year, 1966)

})
