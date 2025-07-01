test_that("Test pettitt-test.R on data set #1", {

	# Load dataset and run Pettitt test
	ams <- load_data("Application_1.csv")
	result <- pettitt_test(ams)

	# Ensure the test results are the same as MATLAB
	expect_equal(length(result$u_t), 102)
  	expect_equal(result$k_statistic, 372)
	expect_equal(result$k_critical, 731.467, tol = 1e-4)
	expect_equal(result$p_value, 0.461, tol = 1e-4)
	expect_equal(result$change_index, 0)
	expect_equal(result$change_year, 0)

})

test_that("Test pettitt-test.R on data set #2", {

	# Load dataset and run Pettitt test
	ams <- load_data("Application_2.csv")
	result <- pettitt_test(ams)

	# Ensure the test results are the same as MATLAB
	expect_equal(length(result$u_t), 91)
  	expect_equal(result$k_statistic, 1871)
	expect_equal(result$k_critical, 616.753, tol = 1e-4)
	expect_equal(result$p_value, 0, tol = 1e-4)
	expect_equal(result$change_index, 45)
	expect_equal(result$change_year, 1972)

})

test_that("Test pettitt-test.R on data set #3.1", {

	# Load dataset and run Pettitt test
	ams <- load_data("Application_3.1.csv")
	result <- pettitt_test(ams)

	# Ensure the test results are the same as MATLAB
	expect_equal(length(result$u_t), 109)
  	expect_equal(result$k_statistic, 914)
	expect_equal(result$k_critical, 807.790, tol = 1e-4)
	expect_equal(result$p_value, 0.022, tol = 1e-4)
	expect_equal(result$change_index, 66)
	expect_equal(result$change_year, 1974)

})

test_that("Test pettitt-test.R on data set #3.2", {

	# Load dataset and run Pettitt test
	ams <- load_data("Application_3.2.csv")
	result <- pettitt_test(ams)

	# Ensure the test results are the same as MATLAB
	expect_equal(length(result$u_t), 89)
  	expect_equal(result$k_statistic, 561)
	expect_equal(result$k_critical, 596.605, tol = 1e-4)
	expect_equal(result$p_value, 0.071, tol = 1e-4)
	expect_equal(result$change_index, 0)
	expect_equal(result$change_year, 0)

})

test_that("Test pettitt-test.R on data set #3.3", {

	# Load dataset and run Pettitt test
	ams <- load_data("Application_3.3.csv")
	result <- pettitt_test(ams)

	# Ensure the test results are the same as MATLAB
	expect_equal(length(result$u_t), 97)
  	expect_equal(result$k_statistic, 1381)
	expect_equal(result$k_critical, 678.517, tol = 1e-4)
	expect_equal(result$p_value, 0, tol = 1e-4)
	expect_equal(result$change_index, 46)
	expect_equal(result$change_year, 1966)

})
