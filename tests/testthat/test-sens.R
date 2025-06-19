test_that("Test sens-estimator.R on means of data set #1", {

	# Load the data for the means
	df <- load_data("Application_1.csv")
	test <- sens.trend(df$max, df$year)

	# Ensure the test results are the same as MATLAB
  	expect_equal(test$sens.slope, 116.279, tol = 1e-4)
	expect_equal(test$sens.intercept, 1731.7442, tol = 1e-4)

})

test_that("Test sens-estimator.R on means of data set #2", {

	# Load the data for the means
	df <- load_data("Application_2.csv")
	test <- sens.trend(df$max, df$year)

	# Ensure the test results are the same as MATLAB
  	expect_equal(test$sens.slope, -1385.7143, tol = 1e-4)
	expect_equal(test$sens.intercept, 2697.7143, tol = 1e-4)

})

test_that("Test sens-estimator.R on means of data set #3.1", {

	# Load the data for the means
	df <- load_data("Application_3.1.csv")
	test <- sens.trend(df$max, df$year)

	# Ensure the test results are the same as MATLAB
  	expect_equal(test$sens.slope, -48.8234, tol = 1e-4)
	expect_equal(test$sens.intercept, 232.0352, tol = 1e-4)

})

test_that("Test sens-estimator.R on means of data set #3.2", {

	# Load the data for the means
	df <- load_data("Application_3.2.csv")
	test <- sens.trend(df$max, df$year)

	# Ensure the test results are the same as MATLAB
  	expect_equal(test$sens.slope, 13.7026, tol = 1e-4)
	expect_equal(test$sens.intercept, 59.8854, tol = 1e-4)

})

test_that("Test sens-estimator.R on means of data set #3.3", {

	# Load the data for the means
	df <- load_data("Application_3.3.csv")
	test <- sens.trend(df$max, df$year)

	# Ensure the test results are the same as MATLAB
  	expect_equal(test$sens.slope, 40.3939, tol = 1e-4)
	expect_equal(test$sens.intercept, 11.2479, tol = 1e-4)

})
