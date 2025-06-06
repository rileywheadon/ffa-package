test_that("Test sens-estimator.R on variances of data set #1", {

	# Load the data and run sen's estimator 
	df <- load_data("Application_1.csv", clean = FALSE)
	df_variance <- mw.variance(df)
	test <- sens.trend(df_variance$std, df_variance$year)

	# Ensure the test results are the same as MATLAB
  	expect_equal(test$sens.slope, 0.8474, tolerance = 1e-4)
	expect_equal(test$sens.intercept, -794.4911, tolerance = 1e-4)

})

test_that("Test sens-estimator.R on variances of data set #2", {

	# Load the data and run sen's estimator 
	df <- load_data("Application_2.csv", clean = FALSE)
	df_variance <- mw.variance(df)
	test <- sens.trend(df_variance$std, df_variance$year)

	# Ensure the test results are the same as MATLAB
  	expect_equal(test$sens.slope, -3.4902, tolerance = 1e-4)
	expect_equal(test$sens.intercept, 7.3114e3, tolerance = 1e-4)

})

test_that("Test sens-estimator.R on variances of data set #3.1", {

	# Load the data and run sen's estimator 
	df <- load_data("Application_3.1.csv", clean = FALSE)
	df_variance <- mw.variance(df)
	test <- sens.trend(df_variance$std, df_variance$year)

	# Ensure the test results are the same as MATLAB
  	expect_equal(test$sens.slope, -0.0094, tolerance = 1e-4)
	expect_equal(test$sens.intercept, 71.6560, tolerance = 1e-4)

})

test_that("Test sens-estimator.R on variances of data set #3.2", {

	# Load the data and run sen's estimator 
	df <- load_data("Application_3.2.csv", clean = FALSE)
	df_variance <- mw.variance(df)
	test <- sens.trend(df_variance$std, df_variance$year)

	# Ensure the test results are the same as MATLAB
  	expect_equal(test$sens.slope, 0.2260, tolerance = 1e-4)
	expect_equal(test$sens.intercept, -424.3596, tolerance = 1e-4)

})

test_that("Test sens-estimator.R on variances of data set #3.3", {

	# Load the data and run sen's estimator 
	df <- load_data("Application_3.3.csv", clean = FALSE)
	df_variance <- mw.variance(df)
	test <- sens.trend(df_variance$std, df_variance$year)

	# Ensure the test results are the same as MATLAB
  	expect_equal(test$sens.slope, 0.1346, tolerance = 1e-4)
	expect_equal(test$sens.intercept, -249.9469, tolerance = 1e-4)

})
