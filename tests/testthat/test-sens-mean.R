test_that("Test sens-estimator.R on means of data set #1", {

	# Load the data for the means
	df <- load_data("Application_1.csv")
	test <- sens.trend(df$max, df$year)

	# Ensure the test results are the same as MATLAB
  	expect_equal(test$sens.slope, 1.1628, tolerance = 1e-4)
	expect_equal(test$sens.intercept, -477.5581, tolerance = 1e-4)

})

test_that("Test sens-estimator.R on means of data set #2", {

	# Load the data for the means
	df <- load_data("Application_2.csv")
	test <- sens.trend(df$max, df$year)

	# Ensure the test results are the same as MATLAB
  	expect_equal(test$sens.slope, -13.8571, tolerance = 1e-4)
	expect_equal(test$sens.intercept, 2.9026e4, tolerance = 1e-4)

})

test_that("Test sens-estimator.R on means of data set #3.1", {

	# Load the data for the means
	df <- load_data("Application_3.1.csv")
	test <- sens.trend(df$max, df$year)

	# Ensure the test results are the same as MATLAB
  	expect_equal(test$sens.slope, -0.4882, tolerance = 1e-4)
	expect_equal(test$sens.intercept, 1.1597e3, tolerance = 1e-4)

})

test_that("Test sens-estimator.R on means of data set #3.2", {

	# Load the data for the means
	df <- load_data("Application_3.2.csv")
	test <- sens.trend(df$max, df$year)

	# Ensure the test results are the same as MATLAB
  	expect_equal(test$sens.slope, 0.1370, tolerance = 1e-4)
	expect_equal(test$sens.intercept, -200.4634, tolerance = 1e-4)

})

test_that("Test sens-estimator.R on means of data set #3.3", {

	# Load the data for the means
	df <- load_data("Application_3.3.csv")
	test <- sens.trend(df$max, df$year)

	# Ensure the test results are the same as MATLAB
  	expect_equal(test$sens.slope, 0.4039, tolerance = 1e-4)
	expect_equal(test$sens.intercept, -756.2370, tolerance = 1e-4)

})
