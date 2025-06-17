test_that("Test runs-test.R on means of data set #1", {

	# Load the data and run the Runs test
	df <- load_data("Application_1.csv")
	trend <- sens.trend(df$max, df$year)
	test <- runs.test(trend$residuals)

	# Ensure the test results are the same as the runs.test implementation in randtests
  	expect_equal(test$n, 102)
  	expect_equal(test$n.plus, 51)
  	expect_equal(test$n.minus, 51)
	expect_equal(test$runs, 54)
	expect_equal(test$statistic, 0.3980, tol = 1e-4)
	expect_equal(test$p.value, 0.6906, tol = 1e-4)
	
})

test_that("Test runs-test.R on means of data set #2", {

	# Load the data and run the Runs test
	df <- load_data("Application_2.csv")
	trend <- sens.trend(df$max, df$year)
	test <- runs.test(trend$residuals)

	# Ensure the test results are the same as the runs.test implementation in randtests
  	expect_equal(test$n, 90)
  	expect_equal(test$n.plus, 45)
  	expect_equal(test$n.minus, 45)
	expect_equal(test$runs, 25)
	expect_equal(test$statistic, -4.4523, tol = 1e-4)
	expect_equal(test$p.value, 8.497e-6, tol = 1e-4)
	
})

test_that("Test runs-test.R on means of data set #3.1", {

	# Load the data and run the Runs test
	df <- load_data("Application_3.1.csv")
	trend <- sens.trend(df$max, df$year)
	test <- runs.test(trend$residuals)

	# Ensure the test results are the same as the runs.test implementation in randtests
  	expect_equal(test$n, 108)
  	expect_equal(test$n.plus, 54)
  	expect_equal(test$n.minus, 54)
	expect_equal(test$runs, 59)
	expect_equal(test$statistic, 0.77342, tol = 1e-4)
	expect_equal(test$p.value, 0.4393, tol = 1e-4)
	
})

test_that("Test runs-test.R on means of data set #3.2", {

	# Load the data and run the Runs test
	df <- load_data("Application_3.2.csv")
	trend <- sens.trend(df$max, df$year)
	test <- runs.test(trend$residuals)

	# Ensure the test results are the same as the runs.test implementation in randtests
  	expect_equal(test$n, 88)
  	expect_equal(test$n.plus, 44)
  	expect_equal(test$n.minus, 44)
	expect_equal(test$runs, 49)
	expect_equal(test$statistic, 0.85775, tol = 1e-4)
	expect_equal(test$p.value, 0.3910, tol = 1e-4)
	
})

test_that("Test runs-test.R on means of data set #3.3", {

	# Load the data and run the Runs test
	df <- load_data("Application_3.3.csv")
	trend <- sens.trend(df$max, df$year)
	test <- runs.test(trend$residuals)

	# Ensure the test results are the same as the runs.test implementation in randtests
  	expect_equal(test$n, 96)
  	expect_equal(test$n.plus, 48)
  	expect_equal(test$n.minus, 48)
	expect_equal(test$runs, 47)
	expect_equal(test$statistic, -0.41041, tol = 1e-4)
	expect_equal(test$p.value, 0.6815, tol = 1e-4)
	
})
