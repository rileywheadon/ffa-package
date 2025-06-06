test_that("Test pettitt-test.R on data set #1", {

	# Load dataset and run Pettitt test
	df <- load_data("Application_1.csv")
	test <- pettitt.test(df$max, df$year)

	# Ensure the test results are the same as MATLAB
	expect_equal(length(test$ut), 102)
  	expect_equal(test$k.statistic, 372)
	expect_equal(test$k.critical, 731.467, tolerance = 1e-4)
	expect_equal(test$p.value, 0.461, tolerance = 1e-4)
	expect_equal(test$change.index, 0)
	expect_equal(test$change.year, 0)

})

test_that("Test pettitt-test.R on data set #2", {

	# Load dataset and run Pettitt test
	df <- load_data("Application_2.csv")
	test <- pettitt.test(df$max, df$year)

	# Ensure the test results are the same as MATLAB
	expect_equal(length(test$ut), 91)
  	expect_equal(test$k.statistic, 1871)
	expect_equal(test$k.critical, 616.753, tolerance = 1e-4)
	expect_equal(test$p.value, 0, tolerance = 1e-4)
	expect_equal(test$change.index, 45)
	expect_equal(test$change.year, 1972)

})

test_that("Test pettitt-test.R on data set #3.1", {

	# Load dataset and run Pettitt test
	df <- load_data("Application_3.1.csv")
	test <- pettitt.test(df$max, df$year)

	# Ensure the test results are the same as MATLAB
	expect_equal(length(test$ut), 109)
  	expect_equal(test$k.statistic, 914)
	expect_equal(test$k.critical, 807.790, tolerance = 1e-4)
	expect_equal(test$p.value, 0.022, tolerance = 1e-4)
	expect_equal(test$change.index, 66)
	expect_equal(test$change.year, 1974)

})

test_that("Test pettitt-test.R on data set #3.2", {

	# Load dataset and run Pettitt test
	df <- load_data("Application_3.2.csv")
	test <- pettitt.test(df$max, df$year)

	# Ensure the test results are the same as MATLAB
	expect_equal(length(test$ut), 89)
  	expect_equal(test$k.statistic, 561)
	expect_equal(test$k.critical, 596.605, tolerance = 1e-4)
	expect_equal(test$p.value, 0.071, tolerance = 1e-4)
	expect_equal(test$change.index, 0)
	expect_equal(test$change.year, 0)

})

test_that("Test pettitt-test.R on data set #3.3", {

	# Load dataset and run Pettitt test
	df <- load_data("Application_3.3.csv")
	test <- pettitt.test(df$max, df$year)

	# Ensure the test results are the same as MATLAB
	expect_equal(length(test$ut), 97)
  	expect_equal(test$k.statistic, 1381)
	expect_equal(test$k.critical, 678.517, tolerance = 1e-4)
	expect_equal(test$p.value, 0, tolerance = 1e-4)
	expect_equal(test$change.index, 46)
	expect_equal(test$change.year, 1966)

})
