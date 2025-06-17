test_that("Test pp-test.R on data set #1", {

	# Run the PP test
	df <- load_data("Application_1.csv")
	test <- pp.test(df$max)

	# Test the results against aTSA:pp.test
	expect_equal(test$statistic, -96.6503, tol = 1e-4)
	expect_equal(test$p.value, 0.01, tol = 1e-4)

})

test_that("Test pp-test.R on data set #2", {

	# Run the PP test
	df <- load_data("Application_2.csv")
	test <- pp.test(df$max)

	# Test the results against aTSA:pp.test
	expect_equal(test$statistic, -40.21905, tol = 1e-4)
	expect_equal(test$p.value, 0.01, tol = 1e-4)

})

test_that("Test pp-test.R on data set #3.1", {

	# Run the PP test
	df <- load_data("Application_3.1.csv")
	test <- pp.test(df$max)

	# Test the results against aTSA:pp.test
	expect_equal(test$statistic, -96.5249, tol = 1e-4)
	expect_equal(test$p.value, 0.01, tol = 1e-4)

})

test_that("Test pp-test.R on data set #3.2", {

	# Run the PP test
	df <- load_data("Application_3.2.csv")
	test <- pp.test(df$max)

	# Test the results against aTSA:pp.test
	expect_equal(test$statistic, -90.0313, tol = 1e-4)
	expect_equal(test$p.value, 0.01, tol = 1e-4)

})

test_that("Test pp-test.R on data set #3.3", {

	# Run the PP test
	df <- load_data("Application_3.3.csv")
	test <- pp.test(df$max)

	# Test the results against aTSA:pp.test
	expect_equal(test$statistic, -90.0663, tol = 1e-4)
	expect_equal(test$p.value, 0.01, tol = 1e-4)

})
