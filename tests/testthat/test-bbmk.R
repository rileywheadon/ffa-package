# NOTE: Tolerance is high due to randomness in the bootstrap
test_that("Test bbmk-test.R on data set #2", {
	set.seed(1)

	# Load dataset and run BB-MK test with profiling
	df <- load_data("Application_2.csv")
	start <- Sys.time()
	test <- bbmk.test(df$max)
	end <- Sys.time()
	# print(end - start)

	# Ensure the test results are the same as MATLAB
	expect_equal(test$p.value, 0.2105, tol = 5e-2)
	expect_equal(unname(test$bounds), c(-1862, 1698), tol = 5e-2)
	
})

test_that("Test bbmk-test.R on data set #3.3", {
	set.seed(1)

	# Load dataset and run BB-MK test
	df <- load_data("Application_3.3.csv")
	test <- bbmk.test(df$max)

	# Ensure the test results are the same as MATLAB
	expect_equal(test$p.value, 8e-6, tol = 5e-2)
	expect_equal(unname(test$bounds), c(-902, 894), tol = 5e-2)
	
})
