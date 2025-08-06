# NOTE: Tolerance is high due to randomness in the bootstrap
test_that("eda-bbmk-test.R works on KOOTENAI RIVER (08NH021)", {
	set.seed(1)

	# Load dataset and run BB-MK test 
	df <- data_local("CAN-08NH021.csv")
	test <- eda_bbmk_test(df$max)

	# Ensure the test results are the same as MATLAB
	expect_false(test$reject)
	expect_equal(test$p_value, 0.2, tol = 1e-2)
	expect_equal(test$statistic, -1398, tol = 1e-2)
	expect_equal(unname(test$bounds), c(-1862, 1698), tol = 1e-2)
	
})

test_that("eda-bbmk-test.R works on OKANAGAN RIVER (08NM050)", {
	set.seed(1)

	# Load dataset and run BB-MK test
	df <- data_local("CAN-08NM050.csv")
	test <- eda_bbmk_test(df$max)

	# Ensure the test results are the same as MATLAB
	expect_true(test$reject)
	expect_equal(test$p_value, 0, tol = 1e-2)
	expect_equal(test$statistic, 1822, tol = 1e-2)
	expect_equal(unname(test$bounds), c(-892, 874), tol = 1e-2)
	
})
