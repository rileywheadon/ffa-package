test_that("eda-mk-test.R works on ATHABASCA RIVER (07BE001)", {

	# Load dataset and run MK test
	df <- data_local("CAN-07BE001.csv")
	test <- eda_mk_test(df$max)

	# Ensure the test results are the same as MATLAB
	expect_equal(test$statistic, 165)
	expect_equal(test$variance, 1.1959e5, tol = 1e-4)
	expect_equal(test$p_value, 0.6353, tol = 1e-4)
	expect_equal(test$reject, FALSE)

})

test_that("eda-mk-test.R works on KOOTENAI RIVER (08NH021)", {

	# Load dataset and run MK test
	df <- data_local("CAN-08NH021.csv")
	test <- eda_mk_test(df$max)

	# Ensure the test results are the same as MATLAB
	expect_equal(test$statistic, -1398)
	expect_equal(test$variance, 8.5071e4, tol = 1e-4)
	expect_equal(test$p_value, 0, tol = 1e-4)
	expect_equal(test$reject, TRUE)

})

test_that("eda-mk-test.R works on BOW RIVER (05BB001)", {

	# Load dataset and run MK test
	df <- data_local("CAN-05BB001.csv")
	test <- eda_mk_test(df$max)

	# Ensure the test results are the same as MATLAB
	expect_equal(test$statistic, -1035)
	expect_equal(test$variance, 145817, tol = 1e-4)
	expect_equal(test$p_value, 0.0068, tol = 1e-4)
	expect_equal(test$reject, TRUE)

})

test_that("eda-mk-test.R works on CHILLIWACK RIVER (08MH016)", {

	# Load dataset and run MK test
	df <- data_local("CAN-08MH016.csv")
	test <- eda_mk_test(df$max)

	# Ensure the test results are the same as MATLAB
	expect_equal(test$statistic, 518)
	expect_equal(test$variance, 7.9617e4, tol = 1e-4)
	expect_equal(test$p_value, 0.0669, tol = 1e-4)
	expect_equal(test$reject, FALSE)

})

test_that("eda-mk-test.R works on OKANAGAN RIVER (08NM050)", {

	# Load dataset and run MK test
	df <- data_local("CAN-08NM050.csv")
	test <- eda_mk_test(df$max)

	# Ensure the test results are the same as MATLAB
	expect_equal(test$statistic, 1822)
	expect_equal(test$variance, 102933, tol = 1e-4)
	expect_equal(test$p_value, 0, tol = 1e-4)
	expect_equal(test$reject, TRUE)

})

test_that("eda-mk-test.R behaves correctly when S = 0", {

	data <- rep(1, 100)
	test <- eda_mk_test(data)

	expect_equal(test$statistic, 0)
	expect_equal(test$variance, 0)
	expect_equal(test$p_value, 1)
	expect_equal(test$reject, FALSE)

})
