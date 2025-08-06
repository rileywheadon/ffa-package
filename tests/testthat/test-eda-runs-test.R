test_that("eda-runs-test.R works on means of ATHABASCA RIVER (07BE001)", {

	# Load the data and run the Runs test
	df <- data_local("CAN-07BE001.csv")
	sens <- eda_sens_trend(df$max, df$year)
	test <- eda_runs_test(sens$residuals)

	# Ensure the test results are the same as the runs.testimplementation in randtests
  	expect_equal(test$n, 102)
	expect_equal(test$runs, 54)
	expect_equal(test$statistic, 0.3980, tol = 1e-4)
	expect_equal(test$p_value, 0.6906, tol = 1e-4)
	
})

test_that("eda-runs-test.R works on means of KOOTENAI RIVER (08NH021)", {

	# Load the data and run the Runs test
	df <- data_local("CAN-08NH021.csv")
	sens <- eda_sens_trend(df$max, df$year)
	test <- eda_runs_test(sens$residuals)

	# Ensure the test results are the same as the runs.testimplementation in randtests
  	expect_equal(test$n, 90)
	expect_equal(test$runs, 25)
	expect_equal(test$statistic, -4.4523, tol = 1e-4)
	expect_equal(test$p_value, 8.497e-6, tol = 1e-4)
	
})

test_that("eda-runs-test.R works on means of BOW RIVER (05BB001)", {

	# Load the data and run the Runs test
	df <- data_local("CAN-05BB001.csv")
	sens <- eda_sens_trend(df$max, df$year)
	test <- eda_runs_test(sens$residuals)

	# Ensure the test results are the same as the runs.testimplementation in randtests
  	expect_equal(test$n, 108)
	expect_equal(test$runs, 59)
	expect_equal(test$statistic, 0.77342, tol = 1e-4)
	expect_equal(test$p_value, 0.4393, tol = 1e-4)
	
})

test_that("eda-runs-test.R works on means of CHILLIWACK RIVER (08MH016)", {

	# Load the data and run the Runs test
	df <- data_local("CAN-08MH016.csv")
	sens <- eda_sens_trend(df$max, df$year)
	test <- eda_runs_test(sens$residuals)

	# Ensure the test results are the same as the runs.testimplementation in randtests
  	expect_equal(test$n, 88)
	expect_equal(test$runs, 49)
	expect_equal(test$statistic, 0.85775, tol = 1e-4)
	expect_equal(test$p_value, 0.3910, tol = 1e-4)
	
})

test_that("eda-runs-test.R works on means of OKANAGAN RIVER (08NM050)", {

	# Load the data and run the Runs test
	df <- data_local("CAN-08NM050.csv")
	sens <- eda_sens_trend(df$max, df$year)
	test <- eda_runs_test(sens$residuals)

	# Ensure the test results are the same as the runs.testimplementation in randtests
  	expect_equal(test$n, 96)
	expect_equal(test$runs, 47)
	expect_equal(test$statistic, -0.41041, tol = 1e-4)
	expect_equal(test$p_value, 0.6815, tol = 1e-4)
	
})
