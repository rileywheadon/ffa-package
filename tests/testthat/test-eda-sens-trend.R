test_that("eda-sens-trend.R works on means of ATHABASCA RIVER (07BE001)", {

	# Load the data for the means
	df <- data_local("CAN-07BE001.csv")
	sens <- eda_sens_trend(df$max, df$year)

	# Ensure the test results are the same as MATLAB
  	expect_equal(sens$slope, 116.279, tol = 1e-4)
	expect_equal(sens$intercept, 1731.7442, tol = 1e-4)

})

test_that("eda-sens-trend.R works on means of KOOTENAI RIVER (08NH021)", {

	# Load the data for the means
	df <- data_local("CAN-08NH021.csv")
	sens <- eda_sens_trend(df$max, df$year)

	# Ensure the test results are the same as MATLAB
  	expect_equal(sens$slope, -1385.7143, tol = 1e-4)
	expect_equal(sens$intercept, 2697.7143, tol = 1e-4)

})

test_that("eda-sens-trend.R works on means of BOW RIVER (05BB001)", {

	# Load the data for the means
	df <- data_local("CAN-05BB001.csv")
	sens <- eda_sens_trend(df$max, df$year)

	# Ensure the test results are the same as MATLAB
  	expect_equal(sens$slope, -48.8234, tol = 1e-4)
	expect_equal(sens$intercept, 232.0352, tol = 1e-4)

})

test_that("eda-sens-trend.R works on means of CHILLIWACK RIVER (08MH016)", {

	# Load the data for the means
	df <- data_local("CAN-08MH016.csv")
	sens <- eda_sens_trend(df$max, df$year)

	# Ensure the test results are the same as MATLAB
  	expect_equal(sens$slope, 13.7026, tol = 1e-4)
	expect_equal(sens$intercept, 59.8854, tol = 1e-4)

})

test_that("eda-sens-trend.R works on means of OKANAGAN RIVER (08NM050)", {

	# Load the data for the means
	df <- data_local("CAN-08NM050.csv")
	sens <- eda_sens_trend(df$max, df$year)

	# Ensure the test results are the same as MATLAB
  	expect_equal(sens$slope, 40.3939, tol = 1e-4)
	expect_equal(sens$intercept, 11.2479, tol = 1e-4)

})
