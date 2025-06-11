test_that("Test model-assessment on data set #1 with GEV/L-moments/S-bootstrap.", {

	# Load dataset and run L-moments estimation with uncertainty analysis
	df <- load_data("Application_1.csv")
	params <- lmom.estimation(df$max, "GEV")
	uncertainty <- sb.uncertainty(df$max, "GEV", "L-moments")
	assessment <- model.assessment(df$max, "GEV", params, uncertainty)

	# Test results against MATLAB
	expect_equal(assessment$R2  ,   0.9922, tol = 1e-3)
	expect_equal(assessment$RMSE,  95.9225, tol = 1e-3)
	expect_equal(assessment$bias, -20.6072, tol = 1e-2)
	expect_equal(assessment$AIC , 471.4811, tol = 1e-3)
	expect_equal(assessment$BIC , 479.3561, tol = 1e-3)
	expect_equal(assessment$AW  , 603.0077, tol = 1e-2)
	expect_equal(assessment$POC , 100.0000)
	expect_equal(assessment$CWI , 545.6239, tol = 1e-2)

})

test_that("Test model-assessment on data set #2 with GPA/L-moments/S-bootstrap.", {

	# Load dataset and run L-moments estimation with uncertainty analysis
	df <- load_data("Application_2.csv")
	params <- lmom.estimation(df$max, "GPA")
	uncertainty <- sb.uncertainty(df$max, "GPA", "L-moments")
	assessment <- model.assessment(df$max, "GPA", params, uncertainty)

	# Test results against MATLAB
	expect_equal(assessment$R2  ,   0.9889, tol = 1e-3)
	expect_equal(assessment$RMSE,  73.2271, tol = 1e-3)
	expect_equal(assessment$bias,  -3.6277, tol = 1e-3)
	expect_equal(assessment$AIC , 396.7145, tol = 1e-3)
	expect_equal(assessment$BIC , 404.2471, tol = 1e-3)
	expect_equal(assessment$AW  , 430.1129, tol = 1e-2)
	expect_equal(assessment$POC , 100.0000)
	expect_equal(assessment$CWI , 389.1822, tol = 1e-2)

})

# NOTE: Test case on dataset #3.1 with PE3 has been removed, see matlab.md

test_that("Test model-assessment on data set #3.2 with GNO/L-moments/S-bootstrap.", {

	# Load dataset and run L-moments estimation with uncertainty analysis
	df <- load_data("Application_3.2.csv")
	params <- lmom.estimation(df$max, "GNO")
	uncertainty <- sb.uncertainty(df$max, "GNO", "L-moments")
	assessment <- model.assessment(df$max, "GNO", params, uncertainty)

	# Test results against MATLAB
	expect_equal(assessment$R2  ,   0.9884, tol = 1e-3)
	expect_equal(assessment$RMSE,   2.6046, tol = 1e-3)
	expect_equal(assessment$bias,  -0.3580, tol = 1e-3)
	expect_equal(assessment$AIC ,  91.1979, tol = 1e-3)
	expect_equal(assessment$BIC ,  98.6638, tol = 1e-3)
	expect_equal(assessment$AW  ,  15.4962, tol = 1e-2)
	expect_equal(assessment$POC , 100.0000)
	expect_equal(assessment$CWI ,  14.0216, tol = 1e-2)

})

test_that("Test model-assessment on data set #3.3 with GPA/L-moments/S-bootstrap.", {

	# Load dataset and run L-moments estimation with uncertainty analysis
	df <- load_data("Application_3.3.csv")
	params <- lmom.estimation(df$max, "GPA")
	uncertainty <- sb.uncertainty(df$max, "GPA", "L-moments")
	assessment <- model.assessment(df$max, "GPA", params, uncertainty)

	# Test results against MATLAB
	expect_equal(assessment$R2  ,   0.9906, tol = 1e-3)
	expect_equal(assessment$RMSE,   1.8012, tol = 1e-3)
	expect_equal(assessment$bias,  -0.0907, tol = 1e-3)
	expect_equal(assessment$AIC ,  63.0783, tol = 1e-3)
	expect_equal(assessment$BIC ,  70.8024, tol = 1e-3)
	expect_equal(assessment$AW  ,  11.1448, tol = 1e-2)
	expect_equal(assessment$POC , 100.0000)
	expect_equal(assessment$CWI ,  10.0842, tol = 1e-2)

})
