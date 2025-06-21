test_that("Test model-assessment on data set #1 with GEV/L-moments/S-bootstrap.", {

	# Use identical parameters and uncertainty results to the MATLAB version 
	params <- c(1600.219872, 616.666030, 0.120747)
	uncertainty <- list("1900" = list(
		t = c(2, 5, 10, 20, 50, 100),
		ci_lower = c(1678.8540, 2361.9491, 2821.1799, 3246.2090, 3764.1510, 4134.4393),
		ci_upper = c(1997.5810, 2883.5746, 3602.7005, 4449.9595, 5840.9811, 7144.6336) 
	))

	# Load dataset and run L-moments estimation with uncertainty analysis
	df <- load_data("Application_1.csv")
	assessment <- model.assessment(df$max, df$year, "GEV", params, uncertainty)

	# Test results against MATLAB
	expect_equal(assessment$R2  ,   0.9922, tol = 1e-4)
	expect_equal(assessment$RMSE,  95.9225, tol = 1e-4)
	expect_equal(assessment$bias, -20.6072, tol = 1e-4)
	expect_equal(assessment$AIC , 471.4811, tol = 1e-4)
	expect_equal(assessment$BIC , 479.3561, tol = 1e-4)
	expect_equal(assessment$AIC_MLL, 1656.8821, tol = 1e-4)
	expect_equal(assessment$BIC_MLL, 1664.7570, tol = 1e-4)
	expect_equal(assessment$AW  , 603.0077, tol = 1e-4)
	expect_equal(assessment$POC , 100.0000, tol = 1e-4)
	expect_equal(assessment$CWI , 545.6239, tol = 1e-4)

})

test_that("Test model-assessment on data set #2 with WEI/L-moments/S-bootstrap.", {

	# Use identical parameters and uncertainty results to the MATLAB version 
	params <- c(327.675316, 1520.238106, 1.988484)
	uncertainty <- list("1900" = list(
		t = c(2, 5, 10, 20, 50, 100),
		ci_lower = c(1430.3752, 2058.0287, 2391.4038, 2656.8127, 2944.4725, 3130.8841),
		ci_upper = c(1759.9254, 2462.2588, 2895.8749, 3295.6570, 3790.4480, 4141.0544) 
	))

	# Load dataset and run L-moments estimation with uncertainty analysis
	df <- load_data("Application_2.csv")
	assessment <- model.assessment(df$max, df$year, "WEI", params, uncertainty)

	# Test results against MATLAB
	expect_equal(assessment$R2  ,   0.9862, tol = 1e-4)
	expect_equal(assessment$RMSE,  81.8381, tol = 1e-4)
	expect_equal(assessment$bias,  -6.6950, tol = 1e-4)
	expect_equal(assessment$AIC , 406.8316, tol = 1e-4)
	expect_equal(assessment$BIC , 414.3641, tol = 1e-4)
	expect_equal(assessment$AIC_MLL, 1442.7845, tol = 1e-4)
	expect_equal(assessment$BIC_MLL, 1450.3171, tol = 1e-4)
	expect_equal(assessment$AW  , 440.2948, tol = 1e-4)
	expect_equal(assessment$POC ,  97.7778, tol = 1e-4)
	expect_equal(assessment$CWI , 416.5010, tol = 1e-4)

})

# NOTE: Test case on dataset #3.1 with PE3 has been removed, see matlab.md

test_that("Test model-assessment on data set #3.2 with GNO/L-moments/S-bootstrap.", {

	# Use identical parameters and uncertainty results to the MATLAB version 
	params <- c(68.735078, 20.032161, -0.378291)
	uncertainty <- list("1900" = list(
		t = c(2, 5, 10, 20, 50, 100),
		ci_lower = c(64.2910, 82.0855,  92.8011, 102.0788, 113.0530, 120.7327),
		ci_upper = c(73.4937, 95.4063, 111.4363, 128.3836, 152.6660, 172.5685) 
	))

	# Load dataset and run L-moments estimation with uncertainty analysis
	df <- load_data("Application_3.2.csv")
	assessment <- model.assessment(df$max, df$year, "GNO", params, uncertainty)

	# Test results against MATLAB
	expect_equal(assessment$R2  ,   0.9884, tol = 1e-4)
	expect_equal(assessment$RMSE,   2.6046, tol = 1e-4)
	expect_equal(assessment$bias,  -0.3580, tol = 1e-4)
	expect_equal(assessment$AIC ,  91.1979, tol = 1e-4)
	expect_equal(assessment$BIC ,  98.6638, tol = 1e-4)
	expect_equal(assessment$AIC_MLL, 791.5803, tol = 1e-4)
	expect_equal(assessment$BIC_MLL, 799.0462, tol = 1e-4)
	expect_equal(assessment$AW  ,  15.4410, tol = 1e-4)
	expect_equal(assessment$POC , 100.0000, tol = 1e-4)
	expect_equal(assessment$CWI ,  13.9716, tol = 1e-4)

})

test_that("Test model-assessment on data set #3.3 with LP3/L-moments/S-bootstrap.", {

	# Use identical parameters and uncertainty results to the MATLAB version 
	params <- c(3.51100, 0.55044, -0.54165)
	uncertainty <- list("1900" = list(
		t = c(2, 5, 10, 20, 50, 100),
		ci_lower = c(31.2190, 47.9695, 57.7789, 65.6528, 73.6143, 78.2450),
		ci_upper = c(39.5256, 59.6762, 73.0359, 86.4724, 105.1122, 120.1799) 
	))

	# Load dataset and run L-moments estimation with uncertainty analysis
	df <- load_data("Application_3.3.csv")
	assessment <- model.assessment(df$max, df$year, "LP3", params, uncertainty)

	# Test results against MATLAB. 
	expect_equal(assessment$R2  ,   0.9669, tol = 1e-2)
	expect_equal(assessment$RMSE,   3.4106, tol = 1e-2)
	expect_equal(assessment$bias,  -0.0061, tol = 1e-2)
	expect_equal(assessment$AIC , 125.0092, tol = 1e-2)
	expect_equal(assessment$BIC , 132.7334, tol = 1e-2)
	expect_equal(assessment$AIC_MLL, 833.1819, tol = 1e-4)
	expect_equal(assessment$BIC_MLL, 840.9061, tol = 1e-4)
	expect_equal(assessment$AW  ,  13.1347, tol = 1e-4)
	expect_equal(assessment$POC ,  85.4167, tol = 1e-4)
	expect_equal(assessment$CWI ,  15.9096, tol = 1e-4)

})

test_that("Test model-assessment on data set #1 with GEV110/MLE/S-bootstrap.", {
	set.seed(1)

	# Load dataset and run L-moments estimation with uncertainty analysis
	df <- load_data("Application_1.csv")
	params <- mle.estimation(df$max, "GEV110", df$year)$params
	uncertainty <- sb.uncertainty(df$max, df$year, "GEV100", "MLE", "all", 1000)
	assessment <- model.assessment(df$max, df$year, "GEV100", params, uncertainty)
	print(assessment)

	# Test results against MATLAB (TBD)
	expect_equal(assessment$R2  ,   0.9891, tol = 1e-4)
	expect_equal(assessment$RMSE,  97.0161, tol = 1e-4)
	expect_equal(assessment$bias,   5.5685, tol = 1e-4)
	expect_equal(assessment$AIC , 474.6374, tol = 1e-4)
	expect_equal(assessment$BIC , 485.1373, tol = 1e-4)
	expect_equal(assessment$AIC_MLL, 1657.8901, tol = 1e-4)
	expect_equal(assessment$BIC_MLL, 1668.3900, tol = 1e-4)
	expect_equal(assessment$AW  , 748.5596, tol = 1e-2)
	expect_equal(assessment$POC , 100     , tol = 1e-2)
	expect_equal(assessment$CWI , 677.3248, tol = 1e-2)

})
