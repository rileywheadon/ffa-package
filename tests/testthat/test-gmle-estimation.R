# NOTE: The R function optim() and the MATLAB function fminsearch() are not identical.
# For these tests, we require the R implementation to meet one of the following two criteria:
#  1. Produce a higher MLL than the MATLAB implementation.
#  2. Produce the same as the MLL as the MATLAB implementation and yield the same parameters.

# Helper function to check validate results
validate_results <- function(df, model, expected) {
	prior <- c(6, 9) 
	observed <- gmle.estimation(df, model, prior)
	if (observed$mll > expected$mll + 0.001) {
		print(sprintf("Skip %s: %.3f > %.3f.", model, observed$mll, expected$mll))
	} else {
		expect_equal(observed$params, expected$params, tol = 1e-4)
		expect_equal(observed$mll   , expected$mll   , tol = 1e-4)
	} 
}

test_that("Test gmle-estimation.R on data set #1", {

	# Load dataset and run L-moments estimation
	df <- load_data("Application_1.csv", clean = FALSE)

	# Generalized Extreme Value (GEV) Distribution
	validate_results(df, "GEV", list(
		 params = c(1607.2262, 656.1483, 0.1129), 
		 mll = -709.8813
	))

	validate_results(df, "GEV100", list(
		params = c(1487.6338, 230.3059, 646.2991, 0.1135),
		mll = -709.2092
	))

	# NOTE: R implementation gives same MLL but different parameters
	validate_results(df, "GEV110", list(
		params = c(1505.3244, 194.4710, 673.0143, -57.3739, 0.1138),
		mll = -709.0962
	))

})

test_that("Test gmle-estimation.R on data set #2", {

	# Load dataset and run L-moments estimation
	df <- load_data("Application_2.csv", clean = FALSE)

	# Generalized Extreme Value (GEV) Distribution
	validate_results(df, "GEV", list(
		 params = c(1315.6021, 553.3844, 0.1131), 
		 mll = -618.1594
	))

	validate_results(df, "GEV100", list(
		params = c(1871.0704, -1001.6282, 498.7795, 0.1111),
		mll = -606.0335
	))

	validate_results(df, "GEV110", list(
		params = c(1570.3348, -289.4988, 398.6958, 358.7930, 0.1137),
		mll = -616.6930
	))

})

test_that("Test gmle-estimation.R on data set #3.1", {

	# Load dataset and run L-moments estimation
	df <- load_data("Application_3.1.csv", clean = FALSE)

	# Generalized Extreme Value (GEV) Distribution
	validate_results(df, "GEV", list(
		 params = c(181.5003, 46.8537, 0.1130), 
		 mll = -471.1144
	))

	validate_results(df, "GEV100", list(
		params = c(201.0305, -37.4812, 45.5528, 0.1129),
		mll = -467.7028
	))

	validate_results(df, "GEV110", list(
		params = c(204.3781, -44.1795, 50.7347, -10.9589, 0.1132),
		mll = -467.3164
	))

})

test_that("Test gmle-estimation.R on data set #3.2", {

	# Load dataset and run L-moments estimation
	df <- load_data("Application_3.2.csv", clean = FALSE)

	# Generalized Extreme Value (GEV) Distribution
	validate_results(df, "GEV", list(
		 params = c(61.8782, 16.7216, 0.1125), 
		 mll = -292.3070
	))

	validate_results(df, "GEV100", list(
		params = c(56.8414, 10.1595, 16.2665, 0.1131),
		mll = -290.5041
	))

	validate_results(df, "GEV110", list(
		params = c(59.9994, 4.2448, 17.9675, -3.0089, 0.1136),
		mll = -291.4294 
	))

})

test_that("Test gmle-estimation.R on data set #3.3", {

	# Load dataset and run L-moments estimation
	df <- load_data("Application_3.3.csv", clean = FALSE)

	# Generalized Extreme Value (GEV) Distribution
	validate_results(df, "GEV", list(
		 params = c(28.5406, 14.8172, 0.1132), 
		 mll = -308.0260
	))

	validate_results(df, "GEV100", list(
		params = c(21.1740, 16.9244, 14.6350, 0.1096),
		mll = -301.7242
	))

	validate_results(df, "GEV110", list(
		params = c(22.6165, 5.2098, 6.8001, 16.8263, 0.1100),
		mll = -301.6069
	))

})

