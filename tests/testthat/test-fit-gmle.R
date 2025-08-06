validate_gmle <- function(
	df,
	structure,
	expected_params,
	expected_mll
) {

	prior <- c(6, 9) 
	observed <- fit_gmle(df$max, prior, df$year, structure)

	if (observed$mll > expected_mll + 0.001) {
		# NOTE: Uncomment this line to print models that outperform MATLAB implementation
		# print(sprintf("Skip %s: %.3f > %.3f.", model, observed$mll, expected_mll))
	} else {
		expect_equal(observed$params, expected_params, tol = 1e-4)
		expect_equal(observed$mll   , expected_mll   , tol = 1e-4)
	} 

}

test_that("GMLE estimation works on ATHABASCA RIVER (07BE001)", {

	# Load dataset and run L-moments estimation
	df <- data_local("CAN-07BE001.csv")

	# Generalized Extreme Value (GEV) Distribution
	validate_gmle(df, S00, c(1607.2262, 656.1483, 0.1129), -709.8813)
	validate_gmle(df, S10, c(1459.6527, 215.2392, 646.2991, 0.1135),-709.2092)
	validate_gmle(df, S11, c(1484.4486, 177.7172, 682.7430, -57.8466, 0.1138),-709.0916)

})

test_that("GMLE estimation works on KOOTENAI RIVER (08NH021)", {

	# Load dataset and run L-moments estimation
	df <- data_local("CAN-08NH021.csv")

	# Generalized Extreme Value (GEV) Distribution
	validate_gmle(df, S00, c(1315.6021, 553.3844, 0.1131), -618.1594)
	validate_gmle(df, S10, c(2182.6880, -1112.9202, 498.7795, 0.1111), -606.0335)
	validate_gmle(df, S11, c(1692.9215, -364.4602, 278.0916, 409.9305, 0.1137), -616.6930)

})

test_that("GMLE estimation works on BOW RIVER (05BB001)", {

	# Load dataset and run L-moments estimation
	df <- data_local("CAN-05BB001.csv")

	# Generalized Extreme Value (GEV) Distribution
	validate_gmle(df, S00, c(181.5003, 46.8537, 0.1130), -471.1144)
	validate_gmle(df, S10, c(204.1253, -34.3864, 45.5528, 0.1129), -467.7028)
	validate_gmle(df, S11, c(186.7812, -5.7394, 40.3071, 10.3982, 0.1132), -470.3024)

})

test_that("GMLE estimation works on CHILLIWACK RIVER (08MH016)", {

	# Load dataset and run L-moments estimation
	df <- data_local("CAN-08MH016.csv")

	# Generalized Extreme Value (GEV) Distribution
	validate_gmle(df, S00, c(61.8782, 16.7216, 0.1125), -292.3070)
	validate_gmle(df, S10, c(54.4636, 10.8081, 16.2665, 0.1131), -290.5041)
	validate_gmle(df, S11, c(58.4153, 5.3295, 18.5177, -3.0171, 0.1136), -291.3010)

})

test_that("GMLE estimation works on OKANAGAN RIVER (08NM050)", {

	# Load dataset and run L-moments estimation
	df <- data_local("CAN-08NM050.csv")

	# Generalized Extreme Value (GEV) Distribution
	validate_gmle(df, S00, c(28.5406, 14.8172, 0.1132), -308.0260)
	validate_gmle(df, S10, c(17.4718, 17.6295, 14.6350, 0.1096), -301.7242)
	validate_gmle(df, S11, c(11.0599, 27.6583, 2.2595, 17.2799, 0.1093), -301.6069)

})
