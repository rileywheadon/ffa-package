# NOTE: The R function optim() and the MATLAB function fminsearch() are not identical.
# For these tests, we require the R implementation either produces a higher MLL (a
# better fit) than the MATLAB implementation OR produce the same MLL and parameters
# (the same fit) as the MATLAB implementation.

validate_mle <- function(df, model, trend, expected_params, expected_mll) {

	observed <- fit_maximum_likelihood(df$max, model, NULL, df$year, trend)

	# Skip the test if we improved over the MATLAB results 
	if (observed$mll > expected_mll + 1e-4) {
		# print(sprintf("Skip %s: %.4f > %.4f.", model, observed$mll, expected_mll))
	} else {
		expect_equal(observed$params, expected_params, tol = 1e-3)
		expect_equal(observed$mll   , expected_mll   , tol = 1e-3)
	} 

}

test_that("Test fit-maximum-likelihood.R on data set #1", {
	set.seed(1)

	# Load dataset and run L-moments estimation
	df <- load_data("Application_1.csv")

	# Gumbel (GUM) Distribution
	validate_mle(df, "GUM", trend_00, c(1642.8856, 665.2585), -825.7803)
	validate_mle(df, "GUM", trend_10, c(1521.3786, 178.9614, 661.2450), -825.3975)
	validate_mle(df, "GUM", trend_11, c(1598.8194, 68.1177, 714.4430, -77.4023), -825.5232)

	# Normal (NOR) Distribution
	validate_mle(df, "NOR", trend_00, c(2039.1863, 913.5010), -840.0948)
	validate_mle(df, "NOR", trend_10, c(1899.2168, 204.6866, 911.2851), -839.8470)
	validate_mle(df, "NOR", trend_11, c(1894.9573, 210.9357, 882.7468, 41.6830), -839.8329)

	# Log-Normal (LNO) Distribution
	validate_mle(df, "LNO", trend_00, c(7.5285, 0.4323), -827.0937)
	validate_mle(df, "LNO", trend_10, c(7.4456, 0.1212, 0.4306), -826.7053)
	validate_mle(df, "LNO", trend_11, c(7.4528, 0.1107, 0.4795, -0.0724), -826.4169)

	# Generalized Extreme Value (GEV) Distribution
	validate_mle(df, "GEV", trend_00, c(1624.9889, 655.3234, 0.0510), -825.4411)
	validate_mle(df, "GEV", trend_10, c(1488.7790, 194.9992, 648.0107, 0.0616), -824.9450)
	validate_mle(df, "GEV", trend_11, c(1501.1714, 174.5196, 671.1342, -36.6405, 0.0657), -824.9003)

	# Generalized Logistic (GLO) Distribution
	validate_mle(df, "GLO", trend_00, c(1852.1263, 439.0156, -0.2317), -825.2516)
	validate_mle(df, "GLO", trend_10, c(1737.3926, 166.5912, 439.0925, -0.2413), -824.9231)
	validate_mle(df, "GLO", trend_11, c(1809.0403, 58.8137, 476.4375, -52.9755, -0.2483), -824.9879)

	# Generalized Normal (GNO) Distribution
	validate_mle(df, "GNO", trend_00, c(1885.3744, 797.8436, -0.3654), -826.2868)
	validate_mle(df, "GNO", trend_10, c(1707.6218, 246.3211, 792.1433, -0.3904), -825.5555)
	validate_mle(df, "GNO", trend_11, c(1907.2204, -41.4241, 925.0936, -187.2600, -0.4051), -825.7523)

	# Pearson Type III (PE3) Distribution
	validate_mle(df, "PE3", trend_00, c(2039.1863, 865.5583, 0.8802), -827.6846)
	validate_mle(df, "PE3", trend_10, c(1823.7045, 315.1131, 870.5665, 0.9708), -826.6893)
 	validate_mle(df, "PE3", trend_11, c(1913.0689, 185.6621, 971.2918, -138.4952, 1.0140), -826.4840)

	# Log-Pearson Type III (LP3) Distribution
	validate_mle(df, "LP3", trend_00, c(7.5285, 0.4318, -0.1298), -826.8326)
	validate_mle(df, "LP3", trend_10, c(7.4503, 0.1144, 0.4302, -0.1202), -826.4880)
	validate_mle(df, "LP3", trend_11, c(7.4515, 0.1126, 0.4712, -0.0607, -0.0947), -826.3017)

	# Weibull (WEI) Distribution
	validate_mle(df, "WEI", trend_00, c(274.1452, 1995.0369, 2.0333), -831.8793)
	validate_mle(df, "WEI", trend_10, c(113.9974, 567.4221, 1728.4917, 1.7550), -829.1212)
	validate_mle(df, "WEI", trend_11, c(82.4566, 658.1596, 1987.2914, -436.5257, 1.7092), -828.4383)

})

# Helper function to validate GMLE
validate_gmle <- function(df, model, trend, expected_params, expected_mll) {

	prior <- c(6, 9) 
	observed <- fit_maximum_likelihood(df$max, model, prior, df$year, trend)

	if (observed$mll > expected_mll + 0.001) {
		# print(sprintf("Skip %s: %.3f > %.3f.", model, observed$mll, expected_mll))
	} else {
		expect_equal(observed$params, expected_params, tol = 1e-4)
		expect_equal(observed$mll   , expected_mll   , tol = 1e-4)
	} 

}

test_that("Test GMLE estimation on data set #1", {

	# Load dataset and run L-moments estimation
	df <- load_data("Application_1.csv")

	# Generalized Extreme Value (GEV) Distribution
	validate_gmle(df, "GEV", trend_00, c(1607.2262, 656.1483, 0.1129), -709.8813)
	validate_gmle(df, "GEV", trend_10, c(1459.6527, 215.2392, 646.2991, 0.1135),-709.2092)
	validate_gmle(df, "GEV", trend_11, c(1484.4486, 177.7172, 682.7430, -57.8466, 0.1138),-709.0916)

})

test_that("Test GMLE estimation on data set #2", {

	# Load dataset and run L-moments estimation
	df <- load_data("Application_2.csv")

	# Generalized Extreme Value (GEV) Distribution
	validate_gmle(df, "GEV", trend_00, c(1315.6021, 553.3844, 0.1131), -618.1594)
	validate_gmle(df, "GEV", trend_10, c(2182.6880, -1112.9202, 498.7795, 0.1111), -606.0335)
	validate_gmle(df, "GEV", trend_11, c(1692.9215, -364.4602, 278.0916, 409.9305, 0.1137), -616.6930)

})

test_that("Test GMLE estimation on data set #3.1", {

	# Load dataset and run L-moments estimation
	df <- load_data("Application_3.1.csv")

	# Generalized Extreme Value (GEV) Distribution
	validate_gmle(df, "GEV", trend_00, c(181.5003, 46.8537, 0.1130), -471.1144)
	validate_gmle(df, "GEV", trend_10, c(204.1253, -34.3864, 45.5528, 0.1129), -467.7028)
	validate_gmle(df, "GEV", trend_11, c(186.7812, -5.7394, 40.3071, 10.3982, 0.1132), -470.3024)

})

test_that("Test GMLE estimation on data set #3.2", {

	# Load dataset and run L-moments estimation
	df <- load_data("Application_3.2.csv")

	# Generalized Extreme Value (GEV) Distribution
	validate_gmle(df, "GEV", trend_00, c(61.8782, 16.7216, 0.1125), -292.3070)
	validate_gmle(df, "GEV", trend_10, c(54.4636, 10.8081, 16.2665, 0.1131), -290.5041)
	validate_gmle(df, "GEV", trend_11, c(58.4153, 5.3295, 18.5177, -3.0171, 0.1136), -291.3010)

})

test_that("Test GMLE estimation on data set #3.3", {

	# Load dataset and run L-moments estimation
	df <- load_data("Application_3.3.csv")

	# Generalized Extreme Value (GEV) Distribution
	validate_gmle(df, "GEV", trend_00, c(28.5406, 14.8172, 0.1132), -308.0260)
	validate_gmle(df, "GEV", trend_10, c(17.4718, 17.6295, 14.6350, 0.1096), -301.7242)
	validate_gmle(df, "GEV", trend_11, c(11.0599, 27.6583, 2.2595, 17.2799, 0.1093), -301.6069)

})

