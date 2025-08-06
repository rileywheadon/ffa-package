validate_mle <- function(
	df,
	distribution,
	structure,
	expected_params,
	expected_mll
) {

	observed <- fit_mle(df$max, distribution, df$year, structure)

	if (observed$mll > expected_mll + 1e-4) {
		# NOTE: Uncomment this line to print models that outperform MATLAB implementation
		# print(sprintf("Skip %s: %.4f > %.4f.", model, observed$mll, expected_mll))
	} else {
		expect_equal(observed$params, expected_params, tol = 1e-3)
		expect_equal(observed$mll   , expected_mll   , tol = 1e-3)
	} 

}

test_that("fit-maximum-likelihood.R works on ATHABASCA RIVER (07BE001)", {
	set.seed(1)

	# Load dataset and run L-moments estimation
	df <- data_local("CAN-07BE001.csv")

	# Gumbel (GUM) Distribution
	validate_mle(df, "GUM", S00, c(1642.8856, 665.2585), -825.7803)
	validate_mle(df, "GUM", S10, c(1521.3786, 178.9614, 661.2450), -825.3975)
	validate_mle(df, "GUM", S11, c(1598.8194, 68.1177, 714.4430, -77.4023), -825.5232)

	# Normal (NOR) Distribution
	validate_mle(df, "NOR", S00, c(2039.1863, 913.5010), -840.0948)
	validate_mle(df, "NOR", S10, c(1899.2168, 204.6866, 911.2851), -839.8470)
	validate_mle(df, "NOR", S11, c(1894.9573, 210.9357, 882.7468, 41.6830), -839.8329)

	# Log-Normal (LNO) Distribution
	validate_mle(df, "LNO", S00, c(7.5285, 0.4323), -827.0937)
	validate_mle(df, "LNO", S10, c(7.4456, 0.1212, 0.4306), -826.7053)
	validate_mle(df, "LNO", S11, c(7.4528, 0.1107, 0.4795, -0.0724), -826.4169)

	# Generalized Extreme Value (GEV) Distribution
	validate_mle(df, "GEV", S00, c(1624.9889, 655.3234, 0.0510), -825.4411)
	validate_mle(df, "GEV", S10, c(1488.7790, 194.9992, 648.0107, 0.0616), -824.9450)
	validate_mle(df, "GEV", S11, c(1501.1714, 174.5196, 671.1342, -36.6405, 0.0657), -824.9003)

	# Generalized Logistic (GLO) Distribution
	validate_mle(df, "GLO", S00, c(1852.1263, 439.0156, -0.2317), -825.2516)
	validate_mle(df, "GLO", S10, c(1737.3926, 166.5912, 439.0925, -0.2413), -824.9231)
	validate_mle(df, "GLO", S11, c(1809.0403, 58.8137, 476.4375, -52.9755, -0.2483), -824.9879)

	# Generalized Normal (GNO) Distribution
	validate_mle(df, "GNO", S00, c(1885.3744, 797.8436, -0.3654), -826.2868)
	validate_mle(df, "GNO", S10, c(1707.6218, 246.3211, 792.1433, -0.3904), -825.5555)
	validate_mle(df, "GNO", S11, c(1907.2204, -41.4241, 925.0936, -187.2600, -0.4051), -825.7523)

	# Pearson Type III (PE3) Distribution
	validate_mle(df, "PE3", S00, c(2039.1863, 865.5583, 0.8802), -827.6846)
	validate_mle(df, "PE3", S10, c(1823.7045, 315.1131, 870.5665, 0.9708), -826.6893)
 	validate_mle(df, "PE3", S11, c(1913.0689, 185.6621, 971.2918, -138.4952, 1.0140), -826.4840)

	# Log-Pearson Type III (LP3) Distribution
	validate_mle(df, "LP3", S00, c(7.5285, 0.4318, -0.1298), -826.8326)
	validate_mle(df, "LP3", S10, c(7.4503, 0.1144, 0.4302, -0.1202), -826.4880)
	validate_mle(df, "LP3", S11, c(7.4515, 0.1126, 0.4712, -0.0607, -0.0947), -826.3017)

	# Weibull (WEI) Distribution
	validate_mle(df, "WEI", S00, c(274.1452, 1995.0369, 2.0333), -831.8793)
	validate_mle(df, "WEI", S10, c(113.9974, 567.4221, 1728.4917, 1.7550), -829.1212)
	validate_mle(df, "WEI", S11, c(82.4566, 658.1596, 1987.2914, -436.5257, 1.7092), -828.4383)

})

