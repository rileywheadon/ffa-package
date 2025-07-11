# Helper function to validate llv functions
test_llv <- function(model, trend, params, expected) {

	# Load dataset and run L-moments estimation
	df <- load_data("Application_1.csv")

	result_fast <- loglik_fast(df$max, model, params, df$year, trend)

	result_method <- switch(
		model,
		"GUM" = loglik_gum(df$max, params, df$year, trend),
		"NOR" = loglik_nor(df$max, params, df$year, trend),
		"LNO" = loglik_lno(df$max, params, df$year, trend),
		"GEV" = loglik_gev(df$max, params, df$year, trend),
		"GLO" = loglik_glo(df$max, params, df$year, trend),
		"GNO" = loglik_gno(df$max, params, df$year, trend),
		"PE3" = loglik_pe3(df$max, params, df$year, trend),
		"LP3" = loglik_lp3(df$max, params, df$year, trend),
		"WEI" = loglik_wei(df$max, params, df$year, trend),
	)

	expect_equal(result_fast  , expected, tol = 1e-6)
	expect_equal(result_method, expected, tol = 1e-6)

}

test_that("Test llv functions on data set #1", {

	# Gumbel (GUM) Distribution
	test_llv("GUM", trend_00, c(1642.8856, 665.2585), -825.7803)
	test_llv("GUM", trend_10, c(1521.3786, 178.9614, 661.2450), -825.3975)
	test_llv("GUM", trend_11, c(1598.8194, 68.1177, 714.4430, -77.4023), -825.5232)

	# Normal (NOR) Distribution
	test_llv("NOR", trend_00, c(2039.1863, 913.5010), -840.0948)
	test_llv("NOR", trend_10, c(1899.2168, 204.6866, 911.2851), -839.8470)
	test_llv("NOR", trend_11, c(1894.9573, 210.9357, 882.7468, 41.6830), -839.8329)

	# Log-Normal (LNO) Distribution
	test_llv("LNO", trend_00, c(7.5285, 0.4323), -827.0937)
	test_llv("LNO", trend_10, c(7.4456, 0.1212, 0.4306), -826.7053)
	test_llv("LNO", trend_11, c(7.4528, 0.1107, 0.4795, -0.0724), -826.4169)

	# Generalized Extreme Value (GEV) Distribution
	test_llv("GEV", trend_00, c(1624.9889, 655.3234, 0.0510), -825.4411)
	test_llv("GEV", trend_10, c(1488.7790, 194.9992, 648.0107, 0.0616), -824.9450)
	test_llv("GEV", trend_11, c(1501.1714, 174.5196, 671.1342, -36.6405, 0.0657), -824.9003)

	# Generalized Logistic (GLO) Distribution
	test_llv("GLO", trend_00, c(1852.1263, 439.0156, -0.2317), -825.2516)
	test_llv("GLO", trend_10, c(1737.3926, 166.5912, 439.0925, -0.2413), -824.9231)
	test_llv("GLO", trend_11, c(1809.0403, 58.8137, 476.4375, -52.9755, -0.2483), -824.9879)

	# Generalized Normal (GNO) Distribution
	test_llv("GNO", trend_00, c(1885.3744, 797.8436, -0.3654), -826.2868)
	test_llv("GNO", trend_10, c(1707.6218, 246.3211, 792.1433, -0.3904), -825.5555)
	test_llv("GNO", trend_11, c(1907.2204, -41.4241, 925.0936, -187.2600, -0.4051), -825.7523)

	# Pearson Type III (PE3) Distribution
	test_llv("PE3", trend_00, c(2039.1863, 865.5583, 0.8802), -827.6846)
	test_llv("PE3", trend_10, c(1823.7045, 315.1131, 870.5665, 0.9708), -826.6893)
 	test_llv("PE3", trend_11, c(1914.1689, 183.0621, 972.2918, -139.4952, 1.0140), -826.4840)

	# Log-Pearson Type III (LP3) Distribution
	test_llv("LP3", trend_00, c(7.5285, 0.4318, -0.1298), -826.8326)
	test_llv("LP3", trend_10, c(7.4503, 0.1144, 0.4302, -0.1202), -826.4880)
	test_llv("LP3", trend_11, c(7.4515, 0.1126, 0.4712, -0.0607, -0.0947), -826.3017)

	# Weibull (WEI) Distribution
	test_llv("WEI", trend_00, c(274.1452, 1995.0369, 2.0333), -831.8793)
	test_llv("WEI", trend_10, c(113.9974, 567.4221, 1728.4917, 1.7550), -829.1212)
	test_llv("WEI", trend_11, c(82.4566, 658.1596, 1987.2914, -436.5257, 1.7092), -828.4383)

})
