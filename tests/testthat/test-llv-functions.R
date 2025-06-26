# Helper function to check warning messages
validate_errors <- function(n, llvfunc, ns) {
	data <- rep(1, 100) 
	params <- rep(1, n)
	years <- rep(1, 100)

	# Validate the data vector
	expect_error(llvfunc(c("A", "B"), params, years), regexp = "'data' .* numeric")
	expect_error(llvfunc(c(1, NA), params, years), regexp = "'data' .* NaN/NA")
	expect_error(llvfunc(c(1, -1), params, years), regexp = "'data' .* negative")

	# Validate the parameters vector
	expect_error(llvfunc(data, c("A", "B"), years), regexp = "'params' .* numeric")
	expect_error(llvfunc(data, c(1, NaN), years), regexp = "'params' .* NaN/NA")
	expect_error(llvfunc(data, rep(1, n+1), years), regexp = "'params' .* length")

	# Validate the years vector
	if (ns) {
		expect_error(llvfunc(data, params, rep("A", 100)), regexp = "'years' .* numeric")
		expect_error(llvfunc(data, params, c(1, NaN)), regexp = "'years' .* NaN/NA")
		expect_error(llvfunc(data, params, rep(1, 99)), regexp = "'years' .* length")
	}

}

test_that("Test validation for llvxxx helper function.", {
	validate_errors(2, llvgum, FALSE)
	validate_errors(3, llvgum10, TRUE)
	validate_errors(4, llvgum11, TRUE)
	validate_errors(3, llvgev, FALSE)
	validate_errors(4, llvgev100, TRUE)
	validate_errors(5, llvgev110, TRUE)
})

# Helper function to validate llv functions
validate_llv <- function(df, llvfunc, theta, expected) {
	result <- llvfunc(df$max, theta, df$year)
	expect_equal(result, expected, tol = 1e-6)
}

test_that("Test llv functions on data set #1", {

	# Load dataset and run L-moments estimation
	df <- load_data("Application_1.csv")

	# Gumbel (GUM) Distribution
	validate_llv(df, llvgum, c(1642.8856, 665.2585), -825.7803)
	validate_llv(df, llvgum10, c(1521.3786, 178.9614, 661.2450), -825.3975)
	validate_llv(df, llvgum11, c(1598.8194, 68.1177, 714.4430, -77.4023), -825.5232)

	# Normal (NOR) Distribution
	validate_llv(df, llvnor, c(2039.1863, 913.5010), -840.0948)
	validate_llv(df, llvnor10, c(1899.2168, 204.6866, 911.2851), -839.8470)
	validate_llv(df, llvnor11, c(1894.9573, 210.9357, 882.7468, 41.6830), -839.8329)

	# Log-Normal (LNO) Distribution
	validate_llv(df, llvlno, c(7.5285, 0.4323), -827.0937)
	validate_llv(df, llvlno10, c(7.4456, 0.1212, 0.4306), -826.7053)
	validate_llv(df, llvlno11, c(7.4528, 0.1107, 0.4795, -0.0724), -826.4169)

	# Generalized Extreme Value (GEV) Distribution
	validate_llv(df, llvgev, c(1624.9889, 655.3234, 0.0510), -825.4411)
	validate_llv(df, llvgev100, c(1488.7790, 194.9992, 648.0107, 0.0616), -824.9450)
	validate_llv(df, llvgev110, c(1501.1714, 174.5196, 671.1342, -36.6405, 0.0657), -824.9003)

	# Generalized Logistic (GLO) Distribution
	validate_llv(df, llvglo, c(1852.1263, 439.0156, -0.2317), -825.2516)
	validate_llv(df, llvglo100, c(1737.3926, 166.5912, 439.0925, -0.2413), -824.9231)
	validate_llv(df, llvglo110, c(1809.0403, 58.8137, 476.4375, -52.9755, -0.2483), -824.9879)

	# Generalized Normal (GNO) Distribution
	validate_llv(df, llvgno, c(1885.3744, 797.8436, -0.3654), -826.2868)
	validate_llv(df, llvgno100, c(1707.6218, 246.3211, 792.1433, -0.3904), -825.5555)
	validate_llv(df, llvgno110, c(1907.2204, -41.4241, 925.0936, -187.2600, -0.4051), -825.7523)

	# Pearson Type III (PE3) Distribution
	validate_llv(df, llvpe3, c(2039.1863, 865.5583, 0.8802), -827.6846)
	validate_llv(df, llvpe3100, c(1823.7045, 315.1131, 870.5665, 0.9708), -826.6893)
 	validate_llv(df, llvpe3110, c(1914.1689, 183.0621, 972.2918, -139.4952, 1.0140), -826.4840)

	# Log-Pearson Type III (LP3) Distribution
	validate_llv(df, llvlp3, c(7.5285, 0.4318, -0.1298), -826.8326)
	validate_llv(df, llvlp3100, c(7.4503, 0.1144, 0.4302, -0.1202), -826.4880)
	validate_llv(df, llvlp3110, c(7.4515, 0.1126, 0.4712, -0.0607, -0.0947), -826.3017)

	# Weibull (WEI) Distribution
	validate_llv(df, llvwei, c(274.1452, 1995.0369, 2.0333), -831.8793)
	validate_llv(df, llvwei100, c(113.9974, 567.4221, 1728.4917, 1.7550), -829.1212)
	validate_llv(df, llvwei110, c(82.4566, 658.1596, 1987.2914, -436.5257, 1.7092), -828.4383)

})
