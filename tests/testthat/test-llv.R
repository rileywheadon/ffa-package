# Helper function to check warning messages
validate_warning <- function(expr, msg) {
	expect_warning(expr, regexp = msg)
	expect_equal(expr, -Inf)
}

test_that("Test validation for llvxxx helper function.", {

	# Proper handling of non-numeric data
	validate_warning(
		llvgum(c("A", "B", "C"), c(0, 1)),
		"Warning: 'data' is not a numeric vector."
	)

	# Proper handling of missing values
	validate_warning(
		llvgum(c(3, 1, NaN), c(0, 1)),
		"Warning: 'data' contains NaN or NA values."
	)

	validate_warning(
		llvgum(c(5, NA, 2), c(0, 1)),
		"Warning: 'data' contains NaN or NA values."
	)

	# Proper handling of negative values
	validate_warning(
		llvgum(c(-1, 4, 2), c(0, 1)),
		"Warning: 'data' contains negative values."
	)

	# Proper handling of non-numeric parameters
	validate_warning(
		llvgum(c(3, 4, 2), c("A", "B")),
		"Warning: 'params' is not a numeric vector."
	)

	# Proper handling of parameter vectors of invalid length
	validate_warning(
		llvgum(c(3, 4, 2), c(0, 1, 0)),
		"Warning: 'params' for model 'GUM' must have length 2."
	)

	# Proper handling of undefined parameters
	expect_equal(llvgum(c(3, 4, 2), c(0, NaN)), -Inf)

	# Proper handling of undefined years
	validate_warning(
		llvgum10(c(3, 4, 2), c(0, 1, 1), c(NaN, 2, 3)),
		"Warning: 'years' contains NaN or NA values."
	)

	# Proper handling of years with incorrect length
	validate_warning(
		llvgum10(c(3, 4, 2), c(0, 1, 1), c(1, 2, 3, 4)),
		"Warning: 'years' and 'data' have different lengths."
	)

	# Proper handling of negative sclae parameter
	expect_equal(llvgum(c(3, 4, 2), c(0, -1)), -Inf)

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
