# Helper function to validate results
validate <- function(df, gllfunc, params, expected) {
	prior <- c(6, 9)
	result <- gllfunc(df$max, params, prior, df$year)
	expect_equal(result, expected, tol = 1e-6)
}

test_that("Test gmle-estimation.R on data set #1", {

	# Load dataset and test the generalized likelihood function
	df <- load_data("Application_1.csv")
	validate(df, gllgev,  c(1607.2262, 656.1483, 0.1129), -709.8813)
	validate(df, gllgev100, c(1459.6527, 215.2392, 646.2991, 0.1135), -709.2092)
	validate(df, gllgev110, c(1484.4486, 177.7172, 682.7430, -57.8466, 0.1138), -709.0916)

})

test_that("Test gmle-estimation.R on data set #2", {

	# Load dataset and test the generalized likelihood function
	df <- load_data("Application_2.csv")
	validate(df, gllgev,  c(1315.6021, 553.3844, 0.1131), -618.1594)
	validate(df, gllgev100, c(2182.6880, -1112.9202, 498.7795, 0.1111), -606.0335)
	validate(df, gllgev110, c(1692.9215, -364.4602, 278.0916, 409.9305, 0.1137), -616.2448)

})

test_that("Test gmle-estimation.R on data set #3.1", {

	# Load dataset and test the generalized likelihood function
	df <- load_data("Application_3.1.csv")
	validate(df, gllgev,  c(181.5003, 46.8537, 0.1130), -471.1144)
	validate(df, gllgev100, c(204.1253, -34.3864, 45.5528, 0.1129), -467.7028)
	validate(df, gllgev110, c(186.7812, -5.7394, 40.3071, 10.3982, 0.1132), -470.3024)

})

test_that("Test gmle-estimation.R on data set #3.2", {

	# Load dataset and test the generalized likelihood function
	df <- load_data("Application_3.2.csv")
	validate(df, gllgev,  c(61.8782, 16.7216, 0.1125), -292.3070)
	validate(df, gllgev100, c(54.4636, 10.8081, 16.2665, 0.1131), -290.5041)
	validate(df, gllgev110, c(58.4153, 5.3295, 18.5177, -3.0171, 0.1136), -291.3010)

})

test_that("Test gmle-estimation.R on data set #3.3", {

	# Load dataset and test the generalized likelihood function
	df <- load_data("Application_3.3.csv")
	validate(df, gllgev,  c(28.5406, 14.8172, 0.1132), -308.0260)
	validate(df, gllgev100, c(17.4718, 17.6295, 14.6350, 0.1096), -301.7242)
	validate(df, gllgev110, c(11.0599, 27.6583, 2.2595, 17.2799, 0.1093), -291.4825)

})
