# Helper function to validate results
validate <- function(data, model, theta, expected) {
	prior <- c(6, 9)
	result <- generalized.likelihood(data, model, theta, prior)
	expect_equal(result, expected, tol = 1e-6)
}

test_that("Test gmle-estimation.R on data set #1", {

	# Load dataset and test the generalized likelihood function
	df <- load_data("Application_1.csv", clean = FALSE)

	validate(df, "GEV",  c(1607.2262, 656.1483, 0.1129), -709.8813)
	validate(df, "GEV100", c(1487.6338, 230.3059, 646.2991, 0.1135),-709.2092)
	validate(df, "GEV110", c(1505.3244, 194.4710, 673.0143, -57.3739, 0.1138),-709.0962)

})

test_that("Test gmle-estimation.R on data set #2", {

	# Load dataset and test the generalized likelihood function
	df <- load_data("Application_2.csv", clean = FALSE)

	validate(df, "GEV",  c(1315.6021, 553.3844, 0.1131), -618.1594)
	validate(df, "GEV100", c(1871.0704, -1001.6282, 498.7795, 0.1111),-606.0335)
	validate(df, "GEV110", c(1570.3348, -289.4988, 398.6958, 358.7930, 0.1137),-616.6930)

})

test_that("Test gmle-estimation.R on data set #3.1", {

	# Load dataset and test the generalized likelihood function
	df <- load_data("Application_3.1.csv", clean = FALSE)

	validate(df, "GEV",  c(181.5003, 46.8537, 0.1130), -471.1144)
	validate(df, "GEV100", c(201.0305, -37.4812, 45.5528, 0.1129),-467.7028)
	validate(df, "GEV110", c(204.3781, -44.1795, 50.7347, -10.9589, 0.1132),-467.3164)

})

test_that("Test gmle-estimation.R on data set #3.2", {

	# Load dataset and test the generalized likelihood function
	df <- load_data("Application_3.2.csv", clean = FALSE)

	validate(df, "GEV",  c(61.8782, 16.7216, 0.1125), -292.3070)
	validate(df, "GEV100", c(56.8414, 10.1595, 16.2665, 0.1131),-290.5041)
	validate(df, "GEV110", c(59.9994, 4.2448, 17.9675, -3.0089, 0.1136),-291.4294 )

})

test_that("Test gmle-estimation.R on data set #3.3", {

	# Load dataset and test the generalized likelihood function
	df <- load_data("Application_3.3.csv", clean = FALSE)

	validate(df, "GEV",  c(28.5406, 14.8172, 0.1132), -308.0260)
	validate(df, "GEV100", c(21.1740, 16.9244, 14.6350, 0.1096),-301.7242)
	validate(df, "GEV110", c(22.6165, 5.2098, 6.8001, 16.8263, 0.1100),-301.6069)

})
