test_that("Test mw-variance.R on data set #1", {

	df <- load_data("Application_1.csv", clean = FALSE)
	result <- mw.variance(df$max, df$year)

	# Year results from MATLAB
	years <- c(
		1917.5,
		1922.5,
		1927.5,
		1932.5,
		1937.5,
		1942.5,
		1947.5,
		1952.5,
		1957.5,
		1962.5,
		1967.5,
		1972.5,
		1977.5,
		1982.5,
		1987.5,
		1992.5,
		1997.5,
		2002.5,
		2007.5,
		2012.5
	)

	# STD results from MATLAB
	stds <- c(
		745.17,
		492.82,
		590.70,
		814.67,
		368.35,
		1091.6,
		1194.1,
		1331.4,
		1256.4,
		716.99,
		1111.8,
		992.56,
		875.70,
		1173.9,
		963.86,
		678.47,
		652.97,
		486.12,
		912.04,
		957.87
	)

	# Check the years and stds
	expect_equal(result$year, years)
	expect_equal(result$std, stds, tol = 1e-4)

})

test_that("mw-variance.R throws an error on small dataframes.", {

	df <- load_data("Application_1.csv", clean = FALSE)
	expect_error(mw.variance(df[1:5, ]))

})
