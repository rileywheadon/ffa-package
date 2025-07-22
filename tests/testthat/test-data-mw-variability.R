test_that("data-mw-variability.R works on ATHABASCA RIVER (07BE001)", {

	df <- data_local("CAN-07BE001.csv")
	result <- data_mw_variability(df$max, df$year)

	# Year results from MATLAB
	years <- c(
		1917.5, 1922.5, 1927.0, 1929.5, 1940.0, 1942.5, 1947.5, 1952.5,
		1957.5, 1962.5, 1967.5, 1972.5, 1977.5, 1982.5, 1987.5, 1992.5,
		1997.5, 2002.5, 2007.5, 2012.5
	)

	# STD results from MATLAB
	stds <- c(
		745.17, 492.82, 590.70, 814.67, 368.35, 1091.6, 1194.1, 1331.4,
		1256.4, 716.99, 1111.8, 992.56, 875.70, 1173.9, 963.86, 678.47,
		652.97, 486.12, 912.04, 957.87
	)

	# Check the years and stds
	expect_equal(result$year, years)
	expect_equal(result$std, stds, tol = 1e-4)

})

test_that("data-mw-variability.R works on KOOTENAI RIVER (08NH021)", {

	df <- data_local("CAN-08NH021.csv")
	result <- data_mw_variability(df$max, df$year)

	# Year results from MATLAB
	years <- c(
		1932.50, 1937.50, 1942.50, 1947.50, 1952.50, 1957.50, 1962.50, 1967.50,
		1972.50, 1977.50, 1982.50, 1987.50, 1992.50, 1997.50, 2002.50, 2007.50,
		2012.50
	)

	# STD results from MATLAB
	stds <- c(
		422.89, 549.06, 595.90, 675.96, 462.98, 478.89, 462.09, 388.56,
		668.47, 283.56, 190.63, 230.46, 379.75, 404.40, 363.59, 344.20,
		213.65
	)

	# Check the years and stds
	expect_equal(result$year, years)
	expect_equal(result$std, stds, tol = 1e-4)

})

test_that("data-mw-variability.R works on BOW RIVER (05BB001)", {

	df <- data_local("CAN-05BB001.csv")
	result <- data_mw_variability(df$max, df$year)

	# Year results from MATLAB
	years <- c(
		1913.50, 1918.50, 1923.50, 1928.50, 1933.50, 1938.50, 1943.50, 1948.50,
		1953.50, 1958.50, 1963.50, 1968.50, 1973.50, 1978.50, 1983.50, 1988.50,
		1993.50, 1998.50, 2003.50, 2008.50, 2013.11
	)

	# STD results from MATLAB
	stds <- c(
		58.27, 72.99, 70.64,  57.65,  49.07, 40.88, 48.46, 64.90,
		59.98, 44.71, 41.28,  48.76,  57.68, 49.83, 52.74, 60.39,
		52.99, 43.24, 45.09, 108.31, 115.26
	)

	# Check the years and stds
	expect_equal(result$year, years, tol = 1e-4)
	expect_equal(result$std, stds, tol = 1e-4)

})

test_that("data-mw-variability.R works on data set CHILLIWACK RIVER (08MH016)", {

	df <- data_local("CAN-08MH016.csv")
	result <- data_mw_variability(df$max, df$year)

	# Year results from MATLAB
	years <- c(
		1926.50, 1931.50, 1936.50, 1941.50, 1945.50, 1950.00, 1958.50, 1961.50,
		1966.50, 1971.50, 1976.50, 1981.50, 1986.50, 1991.50, 1996.50, 2001.50,
		2006.50, 2011.50
	)

	# STD results from MATLAB
	stds <- c(
		15.7654, 15.6795, 13.1608, 10.8396, 21.0535, 15.9289, 12.9671, 14.9961,
		17.8942, 22.9025, 25.9292, 18.0056, 22.8423, 31.2233, 26.9638, 35.2292,
		34.0462, 17.7105
	)

	# Check the years and stds
	expect_equal(result$year, years)
	expect_equal(result$std, stds, tol = 1e-4)

})

test_that("data-mw-variability.R works on OKANAGAN RIVER (08NM050)", {

	df <- data_local("CAN-08NM050.csv")
	result <- data_mw_variability(df$max, df$year)

	# Year results from MATLAB
	years <- c(
		1925.50, 1930.50, 1935.50, 1940.50, 1945.50, 1950.50, 1955.50, 1960.50,
		1965.50, 1970.50, 1975.50, 1980.50, 1985.50, 1990.50, 1995.50, 2000.50,
		2005.50, 2010.50
	)

	# STD results from MATLAB
	stds <- c(
		17.4264, 17.0059,  6.5193,  8.3191,  8.1178,  8.2841, 12.1823, 15.7169,
		12.5833, 13.6253, 19.8198, 19.4573, 18.6618, 20.0839, 19.9967, 20.2631,
		18.3351, 18.8305
	)

	# Check the years and stds
	expect_equal(result$year, years)
	expect_equal(result$std, stds, tol = 1e-4)

})

test_that("data-mw-variability.R throws an error on small dataframes.", {
	df <- data_local("CAN-07BE001.csv")
	expect_error(data_mw_variability(df$max[1:5], df$year[1:5]))
})
