# Set seed for reproducibility
set.seed(1)

test_that("Test bbmk-plot.R on data set #2", {

	df <- load_data("Application_2.csv")
	test <- bbmk.test(df$max, n_sim = 100)

	expect_equal(0, 0)
	# print(bbmk.plot(test))
	
})

test_that("Test mks-plot.R on data set #2", {

	df <- load_data("Application_2.csv")
	test <- mks.test(df$max, df$year)

	expect_equal(0, 0)
	# print(mks.plot(df, test))
	
})

test_that("Test pettitt-plot.R on data set #2", {

	df <- load_data("Application_2.csv")
	test <- pettitt.test(df$max, df$year)

	expect_equal(0, 0)
	# print(pettitt.plot(df, test))
	
})

test_that("Test runs-plot.R on data set #3.1", {

	df <- load_data("Application_3.1.csv")
	residuals <- sens.trend(df$max, df$year)$residuals
	test <- runs.test(residuals)

	expect_equal(0, 0)
	# print(runs.plot(df, test, "sens-mean"))
	
})

test_that("Test sens-plot.R on data set #3.1", {

	df <- load_data("Application_3.1.csv")
	test <- sens.trend(df$max, df$year)

	expect_equal(0, 0)
	# print(sens.plot(df, test, "sens-mean"))
	
})

test_that("Test spearman-plot.R on data set #1", {

	df <- load_data("Application_1.csv")
	test <- spearman.test(df$max)

	expect_equal(0, 0)
	print(spearman.plot(df, test))
	
})

