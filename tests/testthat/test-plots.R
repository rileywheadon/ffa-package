# Import vdiffr
library(vdiffr)

test_that("Test bbmk-plot.R on data set #2", {

	# Run the BB-MK test
	df <- load_data("Application_2.csv")
	test <- bbmk.test(df$max, n_sim = 100)
	p <- bbmk.plot(test)

	# Basic tests
	expect_s3_class(p, "ggplot")

	# Display results
	# dev.new(width = 10, height = 8)
	# print(p)
	
})

test_that("Test mks-plot.R on data set #2", {

	df <- load_data("Application_2.csv")
	test <- mks.test(df$max, df$year)
	p <- mks.plot(df, test)

	# Basic tests
	expect_s3_class(p, "ggplot")

	# Regression test with vdiffr
  	expect_doppelganger("mks-default", p) 

	# Display results
	# dev.new(width = 10, height = 8)
	# print(p)
	
})

test_that("Test pettitt-plot.R on data set #2", {

	df <- load_data("Application_2.csv")
	test <- pettitt.test(df$max, df$year)
	p <- pettitt.plot(df, test)

	# Basic tests
	expect_s3_class(p, "ggplot")

	# Regression test with vdiffr
  	expect_doppelganger("pettitt-default", p) 

	# Display results
	# dev.new(width = 10, height = 8)
	# print(p)
	
})

test_that("Test runs-plot.R on data set #3.1", {

	df <- load_data("Application_3.1.csv")
	residuals <- sens.trend(df$max, df$year)$residuals
	test <- runs.test(residuals)
	p <- runs.plot(df, test, "sens-mean")

	# Basic tests
	expect_s3_class(p, "ggplot")

	# Regression test with vdiffr
  	expect_doppelganger("runs-default", p) 
	
	# Display results
	# dev.new(width = 10, height = 8)
	# print(p)

})

test_that("Test sens-plot.R on data set #3.1", {

	df <- load_data("Application_3.1.csv")
	test <- sens.trend(df$max, df$year)
	p <- sens.plot(df, test, "sens-mean")

	# Basic tests
	expect_s3_class(p, "ggplot")

	# Regression test with vdiffr
  	expect_doppelganger("sens-default", p) 

	# Display results
	# dev.new(width = 10, height = 8)
	# print(p)

})

test_that("Test spearman-plot.R on data set #1", {

	df <- load_data("Application_1.csv")
	test <- spearman.test(df$max)
	p <- spearman.plot(df, test)

	# Basic tests
	expect_s3_class(p, "ggplot")

	# Regression test with vdiffr
  	expect_doppelganger("spearman-default", p) 
	
	# Display results
	# dev.new(width = 10, height = 8)
	# print(p)

})

