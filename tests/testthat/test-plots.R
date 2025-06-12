test_that("Test bbmk-plot.R on data set #2", {
	set.seed(1)

	# Run the BB-MK test
	df <- load_data("Application_2.csv")
	test <- bbmk.test(df$max, n_sim = 100)
	p <- bbmk.plot(test)

	# Basic tests
	expect_s3_class(p, "ggplot")

	# Regression test with vdiffr
  	vdiffr::expect_doppelganger("bbmk-default", p) 

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
  	vdiffr::expect_doppelganger("mks-default", p) 

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
  	vdiffr::expect_doppelganger("pettitt-default", p) 

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
  	vdiffr::expect_doppelganger("runs-default", p) 
	
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
  	vdiffr::expect_doppelganger("sens-default", p) 

	# Display results
	# dev.new(width = 10, height = 8)
	# print(p)

})

test_that("Test spearman-plot.R on data set #1", {

	df <- load_data("Application_1.csv")
	test <- spearman.test(df$max)
	p <- spearman.plot(test)

	# Basic tests
	expect_s3_class(p, "ggplot")

	# Regression test with vdiffr
  	vdiffr::expect_doppelganger("spearman-default", p) 
	
	# Display results
	# dev.new(width = 10, height = 8)
	# print(p)

})

test_that("Test lmom-plot.R on data set #1 with L-distance", {

	df <- load_data("Application_1.csv")
	results <- ld.selection(df$max)
	p <- lmom.plot(df$max, "L-distance", results)

	# Basic tests
	expect_s3_class(p, "ggplot")

	# Regression test with vdiffr
  	vdiffr::expect_doppelganger("lmom-l-distance", p) 
	
	# Display results
	# dev.new(width = 10, height = 8)
	# print(p)

})


test_that("Test lmom-plot.R on data set #1 with L-kurtosis", {

	df <- load_data("Application_1.csv")
	results <- lk.selection(df$max)
	p <- lmom.plot(df$max, "L-kurtosis", results)

	# Basic tests
	expect_s3_class(p, "ggplot")

	# Regression test with vdiffr
  	vdiffr::expect_doppelganger("lmom-l-kurtosis", p) 
	
	# Display results
	# dev.new(width = 10, height = 8)
	# print(p)

})

test_that("Test lmom-plot.R on data set #1 with Z-statistic", {

	df <- load_data("Application_1.csv")
	results <- z.selection(df$max)
	p <- lmom.plot(df$max, "Z-statistic", results)

	# Basic tests
	expect_s3_class(p, "ggplot")

	# Regression test with vdiffr
  	vdiffr::expect_doppelganger("lmom-z-statistic", p) 
	
	# Display results
	# dev.new(width = 10, height = 8)
	# print(p)

})

test_that("Test uncertainty-plot.R on data set #1", {

	df <- load_data("Application_1.csv")
	results <- sb.uncertainty(df$max, "GEV", "L-moments")
	p <- uncertainty.plot(results)

	# Basic tests
	expect_s3_class(p, "ggplot")

	# Display results
	# dev.new(width = 10, height = 8)
	# print(p)

})

test_that("Test assessment-plot.R on data set #1", {

	df <- load_data("Application_1.csv")
	params <- lmom.estimation(df$max, "GEV")
	uncertainty <- sb.uncertainty(df$max, "GEV", "L-moments")
	assessment <-model.assessment(df$max, "GEV", params, uncertainty)
	p <- assessment.plot(df$max, assessment)

	# Basic tests
	expect_s3_class(p, "ggplot")

	# Regression test with vdiffr
  	vdiffr::expect_doppelganger("assessment-default", p) 
	
	# Display results
	# dev.new(width = 10, height = 8)
	# print(p)

})
