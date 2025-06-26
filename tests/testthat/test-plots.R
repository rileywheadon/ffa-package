test_that("Test bbmk-plot.R on data set #2", {
	set.seed(1)

	# Run the BB-MK test
	df <- load_data("Application_2.csv")
	test <- bbmk.test(df$max, n_sim = 100)
	p <- bbmk.plot(test)

	# Basic tests
	expect_s3_class(p, "ggplot")

	# Regression test with vdiffr
  	# vdiffr::expect_doppelganger("bbmk-default", p) 

	# Display results
	# dev.new(width = 10, height = 8)
	# print(p)
	
})

test_that("Test mks-plot.R on data set #2", {

	df <- load_data("Application_2.csv")
	result <- mks.test(df$max, df$year)
	p <- mks.plot(df$max, df$year, result)

	# Basic tests
	expect_s3_class(p, "ggplot")

	# Regression test with vdiffr
  	# vdiffr::expect_doppelganger("mks-default", p) 

	# Display results
	# dev.new(width = 10, height = 8)
	# print(p)
	
})

test_that("Test pettitt-plot.R on data set #2", {

	df <- load_data("Application_2.csv")
	result <- pettitt.test(df$max, df$year)
	p <- pettitt.plot(df$max, df$year, result)

	# Basic tests
	expect_s3_class(p, "ggplot")

	# Regression test with vdiffr
  	# vdiffr::expect_doppelganger("pettitt-default", p) 

	# Display results
	# dev.new(width = 10, height = 8)
	# print(p)
	
})

test_that("Test runs-plot.R on data set #3.1", {

	df <- load_data("Application_3.1.csv")
	residuals <- sens.trend(df$max, df$year)$residuals
	result <- runs.test(residuals)
	p <- runs.plot(df$year, residuals, result, "sens-mean")

	# Basic tests
	expect_s3_class(p, "ggplot")

	# Regression test with vdiffr
  	# vdiffr::expect_doppelganger("runs-default", p) 
	
	# Display results
	# ggsave("runs-plot.png", plot = p, width = 10, height = 8)
	# dev.new(width = 10, height = 8)
	# print(p)

})

test_that("Test sens-plot.R on data set #3.1", {

	df <- load_data("Application_3.1.csv")
	result <- sens.trend(df$max, df$year)
	p <- sens.plot(df$max, df$year, result, "sens-mean")

	# Basic tests
	expect_s3_class(p, "ggplot")

	# Regression test with vdiffr
  	# vdiffr::expect_doppelganger("sens-default", p) 

	# Display results
	# ggsave("sens-plot.png", plot = p, width = 10, height = 8)
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
  	# vdiffr::expect_doppelganger("spearman-default", p) 
	
	# Display results
	# dev.new(width = 10, height = 8)
	# print(p)

})

test_that("Test lmom-plot.R on data set #1 with L-distance", {

	df <- load_data("Application_1.csv")
	results <- ld.selection(df$max)
	p <- lmom.plot(df$max, results)

	# Basic tests
	expect_s3_class(p, "ggplot")

	# Regression test with vdiffr
  	# vdiffr::expect_doppelganger("lmom-l-distance", p) 
	
	# Display results
	# ggsave("lmom-plot.png", plot = p, width = 10, height = 8)
	# dev.new(width = 10, height = 8)
	# print(p)

})


test_that("Test lmom-plot.R on data set #1 with L-kurtosis", {

	df <- load_data("Application_1.csv")
	results <- lk.selection(df$max)
	p <- lmom.plot(df$max, results)

	# Basic tests
	expect_s3_class(p, "ggplot")

	# Regression test with vdiffr
  	# vdiffr::expect_doppelganger("lmom-l-kurtosis", p) 
	
	# Display results
	# dev.new(width = 10, height = 8)
	# print(p)

})

test_that("Test lmom-plot.R on data set #1 with Z-statistic", {

	df <- load_data("Application_1.csv")
	results <- z.selection(df$max)
	p <- lmom.plot(df$max, results)

	# Basic tests
	expect_s3_class(p, "ggplot")

	# Regression test with vdiffr
  	# vdiffr::expect_doppelganger("lmom-z-statistic", p) 
	
	# Display results
	# dev.new(width = 10, height = 8)
	# print(p)

})

test_that("Test uncertainty-plot.R on data set #3.3", {
	set.seed(1)

	df <- load_data("Application_3.3.csv")
	results <- sb.uncertainty(df$max, df$year, "GEV", "L-moments")
	p <- uncertainty.plot("GEV", results)

	# Basic tests
	expect_s3_class(p, "ggplot")

	# Regression test with vdiffr
  	# vdiffr::expect_doppelganger("s-uncertainty", p) 

	# Display results
	# ggsave("plot-s-uncertainty.png", plot = p, width = 10, height = 8)
	# dev.new(width = 10, height = 8)
	# print(p)

})

test_that("Test uncertainty-plot.R on data set #3.3", {
	set.seed(1)

	df <- load_data("Application_3.3.csv")
	results <- sb.uncertainty(df$max, df$year, "GEV100", "MLE", n_sim = 1000)
	p <- uncertainty.plot("GEV100", results, "2017")

	# Basic tests
	expect_s3_class(p, "ggplot")

	# Regression test with vdiffr
  	# vdiffr::expect_doppelganger("ns-uncertainty", p) 

	# Display results
	# ggsave("plot-ns-uncertainty.png", plot = p, width = 10, height = 8)
	# dev.new(width = 10, height = 8)
	# print(p)

})

test_that("Test assessment-plot.R on data set #3.3", {

	df <- load_data("Application_3.3.csv")
	params <- mle.estimation(df$max, df$year, "GEV")$params
	uncertainty <- sb.uncertainty(df$max, df$year, "GEV", "MLE", n_sim = 1000)
	assessment <-model.assessment(df$max, df$year, "GEV", params, uncertainty)
	p <- assessment.plot(df$max, assessment)

	# Basic tests
	expect_s3_class(p, "ggplot")

	# Regression test with vdiffr
  	# vdiffr::expect_doppelganger("assessment-default", p) 
	
	# Display results
	# ggsave("plot-assessment.png", plot = p, width = 10, height = 8)
	# dev.new(width = 10, height = 8)
	# print(p)

})
