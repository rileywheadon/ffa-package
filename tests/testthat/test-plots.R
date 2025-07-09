test_that("plot-bbmk-test.R on data set #2", {
	set.seed(1)

	# Run the BB-MK test
	df <- load_data("Application_2.csv")
	results <- eda_bbmk_test(df$max, samples = 100L)
	p <- plot_bbmk_test(results)

	# Basic tests
	expect_s3_class(p, "ggplot")

	# Regression test with vdiffr
  	vdiffr::expect_doppelganger("bbmk-default", p) 

})

test_that("plot-bbmk-test.R with custom arguments", {
	set.seed(1)

	# Run the BB-MK test
	df <- load_data("Application_2.csv")
	results <- eda_bbmk_test(df$max, samples = 100L)
	p <- plot_bbmk_test(results, title = "Title", xlabel = "X", ylabel = "Y")

	# Basic tests
	expect_s3_class(p, "ggplot")

	# Regression test with vdiffr
  	vdiffr::expect_doppelganger("bbmk-custom", p) 

})

test_that("plot-mks-test.R on data set #2", {

	df <- load_data("Application_2.csv")
	results <- eda_mks_test(df$max, df$year)
	p <- plot_mks_test(results)

	# Basic tests
	expect_s3_class(p, "ggplot")

	# Regression test with vdiffr
  	vdiffr::expect_doppelganger("mks-default", p) 

})

test_that("plot-mks-test.R with custom arguments", {
	df <- load_data("Application_2.csv")
	results <- eda_mks_test(df$max, df$year)
	p <- plot_mks_test(
		results,
		title = "Title",
		top_xlabel = "X (Top)",
		top_ylabel = "Y (Top)",
		bottom_xlabel = "X (Bottom)",
		bottom_ylabel = "Y (Bottom)"
	)

	# Basic tests
	expect_s3_class(p, "ggplot")

	# Regression test with vdiffr
  	vdiffr::expect_doppelganger("mks-custom", p) 

})

test_that("plot-pettitt-test.R on data set #2", {

	df <- load_data("Application_2.csv")
	results <- eda_pettitt_test(df$max, df$year)
	p <- plot_pettitt_test(results)

	# Basic tests
	expect_s3_class(p, "ggplot")

	# Regression test with vdiffr
  	vdiffr::expect_doppelganger("pettitt-default", p) 

})

test_that("plot-pettitt-test.R with custom arguments", {

	df <- load_data("Application_2.csv")
	results <- eda_pettitt_test(df$max, df$year)
	p <- plot_pettitt_test(
		results,
		title = "Title",
		top_xlabel = "X (Top)",
		top_ylabel = "Y (Top)",
		bottom_xlabel = "X (Bottom)",
		bottom_ylabel = "Y (Bottom)"
	)

	# Basic tests
	expect_s3_class(p, "ggplot")

	# Regression test with vdiffr
  	vdiffr::expect_doppelganger("pettitt-custom", p) 

})

test_that("plot-runs-test.R on data set #3.1", {

	df <- load_data("Application_3.1.csv")
	sens <- eda_sens_trend(df$max, df$year)
	results <- eda_runs_test(sens)
	p <- plot_runs_test(results, "mean")

	# Basic tests
	expect_s3_class(p, "ggplot")

	# Regression test with vdiffr
  	vdiffr::expect_doppelganger("runs-mean", p) 
	

})

test_that("plot-runs-test.R on data set #3.1 variances", {

	df <- load_data("Application_3.1.csv")
	mw <- ams_mw_variance(df$max, df$year)
	sens <- eda_sens_trend(mw$std, mw$year)
	results <- eda_runs_test(sens)
	p <- plot_runs_test(results, "variance")

	# Basic tests
	expect_s3_class(p, "ggplot")

	# Regression test with vdiffr
  	vdiffr::expect_doppelganger("runs-variance", p) 
	

})

test_that("plot-runs-test.R handles invalid 'type' argument", {
		
	df <- load_data("Application_3.1.csv")
	expect_error(
		plot_runs_test(list(years = df$year), "invalid"),
		regexp = "'type' must be either 'mean' or 'variance'"
	) 

})

test_that("plot-runs-test.R works with custom arguments", {

	df <- load_data("Application_3.1.csv")
	sens <- eda_sens_trend(df$max, df$year)
	results <- eda_runs_test(sens)
	p <- plot_runs_test(results, "mean", title = "Title", xlabel = "X", ylabel = "Y")

	# Basic tests
	expect_s3_class(p, "ggplot")

	# Regression test with vdiffr
  	vdiffr::expect_doppelganger("runs-custom", p) 
	
})

test_that("plot-sens-trend.R on data set #3.1", {

	df <- load_data("Application_3.1.csv")
	results <- eda_sens_trend(df$max, df$year)
	p <- plot_sens_trend(results, "mean")

	# Basic tests
	expect_s3_class(p, "ggplot")

	# Regression test with vdiffr
  	vdiffr::expect_doppelganger("sens-mean", p) 

})

test_that("plot-sens-trend.R on data set #3.1 variances", {

	df <- load_data("Application_3.1.csv")
	mw <- ams_mw_variance(df$max, df$year)
	results <- eda_sens_trend(mw$std, mw$year)
	p <- plot_sens_trend(results, "variance")

	# Basic tests
	expect_s3_class(p, "ggplot")

	# Regression test with vdiffr
  	vdiffr::expect_doppelganger("sens-variance", p) 

})

test_that("plot-sens-trend.R handles invalid 'type' argument", {
		
	df <- load_data("Application_3.1.csv")
	expect_error(
		plot_sens_trend(list(data = df$max, years = df$year), "invalid"),
		regexp = "'type' must be either 'mean' or 'variance'"
	) 

})

test_that("plot-sens-trend.R works with custom arguments", {

	df <- load_data("Application_3.1.csv")
	results <- eda_sens_trend(df$max, df$year)
	p <- plot_sens_trend(results, "mean", title = "Title", xlabel = "X", ylabel = "Y")

	# Basic tests
	expect_s3_class(p, "ggplot")

	# Regression test with vdiffr
  	vdiffr::expect_doppelganger("sens-custom", p) 

})

test_that("plot-spearman-test.R on data set #1", {

	df <- load_data("Application_1.csv")
	results <- eda_spearman_test(df$max)
	p <- plot_spearman_test(results)

	# Basic tests
	expect_s3_class(p, "ggplot")

	# Regression test with vdiffr
  	vdiffr::expect_doppelganger("spearman-default", p) 
	
})

test_that("plot-spearman-test.R works with custom arguments", {

	df <- load_data("Application_1.csv")
	results <- eda_spearman_test(df$max)
	p <- plot_spearman_test(results, title = "Title", xlabel = "X", ylabel = "Y")

	# Basic tests
	expect_s3_class(p, "ggplot")

	# Regression test with vdiffr
  	vdiffr::expect_doppelganger("spearman-custom", p) 
	
})

test_that("plot-lmom-diagram.R on data set #1 with L-distance", {

	df <- load_data("Application_1.csv")
	results <- select_ldistance(df$max)
	p <- plot_lmom_diagram(results)

	# Basic tests
	expect_s3_class(p, "ggplot")

	# Regression test with vdiffr
  	vdiffr::expect_doppelganger("lmom-l-distance", p) 

})

test_that("plot-lmom-diagram.R on data set #1 with L-kurtosis", {

	df <- load_data("Application_1.csv")
	results <- select_lkurtosis(df$max)
	p <- plot_lmom_diagram(results)

	# Basic tests
	expect_s3_class(p, "ggplot")

	# Regression test with vdiffr
  	vdiffr::expect_doppelganger("lmom-l-kurtosis", p) 
	
})

test_that("plot-lmom-diagram.R on data set #1 with Z-statistic", {

	df <- load_data("Application_1.csv")
	results <- select_zstatistic(df$max)
	p <- plot_lmom_diagram(results)

	# Basic tests
	expect_s3_class(p, "ggplot")

	# Regression test with vdiffr
  	vdiffr::expect_doppelganger("lmom-z-statistic", p) 
	
})

test_that("plot-lmom-diagram.R works with custom arguments", {

	df <- load_data("Application_1.csv")
	results <- select_ldistance(df$max)
	p <- plot_lmom_diagram(results, title = "Title", xlabel = "X", ylabel = "Y")

	# Basic tests
	expect_s3_class(p, "ggplot")

	# Regression test with vdiffr
  	vdiffr::expect_doppelganger("lmom-custom", p) 

})

test_that("plot-uncertainty.R on data set #3.3", {
	set.seed(1)

	df <- load_data("Application_3.3.csv")
	results <- uncertainty_bootstrap(df$max, "GEV", "L-moments")
	p <- plot_uncertainty(results)

	# Basic tests
	expect_s3_class(p, "ggplot")

	# Regression test with vdiffr
  	vdiffr::expect_doppelganger("uncertainty-s", p) 

})

test_that("plot-uncertainty.R on data set #3.3", {
	set.seed(1)

	df <- load_data("Application_3.3.csv")

	results <- uncertainty_bootstrap(
		df$max,
		"GEV",
		"MLE",
		years = df$year,
		trend = trend_10,
		slice = max(df$year),
		samples = 1000L
	)

	p <- plot_uncertainty(results)

	# Basic tests
	expect_s3_class(p, "ggplot")

	# Regression test with vdiffr
  	vdiffr::expect_doppelganger("uncertainty-ns", p) 

})

test_that("plot-uncertainty.R works with custom arguments", {
	set.seed(1)

	df <- load_data("Application_3.3.csv")
	results <- uncertainty_bootstrap(df$max, "GEV", "L-moments")
	p <- plot_uncertainty(results, title = "Title", xlabel = "X", ylabel = "Y")

	# Basic tests
	expect_s3_class(p, "ggplot")

	# Regression test with vdiffr
  	vdiffr::expect_doppelganger("uncertainty-custom", p) 

})

test_that("plot-model-diagnostics.R on data set #3.3", {

	df <- load_data("Application_3.3.csv")
	params <- fit_maximum_likelihood(df$max, "GEV")$params
	uncertainty <- uncertainty_bootstrap(df$max, "GEV", "MLE", samples = 1000L)
	assessment <- model_diagnostics(df$max, "GEV", params, uncertainty)
	p <- plot_model_diagnostics(assessment)

	# Basic tests
	expect_s3_class(p, "ggplot")

	# Regression test with vdiffr
  	vdiffr::expect_doppelganger("assessment-default", p) 
	
})

test_that("plot-model-diagnostics.R works with custom arguments", {

	df <- load_data("Application_3.3.csv")
	params <- fit_maximum_likelihood(df$max, "GEV")$params
	uncertainty <- uncertainty_bootstrap(df$max, "GEV", "MLE", samples = 1000L)
	assessment <- model_diagnostics(df$max, "GEV", params, uncertainty)
	p <- plot_model_diagnostics(assessment, title = "Title", xlabel = "X", ylabel = "Y")

	# Basic tests
	expect_s3_class(p, "ggplot")

	# Regression test with vdiffr
  	vdiffr::expect_doppelganger("assessment-custom", p) 
	
})
