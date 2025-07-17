test_that("plot-ams-data.R on data set #2", {

	df <- load_data("Application_2.csv")

	# Test the plotting function
	p <- plot_ams_data(df$max, df$year)
	expect_s3_class(p, "ggplot")
  	vdiffr::expect_doppelganger("ams-default", p) 

	# Test with custom arguments
	p <- plot_ams_data(df$max, df$year, title = "Title", xlabel = "X", ylabel = "Y")
	expect_s3_class(p, "ggplot")
  	vdiffr::expect_doppelganger("ams-custom", p) 

})

test_that("plot-bbmk-test.R on data set #2", {
	set.seed(1)

	# Run the BB-MK test
	df <- load_data("Application_2.csv")
	results <- eda_bbmk_test(df$max, samples = 100L)

	# Test the plotting function
	p <- plot_bbmk_test(results)
	expect_s3_class(p, "ggplot")
  	vdiffr::expect_doppelganger("bbmk-default", p) 

	# Test with custom arguments
	p <- plot_bbmk_test(results, title = "Title", xlabel = "X", ylabel = "Y")
	expect_s3_class(p, "ggplot")
  	vdiffr::expect_doppelganger("bbmk-custom", p) 

})

test_that("plot-mks-test.R on data set #2", {

	# Run the MKS test
	df <- load_data("Application_2.csv")
	results <- eda_mks_test(df$max, df$year)

	# Tests the plotting function
	p <- plot_mks_test(results)
	expect_s3_class(p, "ggplot")
  	vdiffr::expect_doppelganger("mks-default", p) 

	# Test with custom arguments
	p <- plot_mks_test(
		results,
		title = "Title",
		top_xlabel = "X (Top)",
		top_ylabel = "Y (Top)",
		bottom_xlabel = "X (Bottom)",
		bottom_ylabel = "Y (Bottom)"
	)

	expect_s3_class(p, "ggplot")
  	vdiffr::expect_doppelganger("mks-custom", p) 

})

test_that("plot-pettitt-test.R on data set #2", {

	# Run the Pettitt test
	df <- load_data("Application_2.csv")
	results <- eda_pettitt_test(df$max, df$year)

	# Test the plotting function
	p <- plot_pettitt_test(results)
	expect_s3_class(p, "ggplot")
  	vdiffr::expect_doppelganger("pettitt-default", p) 

	# Test with custom arguments
	p <- plot_pettitt_test(
		results,
		title = "Title",
		top_xlabel = "X (Top)",
		top_ylabel = "Y (Top)",
		bottom_xlabel = "X (Bottom)",
		bottom_ylabel = "Y (Bottom)"
	)

	expect_s3_class(p, "ggplot")
  	vdiffr::expect_doppelganger("pettitt-custom", p) 

})

test_that("plot-runs-test.R on data set #3.1", {

	# Run the runs test
	df <- load_data("Application_3.1.csv")
	sens <- eda_sens_trend(df$max, df$year)
	results <- eda_runs_test(sens)

	# Test the plotting function
	p <- plot_runs_test(results, "mean")
	expect_s3_class(p, "ggplot")
  	vdiffr::expect_doppelganger("runs-mean", p) 
	
	# Test with custom arguments
	p <- plot_runs_test(results, "mean", title = "Title", xlabel = "X", ylabel = "Y")
	expect_s3_class(p, "ggplot")
	vdiffr::expect_doppelganger("runs-custom", p) 

})

test_that("plot-sens-trend.R on data set #3.1", {

	df <- load_data("Application_3.1.csv")
	results <- eda_sens_trend(df$max, df$year)

	# Test the plotting function
	p <- plot_sens_trend(df$max, df$year, mean_trend = results)
	expect_s3_class(p, "ggplot")
  	vdiffr::expect_doppelganger("sens-mean", p) 

	# Test with custom labels
	p <- plot_sens_trend(
		df$max,
		df$year,
		mean_trend = results,
		title = "Title",
		xlabel = "X",
		ylabel = "Y"
	)

	expect_s3_class(p, "ggplot")
  	vdiffr::expect_doppelganger("sens-custom", p) 

})

test_that("plot-sens-trend.R on data set #3.3 with variability", {

	df <- load_data("Application_3.3.csv")
	mean_trend <- eda_sens_trend(df$max, df$year)
	mw <- ams_mw_variability(df$max, df$year)
	variability_trend <- eda_sens_trend(mw$std, mw$year)

	# Trend in variability
	p <- plot_sens_trend(df$max, df$year, variability_trend = variability_trend)
	expect_s3_class(p, "ggplot")
  	vdiffr::expect_doppelganger("sens-variability", p) 

	# Trend in mean and variability
	p <- plot_sens_trend(df$max, df$year, mean_trend, variability_trend)
	expect_s3_class(p, "ggplot")
  	vdiffr::expect_doppelganger("sens-mean-variability", p) 

})

test_that("plot-spearman-test.R on data set #1", {

	df <- load_data("Application_1.csv")
	results <- eda_spearman_test(df$max)

	# Test the plotting function
	p <- plot_spearman_test(results)
	expect_s3_class(p, "ggplot")
  	vdiffr::expect_doppelganger("spearman-default", p) 
	
	# Test the plotting function
	p <- plot_spearman_test(results, title = "Title", xlabel = "X", ylabel = "Y")
	expect_s3_class(p, "ggplot")
  	vdiffr::expect_doppelganger("spearman-custom", p) 
	
})

test_that("plot-lmom-diagram.R on data set #1 with L-distance", {

	pdf(nullfile())
	df <- load_data("Application_1.csv")
	results <- select_ldistance(df$max)

	# Test the plotting function
	p <- plot_lmom_diagram(results)
	expect_s3_class(p, "ggplot")
  	vdiffr::expect_doppelganger("lmom-ldistance", p) 

	# Test with custom arguments
	p <- plot_lmom_diagram(results, title = "Title", xlabel = "X", ylabel = "Y")
	expect_s3_class(p, "ggplot")
  	vdiffr::expect_doppelganger("lmom-custom", p) 

})

test_that("plot-lmom-diagram.R on data set #1 with L-kurtosis", {

	pdf(nullfile())
	df <- load_data("Application_1.csv")
	results <- select_lkurtosis(df$max)

	# Test the plotting function
	p <- plot_lmom_diagram(results)
	expect_s3_class(p, "ggplot")
  	vdiffr::expect_doppelganger("lmom-lkurtosis", p) 
	
})

test_that("plot-lmom-diagram.R on data set #1 with Z-statistic", {

	pdf(nullfile())
	df <- load_data("Application_1.csv")
	results <- select_zstatistic(df$max)

	# Test the plotting function
	p <- plot_lmom_diagram(results)
	expect_s3_class(p, "ggplot")
  	vdiffr::expect_doppelganger("lmom-zstatistic", p) 
	
})

test_that("plot-sffa.R on data set #3.3", {
	set.seed(1)

	df <- load_data("Application_3.3.csv")
	results <- uncertainty_bootstrap(df$max, "GEV", "L-moments")

	# Test plotting function
	p <- plot_sffa(results)
	expect_s3_class(p, "ggplot")
  	vdiffr::expect_doppelganger("sffa-default", p) 

	# Test with custom arguments
	p <- plot_sffa(results, title = "Title", xlabel = "X", ylabel = "Y")
	expect_s3_class(p, "ggplot")
  	vdiffr::expect_doppelganger("sffa-custom", p) 

})

test_that("plot-nsffa.R on data set #3.3", {
	set.seed(1)

	df <- load_data("Application_3.3.csv")

	results <- uncertainty_bootstrap(
		df$max,
		"GEV",
		"MLE",
		years = df$year,
		trend = trend_10,
		slices = c(1920, 1960, 2000),
		samples = 1000L
	)

	# Test plotting function
	p <- plot_nsffa(results)
	expect_s3_class(p, "ggplot")
  	vdiffr::expect_doppelganger("nsffa-default", p) 

	# Test custom arguments
	p <- plot_nsffa(results, title = "Title", xlable = "X", ylabel = "Y")
	expect_s3_class(p, "ggplot")
  	vdiffr::expect_doppelganger("nsffa-custom", p) 

})

test_that("plot-model-diagnostics.R on data set #3.3", {

	df <- load_data("Application_3.3.csv")
	params <- fit_maximum_likelihood(df$max, "GEV")$params
	uncertainty <- uncertainty_bootstrap(df$max, "GEV", "MLE", samples = 1000L)
	assessment <- model_diagnostics(df$max, "GEV", params, uncertainty)

	# Test plotting function
	p <- plot_model_diagnostics(assessment)
	expect_s3_class(p, "ggplot")
  	vdiffr::expect_doppelganger("diagnostics-default", p) 

	# Test with custom arguments
	p <- plot_model_diagnostics(assessment, title = "Title", xlabel = "X", ylabel = "Y")
	expect_s3_class(p, "ggplot")
  	vdiffr::expect_doppelganger("diagnostics-custom", p) 
	
})
