# NOTE: Skip plotting tests works on CRAN to avoid unpredictable errors with vdiffr.
test_that("plot-ams-data.R works on KOOTENAI RIVER (08NH021)", {
	skip_on_cran()

	df <- data_local("CAN-08NH021.csv")

	# Test the plotting function
	p <- plot_ams_data(df$max, df$year)
	expect_s3_class(p, "ggplot")
  	vdiffr::expect_doppelganger("ams-default", p) 

	# Test with custom arguments
	p <- plot_ams_data(df$max, df$year, title = "Title", xlabel = "X", ylabel = "Y")
	expect_s3_class(p, "ggplot")
  	vdiffr::expect_doppelganger("ams-custom", p) 

})

test_that("plot-bbmk-test.R works on KOOTENAI RIVER (08NH021)", {
	skip_on_cran()
	set.seed(1)

	# Run the BB-MK test
	df <- data_local("CAN-08NH021.csv")
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

test_that("plot-mks-test.R works on KOOTENAI RIVER (08NH021)", {
	skip_on_cran()

	# Run the MKS test
	df <- data_local("CAN-08NH021.csv")
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

test_that("plot-pettitt-test.R works on KOOTENAI RIVER (08NH021)", {
	skip_on_cran()

	# Run the Pettitt test
	df <- data_local("CAN-08NH021.csv")
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

test_that("plot-runs-test.R works on BOW RIVER (05BB001)", {
	skip_on_cran()

	# Run the runs test
	df <- data_local("CAN-05BB001.csv")
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

test_that("plot-sens-trend.R works on BOW RIVER (05BB001)", {
	skip_on_cran()

	df <- data_local("CAN-05BB001.csv")
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

test_that("plot-sens-trend.R works on OKANAGAN RIVER (08NM050) with variability", {
	skip_on_cran()

	df <- data_local("CAN-08NM050.csv")
	mean_trend <- eda_sens_trend(df$max, df$year)
	mw <- data_mw_variability(df$max, df$year)
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

test_that("plot-spearman-test.R works on ATHABASCA RIVER (07BE001)", {
	skip_on_cran()

	df <- data_local("CAN-07BE001.csv")
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

test_that("plot-lmom-diagram.R works on ATHABASCA RIVER (07BE001) with L-distance", {
	skip_on_cran()

	pdf(nullfile())
	df <- data_local("CAN-07BE001.csv")
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

test_that("plot-lmom-diagram.R works on ATHABASCA RIVER (07BE001) with L-kurtosis", {
	skip_on_cran()

	pdf(nullfile())
	df <- data_local("CAN-07BE001.csv")
	results <- select_lkurtosis(df$max)

	# Test the plotting function
	p <- plot_lmom_diagram(results)
	expect_s3_class(p, "ggplot")
  	vdiffr::expect_doppelganger("lmom-lkurtosis", p) 
	
})

test_that("plot-lmom-diagram.R works on ATHABASCA RIVER (07BE001) with Z-statistic", {
	skip_on_cran()

	pdf(nullfile())
	df <- data_local("CAN-07BE001.csv")
	results <- select_zstatistic(df$max)

	# Test the plotting function
	p <- plot_lmom_diagram(results)
	expect_s3_class(p, "ggplot")
  	vdiffr::expect_doppelganger("lmom-zstatistic", p) 
	
})

test_that("plot-sffa.R works on OKANAGAN RIVER (08NM050)", {
	skip_on_cran()
	set.seed(1)

	df <- data_local("CAN-08NM050.csv")
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

test_that("plot-nsffa.R works on OKANAGAN RIVER (08NM050)", {
	skip_on_cran()
	set.seed(1)

	df <- data_local("CAN-08NM050.csv")

	results <- uncertainty_bootstrap(
		df$max,
		"GEV",
		"MLE",
		years = df$year,
		structure = S10,
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

test_that("plot-model-diagnostics.R works on OKANAGAN RIVER (08NM050)", {
	skip_on_cran()

	df <- data_local("CAN-08NM050.csv")
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
