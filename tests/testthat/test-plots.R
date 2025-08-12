# NOTE: Skip plotting tests works on CRAN to avoid unpredictable errors with vdiffr.
test_that("plot-ams-data.R works on OKANAGAN RIVER (08NM050)", {
	skip_on_cran()
	df <- data_local("CAN-08NM050.csv")

	# Test the plotting function
	p <- plot_ams_data(df$max, df$year)
	expect_s3_class(p, "ggplot")
  	vdiffr::expect_doppelganger("ams-default", p) 

	# Test with constant mean/variability
	p <- plot_ams_data(df$max, df$year, "Constant", "Constant")
	expect_s3_class(p, "ggplot")
  	vdiffr::expect_doppelganger("ams-constant", p) 

	# Test with trend in mean/variability
	p <- plot_ams_data(df$max, df$year, "Trend", "Trend")
	expect_s3_class(p, "ggplot")
  	vdiffr::expect_doppelganger("ams-trend", p) 

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
	results <- eda_runs_test(sens$residuals, df$year)

	# Test the plotting function
	p <- plot_runs_test(results, "mean")
	expect_s3_class(p, "ggplot")
  	vdiffr::expect_doppelganger("runs-mean", p) 
	
	# Test with custom arguments
	p <- plot_runs_test(results, "mean", title = "Title", xlabel = "X", ylabel = "Y")
	expect_s3_class(p, "ggplot")
	vdiffr::expect_doppelganger("runs-custom", p) 

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
	set.seed(1)

	df <- data_local("CAN-07BE001.csv")
	results <- select_zstatistic(df$max)

	# Test the plotting function
	p <- plot_lmom_diagram(results)
	expect_s3_class(p, "ggplot")
  	vdiffr::expect_doppelganger("lmom-zstatistic", p) 
	
})

test_that("plot-sffa-fit.R works on ATHABASCA RIVER (07BE001)", {
	skip_on_cran()
	set.seed(1)

	df <- data_local("CAN-07BE001.csv")
	results <- fit_lmoments(df$max, "GEV")

	# Test plotting function
	p <- plot_sffa_fit(results)
	expect_s3_class(p, "ggplot")
  	vdiffr::expect_doppelganger("sffa-fit-default", p) 

	# Test with custom arguments
	p <- plot_sffa_fit(results, title = "Title", xlabel = "X", ylabel = "Y")
	expect_s3_class(p, "ggplot")
  	vdiffr::expect_doppelganger("sffa-fit-custom", p) 

})

test_that("plot-sffa-estimates.R works on OKANAGAN RIVER (08NM050)", {
	skip_on_cran()
	set.seed(1)

	df <- data_local("CAN-08NM050.csv")
	fit <- fit_lmoments(df$max, "GEV")
	uncertainty <- uncertainty_bootstrap(df$max, "GEV", "L-moments")

	# Test plotting function
	p <- plot_sffa_estimates(fit)
	expect_s3_class(p, "ggplot")
  	vdiffr::expect_doppelganger("sffa-estimates-default", p) 

	# Test with confidence interval
	p <- plot_sffa_estimates(uncertainty)
	expect_s3_class(p, "ggplot")
  	vdiffr::expect_doppelganger("sffa-estimates-uncertainty", p) 

	# Test with custom arguments
	p <- plot_sffa_estimates(fit, title = "Title", xlabel = "X", ylabel = "Y")
	expect_s3_class(p, "ggplot")
  	vdiffr::expect_doppelganger("sffa-estimates-custom", p) 

})


test_that("plot-nsffa-fit.R works on OKANAGAN RIVER (08NM050)", {
	skip_on_cran()
	set.seed(1)

	df <- data_local("CAN-08NM050.csv")
	results <- fit_mle(df$max, "GEV", df$year, S11)

	# Test plotting function
	p <- plot_nsffa_fit(results)
	expect_s3_class(p, "ggplot")
  	vdiffr::expect_doppelganger("nsffa-fit-default", p) 

	# Test with custom arguments
	p <- plot_nsffa_fit(results, title = "Title", xlabel = "X", ylabel = "Y")
	expect_s3_class(p, "ggplot")
  	vdiffr::expect_doppelganger("nsffa-fit-custom", p) 

})


test_that("plot-nsffa-estimates.R works on OKANAGAN RIVER (08NM050)", {

	skip_on_cran()
	set.seed(1)
	df <- data_local("CAN-08NM050.csv")

	fit <- fit_mle(
		df$max,
		"GEV",
		ns_years = df$year,
		ns_structure = S10
	)

	uncertainty <- uncertainty_bootstrap(
		df$max,
		"GEV",
		"MLE",
		ns_years = df$year,
		ns_structure = S10,
		ns_slices = c(1920, 1960, 2000),
		samples = 1000L
	)

	# Test plotting function
	p <- plot_nsffa_estimates(fit)
	expect_s3_class(p, "ggplot")
  	vdiffr::expect_doppelganger("nsffa-estimates-default", p) 

	# Test with confidence intervals
	p <- plot_nsffa_estimates(uncertainty)
	expect_s3_class(p, "ggplot")
  	vdiffr::expect_doppelganger("nsffa-estimates-uncertainty", p) 

	# Test custom arguments
	p <- plot_nsffa_estimates(fit, title = "Title", xlabel = "X", ylabel = "Y")
	expect_s3_class(p, "ggplot")
  	vdiffr::expect_doppelganger("nsffa-estimates-custom", p) 

})

test_that("plot-model-assessment.R works on OKANAGAN RIVER (08NM050)", {
	skip_on_cran()

	df <- data_local("CAN-08NM050.csv")
	params <- fit_mle(df$max, "GEV")$params
	uncertainty <- uncertainty_bootstrap(df$max, "GEV", "MLE", samples = 1000L)
	assessment <- model_assessment(df$max, "GEV", params, ci = uncertainty$ci)

	# Test plotting function
	p <- plot_model_assessment(assessment)
	expect_s3_class(p, "ggplot")
  	vdiffr::expect_doppelganger("assessment-default", p) 

	# Test with custom arguments
	p <- plot_model_assessment(assessment, title = "Title", xlabel = "X", ylabel = "Y")
	expect_s3_class(p, "ggplot")
  	vdiffr::expect_doppelganger("assessment-custom", p) 
	
})
