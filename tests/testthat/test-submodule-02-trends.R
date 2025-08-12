test_that("submodule-02-trends.R works on OKANAGAN RIVER (08NM050)", {

	set.seed(1)
	df <- data_local("CAN-08NM050.csv")
	path <- tempdir()
	
	results <- submodule_02(
		df$max,
		df$year,
		default_options,
		integer(0),
		path = path,
		serialize = TRUE
	)

	# Check that the results are a list
	expect_equal(is.list(results), TRUE)

	# Check the MK test (copied from test-eda-mk-test.R)
	mk <- results[[1]]$items$mk
	expect_equal(mk$statistic, 1822)
	expect_equal(mk$variance, 102933, tol = 1e-4)
	expect_equal(mk$p_value, 0, tol = 1e-4)
	expect_equal(mk$reject, TRUE)

	# Check the Spearman test (copied from test-eda-spearman-test.R)
	spearman <- results[[1]]$items$spearman
	expect_equal(spearman$least_lag, 5)

	# Check the BB-MK test (copied from test-eda-bbmk-test.R)
	bbmk <- results[[1]]$items$bbmk
	expect_equal(bbmk$p_value, 8e-6, tol = 5e-2)
	expect_equal(unname(bbmk$bounds), c(-902, 894), tol = 5e-2)

	# Check the PP test (copied from test-eda-pp-test.R)
	pp <- results[[1]]$items$pp
	expect_equal(pp$statistic, -90.0663, tol = 1e-4)
	expect_equal(pp$p_value, 0.01, tol = 1e-4)
	expect_equal(pp$reject, TRUE)

	# Check the KPSS test (copied from test-eda-kpss-test.R)
	kpss <- results[[1]]$items$kpss
	expect_equal(kpss$statistic, 0.0381, tol = 1e-4)
	expect_equal(kpss$p_value, 0.10, tol = 1e-4)
	expect_equal(kpss$reject, FALSE)

	# Check Sen's trend estimator (copied from test-eda-sens-trend.R)
	sens <- results[[1]]$items$sens_mean
  	expect_equal(sens$slope, 40.3939, tol = 1e-4)
	expect_equal(sens$intercept, 11.2479, tol = 1e-4)

	# Check the runs test (copied from test-eda-runs-test.R)
	runs <- results[[1]]$items$runs_mean
	expect_equal(runs$n, 96)
	expect_equal(runs$runs, 47)
	expect_equal(runs$statistic, -0.41041, tol = 1e-4)
	expect_equal(runs$p_value, 0.6815, tol = 1e-4)

	# Check the MW-MK test
	expect_true(is.list(results[[1]]$items$mwmk))

	# Check the White test (copied from test-eda-white-test.R)
	white <- results[[1]]$items$white
	expect_equal(white$statistic, 4.0246, tol = 1e-4)
	expect_equal(white$p_value, 0.1337, tol = 1e-4)

	# Check that the plots were saved to the temporary directory
	expect_true(file.exists(file.path(path, "spearman_1921_2017.png")))
	expect_true(file.exists(file.path(path, "bbmk_1921_2017.png")))
	expect_true(file.exists(file.path(path, "sens_mean_1921_2017.png")))
	expect_true(file.exists(file.path(path, "runs_mean_1921_2017.png")))

	# Check that the 'serialize' option works as intended
	expect_true(is.character(spearman$plot))
	expect_true(is.character(bbmk$plot))
	expect_true(is.character(sens$plot))
	expect_true(is.character(runs$plot))

})


