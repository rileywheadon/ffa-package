test_that("submodule-01-splits.R works on KOOTENAI RIVER (08NH021)", {

	df <- data_local("CAN-08NH021.csv")
	path <- tempdir()

	results <- submodule_01(
		df$max,
		df$year,
		default_options,
		path = path,
		serialize = TRUE
	)

	# Unpack the results
	expect_true(is.list(results))
	pettitt <- results[[1]]$tests$pettitt
	mks <- results[[1]]$tests$mks
	
	# Test the Pettitt test (copied from test-eda-pettitt-test.R)
	expect_equal(length(pettitt$u_series), 91)
  	expect_equal(pettitt$statistic, 1871)
	expect_equal(pettitt$bound, 616.753, tol = 1e-4)
	expect_equal(pettitt$p_value, 0, tol = 1e-4)
	expect_equal(pettitt$change_points$index, 45)
	expect_equal(pettitt$change_points$year, 1972)

	# Test the MKS test (copied from the test-eda-mks-test.R)
	expect_equal(length(mks$progressive_series), 91)
	expect_equal(length(mks$regressive_series), 91)
	expect_equal(mks$p_value, 0.015, tol = 1e-4)
	expect_equal(mks$change_points$index, c(33, 58))
	expect_equal(mks$change_points$statistic, c(2.1805, -2.4335), tol = 1e-4)

	# Check that the plots were saved to the temporary directory
	expect_true(file.exists(file.path(path, "pettitt_1928_2018.png")))
	expect_true(file.exists(file.path(path, "mks_1928_2018.png")))

	# Check that the 'serialize' option works as intended
	expect_true(is.character(pettitt$plot))
	expect_true(is.character(mks$plot))

})
