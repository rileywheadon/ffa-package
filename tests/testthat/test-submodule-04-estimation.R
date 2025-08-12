test_that("submodule-04-estimation.R works on ATHABASCA RIVER (07BE001)", {

	set.seed(1)
	df <- data_local("CAN-07BE001.csv")
	path <- tempdir()
	
	results <- submodule_04(
		df$max,
		df$year,
		"GEV",
		default_options,
		NULL,
		NULL,
		path = path,
		serialize = TRUE
	)

	# Check that the results are a list
	expect_equal(is.list(results), TRUE)

	# Test L-moments parameter estimation (from test-fit-lmoments.R)
	estimation <- results[[1]]$estimation
	expect_equal(estimation$method, "L-moments")
	expect_equal(estimation$distribution, "GEV")
	expect_equal(estimation$params, c(1600.2199, 616.6660, 0.1207), tol = 1e-3)

	# Check that images exist
	expect_true(file.exists(file.path(path, "estimation_1913_2020.png")))
	
	# Check that the 'serialize' option works as intended
	expect_true(is.character(estimation$plot))

})

test_that("submodule-04-estimation.R works with MLE", {

	# Run parameter estimation
	set.seed(1)
	df <- data_local("CAN-07BE001.csv")

	results <- submodule_04(
		df$max,
		df$year,
		"GUM",
		default_options,
		NULL,
		list(S10)
	)

	# Test MLE estimation (from test-fit-mle.R)
	estimation <- results[[1]]$estimation
	expect_equal(estimation$method, "MLE")
	expect_equal(estimation$distribution, "GUM")
	expect_equal(estimation$params, c(1521.3786, 178.9614, 661.2450), tol = 1e-3)
	expect_equal(estimation$mll, -825.3975, tol = 1e-3)

})

test_that("submodule-04-estimation.R works with GMLE", {

	# Update configuration
	options <- default_options
	options$ns_estimation <- "GMLE"
	options$gev_prior <- c(6, 9)

	# Run parameter estimation
	set.seed(1)
	df <- data_local("CAN-07BE001.csv")

	results <- submodule_04(
		df$max,
		df$year,
		"GEV",
		options,
		NULL,
		list(S10)
	)

	# Test GMLE estimation (from test-fit-gmle.R)
	estimation <- results[[1]]$estimation
	expect_equal(estimation$method, "GMLE")
	expect_equal(estimation$distribution, "GEV")
	expect_equal(estimation$params, c(1459.6527, 215.2392, 646.2991, 0.1135), tol = 1e-3)
	expect_equal(estimation$mll, -709.2092, tol = 1e-3)

})
