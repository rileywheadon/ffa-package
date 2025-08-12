test_that("submodule-05-uncertainty.R works on ATHABASCA RIVER (07BE001)", {

	set.seed(1)
	df <- data_local("CAN-07BE001.csv")
	path <- tempdir()
	
	results <- submodule_05(
		df$max,
		df$year,
		"GEV",
		default_options,
		splits = NULL,
		structures = NULL,
		path = path,
		serialize = TRUE
	)

	# Check that the results are a list
	expect_equal(is.list(results), TRUE)

	# Test uncertainty quantification (from test-uncertainty-bootstrap.R)
	uncertainty <- results[[1]]$uncertainty
	ci <- uncertainty$ci
	expect_equal(ci$lower    , c(1679, 2361, 2819, 3244, 3763, 4134), tol = 1e-2)
	expect_equal(ci$estimates, c(1831, 2614, 3195, 3803, 4674, 5393), tol = 1e-2)
	expect_equal(ci$upper    , c(1999, 2885, 3604, 4451, 5842, 7153), tol = 1e-2)

	# Check that images exist
	expect_true(file.exists(file.path(path, "uncertainty_1913_2020.png")))
	
	# Check that the 'serialize' option works as intended
	expect_true(is.character(uncertainty$plot))

})

test_that("submodule-05-uncertainty.R works with RFPL", {

	# Update configuration
	options <- default_options
	options$ns_slices <- 1913L

	# Run parameter estimation
	set.seed(1)
	df <- data_local("CAN-07BE001.csv")
	path <- tempdir()

	results <- submodule_05(
		df$max,
		df$year,
		"GUM",
		options,
		splits = NULL,
		structures = list(S10)
	)
	
	# Test RFPL uncertainty (from test-uncertainty-rfpl.R)
	ci <- results[[1]]$uncertainty$ci_list[[1]]
	expect_equal(ci$lower,     c(1522, 2230, 2687, 3117, 3665, 4073), tol = 1e-2)
	expect_equal(ci$estimates, c(1786, 2536, 3032, 3508, 4124, 4586), tol = 1e-2)
	expect_equal(ci$upper,     c(2059, 2878, 3436, 3977, 4684, 5217), tol = 1e-2)

})

test_that("submodule-05-uncertainty.R works with RFGPL", {

	# Update configuration
	options <- default_options
	options$ns_estimation <- "GMLE"
	options$ns_uncertainty <- "RFGPL"
	options$gev_prior <- c(6, 9)
	options$ns_slices <- 1913L

	# Run parameter estimation
	set.seed(1)
	df <- data_local("CAN-07BE001.csv")
	path <- tempdir()

	results <- submodule_05(
		df$max,
		df$year,
		"GEV",
		options,
		splits = NULL,
		structures = list(S10)
	)

	# Test RFGPL uncertainty (from test-uncertainty-rfgpl.R)
	ci <- results[[1]]$uncertainty$ci_list[[1]]
	expect_equal(ci$lower,     c(1474, 2230, 2766, 3314, 4078, 4696), tol = 1e-2)
	expect_equal(ci$estimates, c(1729, 2544, 3144, 3770, 4660, 5391), tol = 1e-2)
	expect_equal(ci$upper,     c(1994, 2904, 3592, 4322, 5358, 6236), tol = 1e-2)

})
