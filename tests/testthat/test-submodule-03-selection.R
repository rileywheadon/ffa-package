test_that("submodule-03-selection.R works on ATHABASCA RIVER (07BE001)", {

	set.seed(1)
	df <- data_local("CAN-07BE001.csv")
	path <- tempdir()
	
	results <- submodule_03(
		df$max,
		df$year,
		default_options,
		NULL,
		NULL,
		path = path,
		serialize = TRUE
	)

	# Check that the results are a list
	expect_equal(is.list(results), TRUE)

	# Test L-distance selection (from test-select-ldistance.R)
	selection <- results[[1]]$selection
	expect_equal(selection$metrics$GEV, 0.0092, tol = 1e-3)
	expect_equal(selection$metrics$GUM, 0.0923, tol = 1e-3) 
	expect_equal(selection$metrics$NOR, 0.2604, tol = 1e-3) 
	expect_equal(selection$metrics$LNO, 0.0529, tol = 1e-3) 
	expect_equal(selection$metrics$GLO, 0.0199, tol = 1e-3)
	expect_equal(selection$metrics$PE3, 0.0514, tol = 1e-3)
	expect_equal(selection$metrics$LP3, 0.0470, tol = 1e-3)
	expect_equal(selection$metrics$GNO, 0.0235, tol = 1e-3)
	expect_equal(selection$metrics$WEI, 0.0607, tol = 1e-3)
	expect_equal(selection$recommendation, "GEV")

	# Check that images exist
	expect_true(file.exists(file.path(path, "selection_1913_2020.png")))
	
	# Check that the 'serialize' option works as intended
	expect_true(is.character(selection$plot))

})


test_that("submodule-03-selection.R works with L-kurtosis selection", {

	# Update settings
	options <- default_options 
	options$selection <- "L-kurtosis"

	# Run submodule
	set.seed(1)
	df <- data_local("CAN-07BE001.csv")
	path <- tempdir()

	results <- submodule_03(
		df$max,
		df$year,
		options,
		NULL,
		NULL
	)

	# Test L-kurtosis (from test-select-lkurtosis.R)
	selection <- results[[1]]$selection
	expect_equal(selection$metrics$GEV, 0.0103, tol = 1e-3) 
	expect_equal(selection$metrics$GLO, 0.0215, tol = 1e-3) 
	expect_equal(selection$metrics$PE3, 0.0525, tol = 1e-3) 
	expect_equal(selection$metrics$LP3, 0.0470, tol = 1e-3) 
	expect_equal(selection$metrics$GNO, 0.0254, tol = 1e-3) 
	expect_equal(selection$metrics$WEI, 0.0642, tol = 1e-3) 
	expect_equal(selection$recommendation, "GEV") 


})

test_that("submodule-03-selection.R works with Z-statistic selection", {

	# Update settings
	options <- default_options 
	options$selection <- "Z-statistic"
	options$z_samples <- 25000L

	# Run submodule
	set.seed(1)
	df <- data_local("CAN-07BE001.csv")
	path <- tempdir()

	results <- submodule_03(
		df$max,
		df$year,
		options,
		NULL,
		NULL
	)

	# Test Z-statistic selection (from test-select-zstatistic.R)
	selection <- results[[1]]$selection
	expect_equal(selection$reg_params[3], -0.1544, tol = 1e-3)
	expect_equal(selection$reg_params[4], -0.1704, tol = 1e-3)
	expect_equal(selection$log_params[3], NULL)
	expect_equal(selection$log_params[4], NULL)
	expect_equal(selection$reg_bias_t4, -0.0026, tol = 1e-2)
	expect_equal(selection$reg_std_t4 ,  0.0514, tol = 1e-2)
	expect_equal(selection$log_bias_t4, NULL)
	expect_equal(selection$log_std_t4 , NULL)
	expect_equal(selection$metrics$GEV, -0.2524, tol = 1e-2)
	expect_equal(selection$metrics$GLO,  0.3673, tol = 1e-2)
	expect_equal(selection$metrics$PE3, -1.0738, tol = 1e-2)
	expect_equal(selection$metrics$LP3, NULL)
	expect_equal(selection$metrics$GNO, -0.5443, tol = 1e-2)
	expect_equal(selection$metrics$WEI, -1.3005, tol = 1e-2)
	expect_equal(selection$recommendation, "GEV") 

})


test_that("submodule-03-selection.R works with a preset distribution", {

	# Update settings
	options <- default_options 
	options$selection <- "LP3"

	# Run submodule
	set.seed(1)
	df <- data_local("CAN-07BE001.csv")
	path <- tempdir()

	results <- submodule_03(
		df$max,
		df$year,
		options,
		NULL,
		NULL
	)

	selection <- results[[1]]$selection
	expect_equal(selection$method, "Preset")
	expect_equal(selection$recommendation, "LP3")

})
