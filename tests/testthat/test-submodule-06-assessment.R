test_that("submodule-06-assessment.R works with S-FFA", {

	set.seed(1)
	df <- data_local("CAN-07BE001.csv")
	path <- tempdir()

	ci <- data.frame(
		periods = c(2, 5, 10, 20, 50, 100),
		lower = c(1679, 2361, 2819, 3244, 3763, 4134),
		upper = c(1999, 2885, 3604, 4451, 5842, 7153)
	)
	
	results <- submodule_06(
		df$max,
		df$year,
		"GEV",
		list(ci),
		default_options,
		splits = NULL,
		structures = NULL,
		path = path,
		serialize = TRUE
	)

	# Check that the results are a list
	expect_equal(is.list(results), TRUE)

	# Test model assessment (from test-model-assessment.R)
	assessment <- results[[1]]$assessment
	expect_equal(assessment$metrics$R2      ,   0.9922, tol = 1e-4)
	expect_equal(assessment$metrics$RMSE    ,  95.9225, tol = 1e-4)
	expect_equal(assessment$metrics$bias    , -20.6072, tol = 1e-4)
	expect_equal(assessment$metrics$AIC_RMSE, 471.4811, tol = 1e-4)
	expect_equal(assessment$metrics$BIC_RMSE, 479.3561, tol = 1e-4)
	expect_equal(assessment$metrics$AIC_MLL , 1656.8821, tol = 1e-4)
	expect_equal(assessment$metrics$BIC_MLL , 1664.7570, tol = 1e-4)
	expect_equal(assessment$metrics$AW      , 605.0077, tol = 1e-2)
	expect_equal(assessment$metrics$POC     , 100.0000, tol = 1e-2)
	expect_equal(assessment$metrics$CWI     , 548.2392, tol = 1e-2)

	# Check that images exist
	expect_true(file.exists(file.path(path, "assessment_1913_2020.png")))
	
	# Check that the 'serialize' option works as intended
	expect_true(is.character(assessment$plot))

})

test_that("submodule-06-assessment.R works with NS-FFA", {

	# Run model assessment
	set.seed(1)
	df <- data_local("CAN-07BE001.csv")

	results <- submodule_06(
		df$max,
		df$year,
		"GEV",
		list(NULL),
		default_options,
		splits = NULL,
		structures = list(S10),
	)
	
	# Test model assessment (from test-model-assessment.R)
	assessment <- results[[1]]$assessment
	expect_equal(assessment$metrics$AIC_MLL, 1657.8901, tol = 1e-4)
	expect_equal(assessment$metrics$BIC_MLL, 1668.3900, tol = 1e-4)

})

