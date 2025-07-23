# Define list of valid options
default_options <- list(
	alpha = 0.05,
	bbmk_samples = 10000L,
	window_size = 10L,
	window_step = 5L,
	selection = "L-distance",
	z_samples = 10000L,
	s_estimation = "L-moments",
	ns_estimation = "MLE",
	gev_prior = c(6, 9),
	s_uncertainty = "Bootstrap",
	ns_uncertainty = "RFPL",
	return_periods = c(2L, 5L, 10L, 20L, 50L, 100L),
	slices = c(1925L, 1975L, 2025L),
	sb_samples = 1000L,
	rfpl_tolerance = 0.01,
	pp_formula = "Weibull"
)

test_that("validate_config helper function identifies errors in keyword arguments", {

	# Valid configuration passes
	options <- validate_config(default_options)
    expect_type(options, "list")

	# Unexpected field throws error
    options_01 <- default_options
    options_01$extra_field <- "invalid"
    expect_error(validate_config(options_01), "Unknown options: 'extra_field'")

	# Type mismatch throws error
    options_02 <- default_options
    options_02$selection <- 10
    expect_error(validate_config(options_02), "Invalid type for 'selection'")

    # Invalid length throws error
    options_03 <- default_options
    options_03$alpha <- c(0.02, 0.05)
    expect_error(validate_config(options_03), "'alpha' must have length 1, got 2")

	# Invalid enumeration value throws error
    options_04 <- default_options
    options_04$selection <- "random"
    expect_error(validate_config(options_04), "must be one of")

	# Invalid numeric value throws error
    options_05 <- default_options
    options_05$alpha <- 0.001
    expect_error(validate_config(options_05), "'alpha' must be within")

	# Cross-field constraint: GMLE requires GEV
    options_06 <- default_options
    options_06$selection <- "GLO"
    options_06$s_estimation <- "GMLE"
    expect_error(validate_config(options_06), "s_estimation: 'GMLE'.*selection: 'GEV'")

    options_07 <- default_options
    options_07$selection <- "GLO"
    options_07$ns_estimation <- "GMLE"
    expect_error(validate_config(options_07), "ns_estimation: 'GMLE'.*selection: 'GEV'")

    # Cross-field constraint: RFPL requires MLE
    options_08 <- default_options
    options_08$selection <- "GEV"
    options_08$s_estimation <- "GMLE"
    options_08$s_uncertainty <- "RFPL"
    expect_error(validate_config(options_08), "s_uncertainty: 'RFPL'.*s_estimation: 'MLE'")

    options_09 <- default_options
    options_09$selection <- "GEV"
    options_09$ns_estimation <- "GMLE"
    options_09$ns_uncertainty <- "RFPL"
    expect_error(validate_config(options_09), "ns_uncertainty: 'RFPL'.*s_estimation: 'MLE'")

	# Cross-field constraint: RFGPL requires GMLE
    options_10 <- default_options
    options_10$s_estimation <- "MLE"
    options_10$s_uncertainty <- "RFGPL"
    expect_error(validate_config(options_10), "s_uncertainty: 'RFGPL'.*s_estimation: 'GMLE'")

    options_11 <- default_options
    options_11$ns_estimation <- "MLE"
    options_11$ns_uncertainty <- "RFGPL"
    expect_error(validate_config(options_11), "ns_uncertainty: 'RFGPL'.*ns_estimation: 'GMLE'")

})

test_that("helper change_point_detection gives same results as R package on 08NH021", {

	df <- data_local("CAN-08NH021.csv")
	results <- change_point_detection(df$max, df$year, default_options, tempdir())

	# Unpack the results
	expect_true(is.list(results))
	pettitt <- results$items$pettitt
	mks <- results$items$mks
	
	# Test the Pettitt test (copied from the R package)
	expect_equal(length(pettitt$u_t), 91)
  	expect_equal(pettitt$k_statistic, 1871)
	expect_equal(pettitt$k_critical, 616.753, tol = 1e-4)
	expect_equal(pettitt$p_value, 0, tol = 1e-4)
	expect_equal(pettitt$change_index, 45)
	expect_equal(pettitt$change_year, 1972)

	# Test the MKS test (copied from the R package)
	expect_equal(length(mks$s_progressive), 91)
	expect_equal(length(mks$s_regressive), 91)
	expect_equal(nrow(mks$crossing_df), 2)
	expect_equal(nrow(mks$change_df), 2)
	expect_equal(mks$p_value, 0.015, tol = 1e-4)
	expect_equal(mks$crossing_df$cross, c(33, 58))
	expect_equal(mks$crossing_df$statistic, c(2.1805, -2.4335), tol = 1e-4)

})

test_that("helper trend_detection gives same results as R package on 08NM050", {

	set.seed(1)
	df <- data_local("CAN-08NM050.csv")
	partition <- c(min(df$year), max(df$year))
	results <- trend_detection(df$max, df$year, partition, default_options, tempdir())
	expect_equal(is.list(results), TRUE)

	# Check the MK test
	mk <- results$items$mk
	expect_equal(mk$s_statistic, 1822)
	expect_equal(mk$s_variance, 102933, tol = 1e-4)
	expect_equal(mk$p_value, 0, tol = 1e-4)
	expect_equal(mk$reject, TRUE)

	# Check the Spearman test
	spearman <- results$items$spearman
	expect_equal(spearman$least_lag, 5)

	# Check the BB-MK test
	bbmk <- results$items$bbmk
	expect_equal(bbmk$p_value, 8e-6, tol = 5e-2)
	expect_equal(unname(bbmk$bounds), c(-902, 894), tol = 5e-2)

	# Check the PP test
	pp <- results$items$pp
	expect_equal(pp$statistic, -90.0663, tol = 1e-4)
	expect_equal(pp$p_value, 0.01, tol = 1e-4)
	expect_equal(pp$reject, TRUE)

	# Check the KPSS test
	kpss <- results$items$kpss
	expect_equal(kpss$statistic, 0.0381, tol = 1e-4)
	expect_equal(kpss$p_value, 0.10, tol = 1e-4)
	expect_equal(kpss$reject, FALSE)

	# Check Sen's trend estimator 
	sens <- results$items$sens_mean
  	expect_equal(sens$slope, 40.3939, tol = 1e-4)
	expect_equal(sens$intercept, 11.2479, tol = 1e-4)

	# Check the runs test
	runs <- results$items$runs_mean
	expect_equal(runs$n, 96)
  	expect_equal(runs$n_plus, 48)
  	expect_equal(runs$n_minus, 48)
	expect_equal(runs$runs, 47)
	expect_equal(runs$statistic, -0.41041, tol = 1e-4)
	expect_equal(runs$p_value, 0.6815, tol = 1e-4)

	# Check the MW-MK test
	expect_true(is.list(results$items$mwmk))

	# Check the White test
	white <- results$items$white
	expect_equal(white$r_squared, 0.0415, tol = 1e-4)
	expect_equal(white$statistic, 4.0246, tol = 1e-4)
	expect_equal(white$p_value, 0.1337, tol = 1e-4)
	
})

test_that("helper frequency_analysis works with L-distance/S-FFA", {

	set.seed(1)
	df <- data_local("CAN-07BE001.csv")
	partition <- c(min(df$year), max(df$year))
	structure <- list(location = FALSE, scale = FALSE)
	options <- default_options
	results <- frequency_analysis(df$max, df$year, partition, structure, options, tempdir())

	# Test L-distance selection
	selection <- results$items$selection
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

	# Test L-moments parameter estimation
	params <- results$items$estimation$params 
	expect_equal(params[1], 1600.2199, tol = 1e-3)
	expect_equal(params[2],  616.6660, tol = 1e-3)
	expect_equal(params[3],    0.1207, tol = 1e-3)

	# Test S-bootstrap uncertainty quantification
	uncertainty <- results$items$uncertainty[[1]]
	expect_equal(uncertainty$ci_lower , c(1679, 2361, 2819, 3244, 3763, 4134), tol = 1e-2)
	expect_equal(uncertainty$estimates, c(1831, 2614, 3195, 3803, 4674, 5393), tol = 1e-2)
	expect_equal(uncertainty$ci_upper , c(1999, 2885, 3604, 4451, 5842, 7153), tol = 1e-2)

	# Test model assessment
	assessment <- results$items$assessment
	expect_equal(assessment$R2  ,   0.9922, tol = 1e-4)
	expect_equal(assessment$RMSE,  95.9225, tol = 1e-4)
	expect_equal(assessment$bias, -20.6072, tol = 1e-4)
	expect_equal(assessment$AIC , 471.4811, tol = 1e-4)
	expect_equal(assessment$BIC , 479.3561, tol = 1e-4)
	expect_equal(assessment$MLL_AIC, 1656.8821, tol = 1e-4)
	expect_equal(assessment$MLL_BIC, 1664.7570, tol = 1e-4)
	expect_equal(assessment$AW  , 622.0077, tol = 1e-2)
	expect_equal(assessment$POC , 100.0000, tol = 1e-2)
	expect_equal(assessment$CWI , 563.6239, tol = 1e-2)

})

test_that("L-kurtosis selection", {

	# Update configuration
	options <- default_options 
	options$selection <- "L-kurtosis"

	# Run frequency analysis
	set.seed(1)
	df <- data_local("CAN-07BE001.csv")
	partition <- c(min(df$year), max(df$year))
	structure <- list(location = FALSE, scale = FALSE)
	results <- frequency_analysis(df$max, df$year, partition, structure, options, tempdir())

	# Test L-kurtosis selection
	selection <- results$items$selection
	expect_equal(selection$metrics$GEV, 0.0103, tol = 1e-3) 
	expect_equal(selection$metrics$GLO, 0.0215, tol = 1e-3) 
	expect_equal(selection$metrics$PE3, 0.0525, tol = 1e-3) 
	expect_equal(selection$metrics$LP3, 0.0470, tol = 1e-3) 
	expect_equal(selection$metrics$GNO, 0.0254, tol = 1e-3) 
	expect_equal(selection$metrics$WEI, 0.0642, tol = 1e-3) 
	expect_equal(selection$recommendation, "GEV") 

})


test_that("Z-statistic selection", {

	# Update configuration
	options <- default_options
	options$selection <- "Z-statistic"
	options$z_samples <- 25000L

	# Run frequency analysis
	set.seed(1)
	df <- data_local("CAN-07BE001.csv")
	partition <- c(min(df$year), max(df$year))
	structure <- list(location = FALSE, scale = FALSE)
	results <- frequency_analysis(df$max, df$year, partition, structure, options, tempdir())

	# Test Z-statistic selection
	selection <- results$items$selection
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

test_that("Preset selection", {

	# Update configuration
	options <- default_options
	options$selection <- "GEV"

	# Run frequency analysis
	set.seed(1)
	df <- data_local("CAN-07BE001.csv")
	partition <- c(min(df$year), max(df$year))
	structure <- list(location = FALSE, scale = FALSE)
	results <- frequency_analysis(df$max, df$year, partition, structure, options, tempdir())

	# Test Preset selection
	selection <- results$items$selection
	expect_equal(selection$method, "Preset")
	expect_equal(selection$recommendation, "GEV")

})

test_that("Non-stationary estimation with MLE/RFPL", {

	# Update configuration
	options <- default_options
	options$selection <- "GUM"
	options$slices <- 1913L

	# Run frequency analysis
	set.seed(1)
	df <- data_local("CAN-07BE001.csv")
	partition <- c(min(df$year), max(df$year))
	structure <- list(location = TRUE, scale = FALSE)
	results <- frequency_analysis(df$max, df$year, partition, structure, options, tempdir())

	# Test MLE estimation
	estimation <- results$items$estimation
	expect_equal(estimation$params, c(1521.3786, 178.9614, 661.2450), tol = 1e-3)
	expect_equal(estimation$mll, -825.3975, tol = 1e-3)

	# Test RFPL uncertainty
	uncertainty <- results$items$uncertainty[[1]]
	expect_equal(
		uncertainty$ci_lower,
		c(1522.3984, 2230.8239, 2687.2499, 3117.1193, 3665.8628, 4073.3393),
		tol = 1e-2
	)
	expect_equal(
		uncertainty$estimates,
		c(1786.9985, 2536.4715, 3032.6878, 3508.6704, 4124.7811, 4586.4694),
		tol = 1e-2
	)
	expect_equal(
		uncertainty$ci_upper,
		c(2059.8899, 2878.7089, 3436.0561, 3977.4988, 4684.6410, 5217.6126),
		tol = 1e-2
	)

})

test_that("Non-stationary estimation with GMLE/RFGPL", {

	# Update configuration
	options <- default_options
	options$selection <- "GEV"
	options$ns_estimation <- "GMLE"
	options$ns_uncertainty <- "RFGPL"
	options$gev_prior <- c(6, 9)
	options$slices <- 1913L

	# Run frequency analysis
	set.seed(1)
  df <- data_local("CAN-07BE001.csv")
	partition <- c(min(df$year), max(df$year))
	structure <- list(location = TRUE, scale = FALSE)
	results <- frequency_analysis(df$max, df$year, partition, structure, options, tempdir())

	# Test MLE estimation
	estimation <- results$items$estimation
	expect_equal(estimation$params, c(1459.6527, 215.2392, 646.2991, 0.1135), tol = 1e-3)
	expect_equal(estimation$mll, -709.2092, tol = 1e-3)

	# Test RFPL uncertainty
	uncertainty <- results$items$uncertainty[[1]]
	expect_equal(
		uncertainty$ci_lower,
		c(1474.8399, 2230.1006, 2766.5121, 3314.3495, 4078.8225, 4696.9330),
		tol = 1e-2
	)
	expect_equal(
		uncertainty$estimates,
		c(1729.5074, 2544.4615, 3144.6884, 3770.5317, 4660.3977, 5391.7103),
		tol = 1e-2
	)
	expect_equal(
		uncertainty$ci_upper,
		c(1994.6171, 2904.0166, 3592.7090, 4322.1180, 5358.6528, 6236.7929),
		tol = 1e-2
	)

})

test_that("Automatic split point selection behaves as intended", {

	# Create mock Pettitt test objects
	pettitt_fail <- list(reject = F, p_value = 0.2)
	pettitt_reject <- list(reject = T, p_value = 0.01, change_year = 1950)

	# Create mock MKS test objects
	mks_fail <- list(reject = F, p_value = 0.3)
	change_low <- data.frame(year = c(1900, 2000))
	mks_reject_low <- list(reject = T, p_value = 0.005, change_df = change_low)
	change_high <- data.frame(year = c(1901, 1999))
	mks_reject_high <- list(reject = T, p_value = 0.015, change_df = change_high)

	# No split points
	change01 <- list(items = list(pettitt = pettitt_fail, mks = mks_fail))
	expect_equal(splits_automatic(change01), numeric(0))

	# Only split points in Pettitt test
	change02 <- list(items = list(pettitt = pettitt_reject, mks = mks_fail))
	expect_equal(splits_automatic(change02), 1950)

	# Only split points in MKS test
	change03 <- list(items = list(pettitt = pettitt_fail, mks = mks_reject_high))
	expect_equal(splits_automatic(change03), c(1901, 1999))

	# Most significant split point in Pettitt test
	change04 <- list(items = list(pettitt = pettitt_reject, mks = mks_reject_high))
	expect_equal(splits_automatic(change04), 1950)

	# Most significant split points in MKS test
	change05 <- list(items = list(pettitt = pettitt_reject, mks = mks_reject_low))
	expect_equal(splits_automatic(change05), c(1900, 2000))
	
})

test_that("Automatic nonstationary structure selection behaves as intended", {

	# Create mock trend_results objects
	trend01 <- list(items = list())
	trend02 <- list(items = list(sens_mean = T))
	trend03 <- list(items = list(sens_variance = T))
	trend04 <- list(items = list(sens_mean = T, sens_variance = T))

	# Run tests
	expect_equal(structures_automatic(trend01), S00)
	expect_equal(structures_automatic(trend02), S10)
	expect_equal(structures_automatic(trend03), S01)
	expect_equal(structures_automatic(trend04), S11)

})

test_that("The entire framework can be ran in automatic mode", {

  	df <- data_local("CAN-07BE001.csv")
	results <- ffaframework(df$max, df$year, automatic = TRUE)

	# Check the contents of results
	expect_true("summary" %in% names(results))
	expect_true("blocks" %in% names(results))

	# Check the contents of summary
	summary <- results$summary
	expect_equal(summary$splits, numeric(0))
	expect_equal(summary$periods, list(c(1913, 2020)))
	expect_equal(summary$structures, list(list(location = F, scale = F)))

	# Check the names of the blocks 
	blocks <- results$blocks
	expect_true(length(blocks) == 3)
	expect_equal(blocks[[1]]$name, "Change Points")
	expect_equal(blocks[[2]]$name, "Trend Detection")
	expect_equal(blocks[[3]]$name, "Frequency Analysis")

})
