validate_results <- function(data, model, ci_lower, estimates, ci_upper, profile = FALSE) {

	start <- Sys.time()
	results <- rfpl.uncertainty(data, model)
	end <- Sys.time()

	if (profile) print(end - start)

	expect_equal(results$estimates, estimates, tol = 1e-2)
	expect_equal( results$ci_lower,  ci_lower, tol = 1e-2)
	expect_equal( results$ci_upper,  ci_upper, tol = 1e-2)
}

test_that("Test rfpl.uncertainty.R on data set #1", {

	# Load dataset and run RFPL uncertainty quantification
	df <- load_data("Application_1.csv", clean = FALSE)


	# Gumbel (GUM) Distribution
	validate_results(
		df$max,
		"GUM",
		c(1741.2485, 2426.5020, 2867.1961, 3286.3350, 3826.2831, 4229.8251),
		c(1886.7114, 2640.7334, 3139.9616, 3618.8332, 4238.6834, 4703.1739),
		c(2045.9211, 2895.6589, 3470.6316, 4025.8960, 4747.4303, 5289.3077)
	)

	# Normal (NOR) Distribution
	validate_results(
		df$max,
		"NOR",
		c(1860.2251, 2613.6934, 2988.8282, 3292.8548, 3631.0182, 3854.8348),
		c(2039.1863, 2808.0081, 3209.8849, 3541.7617, 3915.2880, 4164.3074),
		c(2218.1475, 3030.2831, 3472.1095, 3842.3210, 4262.8162, 4544.7343)
	)

	# Log-Normal (LNO) Distribution
	validate_results(
		df$max,
		"LNO",
		c(1709.2364, 2441.4758, 2915.7406, 3366.9094, 3951.2060, 4392.6614),
		c(1860.2973, 2676.5999, 3237.2290, 3787.7181, 4520.0296, 5085.3099),
		c(2024.7022, 2973.5043, 3664.9768, 4366.7253, 5328.1280, 6088.5451)
	)

	# Generalized Extreme Value (GEV) Distribution
	validate_results(
		df$max,
		"GEV",
		c(1718.8134, 2426.0784, 2884.1104, 3318.2255, 3850.3142, 4236.9194),
		c(1867.4305, 2646.4788, 3187.5978, 3726.4642, 4453.9792, 5022.2453),
		c(2032.0309, 2920.7517, 3628.0105, 4403.9085, 5543.9541, 6565.5664)
	)

	# Generalized Logistic (GLO) Distribution
	validate_results(
		df$max,
		"GLO",
		c(1708.2399, 2346.9592, 2786.4139, 3239.1327, 3885.4426, 4422.7280),
		c(1852.1263, 2569.8245, 3109.8086, 3705.6535, 4625.6655, 5451.8178),
		c(2008.6903, 2856.5119, 3564.5075, 4401.3459, 5787.5432, 7115.5793)
	)

	# Generalized Normal (GNO) Distribution
	validate_results(
		df$max,
		"GNO",
		c(1729.1479, 2448.4230, 2893.6878, 3299.6882, 3800.9875, 4163.0932),
		c(1885.3745, 2671.5548, 3189.4365, 3684.5420, 4326.3371, 4810.6842),
		c(2052.8196, 2948.8727, 3592.0685, 4242.6101, 5130.4565, 5830.0028)
	)

	# Pearson Type III (PE3) Distribution
	validate_results(
		df$max,
		"PE3",
		c(1754.3914, 2486.4995, 2917.4085, 3299.0242, 3757.7884, 4081.5189),
		c(1914.9203, 2704.0830, 3194.0743, 3641.8024, 4194.7134, 4593.3554),
		c(2084.5699, 2969.3950, 3551.7782, 4100.0220, 4793.1669, 5301.7793)
	)

	# Log-Pearson Type III (LP3) Distribution
	validate_results(
		df$max,
		"LP3",
		c(1718.8998, 2450.5955, 2911.0602, 3330.1357, 3838.6974, 4195.3573),
		c(1877.7326, 2681.9872, 3214.6549, 3723.5453, 4380.8069, 4874.1504),
		c(2051.0201, 2966.0667, 3631.4168, 4322.5733, 5295.8241, 6086.1434)
	)

	# Weibull (WEI) Distribution
	# NOTE: Weibull distribution was not implemented in the MATLAB version
	validate_results(
		df$max,
		"WEI",
		c(1755, 2570, 3013, 3381, 3792, 4065),
		c(1940, 2795, 3281, 3696, 4176, 4502),
		c(2132, 3059, 3620, 4118, 4710, 5124)
	)

})
