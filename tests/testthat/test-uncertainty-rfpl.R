# NOTE: Profiling code:
# Rprof()
# Rprof(NULL)
# print(summaryRprof())

validate_rfpl <- function(
    df,
    distribution,
    structure,
    ci_lower,
    ci_upper,
	slices = NULL
) {

	results <- uncertainty_rfpl(
		df$max,
		distribution,
		ns_years = df$year,
		ns_structure = structure,
		ns_slices = slices
	)$results

	if (!is.data.frame(results)) {
		results <- results[[1]]
	}

	# NOTE: We check the estimates in test-mle-estimation.R
	expect_equal(results$ci_lower, ci_lower, tol = 1e-2)
	expect_equal(results$ci_upper, ci_upper, tol = 1e-2)

}

test_that("basic functionality is working for CRAN", { 

	# Load dataset and run FFPL uncertainty quantification
	df <- data_local("CAN-07BE001.csv")

	# Mean-Trend Gumbel (GUM10) Distribution
	validate_rfpl(
		df, "GUM", S10, 
		c(1522, 2230, 2687, 3117, 3665, 4073),
		c(2059, 2878, 3436, 3977, 4684, 5217),
		slices = min(df$year)
	)

	validate_rfpl(
		df, "GUM", S10,
		c(1726, 2440, 2892, 3320, 3868, 4274),
		c(2233, 3053, 3612, 4154, 4862, 5396),
		slices = max(df$year)
	)

	# NOTE: The Weibull distribution was not implemented in the original framework.

	# Weibull (WEI) Distribution
	validate_rfpl(
		df, "WEI", S00,
		c(1755, 2570, 3013, 3381, 3792, 4065),
		c(2132, 3059, 3620, 4118, 4710, 5124)
	)

	# Mean-Trend Weibull (WEI100) Distribution
	validate_rfpl(
		df, "WEI", S10,
		c(1931, 2753, 3210, 3595, 4031, 4324),
		c(2425, 3384, 4001, 4570, 5268, 5766),
		slices = max(df$year)
	)

	# Mean/Variance Trend (WEI110) Distribution
	validate_rfpl(
		df, "WEI", S11,
		c(1767, 2370, 2726, 3034, 3394, 3642),
		c(2393, 3355, 3951, 4486, 5139, 5600),
		slices = max(df$year)
	)

})


test_that("convergence errors are caught.", {
	set.seed(2)

	# Load dataset and run RFPL uncertainty quantification
	df <- data_local("CAN-05BA001.csv")

	expect_error(uncertainty_rfpl(
		df$max, 
		"GLO",
		ns_years = df$year,
		ns_structure = S01,
		ns_slices = 1900
	), "RFPL/RFGPL uncertainty quantification failed to converge.")

	# Similar error in KOOTENAI RIVER (08NH021)
	df <- data_local("CAN-08NH021.csv")
	df <- subset(df, year >= 1985)

	expect_error(uncertainty_rfpl(
		df$max,
		"WEI",
		ns_years = df$year,
		ns_structure = S11,
		ns_slices = 2000
	), "RFPL/RFGPL uncertainty quantification failed to converge.")

})


test_that("RFPL uncertainty works on ATHABASCA RIVER (07BE001)", {
	skip_on_cran()

	# Load dataset and run RFPL uncertainty quantification
	df <- data_local("CAN-07BE001.csv")

	# Mean + Variance Gumbel (GUM11) Distribution
	validate_rfpl(
		df, "GUM", S11,
		c(1487, 2108, 2493, 2869, 3327, 3671),
		c(2138, 3108, 3773, 4418, 5250, 5863),
		slices = min(df$year)
	)

	validate_rfpl(
		df, "GUM", S11,
		c(1686, 2283, 2653, 3003, 3457, 3809),
		c(2293, 3248, 3887, 4525, 5354, 5976),
		slices = max(df$year)
	)

	# Gumbel (GUM) Distribution
	validate_rfpl(
		df, "GUM", S00,
		c(1741, 2426, 2867, 3286, 3826, 4229),
		c(2045, 2895, 3470, 4025, 4747, 5289)
	)

	# Normal (NOR) Distribution
	validate_rfpl(
		df, "NOR", S00,
		c(1860, 2613, 2988, 3292, 3631, 3854),
		c(2218, 3030, 3472, 3842, 4262, 4544)
	)

	# Log-Normal (LNO) Distribution
	validate_rfpl(
		df, "LNO", S00,
		c(1709, 2441, 2915, 3366, 3951, 4392),
		c(2024, 2973, 3664, 4366, 5328, 6088)
	)

	# Generalized Extreme Value (GEV) Distribution
	validate_rfpl(
		df, "GEV", S00,
		c(1718, 2426, 2884, 3318, 3850, 4236),
		c(2032, 2920, 3628, 4403, 5543, 6565)
	)

	# Generalized Logistic (GLO) Distribution
	validate_rfpl(
		df, "GLO", S00,
		c(1708, 2346, 2786, 3239, 3885, 4422),
		c(2008, 2856, 3564, 4401, 5787, 7115)
	)

	# Generalized Normal (GNO) Distribution
	validate_rfpl(
		df, "GNO", S00,
		c(1729, 2448, 2893, 3299, 3800, 4163),
		c(2052, 2948, 3592, 4242, 5130, 5830)
	)

	# Pearson Type III (PE3) Distribution
	validate_rfpl(
		df, "PE3", S00,
		c(1754, 2486, 2917, 3299, 3757, 4081),
		c(2084, 2969, 3551, 4100, 4793, 5301)
	)

	# Mean-Trend Pearson Type III (PE3100) Distribution
	validate_rfpl(
		df, "PE3", S10, 
		c(1442, 2211, 2679, 3093, 3585, 3929),
		c(2034, 2879, 3441, 3982, 4700, 5229),
		slices = min(df$year)
	)

	validate_rfpl(
		df, "PE3", S10,
		c(1803, 2553, 2999, 3394, 3867, 4201),
		c(2312, 3205, 3809, 4397, 5144, 5701),
		slices = max(df$year)
	)

	# Mean + Variance Pearson Type III (PE3110) Distribution
	validate_rfpl(
		df, "PE3", S11,
		c(1447, 2173, 2593, 2965, 3414, 3741),
		c(2154, 3277, 4067, 4811, 5772, 6489),
		slices = min(df$year)
	)

	validate_rfpl(
		df, "PE3", S11,
		c(1718, 2311, 2674, 3019, 3425, 3719),  
		c(2324, 3271, 3873, 4466, 5204, 5750),
		slices = max(df$year)
	)

	# Log-Pearson Type III (LP3) Distribution
	validate_rfpl(
		df, "LP3", S00,
		c(1718, 2450, 2911, 3330, 3838, 4195),
		c(2051, 2966, 3631, 4322, 5295, 6086)
	)

})
