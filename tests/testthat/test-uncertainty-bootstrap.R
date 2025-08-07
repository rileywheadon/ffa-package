# NOTE: Profiling code:
# Rprof()
# Rprof(NULL)
# print(summaryRprof())

# NOTE: Tolerance is high due to randomness in the bootstrap
test_that("basic functionality is working for CRAN", {
	set.seed(1)
	df <- data_local("CAN-07BE001.csv")

	# Generalized Extreme Value (GEV) Distribution
	GEV <- uncertainty_bootstrap(df$max, "GEV", "L-moments")$ci
	expect_equal(GEV$lower, c(1679, 2361, 2819, 3244, 3763, 4134), tol = 1e-2)
	expect_equal(GEV$upper, c(1999, 2885, 3604, 4451, 5842, 7153), tol = 1e-2)

	# Gumbel (GUM) Distribution
	GUM <- uncertainty_bootstrap(df$max, "GUM", "L-moments")$ci
	expect_equal(GUM$lower, c(1736, 2435, 2884, 3310, 3858, 4268), tol = 1e-2)
	expect_equal(GUM$upper, c(2056, 2951, 3561, 4151, 4917, 5491), tol = 1e-2)

})

test_that("uncertainty-bootstrap.R works with L-moments", {
	skip_on_cran()
	set.seed(1)
	df <- data_local("CAN-07BE001.csv")

	# Normal (NOR) Distribution
	NOR <- uncertainty_bootstrap(df$max, "NOR", "L-moments")$ci
	expect_equal(NOR$lower, c(1877, 2567, 2913, 3191, 3502, 3711), tol = 1e-2)
	expect_equal(NOR$upper, c(2201, 2955, 3365, 3710, 4101, 4364), tol = 1e-2)

	# Log-Normal (LNO) Distribution
	LNO <- uncertainty_bootstrap(df$max, "LNO", "L-moments")$ci
	expect_equal(LNO$lower, c(1713, 2417, 2868, 3296, 3844, 4260), tol = 1e-2)
	expect_equal(LNO$upper, c(2014, 2932, 3601, 4280, 5210, 5944), tol = 1e-2)

	# Generalized Logistic (GLO) Distribution
	GLO <- uncertainty_bootstrap(df$max, "GLO", "L-moments")$ci
	expect_equal(GLO$lower, c(1702, 2326, 2743, 3155, 3730, 4203), tol = 1e-2)
	expect_equal(GLO$upper, c(2003, 2827, 3557, 4480, 6129, 7847), tol = 1e-2)

	# Pearson Type III (PE3) Distribution
	PE3 <- uncertainty_bootstrap(df$max, "PE3", "L-moments")$ci
	expect_equal(PE3$lower, c(1660, 2402, 2873, 3302, 3817, 4195), tol = 1e-2)
	expect_equal(PE3$upper, c(2005, 2949, 3658, 4388, 5386, 6154), tol = 1e-2)

	# Generalized Normal (GNO) Distribution
	GNO <- uncertainty_bootstrap(df$max, "GNO", "L-moments")$ci
	expect_equal(GNO$lower, c(1670, 2377, 2840, 3269, 3799, 4200), tol = 1e-2)
	expect_equal(GNO$upper, c(1998, 2912, 3640, 4455, 5678, 6753), tol = 1e-2)

	# # Weibull (WEI) Distribution
	WEI <- uncertainty_bootstrap(df$max, "WEI", "L-moments")$ci
	expect_equal(WEI$lower, c(1650, 2424, 2907, 3326, 3813, 4148), tol = 1e-2)
	expect_equal(WEI$upper, c(2010, 2980, 3675, 4368, 5286, 5979), tol = 1e-2)

	# Load data from KOOTENAI RIVER (08NH021)
	df <- data_local("CAN-08NH021.csv")

	# Generalized Extreme Value (GEV) Distribution
	GEV <- uncertainty_bootstrap(df$max, "GEV", "L-moments")$ci
	expect_equal(GEV$lower, c(1440, 2034, 2373, 2646, 2930, 3100), tol = 1e-2)
	expect_equal(GEV$upper, c(1749, 2440, 2889, 3336, 3950, 4439), tol = 1e-2)

	# Gumbel (GUM) Distribution
	GUM <- uncertainty_bootstrap(df$max, "GUM", "L-moments")$ci
	expect_equal(GUM$lower, c(1420, 1989, 2353, 2699, 3144, 3477), tol = 1e-2)
	expect_equal(GUM$upper, c(1696, 2435, 2937, 3423, 4055, 4530), tol = 1e-2)

	# Normal (NOR) Distribution
	NOR <- uncertainty_bootstrap(df$max, "NOR", "L-moments")$ci
	expect_equal(NOR$lower, c(1531, 2098, 2381, 2611, 2865, 3033), tol = 1e-2)
	expect_equal(NOR$upper, c(1819, 2436, 2773, 3055, 3378, 3593), tol = 1e-2)

	# Log-Normal (LNO) Distribution
	LNO <- uncertainty_bootstrap(df$max, "LNO", "L-moments")$ci
	expect_equal(LNO$lower, c(1400, 1973, 2340, 2686, 3131, 3465), tol = 1e-2)
	expect_equal(LNO$upper, c(1667, 2423, 2975, 3537, 4307, 4917), tol = 1e-2)

	# Generalized Logistic (GLO) Distribution
	GLO <- uncertainty_bootstrap(df$max, "GLO", "L-moments")$ci
	expect_equal(GLO$lower, c(1461, 1991, 2303, 2586, 2940, 3199), tol = 1e-2)
	expect_equal(GLO$upper, c(1745, 2384, 2865, 3407, 4252, 5019), tol = 1e-2)

	# Log-Pearson Type III (LP3) Distribution
	LP3 <- uncertainty_bootstrap(df$max, "LP3", "L-moments")$ci
	expect_equal(LP3$lower, c(1419, 2027, 2383, 2675, 2981, 3171), tol = 1e-2)
	expect_equal(LP3$upper, c(1732, 2465, 2961, 3469, 4189, 4779), tol = 1e-2)

	# Generalized Normal (GNO) Distribution
	GNO <- uncertainty_bootstrap(df$max, "GNO", "L-moments")$ci
	expect_equal(GNO$lower, c(1440, 2033, 2364, 2636, 2943, 3147), tol = 1e-2)
	expect_equal(GNO$upper, c(1751, 2436, 2891, 3345, 3956, 4434), tol = 1e-2)

	# Weibull (WEI) Distribution
	WEI <- uncertainty_bootstrap(df$max, "WEI", "L-moments")$ci
	expect_equal(WEI$lower, c(1430, 2058, 2391, 2657, 2944, 3131), tol = 1e-2)
	expect_equal(WEI$upper, c(1760, 2462, 2896, 3296, 3790, 4141), tol = 1e-2)

})

test_that("uncertainty-bootstrap.R works on BOW RIVER (05BB001) with MLE", {
	skip_on_cran()
	set.seed(1)
	df <- data_local("CAN-05BB001.csv")

	# Generalized Extreme Value (GEV) Distribution
	GEV <- uncertainty_bootstrap(df$max, "GEV", "MLE", samples = 1000L)$ci
	expect_equal(GEV$lower, c(191, 241, 270, 295, 325, 344), tol = 1e-2)
	expect_equal(GEV$upper, c(213, 273, 314, 362, 430, 488), tol = 1e-2)

	# Gumbel (GUM) Distribution
	GUM <- uncertainty_bootstrap(df$max, "GUM", "MLE", samples = 1000L)$ci
	expect_equal(GUM$lower, c(192, 242, 273, 302, 341, 370), tol = 1e-2)
	expect_equal(GUM$upper, c(212, 272, 313, 351, 402, 440), tol = 1e-2)

	# Normal (NOR) Distribution
	NOR <- uncertainty_bootstrap(df$max, "NOR", "MLE", samples = 1000L)$ci
	expect_equal(NOR$lower, c(201, 251, 276, 296, 318, 333), tol = 1e-2)
	expect_equal(NOR$upper, c(223, 277, 305, 329, 357, 376), tol = 1e-2)

	# Log-Normal (LNO) Distribution
	LNO <- uncertainty_bootstrap(df$max, "LNO", "MLE", samples = 1000L)$ci
	expect_equal(LNO$lower, c(194, 243, 272, 298, 329, 352), tol = 1e-2)
	expect_equal(LNO$upper, c(214, 273, 311, 346, 393, 427), tol = 1e-2)

	# Generalized Logistic (GLO) Distribution
	# NOTE: No tests since this distribution doesn't work in MATLAB

	# Pearson Type III (PE3) Distribution
	# NOTE: No tests since this distribution doesn't work in MATLAB

	# Log-Pearson Type III (LP3) Distribution
	LP3 <- uncertainty_bootstrap(df$max, "LP3", "MLE", samples = 1000L)$ci
	expect_equal(LP3$lower, c(191, 241, 271, 297, 326, 348), tol = 1e-2)
	expect_equal(LP3$upper, c(213, 273, 317, 365, 433, 492), tol = 1e-2)

	# Generalized Normal (GNO) Distribution
	GNO <- uncertainty_bootstrap(df$max, "GNO", "MLE", samples = 1000L)$ci
	expect_equal(GNO$lower, c(190, 241, 271, 297, 328, 350), tol = 1e-2)
	expect_equal(GNO$upper, c(214, 275, 320, 365, 429, 484), tol = 1e-2)

	# Weibull (WEI) Distribution
	# NOTE: No tests since this distribution doesn't work in MATLAB.

})

test_that("uncertainty-bootstrap.R works on ATHABASCA RIVER (07BE001) with non-stationary MLE", {
	set.seed(1)
	df <- data_local("CAN-07BE001.csv")

	# Generalized Extreme Value (GEV) Distribution
	GEV <- uncertainty_bootstrap(
		df$max,
		"GEV",
		"MLE",
		ns_years = df$year,
		ns_structure = S10, 
		ns_slices = c(min(df$year), max(df$year)),
		samples = 2000L
	)$ci_list

	expect_equal(GEV[[1]]$lower, c(1494, 2205, 2665, 3067, 3530, 3848), tol = 1e-2)
	expect_equal(GEV[[1]]$upper, c(2045, 2861, 3497, 4234, 5420, 6483), tol = 1e-2)
	expect_equal(GEV[[2]]$lower, c(1706, 2414, 2860, 3267, 3734, 4049), tol = 1e-2)
	expect_equal(GEV[[2]]$upper, c(2224, 3051, 3689, 4434, 5624, 6686), tol = 1e-2)

})

test_that("uncertainty-bootstrap.R works on ATHABASCA RIVER (07BE001) with GMLE", {
	set.seed(1)
	df <- data_local("CAN-07BE001.csv")

	# Generalized Extreme Value (GEV) Distribution
	GEV <- uncertainty_bootstrap(
		df$max,
		"GEV",
		"GMLE",
		prior = c(6, 9),
		ns_years = df$year,
		ns_structure = S10, 
		ns_slices = c(min(df$year), max(df$year)),
		samples = 2000L
	)$ci_list

	expect_equal(GEV[[1]]$lower, c(1484, 2223, 2743, 3285, 4071, 4697), tol = 1e-2)
	expect_equal(GEV[[1]]$upper, c(2023, 2922, 3575, 4286, 5309, 6148), tol = 1e-2)
	expect_equal(GEV[[2]]$lower, c(1703, 2423, 2954, 3507, 4277, 4887), tol = 1e-2)
	expect_equal(GEV[[2]]$upper, c(2239, 3119, 3792, 4510, 5520, 6350), tol = 1e-2)

})

test_that("convergence errors are caught", {

	set.seed(1)
	df <- data_local("CAN-08NH021.csv")
	df <- subset(df, year >= 1985)

	expect_error(uncertainty_bootstrap(
		df$max,
		"WEI",
		"MLE",
		ns_years = df$year,
		ns_structure = S11, 
		ns_slices = 2000,
		samples = 2000L
	), "Bootstrap uncertainty quantification failed to converge")

})
