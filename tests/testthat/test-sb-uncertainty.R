# NOTE: Profiling code:
# Rprof()
# Rprof(NULL)
# print(summaryRprof())

# NOTE: Tolerance is high due to randomness in the bootstrap
test_that("Test s-bootstrap.R on data set #1 with L-moments", {
	set.seed(1)

	# Load dataset
	df <- load_data("Application_1.csv")

	# Generalized Extreme Value (GEV) Distribution
	GEV <- sb.uncertainty(df$max, df$year, "GEV", "L-moments")[[1]]
	expect_equal(GEV$ci_lower , c(1679, 2361, 2819, 3244, 3763, 4134), tol = 1e-2)
	expect_equal(GEV$estimates, c(1831, 2614, 3195, 3803, 4674, 5393), tol = 1e-2)
	expect_equal(GEV$ci_upper , c(1999, 2885, 3604, 4451, 5842, 7153), tol = 1e-2)

	# Gumbel (GUM) Distribution
	GUM <- sb.uncertainty(df$max, df$year, "GUM", "L-moments")[[1]]
	expect_equal(GUM$ci_lower , c(1736, 2435, 2884, 3310, 3858, 4268), tol = 1e-2)
	expect_equal(GUM$estimates, c(1892, 2684, 3208, 3711, 4361, 4849), tol = 1e-2)
	expect_equal(GUM$ci_upper , c(2056, 2951, 3561, 4151, 4917, 5491), tol = 1e-2)

	# Normal (NOR) Distribution
	NOR <- sb.uncertainty(df$max, df$year, "NOR", "L-moments")[[1]]
	expect_equal(NOR$ci_lower , c(1877, 2567, 2913, 3191, 3502, 3711), tol = 1e-2)
	expect_equal(NOR$estimates, c(2039, 2761, 3139, 3451, 3802, 4036), tol = 1e-2)
	expect_equal(NOR$ci_upper , c(2201, 2955, 3365, 3710, 4101, 4364), tol = 1e-2)

	# Log-Normal (LNO) Distribution
	LNO <- sb.uncertainty(df$max, df$year, "LNO", "L-moments")[[1]]
	expect_equal(LNO$ci_lower , c(1713, 2417, 2868, 3296, 3844, 4260), tol = 1e-2)
	expect_equal(LNO$estimates, c(1861, 2667, 3218, 3759, 4476, 5029), tol = 1e-2)
	expect_equal(LNO$ci_upper , c(2014, 2932, 3601, 4280, 5210, 5944), tol = 1e-2)

	# Generalized Logistic (GLO) Distribution
	GLO <- sb.uncertainty(df$max, df$year, "GLO", "L-moments")[[1]]
	expect_equal(GLO$ci_lower , c(1702, 2326, 2743, 3155, 3730, 4203), tol = 1e-2)
	expect_equal(GLO$estimates, c(1847, 2569, 3123, 3742, 4714, 5599), tol = 1e-2)
	expect_equal(GLO$ci_upper , c(2003, 2827, 3557, 4480, 6129, 7847), tol = 1e-2)

	# Pearson Type III (PE3) Distribution
	PE3 <- sb.uncertainty(df$max, df$year, "PE3", "L-moments")[[1]]
	expect_equal(PE3$ci_lower , c(1660, 2402, 2873, 3302, 3817, 4195), tol = 1e-2)
	expect_equal(PE3$estimates, c(1823, 2668, 3254, 3822, 4560, 5114), tol = 1e-2)
	expect_equal(PE3$ci_upper , c(2005, 2949, 3658, 4388, 5386, 6154), tol = 1e-2)

	# Generalized Normal (GNO) Distribution
	GNO <- sb.uncertainty(df$max, df$year, "GNO", "L-moments")[[1]]
	expect_equal(GNO$ci_lower , c(1670, 2377, 2840, 3269, 3799, 4200), tol = 1e-2)
	expect_equal(GNO$estimates, c(1826, 2636, 3222, 3818, 4638, 5289), tol = 1e-2)
	expect_equal(GNO$ci_upper , c(1998, 2912, 3640, 4455, 5678, 6753), tol = 1e-2)

	# # Weibull (WEI) Distribution
	WEI <- sb.uncertainty(df$max, df$year, "WEI", "L-moments")[[1]]
	expect_equal(WEI$ci_lower , c(1650, 2424, 2907, 3326, 3813, 4148), tol = 1e-2)
	expect_equal(WEI$estimates, c(1819, 2693, 3279, 3828, 4512, 5007), tol = 1e-2)
	expect_equal(WEI$ci_upper , c(2010, 2980, 3675, 4368, 5286, 5979), tol = 1e-2)

})

test_that("Test s-bootstrap.R on data set #2 with L-moments", {
	set.seed(1)

	# Load dataset
	df <- load_data("Application_2.csv")

	# Generalized Extreme Value (GEV) Distribution
	GEV <- sb.uncertainty(df$max, df$year, "GEV", "L-moments")[[1]]
	expect_equal(GEV$ci_lower , c(1440, 2034, 2373, 2646, 2930, 3100), tol = 1e-2)
	expect_equal(GEV$estimates, c(1591, 2235, 2628, 2982, 3409, 3706), tol = 1e-2)
	expect_equal(GEV$ci_upper , c(1749, 2440, 2889, 3336, 3950, 4439), tol = 1e-2)

	# Gumbel (GUM) Distribution
	GUM <- sb.uncertainty(df$max, df$year, "GUM", "L-moments")[[1]]
	expect_equal(GUM$ci_lower , c(1420, 1989, 2353, 2699, 3144, 3477), tol = 1e-2)
	expect_equal(GUM$estimates, c(1555, 2203, 2632, 3044, 3577, 3977), tol = 1e-2)
	expect_equal(GUM$ci_upper , c(1696, 2435, 2937, 3423, 4055, 4530), tol = 1e-2)

	# Normal (NOR) Distribution
	NOR <- sb.uncertainty(df$max, df$year, "NOR", "L-moments")[[1]]
	expect_equal(NOR$ci_lower , c(1531, 2098, 2381, 2611, 2865, 3033), tol = 1e-2)
	expect_equal(NOR$estimates, c(1675, 2267, 2576, 2831, 3119, 3310), tol = 1e-2)
	expect_equal(NOR$ci_upper , c(1819, 2436, 2773, 3055, 3378, 3593), tol = 1e-2)

	# Log-Normal (LNO) Distribution
	LNO <- sb.uncertainty(df$max, df$year, "LNO", "L-moments")[[1]]
	expect_equal(LNO$ci_lower , c(1400, 1973, 2340, 2686, 3131, 3465), tol = 1e-2)
	expect_equal(LNO$estimates, c(1530, 2189, 2641, 3083, 3669, 4121), tol = 1e-2)
	expect_equal(LNO$ci_upper , c(1667, 2423, 2975, 3537, 4307, 4917), tol = 1e-2)

	# Generalized Logistic (GLO) Distribution
	GLO <- sb.uncertainty(df$max, df$year, "GLO", "L-moments")[[1]]
	expect_equal(GLO$ci_lower , c(1461, 1991, 2303, 2586, 2940, 3199), tol = 1e-2)
	expect_equal(GLO$estimates, c(1600, 2183, 2571, 2961, 3507, 3953), tol = 1e-2)
	expect_equal(GLO$ci_upper , c(1745, 2384, 2865, 3407, 4252, 5019), tol = 1e-2)

	# Log-Pearson Type III (LP3) Distribution
	LP3 <- sb.uncertainty(df$max, df$year, "LP3", "L-moments")[[1]]
	expect_equal(LP3$ci_lower , c(1419, 2027, 2383, 2675, 2981, 3171), tol = 1e-2)
	expect_equal(LP3$estimates, c(1569, 2239, 2662, 3051, 3532, 3878), tol = 1e-2)
	expect_equal(LP3$ci_upper , c(1732, 2465, 2961, 3469, 4189, 4779), tol = 1e-2)

	# Generalized Normal (GNO) Distribution
	GNO <- sb.uncertainty(df$max, df$year, "GNO", "L-moments")[[1]]
	expect_equal(GNO$ci_lower , c(1440, 2033, 2364, 2636, 2943, 3147), tol = 1e-2)
	expect_equal(GNO$estimates, c(1592, 2232, 2621, 2974, 3410, 3725), tol = 1e-2)
	expect_equal(GNO$ci_upper , c(1751, 2436, 2891, 3345, 3956, 4434), tol = 1e-2)

	# Weibull (WEI) Distribution
	WEI <- sb.uncertainty(df$max, df$year, "WEI", "L-moments")[[1]]
	expect_equal(WEI$ci_lower , c(1430, 2058, 2391, 2657, 2944, 3131), tol = 1e-2)
	expect_equal(WEI$estimates, c(1592, 2259, 2640, 2967, 3346, 3605), tol = 1e-2)
	expect_equal(WEI$ci_upper , c(1760, 2462, 2896, 3296, 3790, 4141), tol = 1e-2)

})

test_that("Test s-bootstrap.R on data set #3.1 with MLE", {
	set.seed(1)

	# Load dataset
	df <- load_data("Application_3.1.csv")

	# Generalized Extreme Value (GEV) Distribution
	GEV <- sb.uncertainty(df$max, df$year, "GEV", "MLE", n_sim = 1000)[[1]]
	expect_equal(GEV$ci_lower , c(191, 241, 270, 295, 325, 344), tol = 1e-2)
	expect_equal(GEV$estimates, c(201, 256, 293, 328, 374, 409), tol = 1e-2)
	expect_equal(GEV$ci_upper , c(213, 273, 314, 362, 430, 488), tol = 1e-2)

	# Gumbel (GUM) Distribution
	GUM <- sb.uncertainty(df$max, df$year, "GUM", "MLE", n_sim = 1000)[[1]]
	expect_equal(GUM$ci_lower , c(192, 242, 273, 302, 341, 370), tol = 1e-2)
	expect_equal(GUM$estimates, c(202, 256, 292, 327, 371, 405), tol = 1e-2)
	expect_equal(GUM$ci_upper , c(212, 272, 313, 351, 402, 440), tol = 1e-2)

	# Normal (NOR) Distribution
	NOR <- sb.uncertainty(df$max, df$year, "NOR", "MLE", n_sim = 1000)[[1]]
	expect_equal(NOR$ci_lower , c(201, 251, 276, 296, 318, 333), tol = 1e-2)
	expect_equal(NOR$estimates, c(212, 264, 291, 313, 338, 355), tol = 1e-2)
	expect_equal(NOR$ci_upper , c(223, 277, 305, 329, 357, 376), tol = 1e-2)

	# Log-Normal (LNO) Distribution
	LNO <- sb.uncertainty(df$max, df$year, "LNO", "MLE", n_sim = 1000)[[1]]
	expect_equal(LNO$ci_lower , c(194, 243, 272, 298, 329, 352), tol = 1e-2)
	expect_equal(LNO$estimates, c(204, 258, 291, 322, 361, 389), tol = 1e-2)
	expect_equal(LNO$ci_upper , c(214, 273, 311, 346, 393, 427), tol = 1e-2)

	# Generalized Logistic (GLO) Distribution
	# NOTE: No tests since this distribution doesn't work in MATLAB

	# Pearson Type III (PE3) Distribution
 	# NOTE: Tests for PE3 on dataset 3 have been removed due to parameterization issues in MATLAB

	# Log-Pearson Type III (LP3) Distribution
	LP3 <- sb.uncertainty(df$max, df$year, "LP3", "MLE", n_sim = 1000)[[1]]
	expect_equal(LP3$ci_lower , c(191, 241, 271, 297, 326, 348), tol = 1e-2)
	expect_equal(LP3$estimates, c(201, 257, 293, 329, 376, 412), tol = 1e-2)
	expect_equal(LP3$ci_upper , c(213, 273, 317, 365, 433, 492), tol = 1e-2)

	# Generalized Normal (GNO) Distribution
	GNO <- sb.uncertainty(df$max, df$year, "GNO", "MLE", n_sim = 1000)[[1]]
	expect_equal(GNO$ci_lower , c(190, 241, 271, 297, 328, 350), tol = 1e-2)
	expect_equal(GNO$estimates, c(201, 257, 294, 329, 376, 410), tol = 1e-2)
	expect_equal(GNO$ci_upper , c(214, 275, 320, 365, 429, 484), tol = 1e-2)

	# Weibull (WEI) Distribution
	# NOTE: No tests since this distribution doesn't work in MATLAB.

})

test_that("Test s-bootstrap.R on data set #1 with non-stationary MLE", {
	set.seed(1)

	# Load dataset
	df <- load_data("Application_1.csv")

	# Generalized Extreme Value (GEV) Distribution
	GEV100 <- sb.uncertainty(
		df$max,
		df$year,
		"GEV100",
		"MLE",
		slices = "all",
		n_sim = 2000
	)

	GEV100_FIRST <- GEV100[[1]]
	expect_equal(GEV100_FIRST$ci_lower , c(1494, 2205, 2665, 3067, 3530, 3848), tol = 1e-2)
	expect_equal(GEV100_FIRST$estimates, c(1754, 2532, 3078, 3626, 4372, 4960), tol = 1e-2)
	expect_equal(GEV100_FIRST$ci_upper , c(2045, 2861, 3497, 4234, 5420, 6483), tol = 1e-2)
	
	GEV100_LAST <- GEV100[[length(GEV100)]]
	expect_equal(GEV100_LAST$ci_lower , c(1706, 2414, 2860, 3267, 3734, 4049), tol = 1e-2)
	expect_equal(GEV100_LAST$estimates, c(1963, 2741, 3287, 3835, 4581, 5169), tol = 1e-2)
	expect_equal(GEV100_LAST$ci_upper , c(2224, 3051, 3689, 4434, 5624, 6686), tol = 1e-2)

})

test_that("Test s-bootstrap.R on data set #1 with GMLE", {
	set.seed(1)

	# Load dataset
	df <- load_data("Application_1.csv")

	# Generalized Extreme Value (GEV) Distribution
	GEV100 <- sb.uncertainty(
		df$max,
		df$year,
		"GEV100",
		"GMLE",
		slices = "all",
		n_sim = 2000,
		prior = c(6, 9)
	)

	GEV100_FIRST <- GEV100[[1]]
	expect_equal(GEV100_FIRST$ci_lower , c(1484, 2223, 2743, 3285, 4071, 4697), tol = 1e-2)
	expect_equal(GEV100_FIRST$estimates, c(1730, 2544, 3145, 3771, 4660, 5392), tol = 1e-2)
	expect_equal(GEV100_FIRST$ci_upper , c(2023, 2922, 3575, 4286, 5309, 6148), tol = 1e-2)

	GEV100_LAST <- GEV100[[length(GEV100)]]
	expect_equal(GEV100_LAST$ci_lower , c(1703, 2423, 2954, 3507, 4277, 4887), tol = 1e-2)
	expect_equal(GEV100_LAST$estimates, c(1960, 2775, 3375, 4001, 4891, 5622), tol = 1e-2)
	expect_equal(GEV100_LAST$ci_upper , c(2239, 3119, 3792, 4510, 5520, 6350), tol = 1e-2)

})
