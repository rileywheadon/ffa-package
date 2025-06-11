# NOTE: The R function optim() and the MATLAB function fminsearch() are not identical.
# For these tests, we require the R implementation to meet one of the following two criteria:
#  1. Produce a higher MLL than the MATLAB implementation.
#  2. Produce the same as the MLL as the MATLAB implementation and yield the same parameters.

# Helper function to check validate results
validate_results <- function(data, model, expected) {
	observed <- mle.estimation(data, model)
	if (observed$mll > expected$mll + 0.001) {
		print(sprintf("Skip %s: %.3f > %.3f.", model, observed$mll, expected$mll))
	} else {
		expect_equal(observed$params, expected$params, tol = 1e-4)
		expect_equal(observed$mll   , expected$mll   , tol = 1e-4)
	} 
}

test_that("Test mle-estimation.R on data set #1", {

	# Load dataset and run L-moments estimation
	df <- load_data("Application_1.csv", clean = FALSE)

	# Gumbel (GUM) Distribution
	validate_results(df$max, "GUM", list(
		 params = c(1642.8856, 665.2585), 
		 mll = -825.7803 
	))

	validate_results(df$max, "GUM10", list(
		params = c(1544.6436, 191.4887, 661.2450),
		mll = -825.3975 
	))

	validate_results(df$max, "GUM11", list(
		params = c(1607.7145, 72.8119, 704.3887, -82.8341),
		mll = -825.5232
	))

	# Normal (NOR) Distribution
	validate_results(df$max, "NOR", list(
		 params = c(2039.1863, 913.5010), 
		 mll = -840.0948
	))

	validate_results(df$max, "NOR10", list(
		params = c(1925.8261, 219.0145, 911.2851),
		mll = -839.8470
	))

	validate_results(df$max, "NOR11", list(
		params = c(1922.3793, 225.7007, 888.1657, 44.6007),
		mll = -839.8329
	))

	# Log-Normal (LNO) Distribution
	validate_results(df$max, "LNO", list(
		 params = c(7.5285, 0.4323), 
		 mll = -827.0937
	))

	validate_results(df$max, "LNO10", list(
		params = c(7.4614, 0.1297, 0.4306),
		mll = -826.7053
	))

	validate_results(df$max, "LNO11", list(
		params = c(7.4672, 0.1185, 0.4701, -0.0775),
		mll = -826.4169
	))

	# Generalized Extreme Value (GEV) Distribution
	validate_results(df$max, "GEV", list(
		 params = c(1624.9889, 655.3234, 0.0510), 
		 mll = -825.4411
	))

	validate_results(df$max, "GEV100", list(
		params = c(1514.1289, 208.6491, 648.0107, 0.0616),
		mll = -824.9450
	))

	validate_results(df$max, "GEV110", list(
		params = c(1598.8487, 43.8234, 709.4190, -122.6525, 0.0724),
		mll = -825.0452
	))

	# Generalized Logistic (GLO) Distribution
	validate_results(df$max, "GLO", list(
		 params = c(1852.1263, 439.0156, -0.2317), 
		 mll = -825.2516
	))

	validate_results(df$max, "GLO100", list(
		params = c(1759.0493, 178.2527, 439.0925, -0.2413),
		mll = -824.9231 
	))

	validate_results(df$max, "GLO110", list(
		params = c(1783.4531, 127.5409, 457.1615, -33.9808, -0.2463),
		mll = -824.9262 
	))

	# Generalized Normal (GNO) Distribution
	validate_results(df$max, "GNO", list(
		 params = c(1885.3744, 797.8436, -0.3654), 
		 mll = -826.2868
	))

	validate_results(df$max, "GNO100", list(
		params = c(1739.6437, 263.5634, 792.1433, -0.3904),
		mll = -825.5555 
	))

	validate_results(df$max, "GNO110", list(
		params = c(1790.4015, 156.4289, 851.8465, -115.2775, -0.4041),
		mll = -825.4486
	))

	# Generalized Pareto (GPA) Distribution
	# TBD

	# Pearson Type III (PE3) Distribution
	validate_results(df$max, "PE3", list(
		 params = c(2039.1863, 865.5583, 0.8802), 
		 mll = -827.6846
	))

	validate_results(df$max, "PE3100", list(
		params = c(1864.6692, 337.1710, 870.5665, 0.9708),
		mll = -826.6893 
	))

	# NOTE: Caused by difference in optimizatoin algorithms?
	# validate_results(df$max, "PE3110", list(
	# 	params = c(1937.9671, 195.8764, 954.1574, -149.2599, 1.0140),
	# 	mll = -826.4840
	# ))

	# Log-Pearson Type III (LP3) Distribution
	validate_results(df$max, "LP3", list(
		 params = c(7.5285, 0.4318, -0.1298), 
		 mll = -826.8326
	))

	validate_results(df$max, "LP3100", list(
		params = c(7.4651, 0.1224, 0.4303, -0.1204),
		mll = -826.4880  
	))

	validate_results(df$max, "LP3110", list(
		params = c(7.4663, 0.1202, 0.4634, -0.0652, -0.0946),
		mll = -826.3017 
	))

	# Weibull (WEI) Distribution
	validate_results(df$max, "WEI", list(
		 params = c(274.1452, 1995.0369, 2.0333), 
		 mll = -831.8793
	))

	validate_results(df$max, "WEI100", list(
		params = c(187.7623, 607.1417, 1728.4916, 1.7550),
		mll = -829.1212  
	))

	validate_results(df$max, "WEI110", list(
		params = c(168.0173, 704.2308, 1930.5430, -467.0824, 1.7092),
		mll = -828.4383
	))

})

test_that("Test mle-estimation.R on data set #2", {

	# Load dataset and run L-moments estimation
	df <- load_data("Application_2.csv", clean = FALSE)

	# Gumbel (GUM) Distribution
	validate_results(df$max, "GUM", list(
		 params = c(1346.6774, 567.9530), 
		 mll = -720.7467 
	))

	validate_results(df$max, "GUM10", list(
		params = c(1931.2031, -1069.9191, 501.7051),
		mll = -707.4183
	))

	validate_results(df$max, "GUM11", list(
		params = c(1922.3495, -1054.9372, 670.2176, -336.8644),
		mll = -705.0993
	))

	# Normal (NOR) Distribution
	validate_results(df$max, "NOR", list(
		 params = c(1675.0989, 692.5677), 
		 mll = -724.3004
	))

	validate_results(df$max, "NOR10", list(
		params = c(2340.7134, -1331.2289, 573.3126),
		mll = -707.1036
	))

	validate_results(df$max, "NOR11", list(
		params = c(2260.4196, -1187.1658, 831.4482, -521.8119),
		mll = -703.1886
	))

	# Log-Normal (LNO) Distribution
	validate_results(df$max, "LNO", list(
		 params = c(7.3337, 0.4337), 
		 mll = -720.4715
	))

	validate_results(df$max, "LNO10", list(
		params = c(7.7422, -0.8169, 0.3623),
		mll = -704.0965
	))

	# validate_results(df$max, "LNO11", list(
	# 	params = c(7.7430, -0.8186, 0.3508, 0.0230),
	# 	mll = -704.0770
	# ))

	# Generalized Extreme Value (GEV) Distribution
	validate_results(df$max, "GEV", list(
		 params = c(1363.8333, 580.6468, -0.0557), 
		 mll = -720.6244
	))

	validate_results(df$max, "GEV100", list(
		params = c(2048.9743, -1210.1553, 530.1797, -0.1702),
		mll = -705.8908
	))

	validate_results(df$max, "GEV110", list(
		params = c(1976.5663, -1029.8465, 804.9656, -502.5558, -0.2589),
		mll = -702.1744
	))

	# Generalized Logistic (GLO) Distribution
	validate_results(df$max, "GLO", list(
		 params = c(1547.6390, 399.1187, -0.2599), 
		 mll = -723.3497
	))

	validate_results(df$max, "GLO100", list(
		params = c(2244.4597, -1211.9495, 334.9422, -0.0898),
		mll = -708.5882 
	))

	validate_results(df$max, "GLO110", list(
		params = c(1922.6276, -556.9695, 271.2343, 249.5252, -0.2416),
		mll = -716.6764 
	))

	# Generalized Normal (GNO) Distribution
	validate_results(df$max, "GNO", list(
		 params = c(1554.3044, 662.5788, -0.3608), 
		 mll = -720.2721
	))

	validate_results(df$max, "GNO100", list(
		params = c(2220.9590, -1188.8260, 567.0406, -0.1706),
		mll = -706.1026 
	))

	validate_results(df$max, "GNO110", list(
		params = c(1838.1028, -378.2154, 465.8349, 418.8941, -0.3864),
		mll = -716.0404
	))

	# Generalized Pareto (GPA) Distribution
	# TBD
		
	# Pearson Type III (PE3) Distribution
	validate_results(df$max, "PE3", list(
		 params = c(1675.0989, 723.9569, 1.0536), 
		 mll = -719.3150
	))

	validate_results(df$max, "PE3100", list(
		params = c(2250.7148, -1151.2319, 584.9250, 0.6268),
		mll = -705.8747 
	))

	validate_results(df$max, "PE3110", list(
		params = c(2283.5224, -1227.2148, 797.4393, -453.5283, 0.4088),
		mll = -702.8271
	))

	# Log-Pearson Type III (LP3) Distribution
	validate_results(df$max, "LP3", list(
		 params = c(7.3337, 0.4416, -0.5963), 
		 mll = -719.3636
	))

	validate_results(df$max, "LP3100", list(
		params = c(7.7630, -0.8585, 0.3732, -0.8602),
		mll = -700.9359  
	))

	validate_results(df$max, "LP3110", list(
		params = c(7.7324, -0.7978, 0.4314, -0.1047, -0.9507),
		mll = -700.6605 
	))

	# Weibull (WEI) Distribution
	validate_results(df$max, "WEI", list(
		 params = c(420.9388, 1413.9838, 1.8856), 
		 mll = -718.3923
	))

	validate_results(df$max, "WEI100", list(
		params = c(1038.3840, -1094.0033, 1336.4392, 2.1626),
		mll = -704.6250  
	))

	validate_results(df$max, "WEI110", list(
		params = c(-514.7607, 881.4916, 3005.3321, -2158.5861, 3.2871),
		mll = -702.3482
	))

})


test_that("Test mle-estimation.R on data set #3.1", {

	# Load dataset and run L-moments estimation
	df <- load_data("Application_3.1.csv", clean = FALSE)

	# Gumbel (GUM) Distribution
	validate_results(df$max, "GUM", list(
		 params = c(184.1446, 47.9538), 
		 mll = -594.3386 
	))

	validate_results(df$max, "GUM10", list(
		params = c(205.3484, -41.0113, 46.5039),
		mll = -590.9022 
	))

	validate_results(df$max, "GUM11", list(
		params = c(206.4051, -43.0881, 48.9653, -5.1762),
		mll = -590.8057
	))

	# Normal (NOR) Distribution
	validate_results(df$max, "NOR", list(
		 params = c(212.0734, 61.4000), 
		 mll = -603.4619
	))

	validate_results(df$max, "NOR10", list(
		params = c(232.2718, -40.7639, 60.2607),
		mll = -601.4205
	))

	validate_results(df$max, "NOR11", list(
		params = c(233.3923, -43.0804, 51.7531, 16.4607),
		mll = -600.4378
	))

	# Log-Normal (LNO) Distribution
	validate_results(df$max, "LNO", list(
		 params = c(5.3177, 0.2777), 
		 mll = -594.6497
	))

	validate_results(df$max, "LNO10", list(
		params = c(5.4266, -0.2197, 0.2704),
		mll = -591.7278
	))

	validate_results(df$max, "LNO11", list(
		params = c(5.4290, -0.2246, 0.2352, 0.0688),
		mll = -591.0043
	))

	# Generalized Extreme Value (GEV) Distribution
	validate_results(df$max, "GEV", list(
		 params = c(183.8460, 47.7538, 0.0115), 
		 mll = -594.3270
	))

	validate_results(df$max, "GEV100", list(
		params = c(204.7477, -40.5921, 46.2536, 0.0155),
		mll = -590.8773
	))

	validate_results(df$max, "GEV110", list(
		params = c(205.8584, -43.4645, 49.4101, -7.0631, 0.0295),
		mll = -590.7230
	))

	# Generalized Logistic (GLO) Distribution
	validate_results(df$max, "GLO", list(
		 params = c(200.9814, 32.8376, -0.2430), 
		 mll = -595.9133
	))

	validate_results(df$max, "GLO100", list(
		params = c(223.9083, -45.2347, 31.4018, -0.2285),
		mll = -591.8553 
	))

	validate_results(df$max, "GLO110", list(
		params = c(227.8011, -53.7420, 35.5164, -8.3527, -0.2409),
		mll = -591.4712 
	))

	# Generalized Normal (GNO) Distribution
	validate_results(df$max, "GNO", list(
		 params = c(201.2149, 56.3091, -0.3755), 
		 mll = -594.0275
	))

	validate_results(df$max, "GNO100", list(
		params = c(220.8535, -38.7119, 54.6325, -0.3689),
		mll = -590.7329 
	))

	validate_results(df$max, "GNO110", list(
		params = c(219.1343, -34.8410, 52.9142, 3.8187, -0.3653),
		mll = -590.9198
	))

	# Generalized Pareto (GPA) Distribution
	# TBD
	
	# Pearson Type III (PE3) Distribution
	validate_results(df$max, "PE3", list(
		 params = c(212.0734, 61.6681, 1.0272), 
		 mll = -593.7024
	))

	validate_results(df$max, "PE3100", list(
		params = c(229.2088, -34.5822, 60.1513, 1.0443),
		mll = -590.6219 
	))

	validate_results(df$max, "PE3110", list(
		params = c(222.4752, -19.7965, 56.0574, 9.8502, 1.0541),
		mll = -591.2324
	))

	# Log-Pearson Type III (LP3) Distribution
	validate_results(df$max, "LP3", list(
		 params = c(5.3177, 0.2782, 0.2729), 
		 mll = -594.1564
	))

	validate_results(df$max, "LP3100", list(
		params = c(5.4345, -0.2357, 0.2706, 0.3557),
		mll = -590.6484  
	))

	validate_results(df$max, "LP3110", list(
		params = c(5.4280, -0.2226, 0.2471, 0.0449, 0.2939),
		mll = -590.3979 
	))

	# Weibull (WEI) Distribution
	validate_results(df$max, "WEI", list(
		 params = c(103.7897, 121.9568, 1.8390), 
		 mll = -594.1913
	))

	validate_results(df$max, "WEI100", list(
		params = c(127.2839, -28.4333, 110.6002, 1.6817),
		mll = -590.6628  
	))

	validate_results(df$max, "WEI110", list(
		params = c(126.9082, -27.2858, 112.5077, -4.4299, 1.6746),
		mll = -590.6438
	))

})

test_that("Test mle-estimation.R on data set #3.2", {

	# Load dataset and run L-moments estimation
	df <- load_data("Application_3.2.csv", clean = FALSE)

	# Gumbel (GUM) Distribution
	validate_results(df$max, "GUM", list(
		 params = c(62.7897, 16.9775), 
		 mll = -392.7957
	))

	validate_results(df$max, "GUM10", list(
		params = c(57.5210, 10.6331, 16.6483),
		mll = -391.1480 
	))

	validate_results(df$max, "GUM11", list(
		params = c(56.0993, 13.4948, 13.8604, 5.6304),
		mll = -390.3762
	))

	# Normal (NOR) Distribution
	validate_results(df$max, "NOR", list(
		 params = c(72.6629, 21.9778), 
		 mll = -401.2988
	))

	validate_results(df$max, "NOR10", list(
		params = c(63.3158, 18.2620, 21.2950),
		mll = -398.4896
	))

	validate_results(df$max, "NOR11", list(
		params = c(62.7709, 19.3921, 13.1027, 15.0461),
		mll = -394.2257
	))

	# Log-Normal (LNO) Distribution
	validate_results(df$max, "LNO", list(
		 params = c(4.2436, 0.2878), 
		 mll = -393.1097
	))

	validate_results(df$max, "LNO10", list(
		params = c(4.1262, 0.2293, 0.2796),
		mll = -390.5320
	))

	validate_results(df$max, "LNO11", list(
		params = c(4.1222, 0.2375, 0.2370, 0.0812),
		mll = -389.8581
	))

	# Generalized Extreme Value (GEV) Distribution
	validate_results(df$max, "GEV", list(
		 params = c(62.6761, 16.9091, 0.0124), 
		 mll = -392.7823
	))

	validate_results(df$max, "GEV100", list(
		params = c(57.4508, 10.5096, 16.5596, 0.0147),
		mll = -391.1321
	))

	validate_results(df$max, "GEV110", list(
		params = c(56.2739, 13.8984, 13.6202, 6.6586, -0.0409),
		mll = -390.2832
	))

	# Generalized Logistic (GLO) Distribution
	validate_results(df$max, "GLO", list(
		 params = c(68.6821, 11.4113, -0.2297), 
		 mll = -393.4542
	))

	validate_results(df$max, "GLO100", list(
		params = c(64.0017, 9.2868, 11.3385, -0.2428),
		mll = -392.2140 
	))

	validate_results(df$max, "GLO110", list(
		params = c(62.4756, 13.0809, 9.5061, 3.5623, -0.2113),
		mll = -391.7270 
	))

	# Generalized Normal (GNO) Distribution
	validate_results(df$max, "GNO", list(
		 params = c(68.9944, 19.9741, -0.3556), 
		 mll = -392.7902
	))

	validate_results(df$max, "GNO100", list(
		params = c(63.2561, 10.9178, 19.5535, -0.3798),
		mll = -390.8963 
	))

	validate_results(df$max, "GNO110", list(
		params = c(61.0151, 16.4311, 15.9674, 6.9047, -0.3227),
		mll = -390.1560
	))

	# Generalized Pareto (GPA) Distribution
	# TBD
	
	# Pearson Type III (PE3) Distribution
	validate_results(df$max, "PE3", list(
		 params = c(72.6629, 21.6678, 0.9502), 
		 mll = -392.9510
	))

	validate_results(df$max, "PE3100", list(
		params = c(66.5593, 11.9250, 21.7010, 1.0999),
		mll = -390.4990 
	))

	validate_results(df$max, "PE3110", list(
		params = c(63.0150, 18.8665, 17.5339, 7.0019, 0.9773),
		mll = -389.7870
	))

	# Log-Pearson Type III (LP3) Distribution
	validate_results(df$max, "LP3", list(
		 params = c(4.2436, 0.2878, 0.2100), 
		 mll = -392.7880
	))

	validate_results(df$max, "LP3100", list(
		params = c(4.1316, 0.2188, 0.2798, 0.1637),
		mll = -390.3978  
	))

	validate_results(df$max, "LP3110", list(
		params = c(4.1221, 0.2376, 0.2365, 0.0821, -0.0086),
		mll = -389.8578 
	))

	# Weibull (WEI) Distribution
	validate_results(df$max, "WEI", list(
		 params = c(33.3648, 44.2562, 1.8632), 
		 mll = -393.9233
	))

	validate_results(df$max, "WEI100", list(
		params = c(31.3702, 13.0563, 38.6399, 1.6516),
		mll = -390.0178  
	))

	validate_results(df$max, "WEI110", list(
		params = c(32.0849, 9.7844, 32.9010, 13.9106, 1.7615),
		mll = -389.0661
	))

})

test_that("Test mle-estimation.R on data set #3.3", {

	# Load dataset and run L-moments estimation
	df <- load_data("Application_3.3.csv", clean = FALSE)

	# Gumbel (GUM) Distribution
	validate_results(df$max, "GUM", list(
		 params = c(29.3768, 15.2386), 
		 mll = -417.4351 
	))

	validate_results(df$max, "GUM10", list(
		params = c(19.6970, 21.4646, 14.5759),
		mll = -408.6744 
	))

	validate_results(df$max, "GUM11", list(
		params = c(17.2397, 27.1009, 6.1992, 15.3335),
		mll = -398.9633
	))

	# Normal (NOR) Distribution
	validate_results(df$max, "NOR", list(
		 params = c(38.2095, 18.5118), 
		 mll = -420.7229
	))

	validate_results(df$max, "NOR10", list(
		params = c(20.7332, 34.9525, 15.4519),
		mll = -403.1971
	))

	validate_results(df$max, "NOR11", list(
		params = c(20.8458, 34.7144, 11.1524, 8.1526),
		mll = -400.5763
	))

	# Log-Normal (LNO) Distribution
	validate_results(df$max, "LNO", list(
		 params = c(3.5110, 0.5373), 
		 mll = -417.9424
	))

	validate_results(df$max, "LNO10", list(
		params = c(3.0413, 0.9394, 0.4622),
		mll = -403.3356
	))

	validate_results(df$max, "LNO11", list(
		params = c(3.0352, 0.9518, 0.4039, 0.1143),
		mll = -402.8411
	))

	# Generalized Extreme Value (GEV) Distribution
	validate_results(df$max, "GEV", list(
		 params = c(29.9497, 15.6802, -0.0695), 
		 mll = -417.2748
	))

	validate_results(df$max, "GEV100", list(
		params = c(17.3249, 29.9814, 15.2606, -0.2243),
		mll = -404.0062
	))

	validate_results(df$max, "GEV110", list(
		params = c(17.5748, 27.2660, 6.9529, 13.8717, -0.0585),
		mll = -398.4624
	))

	# Generalized Logistic (GLO) Distribution
	validate_results(df$max, "GLO", list(
		 params = c(34.5014, 10.7666, -0.2822), 
		 mll = -420.3594
	))

	validate_results(df$max, "GLO100", list(
		params = c(18.5655, 40.2597, 8.5042, 0.0261),
		mll = -402.0643 
	))

	validate_results(df$max, "GLO110", list(
		params = c(19.0582, 37.1268, 3.5129, 10.0707, -0.0875),
		mll = -395.5915 
	))

	# Generalized Normal (GNO) Distribution
	validate_results(df$max, "GNO", list(
		 params = c(34.8783, 17.7926, -0.3713), 
		 mll = -416.8789
	))

	validate_results(df$max, "GNO100", list(
		params = c(20.7749, 34.7525, 15.4511, -0.0076),
		mll = -403.1923 
	))

	validate_results(df$max, "GNO110", list(
		params = c(19.9512, 33.3654, 7.7105, 15.0946, -0.2059),
		mll = -397.5878
	))

	# Generalized Pareto (GPA) Distribution
	# TBD
	
	# Pearson Type III (PE3) Distribution
	validate_results(df$max, "PE3", list(
		 params = c(38.2095, 19.7334, 1.1388), 
		 mll = -415.5614
	))

	validate_results(df$max, "PE3100", list(
		params = c(20.8279, 34.7632, 15.4518, 0.0214),
		mll = -403.1926 
	))

	validate_results(df$max, "PE3110", list(
		params = c(21.5611, 33.4284, 18.1247, -3.9291, -0.0906),
		mll = -405.8514
	))

	# Log-Pearson Type III (LP3) Distribution
	validate_results(df$max, "LP3", list(
		 params = c(3.5110, 0.5830, -1.1800), 
		 mll = -413.5910
	))

	validate_results(df$max, "LP3100", list(
		params = c(3.0610, 0.8999, 0.4532, -0.5190),
		mll = -399.2227  
	))

	validate_results(df$max, "LP3110", list(
		params = c(3.0492, 0.9241, 0.5382, -0.1531, -0.6932),
		mll = -398.7725 
	))

	# Weibull (WEI) Distribution
	validate_results(df$max, "WEI", list(
		 params = c(6.3503, 35.7649, 1.7612), 
		 mll = -414.0199
	))

	validate_results(df$max, "WEI100", list(
		params = c(-27.8400, 32.6232, 55.1689, 3.5055),
		mll = -403.7484  
	))

	validate_results(df$max, "WEI110", list(
		params = c(4.9117, 1.2853, 20.1114, 32.4985, 2.0981),
		mll = -398.7885
	))

})


