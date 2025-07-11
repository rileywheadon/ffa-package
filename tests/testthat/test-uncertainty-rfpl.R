# NOTE: Profiling code:
# Rprof()
# Rprof(NULL)
# print(summaryRprof())

validate_rfpl <- function(
    df,
    model,
    trend,
    ci_lower,
    estimates,
    ci_upper,
	slices = 1900
) {

	results <- uncertainty_rfpl(
		df$max,
		model,
		years = df$year,
		trend = trend,
		slices = slices
	)[[1]]

	# NOTE: We check the estimates in test-mle-estimation.R
	expect_equal(results$ci_lower, ci_lower, tol = 1e-2)
	expect_equal(results$ci_upper, ci_upper, tol = 1e-2)

}

test_that("Test RFPL uncertainty on data set #1", {

	# Load dataset and run RFPL uncertainty quantification
	df <- load_data("Application_1.csv")

	# Mean-Trend Gumbel (GUM10) Distribution
	validate_rfpl(
		df,
		"GUM",
		trend_10, 
		c(1522.3984, 2230.8239, 2687.2499, 3117.1193, 3665.8628, 4073.3393),
		c(1786.9985, 2536.4715, 3032.6878, 3508.6704, 4124.7811, 4586.4694),
		c(2059.8899, 2878.7089, 3436.0561, 3977.4988, 4684.6410, 5217.6126),
		min(df$year)
	)

	validate_rfpl(
		df,
		"GUM",
		trend_10,
		c(1726.5051, 2440.6013, 2892.3421, 3320.9486, 3868.1696, 4274.6928),
		c(1978.4872, 2727.9602, 3224.1765, 3700.1592, 4316.2698, 4777.9581),
		c(2233.9381, 3053.6494, 3612.1645, 4154.6522, 4862.9392, 5396.6203),
		max(df$year)
	)

	# Mean + Variance Gumbel (GUM11) Distribution
	validate_rfpl(
		df,
		"GUM",
		trend_11,
		c(1487.1335, 2108.2817, 2493.1834, 2869.1881, 3327.9479, 3671.6414),
		c(1865.8820, 2664.2553, 3192.8478, 3699.8865, 4356.1960, 4848.0077),
		c(2138.3204, 3108.6654, 3773.7758, 4418.3952, 5250.5948, 5863.6975),
		min(df$year)
	)

	validate_rfpl(
		df,
		"GUM",
		trend_11,
		c(1686.0641, 2283.9914, 2653.9173, 3003.7117, 3457.5044, 3809.0401),
		c(1908.3342, 2612.8210, 3079.2526, 3526.6649, 4105.7944, 4539.7704),
		c(2293.5600, 3248.7259, 3887.1865, 4525.8726, 5354.7381, 5976.2188),
		max(df$year)
	)

	# Gumbel (GUM) Distribution
	validate_rfpl(
		df,
		"GUM",
		trend_00,
		c(1741.2485, 2426.5020, 2867.1961, 3286.3350, 3826.2831, 4229.8251),
		c(1886.7114, 2640.7334, 3139.9616, 3618.8332, 4238.6834, 4703.1739),
		c(2045.9211, 2895.6589, 3470.6316, 4025.8960, 4747.4303, 5289.3077)
	)

	# Normal (NOR) Distribution
	validate_rfpl(
		df,
		"NOR",
		trend_00,
		c(1860.2251, 2613.6934, 2988.8282, 3292.8548, 3631.0182, 3854.8348),
		c(2039.1863, 2808.0081, 3209.8849, 3541.7617, 3915.2880, 4164.3074),
		c(2218.1475, 3030.2831, 3472.1095, 3842.3210, 4262.8162, 4544.7343)
	)

	# Log-Normal (LNO) Distribution
	validate_rfpl(
		df,
		"LNO",
		trend_00,
		c(1709.2364, 2441.4758, 2915.7406, 3366.9094, 3951.2060, 4392.6614),
		c(1860.2973, 2676.5999, 3237.2290, 3787.7181, 4520.0296, 5085.3099),
		c(2024.7022, 2973.5043, 3664.9768, 4366.7253, 5328.1280, 6088.5451)
	)

	# Generalized Extreme Value (GEV) Distribution
	validate_rfpl(
		df,
		"GEV",
		trend_00,
		c(1718.8134, 2426.0784, 2884.1104, 3318.2255, 3850.3142, 4236.9194),
		c(1867.4305, 2646.4788, 3187.5978, 3726.4642, 4453.9792, 5022.2453),
		c(2032.0309, 2920.7517, 3628.0105, 4403.9085, 5543.9541, 6565.5664)
	)

	# Generalized Logistic (GLO) Distribution
	validate_rfpl(
		df,
		"GLO",
		trend_00,
		c(1708.2399, 2346.9592, 2786.4139, 3239.1327, 3885.4426, 4422.7280),
		c(1852.1263, 2569.8245, 3109.8086, 3705.6535, 4625.6655, 5451.8178),
		c(2008.6903, 2856.5119, 3564.5075, 4401.3459, 5787.5432, 7115.5793)
	)

	# Generalized Normal (GNO) Distribution
	validate_rfpl(
		df,
		"GNO",
		trend_00,
		c(1729.1479, 2448.4230, 2893.6878, 3299.6882, 3800.9875, 4163.0932),
		c(1885.3745, 2671.5548, 3189.4365, 3684.5420, 4326.3371, 4810.6842),
		c(2052.8196, 2948.8727, 3592.0685, 4242.6101, 5130.4565, 5830.0028)
	)

	# Pearson Type III (PE3) Distribution
	validate_rfpl(
		df,
		"PE3",
		trend_00,
		c(1754.3914, 2486.4995, 2917.4085, 3299.0242, 3757.7884, 4081.5189),
		c(1914.9203, 2704.0830, 3194.0743, 3641.8024, 4194.7134, 4593.3554),
		c(2084.5699, 2969.3950, 3551.7782, 4100.0220, 4793.1669, 5301.7793)
	)

	# Mean-Trend Pearson Type III (PE3100) Distribution
	validate_rfpl(
		df,
		"PE3",
		trend_10, 
		c(1442.4692, 2211.3244, 2679.8338, 3093.2880, 3585.4198, 3929.8215),
		c(1727.4635, 2523.9812, 3026.7669, 3490.3593, 4067.3897, 4486.0666),
		c(2034.6779, 2879.3335, 3441.6215, 3982.4646, 4700.2479, 5229.5854),
		min(df$year)
	)

	validate_rfpl(
		df,
		"PE3",
		trend_10,
		c(1803.0525, 2553.0046, 2999.6536, 3394.4067, 3867.7855, 4201.9633),
		c(2064.6345, 2861.1522, 3363.9379, 3827.5302, 4404.5607, 4823.2376),
		c(2312.6469, 3205.8802, 3809.6343, 4397.3172, 5144.7349, 5701.3670),
		max(df$year)
	)

	# Mean + Variance Pearson Type III (PE3110) Distribution
	validate_rfpl(
		df,
		"PE3",
		trend_11,
		c(1447.8079, 2173.9898, 2593.4718, 2965.0315, 3414.7963, 3741.4092),
		c(1781.2818, 2655.4764, 3211.6196, 3726.5904, 4369.9306, 4838.0941),
		c(2154.2783, 3277.9496, 4067.1122, 4811.2966, 5772.8855, 6489.3704),
		min(df$year)
	)

	validate_rfpl(
		df,
		"PE3",
		trend_11,
		c(1718.2682, 2311.9220, 2674.6010, 3019.0793, 3425.9003, 3719.6816),  
		c(2001.6687, 2739.1120, 3208.2572, 3642.6706, 4185.3724, 4580.3006),  
		c(2324.7155, 3271.9784, 3873.1472, 4466.8554, 5204.2687, 5750.7678),
		max(df$year)
	)


	# Log-Pearson Type III (LP3) Distribution
	validate_rfpl(
		df,
		"LP3",
		trend_00,
		c(1718.8998, 2450.5955, 2911.0602, 3330.1357, 3838.6974, 4195.3573),
		c(1877.7326, 2681.9872, 3214.6549, 3723.5453, 4380.8069, 4874.1504),
		c(2051.0201, 2966.0667, 3631.4168, 4322.5733, 5295.8241, 6086.1434)
	)

	# # NOTE: The Weibull distribution was not implemented in the original framework.

	# Weibull (WEI) Distribution
	validate_rfpl(
		df,
		"WEI",
		trend_00,
		c(1755, 2570, 3013, 3381, 3792, 4065),
		c(1940, 2795, 3281, 3696, 4176, 4502),
		c(2132, 3059, 3620, 4118, 4710, 5124)
	)

	# Mean-Trend Weibull (WEI100) Distribution
	validate_rfpl(
		df,
		"WEI",
		trend_10,
		c(1931, 2753, 3210, 3595, 4031, 4324),
		c(2197, 3061, 3574, 4024, 4555, 4921),
		c(2425, 3384, 4001, 4570, 5268, 5766),
		max(df$year)
	)

	# Mean/Variance Trend (WEI110) Distribution
	validate_rfpl(
		df,
		"WEI",
		trend_11,
		c(1767, 2370, 2726, 3034, 3394, 3642),
		c(2053, 2805, 3256, 3653, 4122, 4448),
		c(2393, 3355, 3951, 4486, 5139, 5600),
		max(df$year)
	)

})

validate_rfgpl <- function(
    df,
    model,
    trend,
    ci_lower,
    estimates,
    ci_upper,
    slices = 1900,
	prior = c(6, 9)
) {

	results <- uncertainty_rfpl(
		df$max,
		model,
		prior,
		df$year,
		trend, 
		slices = slices
	)[[1]]

	# NOTE: We check the estimates in test-mle-estimation.R
	expect_equal(results$ci_lower, ci_lower, tol = 1e-2)
	expect_equal(results$ci_upper, ci_upper, tol = 1e-2)

}

test_that("Test RFGPL uncertainty on data set #1", {

	# Load dataset and run RFPL uncertainty quantification
	df <- load_data("Application_1.csv")

	# Generalized Extreme Value (GEV) Distribution
	validate_rfgpl(
		df,
		"GEV",
		trend_00,
		c(1702.9263, 2442.3196, 2969.9533, 3518.5173, 4290.0510, 4908.4944),
		c(1852.7564, 2679.6241, 3288.2573, 3922.5641, 4823.9808, 5564.3972),
		c(2019.0482, 2963.9259, 3668.3303, 4419.4717, 5497.4190, 6391.6196),
	)

	# Mean-Trend Generalized Extreme Value (GEV100) Distribution
	validate_rfgpl(
		df,
		"GEV",
		trend_10, 
		c(1474.8399, 2230.1006, 2766.5121, 3314.3495, 4078.8225, 4696.9330),
		c(1729.5074, 2544.4615, 3144.6884, 3770.5317, 4660.3977, 5391.7103),
		c(1994.6171, 2904.0166, 3592.7090, 4322.1180, 5358.6528, 6236.7929),
		min(df$year)
	)

	validate_rfgpl(
		df,
		"GEV",
		trend_10,
		c(1727.0595, 2489.0774, 3022.5734, 3577.3108, 4339.2693, 4954.5987),
		c(1959.8133, 2774.7674, 3374.9943, 4000.8376, 4890.7037, 5622.0162),
		c(2199.2593, 3107.0631, 3796.5272, 4527.1532, 5581.6741, 6439.1248),
		max(df$year)
	)

	# Mean + Variance Generalized Extreme Value (GEV110) Distribution
	validate_rfgpl(
		df,
		"GEV",
		trend_11,
		c(1472.2997, 2165.6533, 2653.1161, 3154.0474, 3859.5708, 4439.9176),
		c(1760.2623, 2611.9013, 3239.3026, 3893.6073, 4824.1462, 5589.0489),
		c(2134.9612, 3236.3356, 4085.1330, 4979.3474, 6261.8499, 7324.1361),
		min(df$year)
	)

	validate_rfgpl(
		df,
		"GEV",
		trend_11,
		c(1630.1067, 2212.7119, 2615.7992, 3039.7553, 3642.2869, 4121.0152),
		c(1927.2545, 2700.8260, 3270.7150, 3865.0414, 4710.2802, 5405.0664),
		c(2230.9144, 3236.0134, 3971.3349, 4773.2613, 5921.5257, 6871.0598),
		max(df$year)
	)

})
