# Helper function to validate results
validate <- function(df, model, theta, expected) {
	result <- likelihood(df, model, theta)
	expect_equal(result, expected, tol = 1e-6)
}

test_that("Test log-likelihood.R on data set #1", {

	# Load dataset and run L-moments estimation
	df <- load_data("Application_1.csv", clean = FALSE)

	# Gumbel (GUM) Distribution
	validate(df, "GUM", c(1642.8856, 665.2585), -825.7803)
	validate(df, "GUM10", c(1544.6436, 191.4887, 661.2450), -825.3975)
	validate(df, "GUM11", c(1607.7145, 72.8119, 704.3887, -82.8341), -825.5232)

	# Normal (NOR) Distribution
	validate(df, "NOR", c(2039.1863, 913.5010), -840.0948)
	validate(df, "NOR10", c(1925.8261, 219.0145, 911.2851), -839.8470)
	validate(df, "NOR11", c(1922.3793, 225.7007, 888.1657, 44.6007), -839.8329)

	# Log-Normal (LNO) Distribution
	validate(df, "LNO", c(7.5285, 0.4323), -827.0937)
	validate(df, "LNO10", c(7.4614, 0.1297, 0.4306), -826.7053)
	validate(df, "LNO11", c(7.4672, 0.1185, 0.4701, -0.0775), -826.4169)

	# Generalized Extreme Value (GEV) Distribution
	validate(df, "GEV", c(1624.9889, 655.3234, 0.0510), -825.4411)
	validate(df, "GEV100", c(1514.1289, 208.6491, 648.0107, 0.0616), -824.9450)
	validate(df, "GEV110", c(1598.8487, 43.8234, 709.4190, -122.6525, 0.0724), -825.0452)

	# Generalized Logistic (GLO) Distribution
	validate(df, "GLO", c(1852.1263, 439.0156, -0.2317), -825.2516)
	validate(df, "GLO100", c(1759.0493, 178.2527, 439.0925, -0.2413), -824.9231)
	validate(df, "GLO110", c(1783.4531, 127.5409, 457.1615, -33.9808, -0.2463), -824.9262)

	# Generalized Normal (GNO) Distribution
	validate(df, "GNO", c(1885.3744, 797.8436, -0.3654), -826.2868)
	validate(df, "GNO100", c(1739.6437, 263.5634, 792.1433, -0.3904), -825.5555)
	validate(df, "GNO110", c(1790.4015, 156.4289, 851.8465, -115.2775, -0.4041), -825.4486)

	# Pearson Type III (PE3) Distribution
	validate(df, "PE3", c(2039.1863, 865.5583, 0.8802), -827.6846)
	validate(df, "PE3100", c(1864.6692, 337.1710, 870.5665, 0.9708), -826.6893)
 	validate(df, "PE3110", c(1937.9671, 195.8764, 954.1574, -149.2599, 1.0140), -826.4840)

	# Log-Pearson Type III (LP3) Distribution
	validate(df, "LP3", c(7.5285, 0.4318, -0.1298), -826.8326)
	validate(df, "LP3100", c(7.4651, 0.1224, 0.4303, -0.1204), -826.4880)
	validate(df, "LP3110", c(7.4663, 0.1202, 0.4634, -0.0652, -0.0946), -826.3017)

	# Weibull (WEI) Distribution
	validate(df, "WEI", c(274.1452, 1995.0369, 2.0333), -831.8793)
	validate(df, "WEI100", c(187.7623, 607.1417, 1728.4916, 1.7550), -829.1212)
	validate(df, "WEI110", c(168.0173, 704.2308, 1930.5430, -467.0824, 1.7092), -828.4383)

})

test_that("Test log-likelihood.R on data set #2", {

	# Load dataset and run L-moments estimation
	df <- load_data("Application_2.csv", clean = FALSE)

	# Gumbel (GUM) Distribution
	validate(df, "GUM", c(1346.6774, 567.9530), -720.7467)
	validate(df, "GUM10", c(1931.2031, -1069.9191, 501.7051), -707.4183)
	validate(df, "GUM11", c(1922.3495, -1054.9372, 670.2176, -336.8644), -705.0993)

	# Normal (NOR) Distribution
	validate(df, "NOR", c(1675.0989, 692.5677), -724.3004)
	validate(df, "NOR10", c(2340.7134, -1331.2289, 573.3126), -707.1036)
	validate(df, "NOR11", c(2260.4196, -1187.1658, 831.4482, -521.8119), -703.1886)

	# Log-Normal (LNO) Distribution
	validate(df, "LNO", c(7.3337, 0.4337), -720.4715)
	validate(df, "LNO10", c(7.7422, -0.8169, 0.3623), -704.0965)
	validate(df, "LNO11", c(7.7430, -0.8186, 0.3508, 0.0230), -704.0770)

	# Generalized Extreme Value (GEV) Distribution
	validate(df, "GEV", c(1363.8333, 580.6468, -0.0557), -720.6244)
	validate(df, "GEV100", c(2048.9743, -1210.1553, 530.1797, -0.1702), -705.8908)
	validate(df, "GEV110", c(1976.5663, -1029.8465, 804.9656, -502.5558, -0.2589), -702.1744)

	# Generalized Logistic (GLO) Distribution
	validate(df, "GLO", c(1547.6390, 399.1187, -0.2599), -723.3497)
	validate(df, "GLO100", c(2244.4597, -1211.9495, 334.9422, -0.0898), -708.5882)
	validate(df, "GLO110", c(1922.6276, -556.9695, 271.2343, 249.5252, -0.2416), -716.6764)

	# Generalized Normal (GNO) Distribution
	validate(df, "GNO", c(1554.3044, 662.5788, -0.3608), -720.2721)
	validate(df, "GNO100", c(2220.9590, -1188.8260, 567.0406, -0.1706), -706.1026)
	validate(df, "GNO110", c(1838.1028, -378.2154, 465.8349, 418.8941, -0.3864), -716.0404)

	# Pearson Type III (PE3) Distribution
	validate(df, "PE3", c(1675.0989, 723.9569, 1.0536), -719.3150)
	validate(df, "PE3100", c(2250.7148, -1151.2319, 584.9250, 0.6268), -705.8747)
	validate(df, "PE3110", c(2283.5224, -1227.2148, 797.4393, -453.5283, 0.4088), -702.8271)

	# Log-Pearson Type III (LP3) Distribution
	validate(df, "LP3", c(7.3337, 0.4416, -0.5963), -719.3636)
	validate(df, "LP3100", c(7.7630, -0.8585, 0.3732, -0.8602), -700.9359)
	validate(df, "LP3110", c(7.7324, -0.7978, 0.4314, -0.1047, -0.9507), -700.6605)

	# Weibull (WEI) Distribution
	validate(df, "WEI", c(420.9388, 1413.9838, 1.8856), -718.3923)
	validate(df, "WEI100", c(1038.3840, -1094.0033, 1336.4392, 2.1626), -704.6250)
	validate(df, "WEI110", c(-514.7607, 881.4916, 3005.3321, -2158.5861, 3.2871), -702.3482)

})


test_that("Test log-likelihood.R on data set #3.1", {

	# Load dataset and run L-moments estimation
	df <- load_data("Application_3.1.csv", clean = FALSE)

	# Gumbel (GUM) Distribution
	validate(df, "GUM", c(184.1446, 47.9538), -594.3386)
	validate(df, "GUM10", c(205.3484, -41.0113, 46.5039), -590.9022)
	validate(df, "GUM11", c(206.4051, -43.0881, 48.9653, -5.1762), -590.8057)

	# Normal (NOR) Distribution
	validate(df, "NOR", c(212.0734, 61.4000), -603.4619)
	validate(df, "NOR10", c(232.2718, -40.7639, 60.2607), -601.4205)
	validate(df, "NOR11", c(233.3923, -43.0804, 51.7531, 16.4607), -600.4378)

	# Log-Normal (LNO) Distribution
	validate(df, "LNO", c(5.3177, 0.2777), -594.6497)
	validate(df, "LNO10", c(5.4266, -0.2197, 0.2704), -591.7278)
	validate(df, "LNO11", c(5.4290, -0.2246, 0.2352, 0.0688), -591.0043)

	# Generalized Extreme Value (GEV) Distribution
	validate(df, "GEV", c(183.8460, 47.7538, 0.0115), -594.3270)
	validate(df, "GEV100", c(204.7477, -40.5921, 46.2536, 0.0155), -590.8773)
	validate(df, "GEV110", c(205.8584, -43.4645, 49.4101, -7.0631, 0.0295), -590.7230)

	# Generalized Logistic (GLO) Distribution
	validate(df, "GLO", c(200.9814, 32.8376, -0.2430), -595.9133)
	validate(df, "GLO100", c(223.9083, -45.2347, 31.4018, -0.2285), -591.8553)
	validate(df, "GLO110", c(227.8011, -53.7420, 35.5164, -8.3527, -0.2409), -591.4712)

	# Generalized Normal (GNO) Distribution
	validate(df, "GNO", c(201.2149, 56.3091, -0.3755), -594.0275)
	validate(df, "GNO100", c(220.8535, -38.7119, 54.6325, -0.3689), -590.7329)
	validate(df, "GNO110", c(219.1343, -34.8410, 52.9142, 3.8187, -0.3653), -590.9198)

	# Pearson Type III (PE3) Distribution
	validate(df, "PE3", c(212.0734, 61.6681, 1.0272), -593.7024)
	validate(df, "PE3100", c(229.2088, -34.5822, 60.1513, 1.0443), -590.6219)
	validate(df, "PE3110", c(222.4752, -19.7965, 56.0574, 9.8502, 1.0541), -591.2324)

	# Log-Pearson Type III (LP3) Distribution
	validate(df, "LP3", c(5.3177, 0.2782, 0.2729), -594.1564)
	validate(df, "LP3100", c(5.4345, -0.2357, 0.2706, 0.3557), -590.6484)
	validate(df, "LP3110", c(5.4280, -0.2226, 0.2471, 0.0449, 0.2939), -590.3979)

	# Weibull (WEI) Distribution
	validate(df, "WEI", c(103.7897, 121.9568, 1.8390), -594.1913)
	validate(df, "WEI100", c(127.2839, -28.4333, 110.6002, 1.6817), -590.6628)
	validate(df, "WEI110", c(126.9082, -27.2858, 112.5077, -4.4299, 1.6746), -590.6438)

})

test_that("Test log-likelihood.R on data set #3.2", {

	# Load dataset and run L-moments estimation
	df <- load_data("Application_3.2.csv", clean = FALSE)

	# Gumbel (GUM) Distribution
	validate(df, "GUM", c(62.7897, 16.9775), -392.7957)
	validate(df, "GUM10", c(57.5210, 10.6331, 16.6483), -391.1480)
	validate(df, "GUM11", c(56.0993, 13.4948, 13.8604, 5.6304), -390.3762)

	# Normal (NOR) Distribution
	validate(df, "NOR", c(72.6629, 21.9778), -401.2988)
	validate(df, "NOR10", c(63.3158, 18.2620, 21.2950), -398.4896)
	validate(df, "NOR11", c(62.7709, 19.3921, 13.1027, 15.0461), -394.2257)

	# Log-Normal (LNO) Distribution
	validate(df, "LNO", c(4.2436, 0.2878), -393.1097)
	validate(df, "LNO10", c(4.1262, 0.2293, 0.2796), -390.5320)
	validate(df, "LNO11", c(4.1222, 0.2375, 0.2370, 0.0812), -389.8581)

	# Generalized Extreme Value (GEV) Distribution
	validate(df, "GEV", c(62.6761, 16.9091, 0.0124), -392.7823)
	validate(df, "GEV100", c(57.4508, 10.5096, 16.5596, 0.0147), -391.1321)
	validate(df, "GEV110", c(56.2739, 13.8984, 13.6202, 6.6586, -0.0409), -390.2832)

	# Generalized Logistic (GLO) Distribution
	validate(df, "GLO", c(68.6821, 11.4113, -0.2297), -393.4542)
	validate(df, "GLO100", c(64.0017, 9.2868, 11.3385, -0.2428), -392.2140)
	validate(df, "GLO110", c(62.4756, 13.0809, 9.5061, 3.5623, -0.2113), -391.7270)

	# Generalized Normal (GNO) Distribution
	validate(df, "GNO", c(68.9944, 19.9741, -0.3556), -392.7902)
	validate(df, "GNO100", c(63.2561, 10.9178, 19.5535, -0.3798), -390.8963)
	validate(df, "GNO110", c(61.0151, 16.4311, 15.9674, 6.9047, -0.3227), -390.1560)

	# Pearson Type III (PE3) Distribution
	validate(df, "PE3", c(72.6629, 21.6678, 0.9502), -392.9510)
	validate(df, "PE3100", c(66.5593, 11.9250, 21.7010, 1.0999), -390.4990)
	validate(df, "PE3110", c(63.0150, 18.8665, 17.5339, 7.0019, 0.9773), -389.7870)

	# Log-Pearson Type III (LP3) Distribution
	validate(df, "LP3", c(4.2436, 0.2878, 0.2100), -392.7880)
	validate(df, "LP3100", c(4.1316, 0.2188, 0.2798, 0.1637), -390.3978)
	validate(df, "LP3110", c(4.1221, 0.2376, 0.2365, 0.0821, -0.0086), -389.8578)

	# Weibull (WEI) Distribution
	validate(df, "WEI", c(33.3648, 44.2562, 1.8632), -393.9233)
	validate(df, "WEI100", c(31.3702, 13.0563, 38.6399, 1.6516), -390.0178)
	validate(df, "WEI110", c(32.0849, 9.7844, 32.9010, 13.9106, 1.7615), -389.0661)

})

test_that("Test log-likelihood.R on data set #3.3", {

	# Load dataset and run L-moments estimation
	df <- load_data("Application_3.3.csv", clean = FALSE)

	# Gumbel (GUM) Distribution
	validate(df, "GUM", c(29.3768, 15.2386), -417.4351)
	validate(df, "GUM10", c(19.6970, 21.4646, 14.5759), -408.6744)
	validate(df, "GUM11", c(17.2397, 27.1009, 6.1992, 15.3335), -398.9633)

	# Normal (NOR) Distribution
	validate(df, "NOR", c(38.2095, 18.5118), -420.7229)
	validate(df, "NOR10", c(20.7332, 34.9525, 15.4519), -403.1971)
	validate(df, "NOR11", c(20.8458, 34.7144, 11.1524, 8.1526), -400.5763)

	# Log-Normal (LNO) Distribution
	validate(df, "LNO", c(3.5110, 0.5373), -417.9424)
	validate(df, "LNO10", c(3.0413, 0.9394, 0.4622), -403.3356)
	validate(df, "LNO11", c(3.0352, 0.9518, 0.4039, 0.1143), -402.8411)

	# Generalized Extreme Value (GEV) Distribution
	validate(df, "GEV", c(29.9497, 15.6802, -0.0695), -417.2748)
	validate(df, "GEV100", c(17.3249, 29.9814, 15.2606, -0.2243), -404.0062)
	validate(df, "GEV110", c(17.5748, 27.2660, 6.9529, 13.8717, -0.0585), -398.4624)

	# Generalized Logistic (GLO) Distribution
	validate(df, "GLO", c(34.5014, 10.7666, -0.2822), -420.3594)
	validate(df, "GLO100", c(18.5655, 40.2597, 8.5042, 0.0261), -402.0643)
	validate(df, "GLO110", c(19.0582, 37.1268, 3.5129, 10.0707, -0.0875), -395.5915)

	# Generalized Normal (GNO) Distribution
	validate(df, "GNO", c(34.8783, 17.7926, -0.3713), -416.8789)
	validate(df, "GNO100", c(20.7749, 34.7525, 15.4511, -0.0076), -403.1923)
	validate(df, "GNO110", c(19.9512, 33.3654, 7.7105, 15.0946, -0.2059), -397.5878)

	# Pearson Type III (PE3) Distribution
	validate(df, "PE3", c(38.2095, 19.7334, 1.1388), -415.5614)
	validate(df, "PE3100", c(20.8279, 34.7632, 15.4518, 0.0214), -403.1926)
	validate(df, "PE3110", c(21.5611, 33.4284, 18.1247, -3.9291, -0.0906), -405.8514)

	# Log-Pearson Type III (LP3) Distribution
	validate(df, "LP3", c(3.5110, 0.5830, -1.1800), -413.5910)
	validate(df, "LP3100", c(3.0610, 0.8999, 0.4532, -0.5190), -399.2227)
	validate(df, "LP3110", c(3.0492, 0.9241, 0.5382, -0.1531, -0.6932), -398.7725)

	# Weibull (WEI) Distribution
	validate(df, "WEI", c(6.3503, 35.7649, 1.7612), -414.0199)
	validate(df, "WEI100", c(-27.8400, 32.6232, 55.1689, 3.5055), -403.7484)
	validate(df, "WEI110", c(4.9117, 1.2853, 20.1114, 32.4985, 2.0981), -398.7885)

})
