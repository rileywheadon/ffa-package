test_that("fit_lmom methods work on data set #1", {

	# Load dataset and run L-moments estimation
	df <- load_data("Application_1.csv")

	# Gumbel (GUM) Distribution
	GUM <- fit_lmom_gum(df$max)
	expect_equal(GUM[1], 1636.0054, tol = 1e-3)
	expect_equal(GUM[2],  698.4925, tol = 1e-3)

	# Normal (NOR) Distribution
	NOR <- fit_lmom_nor(df$max)
	expect_equal(NOR[1], 2039.1863, tol = 1e-3)
	expect_equal(NOR[2],  858.1479, tol = 1e-3)

	# Log-Normal (LNO) Distribution
	LNO <- fit_lmom_lno(df$max)
	expect_equal(LNO[1], 7.5290, tol = 1e-3)
	expect_equal(LNO[2], 0.4272, tol = 1e-3)

	# Generalized Extreme Value (GEV) Distribution
	GEV <- fit_lmom_gev(df$max)
	expect_equal(GEV[1], 1600.2199, tol = 1e-3)
	expect_equal(GEV[2],  616.6660, tol = 1e-3)
	expect_equal(GEV[3],    0.1207, tol = 1e-3)

	# Generalized Logistic (GLO) Distribution
	GLO <- fit_lmom_glo(df$max)
	expect_equal(GLO[1], 1846.4844, tol = 1e-3)
	expect_equal(GLO[2],  436.0754, tol = 1e-3)
	expect_equal(GLO[3],   -0.2495, tol = 1e-3)

	# Generalized Normal (GNO) Distribution
	GNO <- fit_lmom_gno(df$max)
	expect_equal(GNO[1], 1826.4036, tol = 1e-3)
	expect_equal(GNO[2],  767.1099, tol = 1e-3)
	expect_equal(GNO[3],   -0.5183, tol = 1e-3)

	# Pearson Type III (PE3) Distribution
	PE3 <- fit_lmom_pe3(df$max)
	expect_equal(PE3[1], 2039.1863, tol = 1e-3)
	expect_equal(PE3[2],  920.0709, tol = 1e-3)
	expect_equal(PE3[3],    1.5024, tol = 1e-3)

	# Log-Pearson Type III (LP3) Distribution
	LP3 <- fit_lmom_lp3(df$max)
	expect_equal(LP3[1], 7.5285, tol = 1e-3)
	expect_equal(LP3[2], 0.4242, tol = 1e-3)
	expect_equal(LP3[3], 0.1467, tol = 1e-3)

	# Weibull (WEI) Distribution
	WEI <- fit_lmom_wei(df$max)
	expect_equal(WEI[1],  883.8205, tol = 5e-2)
	expect_equal(WEI[2], 1246.1981, tol = 5e-2)
	expect_equal(WEI[3],    1.2763, tol = 5e-2)

})


test_that("fit_lmom methods work on data set #2", {

	# Load dataset and run L-moments estimation
	df <- load_data("Application_2.csv")

	# Gumbel (GUM) Distribution
	GUM <- fit_lmom_gum(df$max)
	expect_equal(GUM[1], 1344.8637, tol = 1e-3)
	expect_equal(GUM[2],  572.1175, tol = 1e-3)

	# Normal (NOR) Distribution
	NOR <- fit_lmom_nor(df$max)
	expect_equal(NOR[1], 1675.0989, tol = 1e-3)
	expect_equal(NOR[2],  702.8872, tol = 1e-3)

	# Log-Normal (LNO) Distribution
	LNO <- fit_lmom_lno(df$max)
	expect_equal(LNO[1], 7.3329, tol = 1e-3)
	expect_equal(LNO[2], 0.4260, tol = 1e-3)

	# Generalized Extreme Value (GEV) Distribution
	GEV <- fit_lmom_gev(df$max)
	expect_equal(GEV[1], 1368.3942, tol = 1e-3)
	expect_equal(GEV[2],  615.9079, tol = 1e-3)
	expect_equal(GEV[3],   -0.0864, tol = 1e-3)

	# Generalized Logistic (GLO) Distribution
	GLO <- fit_lmom_glo(df$max)
	expect_equal(GLO[1], 1600.0150, tol = 1e-3)
	expect_equal(GLO[2],  387.8618, tol = 1e-3)
	expect_equal(GLO[3],   -0.1159, tol = 1e-3)

	# Generalized Normal (GNO) Distribution
	GNO <- fit_lmom_gno(df$max)
	expect_equal(GNO[1], 1592.2772, tol = 1e-3)
	expect_equal(GNO[2],  686.5012, tol = 1e-3)
	expect_equal(GNO[3],   -0.2379, tol = 1e-3)

	# Pearson Type III (PE3) Distribution
	PE3 <- fit_lmom_pe3(df$max)
	expect_equal(PE3[1], 1675.0989, tol = 1e-3)
	expect_equal(PE3[2],  713.9522, tol = 1e-3)
	expect_equal(PE3[3],    0.7072, tol = 1e-3)

	# Log-Pearson Type III (LP3) Distribution
	LP3 <- fit_lmom_lp3(df$max)
	expect_equal(LP3[1],  7.3337, tol = 1e-3)
	expect_equal(LP3[2],  0.4457, tol = 1e-3)
	expect_equal(LP3[3], -0.3270, tol = 1e-3)

	# Weibull (WEI) Distribution
	WEI <- fit_lmom_wei(df$max)
	expect_equal(WEI[1],  327.6753, tol = 5e-2)
	expect_equal(WEI[2], 1520.2381, tol = 5e-2)
	expect_equal(WEI[3],    1.9885, tol = 5e-2)

})

test_that("fit_lmom methods work on data set #3.1", {

	# Load dataset and run L-moments estimation
	df <- load_data("Application_3.1.csv")

	# Gumbel (GUM) Distribution
	GUM <- fit_lmom_gum(df$max)
	expect_equal(GUM[1], 183.8368, tol = 1e-3)
	expect_equal(GUM[2],  48.9185, tol = 1e-3)

	# Normal (NOR) Distribution
	NOR <- fit_lmom_nor(df$max)
	expect_equal(NOR[1], 212.0734, tol = 1e-3)
	expect_equal(NOR[2],  60.0999, tol = 1e-3)

	# Log-Normal (LNO) Distribution
	LNO <- fit_lmom_lno(df$max)
	expect_equal(LNO[1], 5.3162, tol = 1e-3)
	expect_equal(LNO[2], 0.2853, tol = 1e-3)

	# Generalized Extreme Value (GEV) Distribution
	GEV <- fit_lmom_gev(df$max)
	expect_equal(GEV[1],  184.1039, tol = 1e-3)
	expect_equal(GEV[2],   49.4535, tol = 1e-3)
	expect_equal(GEV[3],   -0.0119, tol = 1e-3)

	# Generalized Normal (GNO) Distribution
	GNO <- fit_lmom_gno(df$max)
	expect_equal(GNO[1], 202.2116, tol = 1e-3)
	expect_equal(GNO[2],  57.3632, tol = 1e-3)
	expect_equal(GNO[3],  -0.3343, tol = 1e-3)

	# Generalized Logistic (GLO) Distribution
	GLO <- fit_lmom_glo(df$max)
	expect_equal(GLO[1], 203.1358, tol = 1e-3)
	expect_equal(GLO[2],  32.4568, tol = 1e-3)
	expect_equal(GLO[3],  -0.1623, tol = 1e-3)

	# Pearson Type III (PE3) Distribution
	PE3 <- fit_lmom_pe3(df$max)
	expect_equal(PE3[1], 212.0734, tol = 1e-3)
	expect_equal(PE3[2],  61.9498, tol = 1e-3)
	expect_equal(PE3[3],   0.9861, tol = 1e-3)

	# Log-Pearson Type III (LP3) Distribution
	LP3 <- fit_lmom_lp3(df$max)
	expect_equal(LP3[1], 5.3177, tol = 1e-3)
	expect_equal(LP3[2], 0.2823, tol = 1e-3)
	expect_equal(LP3[3], 0.2029, tol = 1e-3)

	# Weibull (WEI) Distribution
	WEI <- fit_lmom_wei(df$max)
	expect_equal(WEI[1], 111.9752, tol = 5e-2)
	expect_equal(WEI[2], 112.0731, tol = 5e-2)
	expect_equal(WEI[3],   1.6758, tol = 5e-2)

})


test_that("fit_lmom methods work on data set #3.2", {

	# Load dataset and run L-moments estimation
	df <- load_data("Application_3.2.csv")

	# Gumbel (GUM) Distribution
	GUM <- fit_lmom_gum(df$max)
	expect_equal(GUM[1], 62.6725, tol = 1e-3)
	expect_equal(GUM[2], 17.3080, tol = 1e-3)

	# Normal (NOR) Distribution
	NOR <- fit_lmom_nor(df$max)
	expect_equal(NOR[1], 72.6629, tol = 1e-3)
	expect_equal(NOR[2], 21.2642, tol = 1e-3)

	# Log-Normal (LNO) Distribution
	LNO <- fit_lmom_lno(df$max)
	expect_equal(LNO[1], 4.2424, tol = 1e-3)
	expect_equal(LNO[2], 0.2948, tol = 1e-3)

	# Generalized Extreme Value (GEV) Distribution
	GEV <- fit_lmom_gev(df$max)
	expect_equal(GEV[1], 62.5092, tol = 1e-3)
	expect_equal(GEV[2], 16.9703, tol = 1e-3)
	expect_equal(GEV[3],  0.0209, tol = 1e-3)

	# Generalized Logistic (GLO) Distribution
	GLO <- fit_lmom_glo(df$max)
	expect_equal(GLO[1], 69.1038, tol = 1e-3)
	expect_equal(GLO[2], 11.3444, tol = 1e-3)
	expect_equal(GLO[3], -0.1834, tol = 1e-3)

	# Generalized Normal (GNO) Distribution
	GNO <- fit_lmom_gno(df$max)
	expect_equal(GNO[1], 68.7351, tol = 1e-3)
	expect_equal(GNO[2], 20.0322, tol = 1e-3)
	expect_equal(GNO[3], -0.3783, tol = 1e-3)

	# Pearson Type III (PE3) Distribution
	PE3 <- fit_lmom_pe3(df$max)
	expect_equal(PE3[1], 72.6629, tol = 1e-3)
	expect_equal(PE3[2], 22.0977, tol = 1e-3)
	expect_equal(PE3[3],  1.1114, tol = 1e-3)

	# Log-Pearson Type III (LP3) Distribution
	LP3 <- fit_lmom_lp3(df$max)
	expect_equal(LP3[1], 4.2436, tol = 1e-3)
	expect_equal(LP3[2], 0.2907, tol = 1e-3)
	expect_equal(LP3[3], 0.2560, tol = 1e-3)

	# Weibull (WEI) Distribution
	WEI <- fit_lmom_wei(df$max)
	expect_equal(WEI[1], 39.1979, tol = 5e-2)
	expect_equal(WEI[2], 37.2367, tol = 5e-2)
	expect_equal(WEI[3],  1.5614, tol = 5e-2)

})


test_that("fit_lmom methods work on data set #3.3", {

	# Load dataset and run L-moments estimation
	df <- load_data("Application_3.3.csv")

	# Gumbel (GUM) Distribution
	GUM <- fit_lmom_gum(df$max)
	expect_equal(GUM[1], 29.3534, tol = 1e-3)
	expect_equal(GUM[2], 15.3427, tol = 1e-3)

	# Normal (NOR) Distribution
	NOR <- fit_lmom_nor(df$max)
	expect_equal(NOR[1], 38.2095, tol = 1e-3)
	expect_equal(NOR[2], 18.8496, tol = 1e-3)

	# Log-Normal (LNO) Distribution
	LNO <- fit_lmom_lno(df$max)
	expect_equal(LNO[1], 3.5162, tol = 1e-3)
	expect_equal(LNO[2], 0.5038, tol = 1e-3)

	# Generalized Extreme Value (GEV) Distribution
	GEV <- fit_lmom_gev(df$max)
	expect_equal(GEV[1], 29.9956, tol = 1e-3)
	expect_equal(GEV[2], 16.5362, tol = 1e-3)
	expect_equal(GEV[3], -0.0878, tol = 1e-3)

	# Generalized Logistic (GLO) Distribution
	GLO <- fit_lmom_glo(df$max)
	expect_equal(GLO[1], 36.2114, tol = 1e-3)
	expect_equal(GLO[2], 10.4051, tol = 1e-3)
	expect_equal(GLO[3], -0.1150, tol = 1e-3)

	# Generalized Normal (GNO) Distribution
	GNO <- fit_lmom_gno(df$max)
	expect_equal(GNO[1], 36.0055, tol = 1e-3)
	expect_equal(GNO[2], 18.4170, tol = 1e-3)
	expect_equal(GNO[3], -0.2360, tol = 1e-3)

	# Pearson Type III (PE3) Distribution
	PE3 <- fit_lmom_pe3(df$max)
	expect_equal(PE3[1], 38.2095, tol = 1e-3)
	expect_equal(PE3[2], 19.1418, tol = 1e-3)
	expect_equal(PE3[3],  0.7018, tol = 1e-3)

	# Log-Pearson Type III (LP3) Distribution
	LP3 <- fit_lmom_lp3(df$max)
	expect_equal(LP3[1],  3.5110, tol = 1e-3)
	expect_equal(LP3[2],  0.5504, tol = 1e-3)
	expect_equal(LP3[3], -0.5416, tol = 1e-3)

	# Weibull (WEI) Distribution
	WEI <- fit_lmom_wei(df$max)
	expect_equal(WEI[1],  1.9676, tol = 5e-2)
	expect_equal(WEI[2], 40.8929, tol = 5e-2)
	expect_equal(WEI[3],  1.9956, tol = 5e-2)

})
