test_lmom <- function(model, params, expected) {

	result_fast <- lmom_fast(model, params)

	result_method <- switch(
		model,
		"GUM" = lmom_theoretical_gum(params),
		"NOR" = lmom_theoretical_nor(params),
		"GEV" = lmom_theoretical_gev(params),
		"GLO" = lmom_theoretical_glo(params),
		"GNO" = lmom_theoretical_gno(params),
		"PE3" = lmom_theoretical_pe3(params),
		"WEI" = lmom_theoretical_wei(params)
	)

	expect_equal(result_fast  , expected, tol = 1e-4)
	expect_equal(result_method, expected, tol = 1e-4)

}

test_that("lmom_theoretical_gum matches lmr functions from lmom package.", {
	test_lmom("GUM", c(1636, 698), c(2038.8965, 483.8167, 0.1699, 0.1504))
	test_lmom("GUM", c(29, 15), c(37.6582, 10.3972, 0.1699, 0.1504))
})

test_that("lmom_theoretical_nor matches lmr functions from lmom package.", {
	test_lmom("NOR", c(1636, 698), c(1636, 393.8043, 0, 0.1226))
	test_lmom("NOR", c(29, 15), c(29, 8.4628, 0, 0.1226))
})

test_that("lmom_theoretical_gev matches lmr functions from lmom package.", {
	test_lmom("GEV", c(1600, 616, 0.12), c(1889.5704, 386.5921, 0.0951, 0.1225))
	test_lmom("GEV", c(29, 15, -0.09), c(39.1155, 11.3795, 0.2291, 0.1764))
})

test_that("lmom_theoretical_glo matches lmr functions from lmom package.", {
	test_lmom("GLO", c(1600, 616, 0.12), c(1476.3597, 630.8368, -0.12, 0.1787))
	test_lmom("GLO", c(29, 15, -0.09), c(31.2415, 15.2017, 0.09, 0.1734))
})

test_that("lmom_theoretical_gno matches lmr functions from lmom package.", {
	test_lmom("GNO", c(1600, 616, 0.12), c(1562.9066, 349.6352, -0.0586, 0.1253))
	test_lmom("GNO", c(1592, 686, -0.23), c(1671.9426, 395.6627, 0.1120, 0.1325))
	test_lmom("GNO", c(202, 57, -0.33), c(211.6658, 33.6527, 0.1603, 0.1428))
	test_lmom("GNO", c(68, 20, -0.37), c(71.8296, 11.9468, 0.1794, 0.1479))
	test_lmom("GNO", c(29, 15, -0.09), c(29.6764, 8.4915, 0.0440, 0.1241))
})

test_that("lmom_theoretical_pe3 matches lmr functions from lmom package.", {
	test_lmom("PE3", c(1600, 616, 0.12), c(1600, 347.3844, 0.0195, 0.1227))
	test_lmom("PE3", c(1675, 714, 0.7), c(1675, 396.7138, 0.1147, 0.1266))
	test_lmom("PE3", c(212, 62, 1), c(212, 33.9062, 0.1647, 0.1313))
	test_lmom("PE3", c(72, 22, 1), c(72, 12.0312, 0.1647, 0.1313))
	test_lmom("PE3", c(29, 15, -0.09), c(29, 8.4607, -0.0147, 0.1227))
})

test_that("lmom_theoretical_wei matches lmr functions from lmom package.", {
	test_lmom("WEI", c(883, 1246, 1.3), c(2033.7766, 475.5812, 0.2392, 0.1296))
	test_lmom("WEI", c(1.96, 40.89, 1.99), c(38.2012, 10.6594, 0.1152, 0.1055))
})
