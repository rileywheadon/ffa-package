test_that("framework-ffa works on KOOTENAI RIVER (08NH021) with no splits/structures", {

	df <- data_local("CAN-08NH021.csv")

	results <- framework_ffa(df$max, df$year)
	expect_true(is.list(results))
	expect_equal(length(results$submodules), 4)

	summary <- results$summary
	expect_equal(summary$approach, "S-FFA")
	expect_equal(summary$ns_splits, integer(0))
	expect_equal(summary$ns_structures, list(S00))

})

test_that("framework-ffa works on KOOTENAI RIVER (08NH021) with splits/structures", {

	df <- data_local("CAN-08NH021.csv")

	results <- framework_ffa(df$max, df$year, 1972, list(S10, S10))
	expect_true(is.list(results))
	expect_equal(length(results$submodules), 8)

	summary <- results$summary
	expect_equal(summary$approach, "Piecewise NS-FFA")
	expect_equal(summary$ns_splits, 1972)
	expect_equal(summary$ns_structures, list(S10, S10))

})
