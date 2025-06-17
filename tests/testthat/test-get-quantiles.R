test_that("Test get-quantiles.R", {

	# Test get.quantiles on the median of the GEV distribution
	result <- get.quantiles(0.5, "GEV", c(0, 1, 0))
	expect_equal(result[1, 1], -log(log(2)))

	# Test on multiple probabilities
	result <- get.quantiles(c(0.1, 0.25, 0.5, 0.75, 0.9), "GEV", c(0, 1, 0))
	expect_equal(nrow(result), 5)
	expect_equal(ncol(result), 1)

	# Test on multiple years
	result <- get.quantiles(0.5, "GEV", c(0, 1, 0), c(1900, 1950, 2000))
	expect_equal(nrow(result), 1)
	expect_equal(ncol(result), 3)

	# Test with "collapse" option.
	result <- get.quantiles(c(0.5, 0.4, 0.6), "GEV", c(0, 1, 0), c(1900, 1950, 2000), TRUE)
	expect_equal(nrow(result), 1)
	expect_equal(ncol(result), 3)

})


