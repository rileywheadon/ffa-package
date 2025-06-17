test_that("Test qnt-functions.R", {

	# Test quagev on the median of the GEV distribution
	result <- qntgev(0.5, c(0, 1, 0))
	expect_equal(result, -log(log(2)))

	# Test on multiple probabilities
	# result <- qntgev(c(0.1, 0.25, 0.5, 0.75, 0.9), c(0, 1, 0))
	# expect_equal(length(result), 5)

	# Test on multiple years
	# result <- qntgev100(0.5, c(0, 1, 1, 0), c(1, 2, 3), c(1, 2, 3 ))
	# expect_equal(length(result), 3)

	# Test on multiple years and probabilities
	# result <- qntgev100(c(0.5, 0.9), c(0, 1, 1, 0), c(1, 2, 3), c(1, 2, 3 ))
	# expect_equal(nrow(result), 2)
	# expect_equal(ncol(result), 3)

})


