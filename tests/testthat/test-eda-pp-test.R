test_that("eda-pp-test.R works on ATHABASCA RIVER (07BE001)", {

	# Run the PP test
	df <- data_local("CAN-07BE001.csv")
	test <- eda_pp_test(df$max)

	# Test the results against aTSA:pp.test
	expect_equal(test$statistic, -96.6503, tol = 1e-4)
	expect_equal(test$p_value, 0.01, tol = 1e-4)
	expect_equal(test$reject, TRUE)

})

test_that("eda-pp-test.R works on KOOTENAI RIVER (08NH021)", {

	# Run the PP test
	df <- data_local("CAN-08NH021.csv")
	test <- eda_pp_test(df$max)

	# Test the results against aTSA:pp.test
	expect_equal(test$statistic, -40.21905, tol = 1e-4)
	expect_equal(test$p_value, 0.01, tol = 1e-4)
	expect_equal(test$reject, TRUE)

})

test_that("eda-pp-test.R works on BOW RIVER (05BB001)", {

	# Run the PP test
	df <- data_local("CAN-05BB001.csv")
	test <- eda_pp_test(df$max)

	# Test the results against aTSA:pp.test
	expect_equal(test$statistic, -96.5249, tol = 1e-4)
	expect_equal(test$p_value, 0.01, tol = 1e-4)

})

test_that("eda-pp-test.R works on CHILLIWACK RIVER (08MH016)", {

	# Run the PP test
	df <- data_local("CAN-08MH016.csv")
	test <- eda_pp_test(df$max)

	# Test the results against aTSA:pp.test
	expect_equal(test$statistic, -90.0313, tol = 1e-4)
	expect_equal(test$p_value, 0.01, tol = 1e-4)
	expect_equal(test$reject, TRUE)

})

test_that("eda-pp-test.R works on OKANAGAN RIVER (08NM050)", {

	# Run the PP test
	df <- data_local("CAN-08NM050.csv")
	test <- eda_pp_test(df$max)

	# Test the results against aTSA:pp.test
	expect_equal(test$statistic, -90.0663, tol = 1e-4)
	expect_equal(test$p_value, 0.01, tol = 1e-4)
	expect_equal(test$reject, TRUE)

})

test_that("eda-pp-test.R fails to reject the null when the data has a unit root", {
	set.seed(1)

	n  <- 100   # number of data points
	b0 <- 0.1   # stationary drift
	b1 <- 0.02  # linear trend
	sd <- 0.1   # random walk innovation
	
	# Generate synthetic data with a unit root
	t <- 1:n
    data <- numeric(n)
    data[1] <- 0

  	for (i in 2:n) {
    	eps <- rnorm(1, mean = 0, sd = sd)
    	data[i] <- b0 + b1 * t[i] + data[i - 1] + eps
  	}

	# Run the Phillips-Perron test on the synthetic data
	test <- eda_pp_test(data)
	expect_equal(test$p_value, 0.99)
	expect_equal(test$reject, FALSE)
})


