test_that("eda-spearman-test.R works on ATHABASCA RIVER (07BE001)", {
	df <- data_local("CAN-07BE001.csv")
	result <- eda_spearman_test(df$max)
	expect_equal(result$least_lag, 1)
})

test_that("eda-spearman-test.R works on KOOTENAI RIVER (08NH021)", {
	df <- data_local("CAN-08NH021.csv")
	result <- eda_spearman_test(df$max)
	expect_equal(result$least_lag, 17)
})

test_that("eda-spearman-test.R works on BOW RIVER (05BB001)", {
	df <- data_local("CAN-05BB001.csv")
	result <- eda_spearman_test(df$max)
	expect_equal(result$least_lag, 1)
})

test_that("eda-spearman-test.R works on CHILLIWACK RIVER (08MH016)", {
	df <- data_local("CAN-08MH016.csv")
	result <- eda_spearman_test(df$max)
	expect_equal(result$least_lag, 1)
})

test_that("eda-spearman-test.R works on OKANAGAN RIVER (08NM050)", {
	df <- data_local("CAN-08NM050.csv")
	result <- eda_spearman_test(df$max)
	expect_equal(result$least_lag, 5)
})
