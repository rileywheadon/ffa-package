test_that("Test eda-spearman-test.R on data set #1", {
	df <- load_data("Application_1.csv")
	result <- eda_spearman_test(df$max)
	expect_equal(result$least_lag, 1)
})

test_that("Test eda-spearman-test.R on data set #2", {
	df <- load_data("Application_2.csv")
	result <- eda_spearman_test(df$max)
	expect_equal(result$least_lag, 17)
})

test_that("Test eda-spearman-test.R on data set #3.1", {
	df <- load_data("Application_3.1.csv")
	result <- eda_spearman_test(df$max)
	expect_equal(result$least_lag, 1)
})

test_that("Test eda-spearman-test.R on data set #3.2", {
	df <- load_data("Application_3.2.csv")
	result <- eda_spearman_test(df$max)
	expect_equal(result$least_lag, 1)
})

test_that("Test eda-spearman-test.R on data set #3.3", {
	df <- load_data("Application_3.3.csv")
	result <- eda_spearman_test(df$max)
	expect_equal(result$least_lag, 5)
})
