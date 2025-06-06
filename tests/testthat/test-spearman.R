test_that("Test spearman-test.R on data set #1", {
	df <- load_data("Application_1.csv")
	test <- spearman.test(df$max)
	expect_equal(test$least.lag, 0)
})

test_that("Test spearman-test.R on data set #2", {
	df <- load_data("Application_2.csv")
	test <- spearman.test(df$max)
	expect_equal(test$least.lag, 16)
})


test_that("Test spearman-test.R on data set #3.1", {
	df <- load_data("Application_3.1.csv")
	test <- spearman.test(df$max)
	expect_equal(test$least.lag, 0)
})

test_that("Test spearman-test.R on data set #3.2", {
	df <- load_data("Application_3.2.csv")
	test <- spearman.test(df$max)
	expect_equal(test$least.lag, 0)
})

test_that("Test spearman-test.R on data set #3.3", {
	df <- load_data("Application_3.3.csv")
	test <- spearman.test(df$max)
	expect_equal(test$least.lag, 4)
})
