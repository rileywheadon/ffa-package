test_that("data-screening.R works as intended", {

	data <- c(2, 4, 7, 12, 15)
	years <- c(2000, 2001, 2003, 2005, 2006)
	results <- data_screening(data, years)

	expect_equal(results$years_min, 2000)
	expect_equal(results$years_max, 2006)
	expect_equal(results$data_min, 2)
	expect_equal(results$data_med, 7)
	expect_equal(results$data_max, 15)
	expect_equal(results$missing_years, c(2002, 2004))
	expect_equal(results$missing_count, 2)

})
