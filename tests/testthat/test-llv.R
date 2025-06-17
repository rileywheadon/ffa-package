test_that("Test validation for llvgum.", {
	
	df <- load_data("Application_1.csv")
	results <- llvgum(df$max, c(0, 1, 0))
	print(results)

})
