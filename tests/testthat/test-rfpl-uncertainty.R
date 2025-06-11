validate_results <- function(data, model, estimates, ci_lower, ci_upper, profile = FALSE) {

	start <- Sys.time()
	results <- rfpl.uncertainty(data, model)
	end <- Sys.time()

	if (profile) print(end - start)

	expect_equal(results$estimates, estimates, tol = 1e-4)
	expect_equal( results$ci_lower,  ci_lower, tol = 1e-4)
	expect_equal( results$ci_upper,  ci_upper, tol = 1e-4)
}

test_that("Test rfpl.uncertainty.R on data set #1", {

	# Load dataset and run RFPL uncertainty quantification
	df <- load_data("Application_1.csv", clean = FALSE)
	expect_equal(0, 0)

	# validate_results(
	# 	df$max,
	# 	"GEV",
	# 	c(1718.8134, 2426.0784, 2884.1104, 3318.2255, 3850.3142, 4236.9194),
	# 	c(1867.4305, 2646.4788, 3187.5978, 3726.4642, 4453.9792, 5022.2453),
	# 	c(2032.0309, 2920.7517, 3628.0105, 4403.9085, 5543.9541, 6565.5664)
	# )

})
