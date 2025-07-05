# # Helper function to validate errors (ns stands for non-stationary)
# validate_errors <- function(trend, n) {

# 	f <- function(data, years, params, prior) {
# 		general_loglik_gev(data, years, trend, params, prior)
# 	}

# 	data <- rep(1, 100) 
# 	params <- rep(1, n)
# 	years <- rep(1, 100) 
# 	prior <- c(6, 9)

# 	# Validate the data vector
# 	expect_error(f(c("A", "B"), years, params, prior), regexp = "'data' .* numeric")
# 	expect_error(f(c(1, NA), years, params, prior), regexp = "'data' .* missing")
# 	expect_error(f(c(1, -1), years, params, prior), regexp = "'data' .* positive")

# 	# Validate the years vector
# 	expect_error(f(data, rep("A", 100), params, prior), regexp = "'years' .* numeric")
# 	expect_error(f(data, c(1, NaN), params, prior), regexp = "'years' .* missing")
# 	expect_error(f(data, rep(1, 99), params, prior), regexp = "'years' .* length")

# 	# Validate the parameters vector
# 	expect_error(f(data, years, c("A", "B"), prior), regexp = "'params' .* numeric")
# 	expect_error(f(data, years, c(1, NaN), prior), regexp = "'params' .* missing")
# 	expect_error(f(data, years, rep(1, n+1), prior), regexp = "'params' .* length")

# 	# Validate the prior vector
# 	expect_error(f(data, years, params, c("A", "B")), regexp = "'prior' .* numeric")
# 	expect_error(f(data, years, params, c(1, NaN)), regexp = "'prior' .* missing")
# 	expect_error(f(data, years, params, c(1)), regexp = "'prior' .* length")


# }

# test_that("general_loglik_gev handle invalid inputs", {
# 	validate_errors(trend_00, 3)
# 	validate_errors(trend_10, 4)
# 	validate_errors(trend_11, 5)
# })


