test_that("validate_numeric is working properly", {

	# Vectors, scalars, small and large numbers all work correctly. 	
    expect_silent(validate_numeric('x', c(1, 2, 3.5)))
    expect_silent(validate_numeric('x', 1))           
    expect_silent(validate_numeric('x', c(0, 1e-8))) 
    expect_silent(validate_numeric('x', c(1, 2, 1e100)))

	# Non-vector inputs are rejected
  	expect_error(validate_numeric('x', matrix(c(1, 2, 3))), "'x' must be a vector")
    expect_error(validate_numeric('x', factor(c("a", "b"))), "'x' must be a vector")

	# Non-numeric inputs are rejected
    expect_error(validate_numeric('x', "abc"), "'x' must be numeric")
    expect_error(validate_numeric('x', TRUE), "'x' must be numeric")

    # Rejects the empty numeric vector
    expect_error(validate_numeric('x', numeric(0)), "'x' must be non-empty")

	# Missing values are rejected
  	expect_error(validate_numeric('x', c(1, NA, 3)), "'x' must have no missing values")
  	expect_error(validate_numeric('x', c(1, NaN, 3)), "'x' must have no missing values")

	# Infinite values are rejected
  	expect_error(validate_numeric('x', c(1, Inf, 3)), "'x' must be finite")
  	expect_error(validate_numeric('x', c(1, -Inf, 3)), "'x' must be finite")

	# Optional behaves properly
  	expect_silent(validate_numeric('x', NULL, TRUE))
  	expect_error(validate_numeric('x', NULL, FALSE, "'x' must be a vector"))
	
	# Bounds function as intended
    expect_error(validate_numeric('x', 1, bounds = c(2, 4)), "'x' must be at least 2")
    expect_silent(validate_numeric('x', 3, bounds = c(2, 4)))
    expect_error(validate_numeric('x', 5, bounds = c(2, 4)), "'x' must be at most 4")

	# Size functions as intended
    expect_error(validate_numeric('x', 1, size = 2), "'x' must have length 2")
    expect_silent(validate_numeric('x', c(1, 2), size = 2))

})


test_that("validate_integer is working properly", {

	# Small and large integers work correctly
    expect_silent(validate_integer('x', 1L))
    expect_silent(validate_integer('x', 100000L))

	# Non-integer inputs are rejected
    expect_error(validate_integer('x', "abc"), "'x' must be an integer")
    expect_error(validate_integer('x', TRUE), "'x' must be an integer")
    expect_error(validate_integer('x', 10), "'x' must be an integer")

	# Invalid lengths are rejected
    expect_error(validate_integer('x', c(5L, 3L)), "'x' must have length 1")
    expect_error(validate_integer('x', integer(0)), "'x' must have length 1")

	# Missing values are rejected
  	expect_error(validate_integer('x', as.integer(NA)), "'x' must not be missing")
  	expect_error(validate_integer('x', as.integer(NaN)), "'x' must not be missing")

	# Bounds function as intended
    expect_error(validate_numeric('x', 1L, bounds = c(2L, 4L)), "'x' must be at least 2")
    expect_silent(validate_numeric('x', 3L, bounds = c(2L, 4L)))
    expect_error(validate_numeric('x', 5L, bounds = c(2L, 4L)), "'x' must be at most 4")

})


test_that("validate_logical is working properly", {

	# Integers work correctly
    expect_silent(validate_logical('x', TRUE))
    expect_silent(validate_logical('x', FALSE))

	# Non-numeric logical inputs are rejected
    expect_error(validate_logical('x', "abc"), "'x' must be logical")
    expect_error(validate_logical('x', 125L), "'x' must be logical")
    expect_error(validate_logical('x', 3), "'x' must be logical")

	# Invalid lengths are rejected
    expect_error(validate_logical('x', c(TRUE, FALSE)), "'x' must have length 1")
    expect_error(validate_logical('x', logical(0)), "'x' must have length 1")

	# Missing values are rejected
  	expect_error(validate_logical('x', as.logical(NA)), "'x' must not be missing")
  	expect_error(validate_logical('x', as.logical(NaN)), "'x' must not be missing")

})


test_that("validate_float is working properly", {

	# Small and big numbers both work correctly
    expect_silent(validate_float('x', 1e-8))
    expect_silent(validate_float('x', 1e8))

	# Non-numeric inputs are rejected
    expect_error(validate_float('x', "abc"), "'x' must be numeric")
    expect_error(validate_float('x', TRUE), "'x' must be numeric")
    expect_error(validate_float('x', list(1, 2)), "'x' must be numeric")

    # Rejects invalid lengths
    expect_error(validate_float('x', numeric(0)), "'x' must have length 1")
    expect_error(validate_float('x', c(0.01, 0.02)), "'x' must have length 1")

	# Missing values are rejected
  	expect_error(validate_float('x', as.numeric(NA)), "'x' must not be missing")
  	expect_error(validate_float('x', NaN), "'x' must not be missing")

	# Infinite values are rejected
    expect_error(validate_float('x', -Inf), "'x' must be finite")
    expect_error(validate_float('x',  Inf), "'x' must be finite")

	# Bounds work as intended
    expect_error(validate_float('x', 1, bounds = c(2, 4), "'x' must be at least 2"))
    expect_silent(validate_float('x', 3, bounds = c(2, 4)))
    expect_error(validate_float('x', 5, bounds = c(2, 4), "'x' must be at most 4"))

})


test_that("validate_enum is working properly", {

	# Valid options all pass
	expect_silent(validate_enum("model", "GUM"))
	expect_silent(validate_enum("method", "L-moments"))
	expect_silent(validate_enum("pp_formula", "Weibull"))

	# Non-character inputs are rejected
    expect_error(validate_enum("model", 1), "'model' must be a character string")
    expect_error(validate_enum("model", TRUE), "'model' must be a character string")

	# Incorrect lengths fail
	expect_error(validate_enum("model", character(0)), "'model' must have length 1")
	expect_error(validate_enum("model", c("GUM", "GEV")), "'model' must have length 1")

	# Models outside of the list of options fail
    expect_error(validate_enum("model", NA_character_), "'model' must be one of")
    expect_error(validate_enum("model", "gev"), "'model' must be one of")
    expect_error(validate_enum("model", ""), "'model' must be one of")

})

# Test that validate_trend correctly checks the trend object
test_that("validate_trend is working properly", {
	
	# Check valid cases (NULL or correct trend object)
	expect_silent(validate_trend(NULL))
	expect_silent(validate_trend(trend_00))
	expect_silent(validate_trend(trend_10))
	expect_silent(validate_trend(trend_01))
	expect_silent(validate_trend(trend_11))

	# Throws error if keys 'location' or 'scale' do not exist
	expect_error(validate_trend(list(scale = FALSE)), "'trend' requires item 'location'")
	expect_error(validate_trend(list(location = TRUE)), "'trend' requires item 'scale'")

	# Check that items are logical and have length 1
	invalid_length <- list(location = c(TRUE, FALSE), scale = FALSE)
	invalid_type <- list(location = TRUE, scale = "ABC")
	expect_error(validate_trend(invalid_length), "location' must have length 1")
	expect_error(validate_trend(invalid_type), "scale' must be logical")

})

# Test that validate_params ensures params is correct
test_that("validate_params is working properly", {

	# Passing test cases
    expect_silent(validate_params("GUM", c(1, 2)))
    expect_silent(validate_params("GEV", c(1, 2, 3)))           
    expect_silent(validate_params("GUM", c(1, 2), trend_00))           
    expect_silent(validate_params("GEV", c(1, 2, 3, 1), trend_01))           
    expect_silent(validate_params("GUM", c(1, 2, 3), trend_10))           
    expect_silent(validate_params("GEV", c(1, 2, 3, 1, 2), trend_11))           

	# Failing test cases
    expect_error(validate_params("GEV", c(1, 2)), "'params' must have length 3")
    expect_error(validate_params("GUM", c(1, 2, 3)), "'params' must have length 2")
    expect_error(validate_params("GEV", 3, trend_01), "'params' must have length 4")

})
