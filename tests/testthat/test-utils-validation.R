# Test that validate_data ensures data is a positive, numeric vector
test_that("validate_data is working properly", {

	# Vectors, scalars, small and large numbers all work correctly. 	
    expect_silent(validate_data(c(1, 2, 3.5)))
    expect_silent(validate_data(1))           
    expect_silent(validate_data(c(0, 1e-8))) 
    expect_silent(validate_data(c(1, 2, 1e100)))

	# Non-vector inputs are rejected
    expect_error(validate_data(NULL), "'data' must be a vector")
  	expect_error(validate_data(matrix(c(1, 2, 3))), "'data' must be a vector")
    expect_error(validate_data(factor(c("a", "b"))), "'data' must be a vector")
	expect_error(validate_data(array(1:8, dim = c(2, 2, 2))),  "'data' must be a vector")

	# Non-numeric inputs are rejected
    expect_error(validate_data("abc"), "'data' must be numeric")
    expect_error(validate_data(TRUE), "'data' must be numeric")
    expect_error(validate_data(list(1, 2, 3)), "'data' must be numeric")

    # Rejects the empty numeric vector
    expect_error(validate_data(numeric(0)), "'data' must be non-empty")

	# Missing values are rejected
  	expect_error(validate_data(c(1, NA, 3)), "'data' must have no missing values")
  	expect_error(validate_data(c(1, NaN, 3)), "'data' must have no missing values")

	# Infinite values are rejected
  	expect_error(validate_data(c(1, Inf, 3)), "'data' must be finite")
  	expect_error(validate_data(c(1, -Inf, 3)), "'data' must be finite")

	# Negative values are rejected
    expect_error(validate_data(c(-1e8, 1, 2)), "'data' must be non-negative")
    expect_error(validate_data(c(-1, 2, 3)), "'data' must be non-negative")
    expect_error(validate_data(-5), "'data' must be non-negative")

})

# Test that validate_model ensures model is a string
test_that("validate_model is working properly", {

	# Valid options all pass
	expect_equal(validate_model("GUM"), "GUM")
    expect_equal(validate_model("GEV"), "GEV")
    expect_equal(validate_model("LP3"), "LP3")

	# Matrices are coerced to vectors
    expect_equal(validate_model(as.matrix("WEI")), "WEI")

	# Invalid data types fail
    expect_error(validate_model(1), "'model' must be a character string")
    expect_error(validate_model(TRUE), "'model' must be a character string")
    expect_error(validate_model(list("GUM")), "'model' must be a character string")

	# NULL, NA, and empty data types fail
    expect_error(validate_model(NULL), "'model' must be a character string")
 
	# A vector of options fails
	expect_error(validate_model(c("GUM", "GEV")), "'model' must have length 1")

	# Models outside of the list of options fail
    expect_error(validate_model(NA_character_), "'model' must be one of")
    expect_error(validate_model("gev"), "'model' must be one of")
    expect_error(validate_model(""), "'model' must be one of")

})

# Test that validate_method ensures model is a string
test_that("validate_method is working properly", {

	# Valid options all pass
	expect_equal(validate_method("L-moments"), "L-moments")
    expect_equal(validate_method("MLE"), "MLE")
    expect_equal(validate_method("GMLE"), "GMLE")

	# Invalid data types fail
    expect_error(validate_method(1), "'method' must be a character string")
    expect_error(validate_method(TRUE), "'method' must be a character string")
    expect_error(validate_method(list("GUM")), "'method' must be a character string")

	# NULL, NA, and empty data types fail
    expect_error(validate_method(NULL), "'method' must be a character string")
 
	# A vector of options fails
	expect_error(validate_method(c("GUM", "GEV")), "'method' must have length 1")

	# Models outside of the list of options fail
    expect_error(validate_method(NA_character_), "'method' must be one of")
    expect_error(validate_method("gev"), "'method' must be one of")
    expect_error(validate_method(""), "'method' must be one of")

})

# Test that validate_params ensures params is correct
test_that("validate_params is working properly", {

	# All models and trends work properly
    expect_silent(validate_params(c(1, 2), "GUM"))
    expect_silent(validate_params(c(1, 2, 3), "GEV"))           
    expect_silent(validate_params(c(1, 2), "GUM", trend_00))           
    expect_silent(validate_params(c(1, 2, 3, 1), "GEV", trend_01))           
    expect_silent(validate_params(c(1, 2, 3), "GUM", trend_10))           
    expect_silent(validate_params(c(1, 2, 3, 1, 2), "GEV", trend_11))           

	# Non-vector inputs are rejected
    expect_error(validate_params(NULL, "GUM"), "'params' must be a vector")
  	expect_error(validate_params(matrix(c(1, 2, 3)), "LNO"), "'params' must be a vector")
    expect_error(validate_params(factor(c("a", "b")), "NOR"), "'params' must be a vector")
	expect_error(validate_params(array(1:8), "GLO"),  "'params' must be a vector")

	# Non-numeric inputs are rejected
    expect_error(validate_params("abc", "LNO"), "'params' must be numeric")
    expect_error(validate_params(TRUE, "GNO"), "'params' must be numeric")
    expect_error(validate_params(list(1, 2, 3), "PE3"), "'params' must be numeric")

	# Invalid lengths are rejected
    expect_error(validate_params(c(1, 2), "GEV"), "'params' must have length 3")
    expect_error(validate_params(c(1, 2, 3), "GUM"), "'params' must have length 2")
    expect_error(validate_params(c(2, 4, 3), "GEV", trend_01), "'params' must have length 4")

	# Missing values are rejected
  	expect_error(validate_params(c(1, NA, 3), "GEV"), "'params' must have no missing values")
  	expect_error(validate_params(c(1, NaN, 3), "GEV"), "'params' must have no missing values")

	# Infinite values are rejected
  	expect_error(validate_params(c(1, Inf, 3), "WEI"), "'params' must be finite")
  	expect_error(validate_params(c(1, -Inf, 3), "LP3"), "'params' must be finite")

})


# Test that validate_prior ensures prior is correct
test_that("validate_prior is working properly", {

	# Length-two vectors and NULL inputs work correctly 
    expect_silent(validate_prior(NULL))
    expect_silent(validate_prior(c(1, 2)))
    expect_silent(validate_prior(c(1e-8, 2)))
    expect_silent(validate_prior(c(1e100, 10)))

	# Non-vector inputs are rejected
  	expect_error(validate_prior(matrix(c(1, 2, 3))), "'prior' must be a vector")
    expect_error(validate_prior(factor(c("a", "b"))), "'prior' must be a vector")
	expect_error(validate_prior(array(1:8)),  "'prior' must be a vector")

	# Non-numeric inputs are rejected
    expect_error(validate_prior("abc"), "'prior' must be numeric")
    expect_error(validate_prior(TRUE), "'prior' must be numeric")
    expect_error(validate_prior(list(1, 2, 3)), "'prior' must be numeric")

	# Invalid lengths are rejected
    expect_error(validate_prior(c(1, 2, 3)), "'prior' must have length 2")
    expect_error(validate_prior(2), "'prior' must have length 2")

	# Missing values are rejected
  	expect_error(validate_prior(c(NA, 3)), "'prior' must have no missing values")
  	expect_error(validate_prior(c(NaN, 3)), "'prior' must have no missing values")

	# Infinite values are rejected
  	expect_error(validate_prior(c(Inf, 3)), "'prior' must be finite")
  	expect_error(validate_prior(c(-Inf, 3)), "'prior' must be finite")

	# Negative values are rejected
    expect_error(validate_prior(c(1, 0)), "'prior' must be strictly positive")
    expect_error(validate_prior(c(-1e8, 1)), "'prior' must be strictly positive")
    expect_error(validate_prior(c(1, -100)), "'prior' must be strictly positive")

})

# Test that validate_years ensures years is correct
test_that("validate_years is working properly", {

	# Vectors and NULL inputs work correctly 
    expect_silent(validate_years(NULL))
    expect_silent(validate_years(c(1, 2), c(3, 4)))
    expect_silent(validate_years(c(1e-8, 2e-8, 5)))
    expect_silent(validate_years(c(3, 4, 1e100)))

	# Non-vector inputs are rejected
  	expect_error(validate_years(matrix(c(1, 2, 3))), "'years' must be a vector")
    expect_error(validate_years(factor(c("a", "b"))), "'years' must be a vector")
	expect_error(validate_years(array(1:8)),  "'years' must be a vector")

	# Non-numeric inputs are rejected
    expect_error(validate_years("abc"), "'years' must be numeric")
    expect_error(validate_years(TRUE), "'years' must be numeric")
    expect_error(validate_years(list(1, 2, 3)), "'years' must be numeric")

    # Rejects the empty numeric vector
    expect_error(validate_years(numeric(0)), "'years' must be non-empty")

	# Missing values are rejected
  	expect_error(validate_years(c(NA, 3)), "'years' must have no missing values")
  	expect_error(validate_years(c(NaN, 3)), "'years' must have no missing values")

	# Infinite values are rejected
  	expect_error(validate_years(c(Inf, 3)), "'years' must be finite")
  	expect_error(validate_years(c(-Inf, 3)), "'years' must be finite")

	# Invalid lengths are rejected if data is passed
    expect_error(validate_years(c(2, 3), c(1, 2, 3)), "must have equal length")
    expect_error(validate_years(c(5, 3), 2), "must have equal length")

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

# Test that validate_slice ensures slice is correct
test_that("validate_slice is working properly", {

	# Numeric scalars pass
    expect_silent(validate_slice(2020))
    expect_silent(validate_slice(1e8))
    expect_silent(validate_slice(1e-8))

	# Non-vector inputs are rejected
  	expect_error(validate_slice(matrix(c(1, 2, 3))), "'slice' must be a vector")
    expect_error(validate_slice(factor(c("a", "b"))), "'slice' must be a vector")
	expect_error(validate_slice(array(1:8)),  "'slice' must be a vector")

	# Non-numeric inputs are rejected
    expect_error(validate_slice("abc"), "'slice' must be numeric")
    expect_error(validate_slice(TRUE), "'slice' must be numeric")
    expect_error(validate_slice(list(1, 2, 3)), "'slice' must be numeric")

	# Invalid lengths are rejected
    expect_error(validate_slice(c(5, 3)), "'slice' must have length 1")
    expect_error(validate_slice(numeric(0)), "'slice' must have length 1")

	# Missing values are rejected
  	expect_error(validate_slice(as.numeric(NA)), "'slice' must have no missing values")
  	expect_error(validate_slice(as.numeric(NaN)), "'slice' must have no missing values")

	# Infinite values are rejected
  	expect_error(validate_slice(Inf), "'slice' must be finite")
  	expect_error(validate_slice(-Inf), "'slice' must be finite")

})

# Test that validate_samples ensures samples is correct
test_that("validate_samples is working properly", {

	# Integers work correctly
    expect_silent(validate_samples(1L))
    expect_silent(validate_samples(100000L))

	# Non-vector inputs are rejected
  	expect_error(validate_samples(matrix(c(1, 2, 3))), "'samples' must be a vector")
    expect_error(validate_samples(factor(c("a", "b"))), "'samples' must be a vector")
	expect_error(validate_samples(array(1:8)),  "'samples' must be a vector")

	# Non-numeric inputs are rejected
    expect_error(validate_samples("abc"), "'samples' must be an integer")
    expect_error(validate_samples(TRUE), "'samples' must be an integer")
    expect_error(validate_samples(list(1, 2, 3)), "'samples' must be an integer")

	# Invalid lengths are rejected
    expect_error(validate_samples(c(5L, 3L)), "'samples' must have length 1")
    expect_error(validate_samples(integer(0)), "'samples' must have length 1")

	# Missing values are rejected
  	expect_error(validate_samples(as.integer(NA)), "'samples' must have no missing values")
  	expect_error(validate_samples(as.integer(NaN)), "'samples' must have no missing values")

})

# Test that validate_quiet ensures quiet is correct
test_that("validate_quiet is working properly", {

	# Integers work correctly
    expect_silent(validate_quiet(TRUE))
    expect_silent(validate_quiet(FALSE))

	# Non-vector inputs are rejected
  	expect_error(validate_quiet(matrix(TRUE)), "'quiet' must be a vector")
    expect_error(validate_quiet(factor(c("a", "b"))), "'quiet' must be a vector")
	expect_error(validate_quiet(array(FALSE)),  "'quiet' must be a vector")

	# Non-numeric inputs are rejected
    expect_error(validate_quiet("abc"), "'quiet' must be logical")
    expect_error(validate_quiet(125L), "'quiet' must be logical")
    expect_error(validate_quiet(list(1, 2, 3)), "'quiet' must be logical")

	# Invalid lengths are rejected
    expect_error(validate_quiet(c(TRUE, FALSE)), "'quiet' must have length 1")
    expect_error(validate_quiet(logical(0)), "'quiet' must have length 1")

	# Missing values are rejected
  	expect_error(validate_quiet(as.logical(NA)), "'quiet' must have no missing values")
  	expect_error(validate_quiet(as.logical(NaN)), "'quiet' must have no missing values")

})

# Test that validate_probabilities ensures probabilities is a positive, numeric vector
test_that("validate_probabilities is working properly", {

	# Vectors, scalars, and small numbers all work correctly. 	
    expect_silent(validate_probabilities(c(0.2, 0.5, 0.8, 1)))
    expect_silent(validate_probabilities(c(0, 1e-8, 2e-8, 0.5))) 

	# Non-vector inputs are rejected
    expect_error(validate_probabilities(NULL), "'p' must be a vector")
  	expect_error(validate_probabilities(matrix(c(0.1, 0.2))), "'p' must be a vector")
    expect_error(validate_probabilities(factor(c("a", "b"))), "'p' must be a vector")
	expect_error(validate_probabilities(array(1:8, dim = c(2, 2, 2))),  "'p' must be a vector")

	# Non-numeric inputs are rejected
    expect_error(validate_probabilities("abc"), "'p' must be numeric")
    expect_error(validate_probabilities(TRUE), "'p' must be numeric")
    expect_error(validate_probabilities(list(0.1, 0.2, 0.5)), "'p' must be numeric")

    # Rejects the empty numeric vector
    expect_error(validate_probabilities(numeric(0)), "'p' must be non-empty")

	# Missing values are rejected
  	expect_error(validate_probabilities(c(1, NA, 0.2)), "'p' must have no missing values")
  	expect_error(validate_probabilities(c(1, NaN, 0.1)), "'p' must have no missing values")

	# Invalid values are rejected
    expect_error(validate_probabilities(-1e8), "'p' must be between 0 and 1 inclusive")
    expect_error(validate_probabilities(1 + 1e-8), "'p' must be between 0 and 1 inclusive")

})

# Test that validate_alpha ensures alpha is a numeric scalar between 0.01 and 0.1
test_that("validate_alpha is working properly", {

	# Vectors, scalars, and small numbers all work correctly. 	
    expect_silent(validate_alpha(0.01 + 1e-8))
    expect_silent(validate_alpha(0.1 - 1e-8)) 

	# Non-vector inputs are rejected
    expect_error(validate_alpha(NULL), "'alpha' must be a vector")
  	expect_error(validate_alpha(matrix(c(0.1, 0.2))), "'alpha' must be a vector")
    expect_error(validate_alpha(factor(c("a", "b"))), "'alpha' must be a vector")
	expect_error(validate_alpha(array(1:8, dim = c(2, 2, 2))),  "'alpha' must be a vector")

	# Non-numeric inputs are rejected
    expect_error(validate_alpha("abc"), "'alpha' must be numeric")
    expect_error(validate_alpha(TRUE), "'alpha' must be numeric")
    expect_error(validate_alpha(list(0.1, 0.2, 0.5)), "'alpha' must be numeric")

    # Rejects invalid elngths
    expect_error(validate_alpha(numeric(0)), "'alpha' must have length 1")
    expect_error(validate_alpha(c(0.01, 0.02)), "'alpha' must have length 1")

	# Missing values are rejected
  	expect_error(validate_alpha(as.numeric(NA)), "'alpha' must have no missing values")
  	expect_error(validate_alpha(NaN), "'alpha' must have no missing values")

	# Invalid values are rejected
    expect_error(validate_alpha(00.1 -1e8), "'alpha' must be between 0.01 and 0.1")
    expect_error(validate_alpha(0.1 + 1e-8), "'alpha' must be between 0.01 and 0.1")

})
