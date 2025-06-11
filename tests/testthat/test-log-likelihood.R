test_that("Test log-likelihood.R on data set #2", {

	# Load dataset and run L-moments estimation
	df <- load_data("Application_2.csv", clean = FALSE)

	# Define the covariate
	n <- length(df$max)
	covariate <- ((1:n) - 1) / (n - 1)
	covariate <- covariate[!is.nan(df$max)]

	# Define the data vector
	data <- df$max
	data <- data[!is.nan(data)]

	# Get the distribution
	distribution <- get.distributions()$GPA

	params <- c(472, 1839.8983, -0.5862)
	result <- log.likelihood(data, distribution, NULL, params, covariate)
	print(params) 
	expect_equal(result, -721.7975)

})



