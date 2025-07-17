test_that("Test l-kurtosis.R on data set #1", {

	# Load dataset and run L-distance selection
	df <- load_data("Application_1.csv")
	results <- select_lkurtosis(df$max)
	
	# Check the distances
	expect_equal(results$metrics$GEV, 0.0103, tol = 1e-3) 
	expect_equal(results$metrics$GLO, 0.0215, tol = 1e-3) 
	expect_equal(results$metrics$PE3, 0.0525, tol = 1e-3) 
	expect_equal(results$metrics$LP3, 0.0470, tol = 1e-3) 
	expect_equal(results$metrics$GNO, 0.0254, tol = 1e-3) 
	expect_equal(results$metrics$WEI, 0.0642, tol = 1e-3) 

	# Check the recommendation
	expect_equal(results$recommendation, "GEV") 

})


test_that("Test l-kurtosis.R on data set #2", {

	# Load dataset and run L-distance selection
	df <- load_data("Application_2.csv")
	results <- select_lkurtosis(df$max)
	
	# Check the distances
	expect_equal(results$metrics$GEV, 0.0887, tol = 1e-3) 
	expect_equal(results$metrics$GLO, 0.1350, tol = 1e-3) 
	expect_equal(results$metrics$PE3, 0.0839, tol = 1e-3) 
	expect_equal(results$metrics$LP3, 0.0783, tol = 1e-3) 
	expect_equal(results$metrics$GNO, 0.0903, tol = 1e-3) 
	expect_equal(results$metrics$WEI, 0.0627, tol = 1e-3) 

	# Check the recommendation
	expect_equal(results$recommendation, "WEI") 

})


test_that("Test l-kurtosis.R on data set #3.1", {

	# Load dataset and run L-distance selection
	df <- load_data("Application_3.1.csv")
	results <- select_lkurtosis(df$max)
	
	# Check the distances
	expect_equal(results$metrics$GEV, 0.0245, tol = 1e-3) 
	expect_equal(results$metrics$GLO, 0.0657, tol = 1e-3) 
	expect_equal(results$metrics$PE3, 0.0081, tol = 1e-3) 
	expect_equal(results$metrics$LP3, 0.0247, tol = 1e-3) 
	expect_equal(results$metrics$GNO, 0.0204, tol = 1e-3) 
	expect_equal(results$metrics$WEI, 0.0115, tol = 1e-3) 

	# Check the recommendation
	expect_equal(results$recommendation, "PE3") 

})


test_that("Test l-kurtosis.R on data set #3.2", {

	# Load dataset and run L-distance selection
	df <- load_data("Application_3.2.csv")
	results <- select_lkurtosis(df$max)
	
	# Check the distances
	expect_equal(results$metrics$GEV, 0.0058, tol = 1e-3) 
	expect_equal(results$metrics$GLO, 0.0447, tol = 1e-3) 
	expect_equal(results$metrics$PE3, 0.0165, tol = 1e-3) 
	expect_equal(results$metrics$LP3, 0.0027, tol = 1e-3) 
	expect_equal(results$metrics$GNO, 0.0010, tol = 1e-3) 
	expect_equal(results$metrics$WEI, 0.0347, tol = 1e-3) 

	# Check the recommendation
	expect_equal(results$recommendation, "GNO") 

})


test_that("Test l-kurtosis.R on data set #3.3", {

	# Load dataset and run L-distance selection
	df <- load_data("Application_3.3.csv")
	results <- select_lkurtosis(df$max)
	
	# Check the distances
	expect_equal(results$metrics$GEV, 0.1070, tol = 1e-3) 
	expect_equal(results$metrics$GLO, 0.1535, tol = 1e-3) 
	expect_equal(results$metrics$PE3, 0.1025, tol = 1e-3) 
	expect_equal(results$metrics$LP3, 0.0693, tol = 1e-3) 
	expect_equal(results$metrics$GNO, 0.1088, tol = 1e-3) 
	expect_equal(results$metrics$WEI, 0.0813, tol = 1e-3) 

	# Check the recommendation
	expect_equal(results$recommendation, "LP3") 

})
