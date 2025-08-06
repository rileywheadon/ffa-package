test_that("select-lkurtosis.R works on ATHABASCA RIVER (07BE001)", {

	# Load dataset and run L-kurtosis selection
	df <- data_local("CAN-07BE001.csv")
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


test_that("select-lkurtosis.R works on KOOTENAI RIVER (08NH021)", {

	# Load dataset and run L-kurtosis selection
	df <- data_local("CAN-08NH021.csv")
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


test_that("select-lkurtosis.R works on BOW RIVER (05BB001)", {

	# Load dataset and run L-kurtosis selection
	df <- data_local("CAN-05BB001.csv")
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


test_that("select-lkurtosis.R works on CHILLIWACK RIVER (08MH016)", {

	# Load dataset and run L-kurtosis selection
	df <- data_local("CAN-08MH016.csv")
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


test_that("select-lkurtosis.R works on OKANAGAN RIVER (08NM050)", {

	# Load dataset and run L-kurtosis selection
	df <- data_local("CAN-08NM050.csv")
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

test_that("Nonstationary select-lkurtosis.R works on BOW RIVER (05BB001)", {

	# Load dataset and run L-distance selection
	df <- data_local("CAN-05BB001.csv")
	results <- select_lkurtosis(df$max, df$year, S10)

	# Check the distances
	expect_equal(results$metrics$GEV, 0.0041, tol = 1e-3) 
	expect_equal(results$metrics$GLO, 0.0442, tol = 1e-3) 
	expect_equal(results$metrics$PE3, 0.0150, tol = 1e-3) 
	expect_equal(results$metrics$LP3, 0.0018, tol = 1e-3) 
	expect_equal(results$metrics$GNO, 0.0012, tol = 1e-3) 
	expect_equal(results$metrics$WEI, 0.0340, tol = 1e-3) 

	# Check the recommendation
	expect_equal(results$recommendation, "GNO") 

})

