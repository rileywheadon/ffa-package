test_that("select-ldistance.R works on ATHABASCA RIVER (07BE001)", {

	# Load dataset and run L-distance selection
	df <- data_local("CAN-07BE001.csv")
	results <- select_ldistance(df$max)

	# Check the distances
	expect_equal(results$metrics$GEV, 0.0092, tol = 1e-3) 
	expect_equal(results$metrics$GUM, 0.0923, tol = 1e-3) 
	expect_equal(results$metrics$NOR, 0.2604, tol = 1e-3) 
	expect_equal(results$metrics$LNO, 0.0529, tol = 1e-3) 
	expect_equal(results$metrics$GLO, 0.0199, tol = 1e-3) 
	expect_equal(results$metrics$PE3, 0.0514, tol = 1e-3) 
	expect_equal(results$metrics$LP3, 0.0470, tol = 1e-3) 
	expect_equal(results$metrics$GNO, 0.0235, tol = 1e-3) 
	expect_equal(results$metrics$WEI, 0.0607, tol = 1e-3) 

	# Check the recommendation
	expect_equal(results$recommendation, "GEV") 

})

test_that("select-ldistance.R works on KOOTENAI RIVER (08NH021)", {

	# Load dataset and run L-distance selection
	df <- data_local("CAN-08NH021.csv")
	results <- select_ldistance(df$max)

	# Check the distances
	expect_equal(results$metrics$GEV, 0.0853, tol = 1e-3) 
	expect_equal(results$metrics$GUM, 0.1204, tol = 1e-3) 
	expect_equal(results$metrics$NOR, 0.1407, tol = 1e-3) 
	expect_equal(results$metrics$LNO, 0.0941, tol = 1e-3) 
	expect_equal(results$metrics$GLO, 0.1330, tol = 1e-3) 
	expect_equal(results$metrics$PE3, 0.0837, tol = 1e-3) 
	expect_equal(results$metrics$LP3, 0.0783, tol = 1e-3) 
	expect_equal(results$metrics$GNO, 0.0891, tol = 1e-3) 
	expect_equal(results$metrics$WEI, 0.0625, tol = 1e-3) 

	# Check the recommendation
	expect_equal(results$recommendation, "WEI") 

})

test_that("select-ldistance.R works on BOW RIVER (05BB001)", {

	# Load dataset and run L-distance selection
	df <- data_local("CAN-05BB001.csv")
	results <- select_ldistance(df$max)

	# Check the distances
	expect_equal(results$metrics$GEV, 0.0230, tol = 1e-3) 
	expect_equal(results$metrics$GUM, 0.0285, tol = 1e-3) 
	expect_equal(results$metrics$NOR, 0.1623, tol = 1e-3) 
	expect_equal(results$metrics$LNO, 0.0411, tol = 1e-3) 
	expect_equal(results$metrics$GLO, 0.0636, tol = 1e-3) 
	expect_equal(results$metrics$PE3, 0.0080, tol = 1e-3) 
	expect_equal(results$metrics$LP3, 0.0247, tol = 1e-3) 
	expect_equal(results$metrics$GNO, 0.0198, tol = 1e-3) 
	expect_equal(results$metrics$WEI, 0.0114, tol = 1e-3) 

	# Check the recommendation
	expect_equal(results$recommendation, "PE3") 

})

test_that("select-ldistance.R works on CHILLIWACK RIVER (08MH016)", {

	# Load dataset and run L-distance selection
	df <- data_local("CAN-08MH016.csv")
	results <- select_ldistance(df$max)

	# Check the distances
	expect_equal(results$metrics$GEV, 0.0053, tol = 1e-3) 
	expect_equal(results$metrics$GUM, 0.0135, tol = 1e-3) 
	expect_equal(results$metrics$NOR, 0.1854, tol = 1e-3) 
	expect_equal(results$metrics$LNO, 0.0419, tol = 1e-3) 
	expect_equal(results$metrics$GLO, 0.0428, tol = 1e-3) 
	expect_equal(results$metrics$PE3, 0.0163, tol = 1e-3) 
	expect_equal(results$metrics$LP3, 0.0027, tol = 1e-3) 
	expect_equal(results$metrics$GNO, 0.0009, tol = 1e-3) 
	expect_equal(results$metrics$WEI, 0.0339, tol = 1e-3) 

	# Check the recommendation
	expect_equal(results$recommendation, "GNO") 

})

test_that("select-ldistance.R works on OKANAGAN RIVER (08NM050)", {

	# Load dataset and run L-distance selection
	df <- data_local("CAN-08NM050.csv")
	results <- select_ldistance(df$max)

	# Check the distances
	expect_equal(results$metrics$GEV, 0.1030, tol = 1e-3) 
	expect_equal(results$metrics$GUM, 0.1376, tol = 1e-3) 
	expect_equal(results$metrics$NOR, 0.1513, tol = 1e-3) 
	expect_equal(results$metrics$LNO, 0.1110, tol = 1e-3) 
	expect_equal(results$metrics$GLO, 0.1513, tol = 1e-3) 
	expect_equal(results$metrics$PE3, 0.1022, tol = 1e-3) 
	expect_equal(results$metrics$LP3, 0.0692, tol = 1e-3) 
	expect_equal(results$metrics$GNO, 0.1073, tol = 1e-3) 
	expect_equal(results$metrics$WEI, 0.0810, tol = 1e-3) 

	# Check the recommendation
	expect_equal(results$recommendation, "LP3") 

})
