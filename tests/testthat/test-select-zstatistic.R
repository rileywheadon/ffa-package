# NOTE: Tolerance is higher in some tests because of randomness in the bootstrap
test_that("select-zstatistic.R works on ATHABASCA RIVER (07BE001)", {
	set.seed(1)

	# Load dataset and run Z-statistic selection 
	df <- data_local("CAN-07BE001.csv")
	results <- select_zstatistic(df$max, samples = 25000L)

	# Check the Kappa distribution parameters (only k and h)
	expect_equal(results$reg_params[3], -0.1544, tol = 1e-3)
	expect_equal(results$reg_params[4], -0.1704, tol = 1e-3)
	expect_equal(results$log_params[3], NULL)
	expect_equal(results$log_params[4], NULL)

	# Check the bootstrap summary statistics
	expect_equal(results$reg_bias_t4, -0.0026, tol = 1e-2)
	expect_equal(results$reg_std_t4 ,  0.0514, tol = 1e-2)
	expect_equal(results$log_bias_t4, NULL)
	expect_equal(results$log_std_t4 , NULL)

	# Check the Z-distances
	expect_equal(results$metrics$GEV, -0.2524, tol = 1e-2)
	expect_equal(results$metrics$GLO,  0.3673, tol = 1e-2)
	expect_equal(results$metrics$PE3, -1.0738, tol = 1e-2)
	expect_equal(results$metrics$LP3, NULL)
	expect_equal(results$metrics$GNO, -0.5443, tol = 1e-2)
	expect_equal(results$metrics$WEI, -1.3005, tol = 1e-2)

	# Check the recommendation
	expect_equal(results$recommendation, "GEV") 

})

# NOTE: Skip the remainder of the tests on CRAN to keep the test suite light
test_that("select-zstatistic.R works on KOOTENAI RIVER (08NH021)", {
	skip_on_cran()
	set.seed(1)

	# Load dataset and run Z-statistic selection
	df <- data_local("CAN-08NH021.csv")
	results <- select_zstatistic(df$max, samples = 25000L)

	# Check the Kappa distribution parameters (only k and h)
	expect_equal(results$reg_params[3],     0.5351, tol = 1e-3)
	expect_equal(results$reg_params[4],     0.9253, tol = 1e-3)
	expect_equal(results$log_params[3], 0.7347, tol = 1e-3)
	expect_equal(results$log_params[4], 0.5134, tol = 1e-3)

	# Check the bootstrap summary statistics
	expect_equal(results$reg_bias_t4, 0.0010, tol = 1e-2)
	expect_equal(results$reg_std_t4 , 0.0286, tol = 1e-2)
	expect_equal(results$log_bias_t4, 0.0009, tol = 1e-2)
	expect_equal(results$log_std_t4 , 0.0276, tol = 1e-2)

	# Check the Z-distances
	expect_equal(results$metrics$GEV,  3.1342, tol = 1e-2)
	expect_equal(results$metrics$GLO,  4.7538, tol = 1e-2)
	expect_equal(results$metrics$PE3,  2.9672, tol = 1e-2)
	expect_equal(results$metrics$LP3,  2.8678, tol = 1e-2)
	expect_equal(results$metrics$GNO,  3.1922, tol = 1e-2)
	expect_equal(results$metrics$WEI,  2.2269, tol = 1e-2)

	# Check the recommendation
	expect_equal(results$recommendation, "WEI") 

})

test_that("select-zstatistic.R works on BOW RIVER (05BB001)", {
	skip_on_cran()
	set.seed(1)

	# Load dataset and run Z-statistic selection
	df <- data_local("CAN-05BB001.csv")
	results <- select_zstatistic(df$max, samples = 25000L)

	# Check the Kappa distribution parameters (only k and h)
	expect_equal(results$reg_params[3],     0.1065, tol = 1e-3)
	expect_equal(results$reg_params[4],     0.2881, tol = 1e-3)
	expect_equal(results$log_params[3], 0.2875, tol = 1e-3)
	expect_equal(results$log_params[4], 0.1371, tol = 1e-3)

	# Check the bootstrap summary statistics
	expect_equal(results$reg_bias_t4, 0.0001, tol = 1e-2)
	expect_equal(results$reg_std_t4 , 0.0323, tol = 1e-2)
	expect_equal(results$log_bias_t4, 0.0005, tol = 1e-2)
	expect_equal(results$log_std_t4 , 0.0273, tol = 1e-2)

	# Check the Z-distances
	expect_equal(results$metrics$GEV,  0.7609, tol = 1e-2)
	expect_equal(results$metrics$GLO,  2.0339, tol = 1e-2)
	expect_equal(results$metrics$PE3,  0.2525, tol = 1e-2)
	expect_equal(results$metrics$LP3,  0.9258, tol = 1e-2)
	expect_equal(results$metrics$GNO,  0.6342, tol = 1e-2)
	expect_equal(results$metrics$WEI, -0.3527, tol = 1e-2)

	# Check the recommendation
	expect_equal(results$recommendation, "PE3") 

})

test_that("select-zstatistic.R works on CHILLIWACK RIVER (08MH016)", {
	skip_on_cran()
	set.seed(1)

	# Load dataset and run Z-statistic selection
	df <- data_local("CAN-08MH016.csv")
	results <- select_zstatistic(df$max, samples = 25000L)

	# Check the Kappa distribution parameters (only k and h)
	expect_equal(results$reg_params[3],      0.0001, tol = 1e-3)
	expect_equal(results$reg_params[4],      0.0747, tol = 1e-3) 
	expect_equal(results$log_params[3],  0.1569, tol = 1e-3)
	expect_equal(results$log_params[4], -0.1367, tol = 1e-3) 

	# Check the bootstrap summary statistics
	expect_equal(results$reg_bias_t4, -0.0006, tol = 1e-2)
	expect_equal(results$reg_std_t4 ,  0.0409, tol = 1e-2)
	expect_equal(results$log_bias_t4,  0.0003, tol = 1e-2)
	expect_equal(results$log_std_t4 ,  0.0332, tol = 1e-2)

	# Check the Z-distances
	expect_equal(results$metrics$GEV,  0.1264, tol = 1e-2)
	expect_equal(results$metrics$GLO,  1.0763, tol = 1e-2)
	expect_equal(results$metrics$PE3, -0.4176, tol = 1e-2)
	expect_equal(results$metrics$LP3, -0.0713, tol = 1e-2)
	expect_equal(results$metrics$GNO, -0.0379, tol = 1e-2)
	expect_equal(results$metrics$WEI, -0.8623, tol = 1e-2)

	# Check the recommendation
	expect_equal(results$recommendation, "GNO") 

})

test_that("select-zstatistic.R works on OKANAGAN RIVER (08NM050)", {
	skip_on_cran()
	set.seed(1)

	# Load dataset and run Z-statistic selection
	df <- data_local("CAN-08NM050.csv")
	results <- select_zstatistic(df$max, samples = 25000L)

	# Check the Kappa distribution parameters (only k and h)
	expect_equal(results$reg_params[3],     0.6732, tol = 1e-3)
	expect_equal(results$reg_params[4],     1.1215, tol = 1e-3)
	expect_equal(results$log_params[3], 0.7486, tol = 1e-3)
	expect_equal(results$log_params[4], 0.4114, tol = 1e-3)

	# Check the bootstrap summary statistics
	expect_equal(results$reg_bias_t4, 0.0010, tol = 1e-2)
	expect_equal(results$reg_std_t4 , 0.0273, tol = 1e-2)
	expect_equal(results$log_bias_t4, 0.0008, tol = 1e-2)
	expect_equal(results$log_std_t4 , 0.0275, tol = 1e-2)

	# Check the Z-distances
	expect_equal(results$metrics$GEV, 3.9655, tol = 1e-2)
	expect_equal(results$metrics$GLO, 5.6702, tol = 1e-2)
	expect_equal(results$metrics$PE3, 3.8036, tol = 1e-2)
	expect_equal(results$metrics$LP3, 2.5453, tol = 1e-2)
	expect_equal(results$metrics$GNO, 4.0305, tol = 1e-2)
	expect_equal(results$metrics$WEI, 3.0198, tol = 1e-2)

	# Check the recommendation
	expect_equal(results$recommendation, "LP3") 

})
