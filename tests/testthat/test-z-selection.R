# NOTE: Tolerance is higher in some tests because of randomness in the bootstrap
test_that("Test z-statistic.R on data set #1", {
	set.seed(1)

	# Run Z-statistic selection with optional profiling
	start <- Sys.time()
	df <- load_data("Application_1.csv")
	results <- z.selection(df$max)
	end <- Sys.time()
	# print(end - start)

	# Check the Kappa distribution parameters (only k and h)
	expect_equal(results$params[3], -0.1544, tol = 1e-4)
	expect_equal(results$params[4], -0.1704, tol = 1e-4)
	expect_equal(results$log_params[3], NULL)
	expect_equal(results$log_params[4], NULL)

	# Check the bootstrap summary statistics
	expect_equal(results$bootstrap$bias_t4    , -0.0026, tol = 1e-2)
	expect_equal(results$bootstrap$sd_t4      ,  0.0514, tol = 1e-2)
	expect_equal(results$bootstrap$log_bias_t4, NULL)
	expect_equal(results$bootstrap$log_sd_t4  , NULL)

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


test_that("Test z-statistic.R on data set #2", {
	set.seed(1)

	# Load dataset and run Z-statistic selection
	df <- load_data("Application_2.csv")
	results <- z.selection(df$max)

	# Check the Kappa distribution parameters (only k and h)
	expect_equal(results$params[3],     0.5351, tol = 1e-4)
	expect_equal(results$params[4],     0.9253, tol = 1e-4)
	expect_equal(results$log_params[3], 0.7347, tol = 1e-4)
	expect_equal(results$log_params[4], 0.5134, tol = 1e-4)

	# Check the bootstrap summary statistics
	expect_equal(results$bootstrap$bias_t4    , 0.0010, tol = 1e-2)
	expect_equal(results$bootstrap$sd_t4      , 0.0286, tol = 1e-2)
	expect_equal(results$bootstrap$log_bias_t4, 0.0009, tol = 1e-2)
	expect_equal(results$bootstrap$log_sd_t4  , 0.0276, tol = 1e-2)

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

test_that("Test z-statistic.R on data set #3.1", {
	set.seed(1)

	# Load dataset and run Z-statistic selection
	df <- load_data("Application_3.1.csv")
	results <- z.selection(df$max)

	# Check the Kappa distribution parameters (only k and h)
	expect_equal(results$params[3],     0.1065, tol = 1e-4)
	expect_equal(results$params[4],     0.2881, tol = 1e-4)
	expect_equal(results$log_params[3], 0.2875, tol = 1e-4)
	expect_equal(results$log_params[4], 0.1371, tol = 1e-4)

	# Check the bootstrap summary statistics
	expect_equal(results$bootstrap$bias_t4    , 0.0001, tol = 1e-2)
	expect_equal(results$bootstrap$sd_t4      , 0.0323, tol = 1e-2)
	expect_equal(results$bootstrap$log_bias_t4, 0.0005, tol = 1e-2)
	expect_equal(results$bootstrap$log_sd_t4  , 0.0273, tol = 1e-2)

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

test_that("Test z-statistic.R on data set #3.2", {
	set.seed(1)

	# Load dataset and run Z-statistic selection
	df <- load_data("Application_3.2.csv")
	results <- z.selection(df$max)

	# Check the Kappa distribution parameters (only k and h)
	expect_equal(results$params[3],      0.0001, tol = 1e-4)
	expect_equal(results$params[4],      0.0747, tol = 1e-4) 
	expect_equal(results$log_params[3],  0.1569, tol = 1e-4)
	expect_equal(results$log_params[4], -0.1367, tol = 1e-4) 

	# Check the bootstrap summary statistics
	expect_equal(results$bootstrap$bias_t4    , -0.0006, tol = 1e-2)
	expect_equal(results$bootstrap$sd_t4      ,  0.0409, tol = 1e-2)
	expect_equal(results$bootstrap$log_bias_t4,  0.0003, tol = 1e-2)
	expect_equal(results$bootstrap$log_sd_t4  ,  0.0332, tol = 1e-2)

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

test_that("Test z-statistic.R on data set #3.3", {
	set.seed(1)

	# Load dataset and run Z-statistic selection
	df <- load_data("Application_3.3.csv")
	results <- z.selection(df$max)

	# Check the Kappa distribution parameters (only k and h)
	expect_equal(results$params[3],     0.6732, tol = 1e-4)
	expect_equal(results$params[4],     1.1215, tol = 1e-4)
	expect_equal(results$log_params[3], 0.7486, tol = 1e-4)
	expect_equal(results$log_params[4], 0.4114, tol = 1e-4)

	# Check the bootstrap summary statistics
	expect_equal(results$bootstrap$bias_t4    , 0.0010, tol = 1e-2)
	expect_equal(results$bootstrap$sd_t4      , 0.0273, tol = 1e-2)
	expect_equal(results$bootstrap$log_bias_t4, 0.0008, tol = 1e-2)
	expect_equal(results$bootstrap$log_sd_t4  , 0.0275, tol = 1e-2)

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
