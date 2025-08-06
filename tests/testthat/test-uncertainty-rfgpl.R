# NOTE: Profiling code:
# Rprof()
# Rprof(NULL)
# print(summaryRprof())

validate_rfgpl <- function(
    df,
    structure,
    ci_lower,
    ci_upper,
    slices = NULL
) {

	results <- uncertainty_rfgpl(
		df$max,
		c(6, 9),
		ns_years = df$year,
		ns_structure = structure, 
		ns_slices = slices
	)$results

	if (!is.data.frame(results)) {
		results <- results[[1]]
	}

	# NOTE: We check the estimates in test-mle-estimation.R
	expect_equal(results$ci_lower, ci_lower, tol = 1e-2)
	expect_equal(results$ci_upper, ci_upper, tol = 1e-2)

}

test_that("basic functionality is working for CRAN", { 

	# Load dataset and run RFPL uncertainty quantification
	df <- data_local("CAN-07BE001.csv")

	# Generalized Extreme Value (GEV) Distribution with RFGPL
	validate_rfgpl(
		df, S00,
		c(1702.9263, 2442.3196, 2969.9533, 3518.5173, 4290.0510, 4908.4944),
		c(2019.0482, 2963.9259, 3668.3303, 4419.4717, 5497.4190, 6391.6196),
	)

})

test_that("RFGPL uncertainty works on ATHABASCA RIVER (07BE001)", {
	skip_on_cran()

	# Load dataset and run RFPL uncertainty quantification
	df <- data_local("CAN-07BE001.csv")

	# Mean-Trend Generalized Extreme Value (GEV100) Distribution
	validate_rfgpl(
		df, S10, 
		c(1474.8399, 2230.1006, 2766.5121, 3314.3495, 4078.8225, 4696.9330),
		c(1994.6171, 2904.0166, 3592.7090, 4322.1180, 5358.6528, 6236.7929),
		slices = min(df$year)
	)

	validate_rfgpl(
		df, S10,
		c(1727.0595, 2489.0774, 3022.5734, 3577.3108, 4339.2693, 4954.5987),
		c(2199.2593, 3107.0631, 3796.5272, 4527.1532, 5581.6741, 6439.1248),
		slices = max(df$year)
	)

	# Mean + Variance Generalized Extreme Value (GEV110) Distribution
	validate_rfgpl(
		df, S11,
		c(1472.2997, 2165.6533, 2653.1161, 3154.0474, 3859.5708, 4439.9176),
		c(2134.9612, 3236.3356, 4085.1330, 4979.3474, 6261.8499, 7324.1361),
		slices = min(df$year)
	)

	validate_rfgpl(
		df, S11,
		c(1630.1067, 2212.7119, 2615.7992, 3039.7553, 3642.2869, 4121.0152),
		c(2230.9144, 3236.0134, 3971.3349, 4773.2613, 5921.5257, 6871.0598),
		slices = max(df$year)
	)

})

