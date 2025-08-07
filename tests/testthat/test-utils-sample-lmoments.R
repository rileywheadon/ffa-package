# Helper function for brevity
test_slm <- function(data, expected) {
	expect_equal(utils_sample_lmoments(data), expected, tol = 1e-5)
}

test_that("lmom_sample matches 'lmom' package on ATHABASCA RIVER (07BE001).", {
	df <- data_local("CAN-07BE001.csv")
	test_slm(df$max, c(2039.1863, 484.1581, 0.2495, 0.1970))
})

test_that("lmom_sample matches 'lmom' package on KOOTENAI RIVER (08NH021).", {
	df <- data_local("CAN-08NH021.csv")
	test_slm(df$max, c(1675.0989, 396.5617, 0.1159, 0.0428))
})

test_that("lmom_sample matches 'lmom' package on BOW RIVER (05BB001).", {
	df <- data_local("CAN-05BB001.csv")
	test_slm(df$max, c(212.0734, 33.9077, 0.1623, 0.1229))
})

test_that("lmom_sample matches 'lmom' package on CHILLIWACK RIVER (08MH016).", {
	df <- data_local("CAN-08MH016.csv")
	test_slm(df$max, c(72.6629, 11.9970, 0.1834, 0.1500))
})

test_that("lmom_sample matches 'lmom' package on OKANAGAN RIVER (08NM050).", {
	df <- data_local("CAN-08NM050.csv")
	test_slm(df$max, c(38.2095, 10.6348, 0.1150, 0.0242))
})
