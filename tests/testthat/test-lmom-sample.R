test_lmom <- function(data, expected) {
	expect_equal(lmom_sample(data), expected, tol = 1e-5)
}

test_that("lmom_sample matches samlmu (from lmom package) on data set #1.", {
	df <- load_data("Application_1.csv")
	test_lmom(df$max, c(2039.1863, 484.1581, 0.2495, 0.1970))
})

test_that("lmom_sample matches samlmu (from lmom package) on data set #2.", {
	df <- load_data("Application_2.csv")
	test_lmom(df$max, c(1675.0989, 396.5617, 0.1159, 0.0428))
})

test_that("lmom_sample matches samlmu (from lmom package) on data set #3.1.", {
	df <- load_data("Application_3.1.csv")
	test_lmom(df$max, c(212.0734, 33.9077, 0.1623, 0.1229))
})

test_that("lmom_sample matches samlmu (from lmom package) on data set #3.2.", {
	df <- load_data("Application_3.2.csv")
	test_lmom(df$max, c(72.6629, 11.9970, 0.1834, 0.1500))
})

test_that("lmom_sample matches samlmu (from lmom package) on data set #3.3.", {
	df <- load_data("Application_3.3.csv")
	test_lmom(df$max, c(38.2095, 10.6348, 0.1150, 0.0242))
})
