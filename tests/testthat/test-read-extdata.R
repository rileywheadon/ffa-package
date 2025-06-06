test_that("extdata/Application_1.csv is readable", {

  # Locate the file in the installed package
  csv_path <- system.file("extdata", "Application_1.csv", package = "ffaframework")
  expect_true(nzchar(csv_path), info = "system.file() should return a non-empty path")
  expect_true(file.exists(csv_path), info = "File must exist on disk")

  # Read the file using base R
  df <- read.csv(csv_path, stringsAsFactors = FALSE)

  # Check that the dataframe contains the correct information
  expect_true("year" %in% names(df))
  expect_true("max" %in% names(df))
  expect_equal(nrow(df), 123)
  expect_equal(min(df$year), 1900)
  expect_equal(max(df$year), 2022)

})

test_that("extdata/Application_2.csv is readable", {

  # Locate the file in the installed package
  csv_path <- system.file("extdata", "Application_2.csv", package = "ffaframework")
  expect_true(nzchar(csv_path), info = "system.file() should return a non-empty path")
  expect_true(file.exists(csv_path), info = "File must exist on disk")

  # Read the file using base R
  df <- read.csv(csv_path, stringsAsFactors = FALSE)

  # Check that the dataframe contains the correct information
  expect_true("year" %in% names(df))
  expect_true("max" %in% names(df))
  expect_equal(nrow(df), 91)
  expect_equal(min(df$year), 1928)
  expect_equal(max(df$year), 2018)

})

test_that("extdata/Application_3.1.csv is readable", {

  # Locate the file in the installed package
  csv_path <- system.file("extdata", "Application_3.1.csv", package = "ffaframework")
  expect_true(nzchar(csv_path), info = "system.file() should return a non-empty path")
  expect_true(file.exists(csv_path), info = "File must exist on disk")

  # Read the file using base R
  df <- read.csv(csv_path, stringsAsFactors = FALSE)

  # Check that the dataframe contains the correct information
  expect_true("year" %in% names(df))
  expect_true("max" %in% names(df))
  expect_equal(nrow(df), 121)
  expect_equal(min(df$year), 1900)
  expect_equal(max(df$year), 2020)

})

test_that("extdata/Application_3.2.csv is readable", {

  # Locate the file in the installed package
  csv_path <- system.file("extdata", "Application_3.2.csv", package = "ffaframework")
  expect_true(nzchar(csv_path), info = "system.file() should return a non-empty path")
  expect_true(file.exists(csv_path), info = "File must exist on disk")

  # Read the file using base R
  df <- read.csv(csv_path, stringsAsFactors = FALSE)

  # Check that the dataframe contains the correct information
  expect_true("year" %in% names(df))
  expect_true("max" %in% names(df))
  expect_equal(nrow(df), 121)
  expect_equal(min(df$year), 1900)
  expect_equal(max(df$year), 2020)

})

test_that("extdata/Application_3.3.csv is readable", {

  # Locate the file in the installed package
  csv_path <- system.file("extdata", "Application_3.3.csv", package = "ffaframework")
  expect_true(nzchar(csv_path), info = "system.file() should return a non-empty path")
  expect_true(file.exists(csv_path), info = "File must exist on disk")

  # Read the file using base R
  df <- read.csv(csv_path, stringsAsFactors = FALSE)

  # Check that the dataframe contains the correct information
  expect_true("year" %in% names(df))
  expect_true("max" %in% names(df))
  expect_equal(nrow(df), 121)
  expect_equal(min(df$year), 1900)
  expect_equal(max(df$year), 2020)

})


