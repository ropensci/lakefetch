# ==============================================================================
# Tests for globals.R - Package options functions
# ==============================================================================

test_that("lakefetch_options returns all options when called without arguments", {
  opts <- lakefetch_options()

  expect_type(opts, "list")
  expect_true("buffer_distance_m" %in% names(opts))
  expect_true("angle_resolution_deg" %in% names(opts))
  expect_true("max_fetch_m" %in% names(opts))
  expect_true("gps_tolerance_m" %in% names(opts))
  expect_true("use_parallel" %in% names(opts))
  expect_true("use_nhd" %in% names(opts))
})
test_that("lakefetch_options sets options correctly", {
  # Save original
  original <- lakefetch_options()$buffer_distance_m

  # Set new value
  lakefetch_options(buffer_distance_m = 25)
  expect_equal(lakefetch_options()$buffer_distance_m, 25)

  # Restore
  lakefetch_options(buffer_distance_m = original)
})

test_that("lakefetch_options warns on unknown option", {
  expect_warning(lakefetch_options(fake_option = 123))
})

test_that("lakefetch_reset_options restores defaults", {
  # Change an option
  lakefetch_options(buffer_distance_m = 999)
  expect_equal(lakefetch_options()$buffer_distance_m, 999)

  # Reset
 lakefetch_reset_options()

  # Should be back to default (10)
  expect_equal(lakefetch_options()$buffer_distance_m, 10)
})

test_that("get_opt retrieves individual options", {
  val <- lakefetch:::get_opt("angle_resolution_deg")

  expect_type(val, "double")
  expect_equal(val, lakefetch_options()$angle_resolution_deg)
})

test_that("default option values are sensible", {
  lakefetch_reset_options()
  opts <- lakefetch_options()

  # Buffer should be positive and reasonable
  expect_true(opts$buffer_distance_m > 0)
  expect_true(opts$buffer_distance_m < 100)

  # Angle resolution should divide 360 evenly
  expect_equal(360 %% opts$angle_resolution_deg, 0)

  # Max fetch should be large but not infinite
  expect_true(opts$max_fetch_m > 1000)
  expect_true(opts$max_fetch_m < 1000000)

  # GPS tolerance should be reasonable
  expect_true(opts$gps_tolerance_m > 0)
  expect_true(opts$gps_tolerance_m < 1000)
})

test_that("nhd_available checks correctly", {
  # This function checks if hydrogeofetch is available AND enabled
  result <- lakefetch:::nhd_available()

  expect_type(result, "logical")

  # If use_nhd is FALSE, should return FALSE regardless of package
  lakefetch_options(use_nhd = FALSE)
  expect_false(lakefetch:::nhd_available())

  # Restore
  lakefetch_reset_options()
})

test_that("parallel_available checks correctly", {
  result <- lakefetch:::parallel_available()

  expect_type(result, "logical")

  # If use_parallel is FALSE, should return FALSE
  lakefetch_options(use_parallel = FALSE)
  expect_false(lakefetch:::parallel_available())

  # Restore
  lakefetch_reset_options()
})
