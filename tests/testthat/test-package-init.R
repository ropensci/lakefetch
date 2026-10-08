# ==============================================================================
# Tests for zzz.R package initialization
# ==============================================================================

test_that(".onLoad sets default options", {
  # Save current options
  all_opt_names <- c(
    "lakefetch.buffer_distance_m",
    "lakefetch.angle_resolution_deg",
    "lakefetch.max_fetch_m",
    "lakefetch.validation_buffer_m",
    "lakefetch.default_wind_speed_ms",
    "lakefetch.default_depth_m",
    "lakefetch.gps_tolerance_m",
    "lakefetch.fetch_method",
    "lakefetch.exposure_sheltered_m",
    "lakefetch.exposure_exposed_m",
    "lakefetch.exposure_relative_sheltered",
    "lakefetch.exposure_relative_exposed",
    "lakefetch.use_parallel",
    "lakefetch.use_nhd"
  )

  # All options should be set after package load
  for (opt_name in all_opt_names) {
    expect_false(is.null(getOption(opt_name)),
                 info = paste("Option", opt_name, "should be set"))
  }
})

test_that(".onLoad does not override existing options", {
  # Set a custom value
  old_val <- getOption("lakefetch.buffer_distance_m")
  options(lakefetch.buffer_distance_m = 999)

  # Re-run .onLoad
  lakefetch:::.onLoad("dummy", "lakefetch")

  # Should still be our custom value

  expect_equal(getOption("lakefetch.buffer_distance_m"), 999)

  # Restore
  options(lakefetch.buffer_distance_m = old_val)
})

test_that("default option values are correct", {
  expect_equal(getOption("lakefetch.angle_resolution_deg"), 5)
  expect_equal(getOption("lakefetch.max_fetch_m"), 50000)
  expect_equal(getOption("lakefetch.fetch_method"), "top3")
  expect_equal(getOption("lakefetch.exposure_sheltered_m"), 2500)
  expect_equal(getOption("lakefetch.exposure_exposed_m"), 5000)
  expect_equal(getOption("lakefetch.gps_tolerance_m"), 100)
})
