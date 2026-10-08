# ==============================================================================
# Tests for bathymetry.R
# ==============================================================================

test_that("estimate_depth_empirical calculates correct depth from area", {
  # Cael et al. (2017): depth_mean = 10.3 * area_km2^0.25
  result <- lakefetch:::estimate_depth_empirical(1.0)
  expect_equal(result$depth_mean, 10.3 * (1.0 ^ 0.25))
  expect_equal(result$depth_max, result$depth_mean * 2.5)
  expect_equal(result$source, "empirical")
  expect_equal(result$confidence, "low")
})

test_that("estimate_depth_empirical works for various lake sizes", {
  # Small lake (0.01 km2)
  small <- lakefetch:::estimate_depth_empirical(0.01)
  expect_true(small$depth_mean > 0)
  expect_true(small$depth_mean < small$depth_max)


  # Large lake (1000 km2)
  large <- lakefetch:::estimate_depth_empirical(1000)
  expect_true(large$depth_mean > small$depth_mean)

  # Depth increases with area
  medium <- lakefetch:::estimate_depth_empirical(10)
  expect_true(medium$depth_mean > small$depth_mean)
  expect_true(medium$depth_mean < large$depth_mean)
})

test_that("estimate_depth_empirical caps at reasonable maxima", {
  # Extremely large lake should be capped
  huge <- lakefetch:::estimate_depth_empirical(1e8)
  expect_lte(huge$depth_mean, 200)
  expect_lte(huge$depth_max, 500)
})

test_that("estimate_depth_empirical handles invalid input", {
  result_na <- lakefetch:::estimate_depth_empirical(NA)
  expect_true(is.na(result_na$depth_mean))
  expect_true(is.na(result_na$depth_max))
  expect_equal(result_na$confidence, "none")

  result_zero <- lakefetch:::estimate_depth_empirical(0)
  expect_true(is.na(result_zero$depth_mean))

  result_neg <- lakefetch:::estimate_depth_empirical(-5)
  expect_true(is.na(result_neg$depth_mean))
})

test_that("get_lake_depth returns user depth when provided", {
  lake <- create_circular_lake(radius = 1000)
  result <- get_lake_depth(lake, user_depth = 8.5)
  expect_equal(result$depth_mean, 8.5)
  expect_equal(result$source, "user")
  expect_equal(result$confidence, "high")
})

test_that("get_lake_depth falls back to empirical when no user depth", {
  lake <- create_circular_lake(radius = 1000)
  result <- get_lake_depth(lake)
  expect_equal(result$source, "empirical")
  expect_equal(result$confidence, "low")
  expect_true(result$depth_mean > 0)
})

test_that("get_lake_depth ignores invalid user depths", {
  lake <- create_circular_lake(radius = 1000)

  result_null <- get_lake_depth(lake, user_depth = NULL)
  expect_equal(result_null$source, "empirical")

  result_na <- get_lake_depth(lake, user_depth = NA)
  expect_equal(result_na$source, "empirical")

  result_neg <- get_lake_depth(lake, user_depth = -5)
  expect_equal(result_neg$source, "empirical")

  result_zero <- get_lake_depth(lake, user_depth = 0)
  expect_equal(result_zero$source, "empirical")
})

test_that("add_lake_depth adds correct columns", {
  lake <- create_circular_lake(radius = 1000)
  site <- create_site(500000, 4800000)
  site$lake_osm_id <- "test_circle"

  results <- add_lake_depth(site, lake)

  expect_true("depth_mean_m" %in% names(results))
  expect_true("depth_max_m" %in% names(results))
  expect_true("depth_source" %in% names(results))
  expect_false(is.na(results$depth_mean_m))
  expect_equal(results$depth_source, "empirical")
})

test_that("add_lake_depth uses user_depths when provided", {
  lake <- create_circular_lake(radius = 1000)
  site <- create_site(500000, 4800000)
  site$lake_osm_id <- "test_circle"

  user_depths <- c("test_circle" = 12.5)
  results <- add_lake_depth(site, lake, user_depths = user_depths)

  expect_equal(results$depth_mean_m, 12.5)
  expect_equal(results$depth_source, "user")
})

test_that("add_lake_depth handles NA lake_osm_id", {
  lake <- create_circular_lake(radius = 1000)
  site <- create_site(500000, 4800000)
  site$lake_osm_id <- NA_character_

  results <- add_lake_depth(site, lake)
  expect_true(is.na(results$depth_mean_m))
  expect_true(is.na(results$depth_max_m))
  expect_true(is.na(results$depth_source))
})

