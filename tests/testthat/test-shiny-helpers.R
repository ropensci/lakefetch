# ==============================================================================
# Tests for shiny_app.R helper functions
# ==============================================================================

test_that("reclassify_exposure assigns correct absolute categories", {
  results <- data.frame(
    fetch_effective = c(1000, 3000, 7000),
    stringsAsFactors = FALSE
  )

  reclassified <- lakefetch:::reclassify_exposure(
    results,
    sheltered_m = 2500,
    exposed_m = 5000,
    rel_sheltered = 0.25,
    rel_exposed = 0.50
  )

  expect_equal(reclassified$exposure_category, c("Sheltered", "Moderate", "Exposed"))
})

test_that("reclassify_exposure assigns relative categories when column exists", {
  results <- data.frame(
    fetch_effective = c(1000, 3000, 7000),
    fetch_proportion = c(0.1, 0.35, 0.8),
    stringsAsFactors = FALSE
  )

  reclassified <- lakefetch:::reclassify_exposure(
    results,
    sheltered_m = 2500,
    exposed_m = 5000,
    rel_sheltered = 0.25,
    rel_exposed = 0.50
  )

  expect_true("exposure_relative" %in% names(reclassified))
  expect_equal(reclassified$exposure_relative, c("Sheltered", "Moderate", "Exposed"))
})

test_that("reclassify_exposure skips relative when column missing", {
  results <- data.frame(
    fetch_effective = c(1000, 3000),
    stringsAsFactors = FALSE
  )

  reclassified <- lakefetch:::reclassify_exposure(
    results,
    sheltered_m = 2500,
    exposed_m = 5000,
    rel_sheltered = 0.25,
    rel_exposed = 0.50
  )

  expect_false("exposure_relative" %in% names(reclassified))
})

test_that("reclassify_exposure handles boundary values", {
  results <- data.frame(
    fetch_effective = c(2500, 5000),
    stringsAsFactors = FALSE
  )

  reclassified <- lakefetch:::reclassify_exposure(
    results,
    sheltered_m = 2500,
    exposed_m = 5000,
    rel_sheltered = 0.25,
    rel_exposed = 0.50
  )

  # At exactly sheltered_m, fetch_effective < sheltered_m is FALSE
  expect_equal(reclassified$exposure_category[1], "Moderate")
  # At exactly exposed_m, fetch_effective > exposed_m is FALSE
  expect_equal(reclassified$exposure_category[2], "Moderate")
})
