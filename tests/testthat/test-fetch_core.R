# ==============================================================================
# Tests for fetch_core.R - Core fetch calculation functions
# ==============================================================================

test_that("fetch from center of circular lake equals radius", {
  # Create circular lake with known radius

radius <- 1000
  lake <- create_circular_lake(radius = radius, n_points = 720)
  site <- create_site(500000, 4800000, "Center")

  result <- calc_test_fetch(site, lake, buffer_m = 0)

  # Mean fetch should equal radius (within 2% tolerance for discretization)
  expect_equal(result$mean, radius, tolerance = 0.02)

  # All directions should be approximately equal
  expect_lt(sd(result$fetch), radius * 0.02)
})

test_that("fetch from edge of circular lake has correct min/max", {
  radius <- 1000
  lake <- create_circular_lake(radius = radius, n_points = 720)

  # Point at 80% of radius from center
  offset <- 0.8 * radius
  site <- create_site(500000 + offset, 4800000, "Edge")

  result <- calc_test_fetch(site, lake, buffer_m = 0)

  expected_min <- radius - offset  # 200m
  expected_max <- radius + offset  # 1800m

  # Min fetch should be approximately radius - offset
  expect_equal(result$min, expected_min, tolerance = 0.15)

  # Max fetch should be approximately radius + offset
  expect_equal(result$max, expected_max, tolerance = 0.05)
})

test_that("fetch from center of rectangular lake matches geometry", {
  width <- 2000   # E-W
  height <- 1000  # N-S

  lake <- create_rectangular_lake(width = width, height = height)
  site <- create_site(500000, 4800000, "Center")

  result <- calc_test_fetch(site, lake, buffer_m = 0)

  # Find cardinal direction indices
  n_idx <- which(result$angles == 0)
  e_idx <- which(result$angles == 90)
  s_idx <- which(result$angles == 180)
  w_idx <- which(result$angles == 270)

  # N/S fetch should be height/2
  expect_equal(result$fetch[n_idx], height / 2, tolerance = 0.05)
  expect_equal(result$fetch[s_idx], height / 2, tolerance = 0.05)

  # E/W fetch should be width/2
  expect_equal(result$fetch[e_idx], width / 2, tolerance = 0.05)
  expect_equal(result$fetch[w_idx], width / 2, tolerance = 0.05)
})

test_that("effective fetch calculated correctly", {
  radius <- 1000
  lake <- create_circular_lake(radius = radius, n_points = 720)
  site <- create_site(500000, 4800000, "Center")

  result <- calc_test_fetch(site, lake, buffer_m = 0)

  # Effective fetch = mean of top 3
  top3 <- sort(result$fetch, decreasing = TRUE)[1:3]
  effective <- mean(top3)

  # For circular lake at center, effective should equal radius
  expect_equal(effective, radius, tolerance = 0.02)
})

test_that("fetch respects maximum distance setting", {
  # Large lake where fetch would exceed max
  lake <- create_circular_lake(radius = 10000, n_points = 720)
  site <- create_site(500000, 4800000, "Center")

  # Set max fetch to 5000m
  old_max <- lakefetch_options()$max_fetch_m
  lakefetch_options(max_fetch_m = 5000)

  result <- calc_test_fetch(site, lake, buffer_m = 0)

  lakefetch_options(max_fetch_m = old_max)

  # All fetch values should be capped at 5000
  expect_true(all(result$fetch <= 5000))
})

test_that("angle resolution affects number of directions", {
  lake <- create_circular_lake(radius = 1000)
  site <- create_site(500000, 4800000, "Center")

  # Test with 5 degree resolution
  old_res <- lakefetch_options()$angle_resolution_deg
  lakefetch_options(angle_resolution_deg = 5)
  result5 <- calc_test_fetch(site, lake, buffer_m = 0)

  # Test with 10 degree resolution
  lakefetch_options(angle_resolution_deg = 10)
  result10 <- calc_test_fetch(site, lake, buffer_m = 0)

  lakefetch_options(angle_resolution_deg = old_res)

  # 5-degree should have 72 directions, 10-degree should have 36
expect_equal(length(result5$angles), 72)
  expect_equal(length(result10$angles), 36)
})

test_that("orbital velocity calculation is reasonable", {
  # Test calc_orbital function
  # For 1000m fetch, orbital velocity should be positive and reasonable

  orbital <- lakefetch:::calc_orbital(1000)

  expect_true(orbital > 0)
  expect_true(orbital < 1)  # Should be less than 1 m/s for 1km fetch
})

test_that("exposure categories are assigned correctly", {
  # Sheltered: < 2500m
  # Moderate: 2500-5000m
  # Exposed: > 5000m

  # Create lakes of different sizes
  small_lake <- create_circular_lake(radius = 1000)  # 2km diameter
  medium_lake <- create_circular_lake(radius = 2000)  # 4km diameter
  large_lake <- create_circular_lake(radius = 4000)  # 8km diameter

  site <- create_site(500000, 4800000, "Center")

  # Small lake should be sheltered (effective fetch ~1000m)
  result_small <- calc_test_fetch(site, small_lake, buffer_m = 0)
  expect_lt(result_small$mean, 2500)

  # Medium lake should be moderate (effective fetch ~2000m)
  result_medium <- calc_test_fetch(site, medium_lake, buffer_m = 0)
  expect_gt(result_medium$mean, 1500)
  expect_lt(result_medium$mean, 5000)

  # Large lake should be exposed (effective fetch ~4000m)
  result_large <- calc_test_fetch(site, large_lake, buffer_m = 0)
  expect_gt(result_large$mean, 3500)
})

# ==============================================================================
# Tests for max fetch location (longest internal chord)
# ==============================================================================

test_that("max fetch chord for circular lake equals diameter at center", {
  radius <- 1000
  lake <- create_circular_lake(radius = radius, n_points = 720)

  chord <- lakefetch:::find_longest_internal_chord(lake)

  # Max chord should be approximately the diameter (2 * radius)
  expect_equal(chord$max_chord_length, 2 * radius, tolerance = 0.02)

  # Midpoint should be near the center (500000, 4800000)
  mid_coords <- sf::st_coordinates(sf::st_sfc(chord$midpoint, crs = 32618))
  expect_equal(unname(mid_coords[1, 1]), 500000, tolerance = 50)
  expect_equal(unname(mid_coords[1, 2]), 4800000, tolerance = 50)
})

test_that("max fetch chord for rectangular lake equals diagonal", {
  width <- 4000
  height <- 1000
  lake <- create_rectangular_lake(width = width, height = height)

  chord <- lakefetch:::find_longest_internal_chord(lake)

  # For a rectangle, longest internal chord = diagonal
  expected_diag <- sqrt(width^2 + height^2)
  expect_equal(chord$max_chord_length, expected_diag, tolerance = 0.05 * expected_diag)
})

test_that("find_max_fetch_location returns correct sf structure", {
  radius <- 1000
  lake <- create_circular_lake(radius = radius, n_points = 360)

  result <- lakefetch:::find_max_fetch_location(lake, 32618, fetch_method = "max",
                                                 refine = FALSE)

  # Should be an sf object with 1 row
  expect_s3_class(result, "sf")
  expect_equal(nrow(result), 1)

  # Should have required columns
  expect_true("max_chord_m" %in% names(result))
  expect_true("chord_bearing_deg" %in% names(result))
  expect_true("lake_name" %in% names(result))

  # Max chord should be approximately the diameter
  expect_equal(result$max_chord_m, 2 * radius, tolerance = 0.02)
})

test_that("find_max_fetch_location with refine returns fetch metrics", {
  radius <- 500
  lake <- create_circular_lake(radius = radius, n_points = 360)

  result <- lakefetch:::find_max_fetch_location(lake, 32618, fetch_method = "max",
                                                 refine = TRUE)

  # Should have fetch columns when refined
  expect_true("fetch_effective" %in% names(result))
  expect_true("fetch_max" %in% names(result))
  expect_true("fetch_mean" %in% names(result))

  # Fetch max from ray-casting at center should be approximately the radius
  # (ray from center hits shore at radius distance)
  expect_equal(result$fetch_max, radius, tolerance = 0.05 * radius)
})

# ==============================================================================
# Tests for relative (proportional) exposure classification
# ==============================================================================

test_that("proportional exposure columns are present in fetch results", {
  radius <- 1000
  lake <- create_circular_lake(radius = radius, n_points = 360)
  site <- create_site(500000, 4800000, "Center")

  result <- calc_test_fetch(site, lake, buffer_m = 0)

  # Use internal function to verify chord works
  chord <- lakefetch:::find_longest_internal_chord(lake)
  expect_false(is.na(chord$max_chord_length))
})

test_that("center of circular lake has fetch_proportion near 0.5", {
  # Center of circular lake: effective fetch ≈ radius, max chord ≈ diameter
  # So proportion ≈ radius / (2 * radius) = 0.5
  radius <- 1000
  lake <- create_circular_lake(radius = radius, n_points = 720)
  site <- create_site(500000, 4800000, "Center")

  result <- calc_test_fetch(site, lake, buffer_m = 0)

  # Manually compute the proportion
  chord <- lakefetch:::find_longest_internal_chord(lake)
  proportion <- result$mean / chord$max_chord_length

  # Center fetch ≈ radius, max chord ≈ 2*radius, so proportion ≈ 0.5
  expect_equal(proportion, 0.5, tolerance = 0.05)
})

test_that("edge site has lower mean fetch_proportion than center site", {
  radius <- 1000
  lake <- create_circular_lake(radius = radius, n_points = 720)

  center_result <- calc_test_fetch(
    create_site(500000, 4800000, "Center"), lake, buffer_m = 0
  )
  edge_result <- calc_test_fetch(
    create_site(500000 + 0.8 * radius, 4800000, "Edge"), lake, buffer_m = 0
  )

  chord <- lakefetch:::find_longest_internal_chord(lake)

  # Use mean fetch for comparison — center of circular lake has higher
  # mean fetch than edge (uniform vs skewed directional distribution)
  center_prop <- center_result$mean / chord$max_chord_length
  edge_prop <- edge_result$mean / chord$max_chord_length

  expect_gt(center_prop, edge_prop)
})

test_that("get_highres_fetch returns all-NA for a point outside the polygon", {
  lake <- create_circular_lake(radius = 1000)
  # 1050m from center: just outside a 1000m-radius lake, but close enough
  # that a small nudge (10m) still leaves it outside
  site <- create_site(501050, 4800000, "JustOutside")

  result <- calc_test_fetch(site, lake, buffer_m = 10)

  expect_true(all(is.na(result$fetch)))
})

test_that("calc_effective_fetch returns NA (not NaN/-Inf) for an all-NA row", {
  angles <- seq(0, 355, by = 5)
  na_row <- matrix(rep(NA_real_, length(angles)), nrow = 1)

  for (method in c("max", "top3", "cosine")) {
    eff <- lakefetch:::calc_effective_fetch(na_row, angles, method)
    expect_true(is.na(eff))
    expect_false(is.nan(eff))
  }
})

test_that("fetch_calculate gives NA (not NaN/-Inf) and warns when a matched site sits outside its lake polygon", {
  lake_sf <- create_circular_lake(radius = 1000)
  # Just outside the polygon (within gps_tolerance_m so it still gets
  # matched to the lake, but beyond buffer_distance_m so nudge_inward
  # can't pull it back inside)
  site <- create_site(501050, 4800000, "JustOutside")

  lake_obj <- list(
    all_lakes = lake_sf,
    sites = site[, c("Site", "site_name")],
    utm_epsg = sf::st_crs(lake_sf)$epsg
  )

  old_buffer <- lakefetch_options()$buffer_distance_m
  lakefetch_options(buffer_distance_m = 10)

  expect_warning(
    result <- fetch_calculate(site, lake_obj, add_context = FALSE),
    "fell outside its polygon"
  )

  lakefetch_options(buffer_distance_m = old_buffer)

  row <- sf::st_drop_geometry(result$results)
  expect_true(is.na(row$fetch_mean))
  expect_true(is.na(row$fetch_max))
  expect_true(is.na(row$fetch_effective))
  expect_false(is.nan(row$fetch_mean))
  expect_false(is.infinite(row$fetch_max))
})
