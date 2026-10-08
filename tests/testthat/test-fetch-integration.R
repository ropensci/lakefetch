# ==============================================================================
# Integration tests for fetch_core.R pipeline functions
# ==============================================================================

# --- Helper to create a full lake data structure like get_lake_boundary returns ---
create_test_lake_data <- function(lake_sf = NULL, site_sf = NULL) {
  if (is.null(lake_sf)) lake_sf <- create_circular_lake(radius = 1000)
  if (is.null(site_sf)) {
    site_sf <- create_site(500000, 4800000, name = "Center")
  }
  list(
    all_lakes = lake_sf,
    sites = site_sf,
    utm_epsg = 32618
  )
}

# --- calc_effective_fetch ---

test_that("calc_effective_fetch top3 returns mean of 3 highest", {
  angles <- seq(0, 355, by = 5)
  fetch_vals <- rep(100, length(angles))
  fetch_vals[1:3] <- c(500, 400, 300)  # 3 highest
  fetch_matrix <- matrix(fetch_vals, nrow = 1)

  result <- lakefetch:::calc_effective_fetch(fetch_matrix, angles, "top3")
  expect_equal(result, mean(c(500, 400, 300)))
})

test_that("calc_effective_fetch max returns maximum", {
  angles <- seq(0, 355, by = 5)
  fetch_vals <- rep(100, length(angles))
  fetch_vals[10] <- 999
  fetch_matrix <- matrix(fetch_vals, nrow = 1)

  result <- lakefetch:::calc_effective_fetch(fetch_matrix, angles, "max")
  expect_equal(result, 999)
})

test_that("calc_effective_fetch cosine returns weighted value", {
  angles <- seq(0, 355, by = 5)
  fetch_vals <- rep(500, length(angles))
  fetch_matrix <- matrix(fetch_vals, nrow = 1)

  result <- lakefetch:::calc_effective_fetch(fetch_matrix, angles, "cosine")
  # With uniform fetch, cosine should return ~500
  expect_true(abs(result - 500) < 1)
})

test_that("calc_effective_fetch handles multiple sites", {
  angles <- seq(0, 355, by = 5)
  fetch_matrix <- rbind(
    rep(100, length(angles)),
    rep(200, length(angles)),
    rep(300, length(angles))
  )

  result_top3 <- lakefetch:::calc_effective_fetch(fetch_matrix, angles, "top3")
  expect_equal(length(result_top3), 3)
  expect_equal(result_top3, c(100, 200, 300))

  result_max <- lakefetch:::calc_effective_fetch(fetch_matrix, angles, "max")
  expect_equal(result_max, c(100, 200, 300))
})

test_that("calc_effective_fetch errors on unknown method", {
  angles <- seq(0, 355, by = 5)
  fetch_matrix <- matrix(rep(100, length(angles)), nrow = 1)
  expect_error(lakefetch:::calc_effective_fetch(fetch_matrix, angles, "invalid"))
})

# --- nudge_inward ---

test_that("nudge_inward leaves center points unchanged", {
  lake <- create_circular_lake(radius = 1000)
  lake_boundary <- sf::st_cast(lake, "MULTILINESTRING")

  center_pt <- sf::st_sfc(sf::st_point(c(500000, 4800000)), crs = 32618)
  result <- lakefetch:::nudge_inward(center_pt, lake_boundary, lake, dist = 10)

  # Center is far from shore, should not be nudged
  expect_equal(sf::st_coordinates(sf::st_sfc(result, crs = 32618))[1, 1], 500000,
               ignore_attr = TRUE)
})

test_that("nudge_inward moves shore points inward", {
  lake <- create_circular_lake(radius = 1000)
  lake_boundary <- sf::st_cast(lake, "MULTILINESTRING")

  # Point very close to eastern shore
  shore_pt <- sf::st_sfc(sf::st_point(c(500995, 4800000)), crs = 32618)
  result <- lakefetch:::nudge_inward(shore_pt, lake_boundary, lake, dist = 10)

  # Should be moved inward (toward center, so x should decrease)
  result_coords <- sf::st_coordinates(sf::st_sfc(result, crs = 32618))
  expect_true(result_coords[1, 1] < 500995)
})

test_that("nudge_inward handles NULL boundary gracefully", {
  pt <- sf::st_sfc(sf::st_point(c(500000, 4800000)), crs = 32618)
  result <- lakefetch:::nudge_inward(pt, NULL, NULL, dist = 10)
  expect_true(inherits(result, "sfg"))
})

# --- calculate_fetch_single_lake ---

test_that("calculate_fetch_single_lake returns correct structure", {
  lake <- create_circular_lake(radius = 1000)
  site <- create_site(500000, 4800000, name = "Center")
  site$depth_m <- 5

  result <- lakefetch:::calculate_fetch_single_lake(
    sites = site,
    lake_polygon = lake,
    utm_epsg = 32618,
    lake_name = "Test Lake",
    lake_osm_id = "test_circle",
    fetch_method = "top3"
  )

  expect_type(result, "list")
  expect_true("results" %in% names(result))
  expect_true("lake" %in% names(result))
  expect_true("angles" %in% names(result))

  # Check result columns
  res <- result$results
  expect_true("fetch_mean" %in% names(res))
  expect_true("fetch_max" %in% names(res))
  expect_true("fetch_effective" %in% names(res))
  expect_true("orbital_effective" %in% names(res))
  expect_true("exposure_category" %in% names(res))
  expect_true("fetch_proportion" %in% names(res))
})

test_that("calculate_fetch_single_lake produces reasonable fetch for center of circular lake", {
  lake <- create_circular_lake(radius = 1000)
  site <- create_site(500000, 4800000, name = "Center")
  site$depth_m <- 5

  result <- lakefetch:::calculate_fetch_single_lake(
    sites = site,
    lake_polygon = lake,
    utm_epsg = 32618,
    lake_name = "Test Lake",
    lake_osm_id = "test_circle",
    fetch_method = "top3"
  )

  res <- result$results
  # Center of 1km radius lake should have ~1000m fetch in all directions
  expect_true(res$fetch_mean > 900)
  expect_true(res$fetch_mean < 1100)
  expect_true(res$fetch_max > 900)
  expect_true(res$fetch_effective > 900)
})

test_that("calculate_fetch_single_lake handles multiple sites", {
  lake <- create_circular_lake(radius = 1000)

  site1 <- create_site(500000, 4800000, name = "Center")
  site2 <- create_site(500500, 4800000, name = "East")

  sites <- rbind(site1, site2)
  sites$depth_m <- c(5, 3)

  result <- lakefetch:::calculate_fetch_single_lake(
    sites = sites,
    lake_polygon = lake,
    utm_epsg = 32618,
    lake_name = "Test Lake",
    lake_osm_id = "test_circle",
    fetch_method = "top3"
  )

  expect_equal(nrow(result$results), 2)
  # Eastern site should have less fetch toward east (closer to shore)
  expect_true(result$results$fetch_mean[2] < result$results$fetch_mean[1])
})

test_that("calculate_fetch_single_lake works with cosine method", {
  lake <- create_circular_lake(radius = 1000)
  site <- create_site(500000, 4800000, name = "Center")
  site$depth_m <- 5

  result <- lakefetch:::calculate_fetch_single_lake(
    sites = site,
    lake_polygon = lake,
    utm_epsg = 32618,
    lake_name = "Test Lake",
    lake_osm_id = "test_circle",
    fetch_method = "cosine"
  )

  expect_true(result$results$fetch_effective > 0)
})

# --- fetch_calculate (full integration) ---

test_that("fetch_calculate runs end-to-end with synthetic data", {
  lake_data <- create_test_lake_data()

  result <- fetch_calculate(
    sites = NULL,
    lake = lake_data,
    depth_m = 5,
    add_context = FALSE,
    find_max_fetch = FALSE
  )

  expect_type(result, "list")
  expect_true("results" %in% names(result))
  expect_true("lakes" %in% names(result))
  expect_true("angles" %in% names(result))
  expect_equal(nrow(result$results), 1)
  expect_true(result$results$fetch_effective > 0)
})

test_that("fetch_calculate with find_max_fetch returns max_fetch element", {
  lake_data <- create_test_lake_data()

  result <- fetch_calculate(
    sites = NULL,
    lake = lake_data,
    depth_m = 5,
    add_context = FALSE,
    find_max_fetch = TRUE
  )

  expect_true("max_fetch" %in% names(result))
  expect_true(nrow(result$max_fetch) >= 1)
})

test_that("fetch_calculate with rectangular lake shows directional fetch variation", {
  rect_lake <- create_rectangular_lake(width = 4000, height = 500)
  site <- create_site(500000, 4800000, name = "Center")

  lake_data <- create_test_lake_data(lake_sf = rect_lake, site_sf = site)

  result <- fetch_calculate(
    sites = NULL,
    lake = lake_data,
    depth_m = 5,
    add_context = FALSE
  )

  res <- result$results
  # East-west fetch should be much larger than north-south
  # fetch_90 (east) and fetch_270 (west) should be >> fetch_0 (north) and fetch_180 (south)
  expect_true(res$fetch_max > res$fetch_mean * 1.5)
})

# --- Unmatched sites get NA fetch values ---

test_that("fetch_calculate returns NA for unmatched sites", {
  lake <- create_circular_lake(radius = 1000)
  # One site inside, one site far away (unmatched)
  site_inside <- create_site(500000, 4800000, name = "Inside", epsg = 32618)
  site_outside <- create_site(510000, 4810000, name = "Outside", epsg = 32618)
  sites_both <- rbind(site_inside, site_outside)

  lake_data <- list(
    all_lakes = lake,
    sites = sites_both,
    utm_epsg = 32618
  )

  expect_warning(
    result <- fetch_calculate(
      sites = NULL, lake = lake_data, depth_m = 5, add_context = FALSE
    ),
    "not matched"
  )

  # Should have 2 rows
 expect_equal(nrow(result$results), 2)

  # Inside site should have real fetch values
  inside_row <- result$results[result$results$Site == "Inside", ]
  expect_false(is.na(inside_row$fetch_effective))
  expect_true(inside_row$fetch_effective > 0)

  # Outside site should have NA fetch values
  outside_row <- result$results[result$results$Site == "Outside", ]
  expect_true(is.na(outside_row$fetch_effective))
  expect_true(is.na(outside_row$fetch_mean))
  expect_true(is.na(outside_row$fetch_max))
  expect_true(is.na(outside_row$exposure_category))
})

test_that("fetch_calculate returns all NAs when no sites match any lake", {
  lake <- create_circular_lake(radius = 1000)
  # Site far from the lake
  site_far <- create_site(510000, 4810000, name = "Far", epsg = 32618)

  lake_data <- list(
    all_lakes = lake,
    sites = site_far,
    utm_epsg = 32618
  )

  expect_warning(
    result <- fetch_calculate(
      sites = NULL, lake = lake_data, depth_m = 5, add_context = FALSE
    ),
    "not matched|All fetch values will be NA"
  )

  expect_equal(nrow(result$results), 1)
  expect_true(is.na(result$results$fetch_effective))
  expect_true(is.na(result$results$fetch_mean))
})

test_that("download_lake_osm returns empty lakes when no water found", {
  # Mock download_lake_osm_single to return nothing
  local_mocked_bindings(
    download_lake_osm_single = function(bbox_vec, lake_names = NULL,
                                         overpass_servers = NULL,
                                         name_only = FALSE, ...) {
      list()
    },
    .package = "lakefetch"
  )

  sites <- data.frame(
    Site = "Prairie_Pond",
    latitude = 41.5,
    longitude = -99.5
  )
  sites <- lakefetch::load_sites(sites)

  expect_warning(
    result <- lakefetch:::download_lake_osm(sites),
    "No water bodies were returned by OpenStreetMap"
  )

  expect_equal(nrow(result$all_lakes), 0)
  expect_equal(nrow(result$sites), 1)
})
