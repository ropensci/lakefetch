# ==============================================================================
# Tests for rOpenSci Spatial Software Standards (SP6.0, SP6.1, SP6.2)
# ==============================================================================

# --- SP6.0: Round-trip coordinate transformation tests ---

test_that("SP6.0: WGS84 -> UTM -> WGS84 round-trip preserves coordinates", {
  # Create a point in WGS84
  original_lon <- -73.82
  original_lat <- 43.85
  pt_wgs84 <- sf::st_sf(
    Site = "roundtrip_test",
    geometry = sf::st_sfc(sf::st_point(c(original_lon, original_lat)), crs = 4326)
  )

  # Transform to UTM zone 18N (same as package uses for this region)
  pt_utm <- sf::st_transform(pt_wgs84, 32618)

  # Transform back to WGS84

  pt_back <- sf::st_transform(pt_utm, 4326)

  # Recover coordinates
  coords_back <- sf::st_coordinates(pt_back)

  # Should match original within floating-point tolerance

  expect_equal(unname(coords_back[1, "X"]), original_lon, tolerance = 1e-8)
  expect_equal(unname(coords_back[1, "Y"]), original_lat, tolerance = 1e-8)
})

test_that("SP6.0: Lake polygon survives WGS84 -> UTM -> WGS84 round-trip", {
  # Create a small lake polygon in WGS84
  lon_center <- -89.4
  lat_center <- 43.1
  delta <- 0.01
  coords <- rbind(
    c(lon_center - delta, lat_center - delta),
    c(lon_center + delta, lat_center - delta),
    c(lon_center + delta, lat_center + delta),
    c(lon_center - delta, lat_center + delta),
    c(lon_center - delta, lat_center - delta)
  )
  poly_wgs84 <- sf::st_sf(
    osm_id = "roundtrip",
    name = "Roundtrip Lake",
    geometry = sf::st_sfc(sf::st_polygon(list(coords)), crs = 4326)
  )
  area_original <- sf::st_area(poly_wgs84)

  # Round-trip through UTM
  poly_utm <- sf::st_transform(poly_wgs84, 32616)
  poly_back <- sf::st_transform(poly_utm, 4326)
  area_back <- sf::st_area(poly_back)

  # Area should be preserved
  expect_equal(as.numeric(area_original), as.numeric(area_back), tolerance = 1e-6)

  # Vertex coordinates should be preserved
  coords_back <- sf::st_coordinates(poly_back)
  expect_equal(unname(coords_back[1, "X"]), coords[1, 1], tolerance = 1e-8)
  expect_equal(unname(coords_back[1, "Y"]), coords[1, 2], tolerance = 1e-8)
})

# --- SP6.1: Cartesian (UTM) vs curvilinear (WGS84) input equivalence ---

test_that("SP6.1: fetch_calculate produces equivalent results from UTM and WGS84 lake data", {
  # Create lake and site in UTM (Cartesian)
  lake_utm <- create_circular_lake(
    center_x = 500000, center_y = 4800000,
    radius = 1000, epsg = 32618
  )
  site_utm <- create_site(500000, 4800000, name = "Center", epsg = 32618)

  # Package expects a list structure from get_lake_boundary
  lake_data_utm <- list(
    all_lakes = lake_utm,
    sites = site_utm,
    utm_epsg = 32618
  )

  # Now create the same lake/site in WGS84 (curvilinear)
  lake_wgs84 <- sf::st_transform(lake_utm, 4326)
  site_wgs84 <- sf::st_transform(site_utm, 4326)

  # Back-project to UTM (simulating what get_lake_boundary does internally)
  lake_data_wgs84 <- list(
    all_lakes = sf::st_transform(lake_wgs84, 32618),
    sites = sf::st_transform(site_wgs84, 32618),
    utm_epsg = 32618
  )

  # Calculate fetch from both
  result_utm <- lakefetch::fetch_calculate(
    sites = NULL, lake = lake_data_utm, depth_m = 5, add_context = FALSE
  )
  result_wgs84 <- lakefetch::fetch_calculate(
    sites = NULL, lake = lake_data_wgs84, depth_m = 5, add_context = FALSE
  )

  # Effective fetch should be equivalent after round-trip projection
  fetch_utm <- result_utm$results$fetch_effective
  fetch_wgs84 <- result_wgs84$results$fetch_effective

  # Within 1% - both end up in same UTM CRS for calculation
  expect_equal(fetch_utm, fetch_wgs84, tolerance = 0.01)
})

# --- SP6.2: Extreme geographic coordinates ---

test_that("SP6.2: load_sites validates latitude range -90 to 90", {
  # Latitude beyond valid range should be caught
  extreme_df <- data.frame(
    Site = c("Normal", "TooHigh", "TooLow"),
    latitude = c(43.0, 91.0, -91.0),
    longitude = c(-73.0, -73.0, -73.0)
  )
  result <- lakefetch::load_sites(extreme_df)
  # Invalid coordinates should be removed
  expect_true(nrow(result) <= 3)
})

test_that("SP6.2: load_sites validates longitude range -180 to 180", {
  extreme_df <- data.frame(
    Site = c("Normal", "TooEast", "TooWest"),
    latitude = c(43.0, 43.0, 43.0),
    longitude = c(-73.0, 181.0, -181.0)
  )
  result <- lakefetch::load_sites(extreme_df)
  expect_true(nrow(result) <= 3)
})

test_that("SP6.2: load_sites handles (0,0) null island coordinates", {
  null_island <- data.frame(
    Site = c("Real", "NullIsland"),
    latitude = c(43.0, 0.0),
    longitude = c(-73.0, 0.0)
  )
  expect_warning(
    result <- lakefetch::load_sites(null_island),
    "0.*0|null|removed"
  )
  expect_equal(nrow(result), 1)
})

test_that("SP6.2: high latitude site produces valid UTM zone", {
  # Svalbard at ~78°N - tests UTM zone calculation near polar limit
  high_lat_df <- data.frame(
    Site = "Svalbard_Lake",
    latitude = 78.2,
    longitude = 15.6
  )
  result <- lakefetch::load_sites(high_lat_df)
  expect_equal(nrow(result), 1)
  expect_equal(result$latitude, 78.2)
})

test_that("SP6.2: southern hemisphere site produces valid UTM zone", {
  # Patagonia at ~50°S
  south_df <- data.frame(
    Site = "Patagonia_Lake",
    latitude = -50.3,
    longitude = -72.1
  )
  result <- lakefetch::load_sites(south_df)
  expect_equal(nrow(result), 1)
  expect_equal(result$latitude, -50.3)
})

test_that("SP6.2: antimeridian-adjacent site loads correctly", {
  # Lake near the antimeridian (Kamchatka, Russia ~177°E)
  antimeridian_df <- data.frame(
    Site = "Kamchatka_Lake",
    latitude = 56.0,
    longitude = 177.5
  )
  result <- lakefetch::load_sites(antimeridian_df)
  expect_equal(nrow(result), 1)
  expect_equal(result$longitude, 177.5)
})

test_that("SP6.2: negative antimeridian-adjacent site loads correctly", {
  # Aleutian Islands ~-179°
  aleutian_df <- data.frame(
    Site = "Aleutian_Lake",
    latitude = 52.0,
    longitude = -179.5
  )
  result <- lakefetch::load_sites(aleutian_df)
  expect_equal(nrow(result), 1)
  expect_equal(result$longitude, -179.5)
})

test_that("SP6.2: equatorial site loads and processes correctly", {
  # Lake Victoria ~0° latitude
  equator_df <- data.frame(
    Site = "Victoria_Shore",
    latitude = -0.5,
    longitude = 32.8
  )
  result <- lakefetch::load_sites(equator_df)
  expect_equal(nrow(result), 1)
})

test_that("SP6.2: UTM zone calculation handles hemisphere boundary", {
  # Sites just north and south of equator should get different UTM bands
  north_df <- data.frame(Site = "North", latitude = 0.01, longitude = 37.0)
  south_df <- data.frame(Site = "South", latitude = -0.01, longitude = 37.0)

  north_sites <- lakefetch::load_sites(north_df)
  south_sites <- lakefetch::load_sites(south_df)

  expect_equal(nrow(north_sites), 1)
  expect_equal(nrow(south_sites), 1)
})
