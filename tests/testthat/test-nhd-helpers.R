# ==============================================================================
# Tests for nhd_integration.R helper functions (no network required)
# ==============================================================================

# --- get_connectivity_class ---

test_that("get_connectivity_class returns correct classes", {
  expect_equal(lakefetch:::get_connectivity_class(TRUE, TRUE), "Drainage")
  expect_equal(lakefetch:::get_connectivity_class(TRUE, FALSE), "Headwater")
  expect_equal(lakefetch:::get_connectivity_class(FALSE, TRUE), "Terminal")
  expect_equal(lakefetch:::get_connectivity_class(FALSE, FALSE), "Isolated")
})

test_that("get_connectivity_class handles NA input", {
  result <- lakefetch:::get_connectivity_class(NA, NA)
  expect_true(is.na(result))
})

test_that("get_connectivity_class treats FALSE as no outlet/inlet", {
  # FALSE values treated as absent
  expect_equal(lakefetch:::get_connectivity_class(FALSE, NA), "Isolated")
  expect_equal(lakefetch:::get_connectivity_class(TRUE, NA), "Headwater")
  expect_equal(lakefetch:::get_connectivity_class(NA, TRUE), "Terminal")
})

# --- get_stream_order ---

test_that("get_stream_order returns NA for NULL input", {
  result <- lakefetch:::get_stream_order(NULL)
  expect_true(is.na(result))
  expect_type(result, "integer")
})

test_that("get_stream_order extracts from streamorde column", {
  # Create a mock flowline sf object
  mock_flowline <- sf::st_sf(
    streamorde = 3L,
    geometry = sf::st_sfc(
      sf::st_linestring(matrix(c(0, 0, 1, 1), ncol = 2, byrow = TRUE)),
      crs = 4326
    )
  )
  result <- lakefetch:::get_stream_order(mock_flowline)
  expect_equal(result, 3L)
})

test_that("get_stream_order handles different column names", {
  mock_flowline <- sf::st_sf(
    stream_order = 5L,
    geometry = sf::st_sfc(
      sf::st_linestring(matrix(c(0, 0, 1, 1), ncol = 2, byrow = TRUE)),
      crs = 4326
    )
  )
  result <- lakefetch:::get_stream_order(mock_flowline)
  expect_equal(result, 5L)
})

test_that("get_stream_order returns NA when no order column exists", {
  mock_flowline <- sf::st_sf(
    some_other_col = "test",
    geometry = sf::st_sfc(
      sf::st_linestring(matrix(c(0, 0, 1, 1), ncol = 2, byrow = TRUE)),
      crs = 4326
    )
  )
  result <- lakefetch:::get_stream_order(mock_flowline)
  expect_true(is.na(result))
})

# --- get_lake_context_internal ---

test_that("get_lake_context_internal returns NAs for NULL input", {
  result <- lakefetch:::get_lake_context_internal(NULL)
  expect_true(is.na(result$nhd_permanent_id))
  expect_true(is.na(result$nhd_gnis_name))
  expect_true(is.na(result$nhd_areasqkm))
  expect_true(is.na(result$nhd_ftype))
  expect_true(is.na(result$nhd_fcode))
})

test_that("get_lake_context_internal extracts NHD attributes", {
  mock_nhd <- sf::st_sf(
    permanent_identifier = "12345678",
    gnis_name = "Test Lake",
    areasqkm = 2.5,
    ftype = "LakePond",
    fcode = 39004L,
    geometry = sf::st_sfc(
      sf::st_polygon(list(matrix(c(0, 0, 1, 0, 1, 1, 0, 1, 0, 0), ncol = 2, byrow = TRUE))),
      crs = 4326
    )
  )

  result <- lakefetch:::get_lake_context_internal(mock_nhd)
  expect_equal(result$nhd_permanent_id, "12345678")
  expect_equal(result$nhd_gnis_name, "Test Lake")
  expect_equal(result$nhd_areasqkm, 2.5)
  expect_equal(result$nhd_ftype, "LakePond")
  expect_equal(result$nhd_fcode, 39004L)
})

test_that("get_lake_context_internal handles missing columns gracefully", {
  mock_nhd <- sf::st_sf(
    gnis_name = "Partial Lake",
    geometry = sf::st_sfc(
      sf::st_polygon(list(matrix(c(0, 0, 1, 0, 1, 1, 0, 1, 0, 0), ncol = 2, byrow = TRUE))),
      crs = 4326
    )
  )

  result <- lakefetch:::get_lake_context_internal(mock_nhd)
  expect_equal(result$nhd_gnis_name, "Partial Lake")
  expect_true(is.na(result$nhd_permanent_id))
  expect_true(is.na(result$nhd_areasqkm))
})

# --- calc_distance_bearing ---

test_that("calc_distance_bearing returns NA for NULL input", {
  result <- lakefetch:::calc_distance_bearing(NULL, NULL, 32618)
  expect_true(is.na(result$dist_m))
  expect_true(is.na(result$bearing))
})

test_that("calc_distance_bearing calculates distance and bearing", {
  # Two points ~111 km apart (1 degree latitude)
  from_pt <- sf::st_point(c(-74, 43))
  to_pt <- sf::st_point(c(-74, 44))

  result <- lakefetch:::calc_distance_bearing(from_pt, to_pt, 32618)

  expect_true(result$dist_m > 100000)  # ~111 km
  expect_true(result$dist_m < 120000)
  expect_equal(result$bearing, "N")  # Due north
})

# --- match_lake_to_nhd ---

test_that("match_lake_to_nhd returns NULL for NULL waterbodies", {
  lake <- sf::st_sf(
    geometry = sf::st_sfc(
      sf::st_polygon(list(matrix(c(-74, 43, -73.99, 43, -73.99, 43.01, -74, 43.01, -74, 43),
                                  ncol = 2, byrow = TRUE))),
      crs = 4326
    )
  )
  expect_null(lakefetch:::match_lake_to_nhd(lake, NULL))
})

test_that("match_lake_to_nhd returns NULL for empty waterbodies", {
  lake <- sf::st_sf(
    geometry = sf::st_sfc(
      sf::st_polygon(list(matrix(c(-74, 43, -73.99, 43, -73.99, 43.01, -74, 43.01, -74, 43),
                                  ncol = 2, byrow = TRUE))),
      crs = 4326
    )
  )
  empty_nhd <- sf::st_sf(
    permanent_identifier = character(0),
    geometry = sf::st_sfc(crs = 4326)
  )
  expect_null(lakefetch:::match_lake_to_nhd(lake, empty_nhd))
})

test_that("match_lake_to_nhd returns matching waterbody", {
  lake <- sf::st_sf(
    geometry = sf::st_sfc(
      sf::st_polygon(list(matrix(c(-74, 43, -73.99, 43, -73.99, 43.01, -74, 43.01, -74, 43),
                                  ncol = 2, byrow = TRUE))),
      crs = 4326
    )
  )
  # NHD waterbody overlapping the lake
  nhd <- sf::st_sf(
    permanent_identifier = "12345",
    gnis_name = "Test Lake",
    geometry = sf::st_sfc(
      sf::st_polygon(list(matrix(c(-74.001, 42.999, -73.989, 42.999,
                                    -73.989, 43.011, -74.001, 43.011, -74.001, 42.999),
                                  ncol = 2, byrow = TRUE))),
      crs = 4326
    )
  )

  result <- lakefetch:::match_lake_to_nhd(lake, nhd)
  expect_false(is.null(result))
  expect_equal(result$permanent_identifier, "12345")
})

test_that("match_lake_to_nhd returns NULL for non-overlapping waterbodies", {
  lake <- sf::st_sf(
    geometry = sf::st_sfc(
      sf::st_polygon(list(matrix(c(-74, 43, -73.99, 43, -73.99, 43.01, -74, 43.01, -74, 43),
                                  ncol = 2, byrow = TRUE))),
      crs = 4326
    )
  )
  # NHD waterbody far away
  nhd <- sf::st_sf(
    permanent_identifier = "99999",
    geometry = sf::st_sfc(
      sf::st_polygon(list(matrix(c(-80, 40, -79.99, 40, -79.99, 40.01, -80, 40.01, -80, 40),
                                  ncol = 2, byrow = TRUE))),
      crs = 4326
    )
  )

  result <- lakefetch:::match_lake_to_nhd(lake, nhd)
  expect_null(result)
})

# --- add_lake_context with nhd unavailable ---

test_that("add_lake_context adds NA columns when nhdplusTools not available", {
  lake <- create_circular_lake(radius = 1000)
  site <- create_site(500000, 4800000, name = "Test")
  site$lake_osm_id <- "test_circle"

  # Mock nhd_available to return FALSE
  local_mocked_bindings(
    nhd_available = function() FALSE,
    .package = "lakefetch"
  )

  result <- lakefetch::add_lake_context(site, lake, 32618)

  # Should have all NHD columns, but all NA
  expect_true("nhd_permanent_id" %in% names(result))
  expect_true("connectivity_class" %in% names(result))
  expect_true("outlet_dist_m" %in% names(result))
  expect_true(all(is.na(result$nhd_permanent_id)))
  expect_true(all(is.na(result$connectivity_class)))
})

test_that("calc_distance_bearing gets correct bearing directions", {
  origin <- sf::st_point(c(-74, 43))

  # Point to the east
  east_pt <- sf::st_point(c(-73, 43))
  result_e <- lakefetch:::calc_distance_bearing(origin, east_pt, 32618)
  expect_equal(result_e$bearing, "E")

  # Point to the south
  south_pt <- sf::st_point(c(-74, 42))
  result_s <- lakefetch:::calc_distance_bearing(origin, south_pt, 32618)
  expect_equal(result_s$bearing, "S")

  # Point to the west
  west_pt <- sf::st_point(c(-75, 43))
  result_w <- lakefetch:::calc_distance_bearing(origin, west_pt, 32618)
  expect_equal(result_w$bearing, "W")
})

# --- add_lake_context defensive handling of empty / NA-bbox inputs ---

test_that("add_lake_context returns fetch_results unchanged when lake_polygons is empty", {
  skip_if_not_installed("nhdplusTools")

  # Build minimal fetch_results with NA-typed columns
  fetch_results <- sf::st_sf(
    Site = "S1",
    lake_osm_id = NA_character_,
    geometry = sf::st_sfc(sf::st_point(c(500000, 4800000)), crs = 32618)
  )

  # Empty lake polygons (no rows)
  empty_lakes <- sf::st_sf(
    osm_id = character(0),
    geometry = sf::st_sfc(crs = 32618)
  )

  # Should not error, should not call NHD network, and should still add the
  # NA placeholder columns expected of add_lake_context.
  expect_message(
    result <<- lakefetch::add_lake_context(fetch_results, empty_lakes,
                                            utm_epsg = 32618),
    "No matched lake polygons"
  )

  expect_s3_class(result, "sf")
  expect_true("nhd_permanent_id" %in% names(result))
  expect_true(all(is.na(result$nhd_permanent_id)))
})

test_that("add_lake_context skips NHD lookup when lake bbox contains NA", {
  skip_if_not_installed("nhdplusTools")

  fetch_results <- sf::st_sf(
    Site = "S1",
    lake_osm_id = NA_character_,
    geometry = sf::st_sfc(sf::st_point(c(500000, 4800000)), crs = 32618)
  )

  # Lake polygons with an empty geometry produces an NA bbox
  empty_geom_lakes <- sf::st_sf(
    osm_id = "empty",
    geometry = sf::st_sfc(sf::st_polygon(), crs = 32618)
  )

  expect_message(
    result <<- lakefetch::add_lake_context(fetch_results, empty_geom_lakes,
                                            utm_epsg = 32618),
    "NA - skipping NHD|No matched lake polygons"
  )

  expect_s3_class(result, "sf")
  expect_true("nhd_permanent_id" %in% names(result))
})

test_that("get_watershed_area returns NA when nhdplusTools unavailable", {
  # Simulate nhdplusTools not being installed
  local_mocked_bindings(
    nhd_available = function() FALSE,
    .package = "lakefetch"
  )

  ring <- matrix(c(-74, 43, -73.99, 43, -73.99, 43.01,
                   -74, 43.01, -74, 43),
                 ncol = 2, byrow = TRUE)
  lake_polygon_wgs84 <- sf::st_sf(
    geometry = sf::st_sfc(sf::st_polygon(list(ring)), crs = 4326)
  )
  result <- lakefetch:::get_watershed_area(NULL, lake_polygon_wgs84)
  expect_true(is.na(result))
})

test_that("get_watershed_area returns NA when discover_nhdplus_id fails", {
  skip_if_not_installed("nhdplusTools")

  ring <- matrix(c(-74, 43, -73.99, 43, -73.99, 43.01,
                   -74, 43.01, -74, 43),
                 ncol = 2, byrow = TRUE)
  lake_polygon_wgs84 <- sf::st_sf(
    geometry = sf::st_sfc(sf::st_polygon(list(ring)), crs = 4326)
  )

  local_mocked_bindings(
    .package = "nhdplusTools",
    discover_nhdplus_id = function(point) stop("mock network failure")
  )
  result <- lakefetch:::get_watershed_area(NULL, lake_polygon_wgs84)
  expect_true(is.na(result))
})

test_that("get_watershed_area returns NA when no NHD comid is returned", {
  skip_if_not_installed("nhdplusTools")

  ring <- matrix(c(-74, 43, -73.99, 43, -73.99, 43.01,
                   -74, 43.01, -74, 43),
                 ncol = 2, byrow = TRUE)
  lake_polygon_wgs84 <- sf::st_sf(
    geometry = sf::st_sfc(sf::st_polygon(list(ring)), crs = 4326)
  )

  local_mocked_bindings(
    .package = "nhdplusTools",
    discover_nhdplus_id = function(point) NULL
  )
  result <- lakefetch:::get_watershed_area(NULL, lake_polygon_wgs84)
  expect_true(is.na(result))
})
