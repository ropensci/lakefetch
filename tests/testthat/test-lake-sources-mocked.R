# ==============================================================================
# Tests for lake_sources.R with mocked API calls
# ==============================================================================

# Helper to create mock OSM polygon data (what standardize_osm_sf returns)
create_mock_water_polys <- function(center_lon = -74, center_lat = 43,
                                     n_lakes = 1) {
  lakes <- lapply(seq_len(n_lakes), function(i) {
    offset <- (i - 1) * 0.05
    coords <- matrix(c(
      center_lon + offset, center_lat,
      center_lon + offset + 0.01, center_lat,
      center_lon + offset + 0.01, center_lat + 0.01,
      center_lon + offset, center_lat + 0.01,
      center_lon + offset, center_lat
    ), ncol = 2, byrow = TRUE)
    sf::st_polygon(list(coords))
  })

  sf::st_sf(
    osm_id = paste0("lake_", seq_len(n_lakes)),
    name = paste0("Lake ", LETTERS[seq_len(n_lakes)]),
    geometry = sf::st_sfc(lakes, crs = 4326)
  )
}

# --- download_lake_osm with small spread (single bbox path) ---

test_that("download_lake_osm works with mocked single-bbox query", {
  sites <- data.frame(
    Site = c("S1", "S2"),
    latitude = c(43.005, 43.006),
    longitude = c(-74.005, -74.006),
    stringsAsFactors = FALSE
  )
  sites_sf <- sf::st_as_sf(sites, coords = c("longitude", "latitude"), crs = 4326)

  mock_polys <- create_mock_water_polys()
  mock_polys$area_km2 <- as.numeric(sf::st_area(sf::st_transform(mock_polys, 32618))) / 1e6

  local_mocked_bindings(
    download_lake_osm_single = function(bbox_vec, lake_names = NULL,
                                         overpass_servers = NULL,
                                         name_only = FALSE, ...) {
      list(mock_polys)
    },
    .package = "lakefetch"
  )

  result <- lakefetch:::download_lake_osm(sites)

  expect_type(result, "list")
  expect_true("all_lakes" %in% names(result))
  expect_true("sites" %in% names(result))
  expect_true("utm_epsg" %in% names(result))
  expect_true(nrow(result$all_lakes) >= 1)
})

# --- download_lake_osm with large spread (cluster path) ---
# Note: cluster path calls osmdata functions directly (not download_lake_osm_single),
# so we mock extract_osm_polys to intercept at that level.

test_that("download_lake_osm clusters sites when spread > 0.5 degrees", {
  # The cluster path calls osmdata functions directly (not download_lake_osm_single),
  # which cannot be mocked without network access. cluster_sites() logic is tested
  # separately in test-lake-sources-helpers.R.
  skip("Cluster path requires network access; cluster_sites tested separately")
})

test_that("download_lake_osm handles lake_name column for name-based queries", {
  sites <- data.frame(
    Site = c("S1", "S2"),
    latitude = c(43.005, 43.006),
    longitude = c(-74.005, -74.006),
    lake_name = c("Mirror Lake", "Mirror Lake"),
    stringsAsFactors = FALSE
  )

  mock_polys <- create_mock_water_polys()
  mock_polys$name <- "Mirror Lake"
  mock_polys$area_km2 <- as.numeric(sf::st_area(sf::st_transform(mock_polys, 32618))) / 1e6

  captured_names <- NULL
  local_mocked_bindings(
    download_lake_osm_single = function(bbox_vec, lake_names = NULL,
                                         overpass_servers = NULL,
                                         name_only = FALSE, ...) {
      captured_names <<- lake_names
      list(mock_polys)
    },
    .package = "lakefetch"
  )

  result <- lakefetch:::download_lake_osm(sites)

  # Should have passed lake names to the download function
  expect_true("Mirror Lake" %in% captured_names)
})

test_that("download_lake_osm filters small water bodies", {
  sites <- data.frame(
    Site = "S1",
    latitude = 43.005,
    longitude = -74.005,
    stringsAsFactors = FALSE
  )

  # Create two polys - one large, one tiny
  large_poly <- create_mock_water_polys()
  large_poly$area_km2 <- 1.5

  tiny_coords <- matrix(c(
    -74.001, 43.001,
    -74.0009, 43.001,
    -74.0009, 43.0011,
    -74.001, 43.0011,
    -74.001, 43.001
  ), ncol = 2, byrow = TRUE)
  tiny_poly <- sf::st_sf(
    osm_id = "tiny",
    name = "Puddle",
    geometry = sf::st_sfc(sf::st_polygon(list(tiny_coords)), crs = 4326)
  )
  tiny_poly$area_km2 <- 0.00001  # Below minimum threshold

  both <- rbind(large_poly, tiny_poly)

  local_mocked_bindings(
    download_lake_osm_single = function(bbox_vec, lake_names = NULL,
                                         overpass_servers = NULL,
                                         name_only = FALSE, ...) {
      list(both)
    },
    .package = "lakefetch"
  )

  result <- lakefetch:::download_lake_osm(sites)

  # Tiny water bodies should be filtered out
  expect_true(all(result$all_lakes$area_km2 > 0.0001))
})

test_that("download_lake_osm warns and creates fallback when no water bodies found", {
  sites <- data.frame(
    Site = "S1",
    latitude = 43.005,
    longitude = -74.005,
    stringsAsFactors = FALSE
  )

  local_mocked_bindings(
    download_lake_osm_single = function(bbox_vec, lake_names = NULL,
                                         overpass_servers = NULL,
                                         name_only = FALSE, ...) {
      list()  # No results
    },
    .package = "lakefetch"
  )

  expect_warning(
    result <- lakefetch:::download_lake_osm(sites),
    "No water bodies were returned by OpenStreetMap"
  )

  # Should return empty lake set with sites, not a fallback boundary
  expect_type(result, "list")
  expect_true("all_lakes" %in% names(result))
  expect_equal(nrow(result$all_lakes), 0)
  expect_true("sites" %in% names(result))
  expect_equal(nrow(result$sites), 1)
})

# --- download_lake_osm_single with mocked queries ---

test_that("download_lake_osm_single tries name query first", {
  mock_osm_result <- list(
    osm_polygons = sf::st_sf(
      osm_id = "123",
      name = "Test Lake",
      geometry = sf::st_sfc(
        sf::st_polygon(list(matrix(c(0, 0, 1, 0, 1, 1, 0, 1, 0, 0),
                                    ncol = 2, byrow = TRUE))),
        crs = 4326
      )
    ),
    osm_multipolygons = NULL
  )

  local_mocked_bindings(
    query_osm_by_name = function(bbox, names, overpass_servers, max_attempts = 3, ...) {
      mock_osm_result
    },
    .package = "lakefetch"
  )

  bbox <- c(left = -74.1, bottom = 42.9, right = -73.9, top = 43.1)
  result <- lakefetch:::download_lake_osm_single(
    bbox, lake_names = "Test Lake",
    overpass_servers = c("https://overpass-api.de/api/interpreter")
  )

  expect_true(length(result) > 0)
})

test_that("get_lake_boundary dispatches to OSM download when no file", {
  sites <- data.frame(
    Site = "S1",
    latitude = 43.005,
    longitude = -74.005,
    stringsAsFactors = FALSE
  )

  mock_polys <- create_mock_water_polys()
  mock_polys$area_km2 <- as.numeric(sf::st_area(sf::st_transform(mock_polys, 32618))) / 1e6

  local_mocked_bindings(
    download_lake_osm_single = function(bbox_vec, lake_names = NULL,
                                         overpass_servers = NULL,
                                         name_only = FALSE, ...) {
      list(mock_polys)
    },
    .package = "lakefetch"
  )

  result <- lakefetch::get_lake_boundary(sites)

  expect_type(result, "list")
  expect_true("all_lakes" %in% names(result))
})

# --- v0.1.5 regression tests for Khondula review fixes ---

test_that("download_lake_osm computes valid UTM EPSG for sf input in projected CRS", {
  # Khondula: passing example_lake (EPSG:32618 UTM) to get_lake_boundary
  # produced EPSG:32683364 because the UTM zone was derived from raw projected
  # coordinates instead of WGS84.
  sites_wgs <- sf::st_sf(
    Site = "S1",
    geometry = sf::st_sfc(sf::st_point(c(-74.05, 43.05)), crs = 4326)
  )
  sites_utm <- sf::st_transform(sites_wgs, 32618)

  mock_polys <- create_mock_water_polys()
  mock_polys$area_km2 <- as.numeric(sf::st_area(
    sf::st_transform(mock_polys, 32618))) / 1e6

  local_mocked_bindings(
    download_lake_osm_single = function(bbox_vec, lake_names = NULL,
                                         overpass_servers = NULL,
                                         name_only = FALSE, ...) {
      list(mock_polys)
    },
    .package = "lakefetch"
  )

  result <- lakefetch:::download_lake_osm(sites_utm)

  # Valid northern-hemisphere UTM EPSGs are 32601-32660. The pre-fix bug
  # produced numbers like 32683364.
  expect_gte(result$utm_epsg, 32601)
  expect_lte(result$utm_epsg, 32660)
  # The expected zone for (-74°, 43°) is 18N -> EPSG:32618
  expect_equal(result$utm_epsg, 32618)
})

test_that("download_lake_osm detects flexible lat/lon column names", {
  # Khondula: load_sites accepts 'lat'/'lon', get_lake_boundary did not.
  # Now get_lake_boundary should accept abbreviated, capitalized, and y/x
  # column names on data.frame input.
  mock_polys <- create_mock_water_polys()
  mock_polys$area_km2 <- as.numeric(sf::st_area(
    sf::st_transform(mock_polys, 32618))) / 1e6

  local_mocked_bindings(
    download_lake_osm_single = function(bbox_vec, lake_names = NULL,
                                         overpass_servers = NULL,
                                         name_only = FALSE, ...) {
      list(mock_polys)
    },
    .package = "lakefetch"
  )

  # Variant 1: 'lat'/'lon'
  sites_lat <- data.frame(Site = "S1", lat = 43.005, lon = -74.005,
                          stringsAsFactors = FALSE)
  expect_no_error(
    result1 <- lakefetch:::download_lake_osm(sites_lat)
  )
  expect_true("utm_epsg" %in% names(result1))

  # Variant 2: capitalized
  sites_cap <- data.frame(Site = "S1", Latitude = 43.005, Longitude = -74.005,
                          stringsAsFactors = FALSE)
  expect_no_error(lakefetch:::download_lake_osm(sites_cap))

  # Variant 3: y/x convention
  sites_xy <- data.frame(Site = "S1", y = 43.005, x = -74.005,
                         stringsAsFactors = FALSE)
  expect_no_error(lakefetch:::download_lake_osm(sites_xy))

  # Variant 4: missing both columns -> actionable error listing available cols
  sites_bad <- data.frame(Site = "S1", foo = 1, bar = 2,
                          stringsAsFactors = FALSE)
  expect_error(
    lakefetch:::download_lake_osm(sites_bad),
    "latitude / longitude columns"
  )
})

test_that("download_lake_osm takes name-filtered fast path when all sites named and spread > 0.5 deg", {
  # Khondula: wisconsin_lakes (3 lakes, ~1 degree spread, all with lake.name)
  # took 18-50 minutes via per-cluster broad queries. The fast path issues a
  # single name-filtered query covering the union bbox instead.
  sites <- data.frame(
    Site = c("S1", "S2", "S3"),
    latitude = c(43.10, 43.06, 42.59),
    longitude = c(-89.42, -89.36, -88.43),
    lake.name = c("Lake Mendota", "Lake Monona", "Geneva Lake"),
    stringsAsFactors = FALSE
  )

  mock_polys <- create_mock_water_polys(center_lon = -89, center_lat = 43,
                                         n_lakes = 3)
  mock_polys$area_km2 <- as.numeric(sf::st_area(
    sf::st_transform(mock_polys, 32616))) / 1e6

  mock_osm_result <- list(
    osm_polygons = sf::st_sf(
      osm_id = "mock_1", name = "Lake Mendota",
      geometry = sf::st_sfc(
        sf::st_polygon(list(matrix(c(-89.5, 42.5, -88.3, 42.5,
                                      -88.3, 43.2, -89.5, 43.2,
                                      -89.5, 42.5), ncol = 2, byrow = TRUE))),
        crs = 4326
      )
    ),
    osm_multipolygons = NULL
  )

  name_call_count <- 0
  osmdata_call_count <- 0

  local_mocked_bindings(
    query_osm_by_name = function(bbox, names, overpass_servers,
                                  max_attempts = 3, ...) {
      name_call_count <<- name_call_count + 1
      # Verify the name-targeted query gets the full union bbox, not a small
      # per-cluster bbox.
      bbox_span_x <- as.numeric(bbox["right"] - bbox["left"])
      expect_gt(bbox_span_x, 0.5)
      mock_osm_result
    },
    .package = "lakefetch"
  )

  # Also intercept osmdata::osmdata_sf. The pre-fix cluster path calls
  # osmdata_sf() directly (not download_lake_osm_single), so mocking this is
  # the only way to detect a regression where the fast path fails to trigger
  # and the function silently falls through to per-cluster broad queries.
  local_mocked_bindings(
    osmdata_sf = function(q) {
      osmdata_call_count <<- osmdata_call_count + 1
      mock_osm_result
    },
    .package = "osmdata"
  )

  result <- lakefetch:::download_lake_osm(sites)

  # Fast path: exactly one name-filtered call, ZERO direct osmdata_sf calls
  # (those would indicate the cluster path ran).
  expect_equal(name_call_count, 1)
  expect_equal(osmdata_call_count, 0)
  expect_true("all_lakes" %in% names(result))
})

test_that("assign_sites_to_lakes emits warning when sites cannot be matched", {
  # Khondula: unmatched sites only produced messages; users missed them.
  # Now also a warning() is emitted that surfaces through warnings().
  sites <- sf::st_sf(
    Site = c("S1"),
    lake.name = "Imaginary Lake",
    geometry = sf::st_sfc(sf::st_point(c(500000, 4800000)), crs = 32618)
  )

  # A polygon far from the site (>500 km away)
  poly_coords <- matrix(c(
    0, 0, 1000, 0, 1000, 1000, 0, 1000, 0, 0
  ), ncol = 2, byrow = TRUE)
  far_lakes <- sf::st_sf(
    osm_id = "far_lake",
    name = "Far Lake",
    area_km2 = 1.0,
    geometry = sf::st_sfc(sf::st_polygon(list(poly_coords)), crs = 32618)
  )

  expect_warning(
    lakefetch::assign_sites_to_lakes(sites, far_lakes, tolerance_m = 50),
    "could not be assigned to any lake polygon"
  )
})

test_that("query_osm_by_name does not retry on 'differing rows' parse errors", {
  # Regression for lakefetch#2 (Pakillo): the "arguments imply differing
  # number of rows" error from osmdata::osmdata_sf() is deterministic, so
  # retrying only wastes minutes on rate-limit backoff. Test that we call
  # osmdata_sf at most once when it throws that error.
  n_calls <- 0
  local_mocked_bindings(
    .package = "osmdata",
    opq = function(bbox, ...) structure(list(bbox = bbox), class = "overpass_query"),
    add_osm_feature = function(opq, ...) opq,
    osmdata_sf = function(q) {
      n_calls <<- n_calls + 1
      stop("arguments imply differing number of rows: 1160, 0")
    },
    set_overpass_url = function(url) invisible(NULL)
  )

  bbox <- c(left = -74.5, bottom = 43, right = -74, top = 43.5)
  res <- lakefetch:::query_osm_by_name(
    bbox, names = "Blue Mountain Lake",
    overpass_servers = c("https://example.invalid/api",
                          "https://example2.invalid/api"),
    max_attempts = 3
  )

  # Fatal parse error -> should short-circuit after exactly one attempt.
  expect_null(res)
  expect_equal(n_calls, 1)
})

test_that("query_osm_by_name does not retry on HTTP 429 rate-limit errors", {
  # Regression for lakefetch#2 (Pakillo): a 429 from Overpass triggers
  # osmdata's internal 60-second backoff. Retrying immediately just
  # compounds that wait for no benefit. Test that we short-circuit.
  n_calls <- 0
  local_mocked_bindings(
    .package = "osmdata",
    opq = function(bbox, ...) structure(list(bbox = bbox), class = "overpass_query"),
    add_osm_feature = function(opq, ...) opq,
    osmdata_sf = function(q) {
      n_calls <<- n_calls + 1
      stop("HTTP 429 Too Many Requests.")
    },
    set_overpass_url = function(url) invisible(NULL)
  )

  bbox <- c(left = -74.5, bottom = 43, right = -74, top = 43.5)
  res <- lakefetch:::query_osm_by_name(
    bbox, names = "Blue Mountain Lake",
    overpass_servers = c("https://example.invalid/api",
                          "https://example2.invalid/api"),
    max_attempts = 3
  )

  expect_null(res)
  expect_equal(n_calls, 1)
})

# --- v0.1.10 regression tests -----------------------------------------------

test_that("get_lake_boundary passes total_timeout_s to download_lake_osm", {
  # total_timeout_s is the new v0.1.10 wall-clock cap. Verify it's plumbed
  # from the exported function down into the internal helper.
  args_seen <- NULL
  local_mocked_bindings(
    download_lake_osm = function(sites_df, timeout = 90,
                                  total_timeout_s = 300) {
      args_seen <<- list(timeout = timeout,
                          total_timeout_s = total_timeout_s)
      list(all_lakes = sf::st_sf(osm_id = character(0),
                                  geometry = sf::st_sfc(crs = 32618)),
           sites = sites_df, utm_epsg = 32618)
    },
    .package = "lakefetch"
  )

  sites <- data.frame(Site = "S1", latitude = 43, longitude = -74)
  lakefetch::get_lake_boundary(sites, timeout = 45, total_timeout_s = 60)

  expect_equal(args_seen$timeout, 45)
  expect_equal(args_seen$total_timeout_s, 60)
})

test_that("download_lake_osm aborts cluster loop when total_timeout_s exceeded", {
  # Force the cluster path by making sites span > 0.5 degrees, then use a
  # tiny total_timeout_s (0.5s) that will always be exceeded before the
  # first cluster completes. Every mocked query returns something so we
  # measure how many clusters got processed - should be 0 or 1 given the
  # extreme budget.
  sites <- data.frame(
    Site = c("A", "B", "C"),
    latitude = c(43, 43.5, 44),
    longitude = c(-90, -89, -88),
    stringsAsFactors = FALSE
  )

  n_queries <- 0
  slow_osm <- function(q) {
    n_queries <<- n_queries + 1
    Sys.sleep(1)  # Each query takes 1 second
    list(osm_polygons = NULL, osm_multipolygons = NULL)
  }

  local_mocked_bindings(
    .package = "osmdata",
    opq = function(bbox, ...) structure(list(bbox = bbox),
                                         class = "overpass_query"),
    add_osm_feature = function(opq, ...) opq,
    osmdata_sf = slow_osm,
    set_overpass_url = function(url) invisible(NULL)
  )

  suppressWarnings(
    result <- lakefetch:::download_lake_osm(sites, timeout = 5,
                                             total_timeout_s = 0.5)
  )

  # With 3 clusters at 1s each and a 0.5s budget, we should have aborted
  # early rather than run all 3 queries.
  expect_lt(n_queries, 3)
  # But the function should still return a valid (possibly empty) result
  expect_type(result, "list")
  expect_true("all_lakes" %in% names(result))
})
