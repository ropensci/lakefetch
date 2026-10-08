# ==============================================================================
# Helper functions for creating test geometries
# ==============================================================================

#' Create a circular lake polygon
#' @param center_x X coordinate of center (in meters, UTM)
#' @param center_y Y coordinate of center (in meters, UTM)
#' @param radius Radius in meters
#' @param n_points Number of points to approximate circle
#' @param epsg EPSG code for CRS
#' @return sf object with lake polygon
create_circular_lake <- function(center_x = 500000, center_y = 4800000,
                                  radius = 1000, n_points = 360, epsg = 32618) {
  angles <- seq(0, 2 * pi, length.out = n_points + 1)
  x <- center_x + radius * cos(angles)
  y <- center_y + radius * sin(angles)

  coords <- cbind(x, y)
  poly <- sf::st_polygon(list(coords))
  lake_sf <- sf::st_sf(
    osm_id = "test_circle",
    name = "Test Circular Lake",
    geometry = sf::st_sfc(poly, crs = epsg)
  )
  lake_sf$area_km2 <- as.numeric(sf::st_area(lake_sf)) / 1e6
  return(lake_sf)
}

#' Create a rectangular lake polygon
#' @param center_x X coordinate of center
#' @param center_y Y coordinate of center
#' @param width Width in meters (E-W)
#' @param height Height in meters (N-S)
#' @param epsg EPSG code for CRS
#' @return sf object with lake polygon
create_rectangular_lake <- function(center_x = 500000, center_y = 4800000,
                                     width = 2000, height = 1000, epsg = 32618) {
  half_w <- width / 2
  half_h <- height / 2

  coords <- rbind(
    c(center_x - half_w, center_y - half_h),
    c(center_x + half_w, center_y - half_h),
    c(center_x + half_w, center_y + half_h),
    c(center_x - half_w, center_y + half_h),
    c(center_x - half_w, center_y - half_h)
  )

  poly <- sf::st_polygon(list(coords))
  lake_sf <- sf::st_sf(
    osm_id = "test_rectangle",
    name = "Test Rectangular Lake",
    geometry = sf::st_sfc(poly, crs = epsg)
  )
  lake_sf$area_km2 <- as.numeric(sf::st_area(lake_sf)) / 1e6
  return(lake_sf)
}

#' Create a test site point
#' @param x X coordinate
#' @param y Y coordinate
#' @param name Site name
#' @param epsg EPSG code for CRS
#' @return sf object with point
create_site <- function(x, y, name = "Test Site", epsg = 32618) {
  site_sf <- sf::st_sf(
    Site = name,
    site_name = name,
    lake_osm_id = "test",
    lake_name = "Test Lake",
    geometry = sf::st_sfc(sf::st_point(c(x, y)), crs = epsg)
  )
  return(site_sf)
}

#' Get lake boundary as linestring
#' @param lake_sf Lake polygon sf object
#' @return sf linestring of lake boundary
#' @keywords internal
lake_boundary_as_lines <- function(lake_sf) {
  tryCatch({
    sf::st_cast(lake_sf, "MULTILINESTRING")
  }, error = function(e) {
    sf::st_boundary(lake_sf)
  })
}

#' Calculate fetch for a test site
#' @param site_sf Site point sf object
#' @param lake_sf Lake polygon sf object
#' @param buffer_m Buffer distance (0 for precise testing)
#' @return List with fetch results
calc_test_fetch <- function(site_sf, lake_sf, buffer_m = 0) {
  old_buffer <- lakefetch_options()$buffer_distance_m

  lakefetch_options(buffer_distance_m = buffer_m)

  lake_boundary <- lake_boundary_as_lines(lake_sf)
  angle_res <- lakefetch_options()$angle_resolution_deg
  angles <- seq(0, 360 - angle_res, by = angle_res)

  fetch_dists <- lakefetch:::get_highres_fetch(site_sf, lake_boundary, lake_sf, angles)

  lakefetch_options(buffer_distance_m = old_buffer)

  list(
    angles = angles,
    fetch = fetch_dists,
    mean = mean(fetch_dists),
    max = max(fetch_dists),
    min = min(fetch_dists)
  )
}
