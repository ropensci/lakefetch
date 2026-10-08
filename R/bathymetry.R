# ==============================================================================
# Lake Depth and Bathymetry Functions
# ==============================================================================
# Functions to estimate lake depth using empirical relationships.
# Used for wave orbital velocity calculations.
# ==============================================================================

#' Get Lake Depth Estimates
#'
#' Retrieves or estimates lake depth for wave calculations. Uses user-provided
#' depth if available, otherwise estimates from lake surface area using
#' empirical relationships.
#'
#' @param lake_polygon sf polygon of the lake
#' @param site_coords Coordinates of the sample site (optional, for future
#'   bathymetry grid support)
#' @param user_depth User-provided depth in meters (highest priority)
#' @param method Method for depth estimation: "auto" or "empirical"
#'
#' @return A list with elements:
#'   \item{depth_mean}{Estimated mean depth in meters}
#'   \item{depth_max}{Estimated maximum depth in meters (if available)}
#'   \item{source}{Source of the estimate ("user" or "empirical")}
#'   \item{confidence}{Confidence level ("high", "medium", "low")}
#'
#' @details
#' Depth estimation methods:
#' \enumerate{
#'   \item User-provided: Direct input, highest confidence
#'   \item Empirical: Estimated from lake surface area using published relationships
#' }
#'
#' The empirical method uses the relationship from Cael et al. (2017):
#' mean_depth ~ 10.3 * area_km2^0.25
#'
#' @examples
#' data(example_lake)
#'
#' # With user-provided depth
#' depth <- get_lake_depth(example_lake, user_depth = 8.5)
#'
#' # Estimate from lake area
#' depth <- get_lake_depth(example_lake)
#'
#' @references
#' Messager, M.L., Lehner, B., Grill, G., Nedeva, I., Schmitt, O. (2016):
#' Estimating the volume and age of water stored in global lakes using a
#' geo-statistical approach. Nature Communications, 7: 13603.
#'
#' Cael, B.B., Heathcote, A.J., Seekell, D.A. (2017): The volume and mean
#' depth of Earth's lakes. Geophysical Research Letters, 44: 209-218.
#'
#' @export
get_lake_depth <- function(lake_polygon, site_coords = NULL, user_depth = NULL,
                           method = "auto") {


  # If user provided depth, use it directly

if (!is.null(user_depth) && !is.na(user_depth) && user_depth > 0) {
    return(list(
      depth_mean = user_depth,
      depth_max = NA_real_,
      source = "user",
      confidence = "high"
    ))
  }

  # Calculate lake area for empirical estimation
  lake_area_m2 <- as.numeric(sf::st_area(sf::st_union(lake_polygon)))
  lake_area_km2 <- lake_area_m2 / 1e6

  # Use empirical estimation from lake area
  return(estimate_depth_empirical(lake_area_km2))
}



#' Estimate Lake Depth from Surface Area
#'
#' Uses empirical relationships between lake surface area and depth.
#'
#' @param lake_area_km2 Lake surface area in square kilometers
#'
#' @return List with depth estimates
#'
#' @details
#' Uses the global empirical relationship from Cael et al. (2017):
#' mean_depth (m) = 10.3 * area (km2)^0.25
#'
#' Maximum depth is estimated as approximately 2.5-3x mean depth based on
#' typical lake morphometry.
#'
#' @noRd
estimate_depth_empirical <- function(lake_area_km2) {

  if (is.na(lake_area_km2) || lake_area_km2 <= 0) {
    return(list(
      depth_mean = NA_real_,
      depth_max = NA_real_,
      source = "empirical",
      confidence = "none"
    ))
  }

  # Cael et al. (2017) global relationship
  # Mean depth (m) = 10.3 * Area (km2)^0.25
  depth_mean <- 10.3 * (lake_area_km2 ^ 0.25)

  # Max depth typically 2-3x mean depth
  # Using 2.5 as middle estimate
  depth_max <- depth_mean * 2.5

  # Cap estimates at reasonable values
  depth_mean <- min(depth_mean, 200)  # Few lakes >200m mean depth
  depth_max <- min(depth_max, 500)    # Few lakes >500m max depth

  message("  Lake depth estimated from area (", round(lake_area_km2, 2),
          " km2): mean ~ ", round(depth_mean, 1), "m, max ~ ",
          round(depth_max, 1), "m")

  return(list(
    depth_mean = depth_mean,
    depth_max = depth_max,
    source = "empirical",
    confidence = "low"
  ))
}


#' Add Depth Information to Fetch Results
#'
#' Looks up or estimates depth for each lake in the fetch results and
#' adds depth columns.
#'
#' @param fetch_results sf object with fetch results
#' @param lakes sf object with lake polygons
#' @param user_depths Named vector of user-provided depths (names = lake IDs)
#'
#' @return fetch_results with added depth columns
#'
#' @examplesIf interactive()
#' data(adirondack_sites)
#' sites <- load_sites(adirondack_sites)
#' lake <- get_lake_boundary(sites)
#' results <- fetch_calculate(sites, lake)
#'
#' # Add depth estimates
#' results$results <- add_lake_depth(results$results, results$lakes)
#'
#' # Or provide known depths using an actual lake_osm_id from results
#' lake_id <- results$lakes$osm_id[1]
#' depths <- setNames(15.5, lake_id)
#' results$results <- add_lake_depth(results$results, results$lakes, user_depths = depths)
#'
#' @export
add_lake_depth <- function(fetch_results, lakes, user_depths = NULL) {

  # Get unique lakes
  lake_ids <- unique(fetch_results$lake_osm_id)
  lake_ids <- lake_ids[!is.na(lake_ids)]

  # Look up depth for each lake
  depth_lookup <- list()

  for (lake_id in lake_ids) {
    lake_poly <- lakes[lakes$osm_id == lake_id, ]

    # Check for user-provided depth
    user_d <- if (!is.null(user_depths) && lake_id %in% names(user_depths)) {
      user_depths[[lake_id]]
    } else {
      NULL
    }

    depth_info <- get_lake_depth(lake_poly, user_depth = user_d)
    depth_lookup[[lake_id]] <- depth_info
  }

  # Add depth columns to results
  fetch_results$depth_mean_m <- vapply(fetch_results$lake_osm_id, function(id) {
    if (is.na(id) || is.null(depth_lookup[[id]])) NA_real_
    else depth_lookup[[id]]$depth_mean
  }, numeric(1))

  fetch_results$depth_max_m <- vapply(fetch_results$lake_osm_id, function(id) {
    if (is.na(id) || is.null(depth_lookup[[id]])) NA_real_
    else depth_lookup[[id]]$depth_max
  }, numeric(1))

  fetch_results$depth_source <- vapply(fetch_results$lake_osm_id, function(id) {
    if (is.na(id) || is.null(depth_lookup[[id]])) NA_character_
    else depth_lookup[[id]]$source
  }, character(1))

  return(fetch_results)
}


