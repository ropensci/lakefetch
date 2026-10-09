# ==============================================================================
# Package Initialization
# ==============================================================================

#' @importFrom sf st_as_sf st_transform st_bbox st_crs st_geometry st_sfc
#' @importFrom sf st_point st_linestring st_polygon st_sf st_drop_geometry
#' @importFrom sf st_intersects st_intersection st_union st_buffer st_area
#' @importFrom sf st_distance st_nearest_points st_nearest_feature st_join
#' @importFrom sf st_is_valid st_make_valid st_cast st_boundary st_centroid
#' @importFrom sf st_coordinates st_geometry_type st_is_empty st_read sf_use_s2
#' @importFrom osmdata opq add_osm_feature osmdata_sf
#' @importFrom ggplot2 ggplot aes geom_sf geom_bar geom_hline scale_color_manual
#' @importFrom ggplot2 scale_fill_manual coord_sf labs theme_minimal theme
#' @importFrom ggplot2 element_text
#' @importFrom stats setNames ave
#' @importFrom utils read.csv txtProgressBar setTxtProgressBar
#' @importFrom graphics par plot lines polygon text title
#' @importFrom grDevices rgb png dev.off
NULL

#' lakefetch: Calculate Fetch and Wave Exposure for Lake Sampling Points
#'
#' The lakefetch package provides tools for calculating fetch (open water
#' distance) and wave exposure metrics for lake sampling points. It downloads
#' lake boundaries from OpenStreetMap, calculates directional fetch using
#' ray-casting, and optionally integrates with NHD for hydrological context.
#'
#' @section Main Functions:
#' \describe{
#'   \item{\code{\link{fetch_calculate}}}{Main entry point for fetch calculation}
#'   \item{\code{\link{load_sites}}}{Load and validate site data}
#'   \item{\code{\link{get_lake_boundary}}}{Get lake boundary from OSM or file}
#'   \item{\code{\link{add_lake_context}}}{Add NHD hydrological context}
#'   \item{\code{\link{fetch_app}}}{Launch interactive Shiny app}
#' }
#'
#' @section Visualization:
#' \describe{
#'   \item{\code{\link{plot_fetch_map}}}{Map of sites colored by exposure}
#'   \item{\code{\link{plot_fetch_bars}}}{Bar chart of effective fetch}
#'   \item{\code{\link{plot_fetch_rose}}}{Rose diagram for single site}
#'   \item{\code{\link{create_ray_geometries}}}{Create ray lines for mapping}
#' }
#'
#' @section Configuration:
#' \describe{
#'   \item{\code{\link{lakefetch_options}}}{Get/set package options}
#'   \item{\code{\link{lakefetch_reset_options}}}{Reset options to defaults}
#' }
#'
#' @docType package
#' @name lakefetch-package
#' @aliases lakefetch
"_PACKAGE"

.onLoad <- function(libname, pkgname) {
  # Set default options if not already set
  op <- options()
  op_lakefetch <- list(
    lakefetch.buffer_distance_m = 10,
    lakefetch.angle_resolution_deg = 5,
    lakefetch.max_fetch_m = 50000,
    lakefetch.validation_buffer_m = 10,
    lakefetch.default_wind_speed_ms = 10,
    lakefetch.default_depth_m = 10,
    lakefetch.gps_tolerance_m = 100,
    lakefetch.fetch_method = "top3",
    lakefetch.exposure_sheltered_m = 2500,
    lakefetch.exposure_exposed_m = 5000,
    lakefetch.exposure_relative_sheltered = 0.25,
    lakefetch.exposure_relative_exposed = 0.50,
    lakefetch.use_parallel = TRUE,
    lakefetch.use_nhd = TRUE
  )

  toset <- !(names(op_lakefetch) %in% names(op))
  if (any(toset)) options(op_lakefetch[toset])

  invisible()
}

.onAttach <- function(libname, pkgname) {
  # Check for optional dependencies
  nhd_msg <- if (requireNamespace("hydrogeofetch", quietly = TRUE)) {
    "hydrogeofetch available - NHD integration enabled"
  } else {
    "Install 'hydrogeofetch' for outlet/inlet detection"
  }

  packageStartupMessage("lakefetch ", utils::packageVersion("lakefetch"), " loaded")
  packageStartupMessage("  ", nhd_msg)
}

# Suppress R CMD check notes about undefined global variables
# These are column names used in ggplot2 aes() and dplyr operations,
# plus shiny functions used in reactive contexts
utils::globalVariables(c(
  "exposure_category", "fetch_effective", "Site", "Distance", "Angle",
  "lake_name", "osm_id", "area_km2", ".data", "req"
))
