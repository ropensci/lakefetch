# ==============================================================================
# Dataset Documentation
# ==============================================================================
# Documentation for example datasets included with the lakefetch package.
# ==============================================================================

#' Adirondack Lake Sampling Sites
#'
#' A dataset containing example lake sampling sites from the Adirondack region
#' of New York State. These synthetic but realistic coordinates demonstrate
#' typical multi-lake sampling scenarios.
#'
#' @name adirondack_sites
#' @docType data
#' @format A data frame with 12 rows and 5 variables:
#' \describe{
#'   \item{Site}{Unique site identifier}
#'   \item{lake.name}{Name of the lake}
#'   \item{latitude}{Latitude in decimal degrees (WGS84)}
#'   \item{longitude}{Longitude in decimal degrees (WGS84)}
#'   \item{datetime}{Date and time of sampling (POSIXct)}
#' }
#'
#' @details
#' The dataset includes sites from four Adirondack lakes:
#' \itemize{
#'   \item Blue Mountain Lake (3 sites)
#'   \item Raquette Lake (4 sites)
#'   \item Long Lake (2 sites)
#'   \item Tupper Lake (3 sites)
#' }
#'
#' @examples
#' # Load the dataset
#' data(adirondack_sites)
#'
#' # View structure
#' str(adirondack_sites)
#'
#' @examplesIf interactive()
#' # Use with lakefetch (requires internet connection)
#' sites <- load_sites(adirondack_sites)
#' lake_data <- get_lake_boundary(sites)
#' results <- fetch_calculate(sites, lake_data)
#'
#' @source Synthetic data for demonstration purposes
"adirondack_sites"


#' Blue Mountain Lake Polygon (Example Lake)
#'
#' The OpenStreetMap boundary polygon for Blue Mountain Lake in Hamilton
#' County, New York. Bundled with the package so that fetch examples can
#' run offline (no internet or Overpass API call required) and pkgdown
#' pages can render plot output. The coordinates match the sites in
#' \code{system.file("extdata", "sample_sites.csv", package = "lakefetch")},
#' so the two datasets can be used together end-to-end.
#'
#' @name example_lake
#' @docType data
#' @format An sf object with 1 row and 3 fields plus geometry:
#' \describe{
#'   \item{osm_id}{OSM relation identifier}
#'   \item{name}{Lake name ("Blue Mountain Lake")}
#'   \item{area_km2}{Surface area in square kilometers}
#'   \item{geometry}{MULTIPOLYGON geometry in UTM Zone 18N (EPSG:32618)}
#' }
#'
#' @examples
#' data(example_lake)
#' print(example_lake)
#'
#' # Plot the lake
#' library(ggplot2)
#' ggplot(example_lake) + geom_sf()
#'
#' # Load matching sample sites (they lie inside this polygon) and
#' # compute fetch end-to-end without touching OSM. First convert
#' # example_lake into the multi-lake list format that fetch_calculate()
#' # expects:
#' sites <- load_sites(system.file("extdata", "sample_sites.csv",
#'                                  package = "lakefetch"))
#'
#' @source Downloaded from OpenStreetMap
#'   \url{https://www.openstreetmap.org/}. See
#'   \code{data-raw/create_example_data.R} for the exact query.
"example_lake"


#' Wisconsin Lake Sampling Sites
#'
#' A dataset containing example sampling sites from well-known Wisconsin lakes.
#' These coordinates are useful for testing with real lake boundaries from
#' OpenStreetMap.
#'
#' @name wisconsin_lakes
#' @docType data
#' @format A data frame with 8 rows and 4 variables:
#' \describe{
#'   \item{Site}{Unique site identifier}
#'   \item{lake.name}{Name of the lake}
#'   \item{latitude}{Latitude in decimal degrees (WGS84)}
#'   \item{longitude}{Longitude in decimal degrees (WGS84)}
#' }
#'
#' @details
#' The dataset includes sites from three Wisconsin lakes:
#' \itemize{
#'   \item Lake Mendota (3 sites) - Madison's largest lake, well-studied
#'   \item Lake Monona (2 sites) - Connected to Mendota via Yahara River
#'   \item Geneva Lake (3 sites) - Popular recreational lake in SE Wisconsin
#' }
#'
#' @examples
#' # Load the dataset
#' data(wisconsin_lakes)
#'
#' # View the data
#' head(wisconsin_lakes)
#'
#' @examplesIf interactive()
#' # Use with lakefetch (requires internet connection)
#' sites <- load_sites(wisconsin_lakes)
#' lake_data <- get_lake_boundary(sites)
#' results <- fetch_calculate(sites, lake_data)
#'
#' @source Synthetic data based on real lake locations
"wisconsin_lakes"
