# ==============================================================================
# Create Example Datasets for lakefetch Package
# ==============================================================================
# This script creates example datasets included with the package.
# Run this script to regenerate the data files in data/
#
# Usage: source("data-raw/create_example_data.R")
# ==============================================================================

library(sf)

# ==============================================================================
# Dataset 1: adirondack_sites
# ==============================================================================
# Example lake sampling sites from the Adirondack region of New York State.
# These are synthetic but realistic coordinates for demonstration purposes.

adirondack_sites <- data.frame(
  Site = c(
    "Blue_Mountain_1", "Blue_Mountain_2", "Blue_Mountain_3",
    "Raquette_1", "Raquette_2", "Raquette_3", "Raquette_4",
    "Long_Lake_1", "Long_Lake_2",
    "Tupper_1", "Tupper_2", "Tupper_3"
  ),
  lake.name = c(
    rep("Blue Mountain Lake", 3),
    rep("Raquette Lake", 4),
    rep("Long Lake", 2),
    rep("Tupper Lake", 3)
  ),
  latitude = c(
    # Blue Mountain Lake - verified in-water (OSM relation 2202972)
    43.8645, 43.8674, 43.8648,
    # Raquette Lake - verified in-water (OSM relation; lake area ~22 km2)
    43.8613, 43.8196, 43.8474, 43.8210,
    # Long Lake - verified in-water (OSM relation 1871997)
    43.9846, 43.9756,
    # Tupper Lake - verified in-water (OSM relation; lake area ~21 km2)
    44.2288, 44.2121, 44.1818
  ),
  longitude = c(
    # Blue Mountain Lake (~74.45°W)
    -74.4404, -74.4542, -74.4489,
    # Raquette Lake (~74.63°W)
    -74.6540, -74.6443, -74.6387, -74.6090,
    # Long Lake (~74.42°W)
    -74.4128, -74.4189,
    # Tupper Lake (~74.50°W)
    -74.4675, -74.4824, -74.5022
  ),
  datetime = as.POSIXct(c(
    "2024-07-15 09:30:00", "2024-07-15 10:15:00", "2024-07-15 11:00:00",
    "2024-07-16 08:45:00", "2024-07-16 09:30:00", "2024-07-16 10:15:00", "2024-07-16 11:00:00",
    "2024-07-17 09:00:00", "2024-07-17 10:00:00",
    "2024-07-18 08:30:00", "2024-07-18 09:15:00", "2024-07-18 10:00:00"
  ), tz = "America/New_York"),
  stringsAsFactors = FALSE
)

cat("Created adirondack_sites:", nrow(adirondack_sites), "sites across",
    length(unique(adirondack_sites$lake.name)), "lakes\n")

# ==============================================================================
# Dataset 2: example_lake
# ==============================================================================
# The real OpenStreetMap polygon for Blue Mountain Lake (Hamilton County,
# NY). Bundling this lets @examples and the pkgdown site render plots
# without needing an internet connection to Overpass, and matches the
# location of inst/extdata/sample_sites.csv so the two can be used together.

message("Fetching Blue Mountain Lake polygon from OpenStreetMap...")
library(osmdata)

bml_bbox <- c(left = -74.47, bottom = 43.855,
              right = -74.42, top = 43.885)
bml_osm <- opq(bbox = bml_bbox, timeout = 120) |>
  add_osm_feature(key = "name", value = "Blue Mountain Lake",
                   value_exact = FALSE) |>
  osmdata_sf()

# The lake is stored as an OSM relation, so it appears in osm_multipolygons.
if (is.null(bml_osm$osm_multipolygons) ||
    nrow(bml_osm$osm_multipolygons) == 0) {
  stop("Could not fetch Blue Mountain Lake polygon from OSM. ",
       "Retry when Overpass is responsive.")
}

# Take the largest matching multipolygon in case OSM returns duplicates.
bml_wgs <- bml_osm$osm_multipolygons
bml_wgs <- bml_wgs[which.max(as.numeric(sf::st_area(bml_wgs))), ]

# Store in UTM 18N (matches sample_sites.csv location).
bml_utm <- sf::st_transform(bml_wgs, 32618)
bml_utm <- sf::st_make_valid(bml_utm)

example_lake <- sf::st_sf(
  osm_id = as.character(bml_utm$osm_id[[1]]),
  name = "Blue Mountain Lake",
  area_km2 = as.numeric(sf::st_area(bml_utm)) / 1e6,
  geometry = sf::st_geometry(bml_utm)
)

cat("Created example_lake: Blue Mountain Lake,",
    round(example_lake$area_km2, 2), "km^2\n")

# ==============================================================================
# Dataset 3: wisconsin_lakes
# ==============================================================================
# A small dataset of well-known Wisconsin lakes for testing.

wisconsin_lakes <- data.frame(
  Site = c(
    "Mendota_N", "Mendota_S", "Mendota_Deep",
    "Monona_1", "Monona_2",
    "Geneva_E", "Geneva_W", "Geneva_Center"
  ),
  lake.name = c(
    rep("Lake Mendota", 3),
    rep("Lake Monona", 2),
    rep("Geneva Lake", 3)
  ),
  latitude = c(
    # Lake Mendota
    43.1125, 43.0756, 43.0995,
    # Lake Monona
    43.0634, 43.0589,
    # Geneva Lake (all three verified inside the OSM polygon; see notes below)
    42.584896, 42.573359, 42.566069
  ),
  longitude = c(
    # Lake Mendota
    -89.4234, -89.4012, -89.4045,
    # Lake Monona
    -89.3612, -89.3789,
    # Geneva Lake
    #   Geneva_E:      northeast area of the lake
    #   Geneva_W:      western portion
    #   Geneva_Center: south-central portion
    # The lake's actual OSM bbox is 42.5455-42.5914 N, -88.5727 to -88.4329 W;
    # earlier coordinates in this dataset were on land near the shore.
    -88.442054, -88.535460, -88.501107
  ),
  stringsAsFactors = FALSE
)

cat("Created wisconsin_lakes:", nrow(wisconsin_lakes), "sites across",
    length(unique(wisconsin_lakes$lake.name)), "lakes\n")

# ==============================================================================
# Save datasets
# ==============================================================================

# Create data directory if needed
if (!dir.exists("data")) {
  dir.create("data")
}

# Save as .rda files (R data format)
save(adirondack_sites, file = "data/adirondack_sites.rda", compress = "xz")
save(example_lake, file = "data/example_lake.rda", compress = "xz")
save(wisconsin_lakes, file = "data/wisconsin_lakes.rda", compress = "xz")

cat("\nDatasets saved to data/:\n")
cat("  - adirondack_sites.rda\n")
cat("  - example_lake.rda\n")
cat("  - wisconsin_lakes.rda\n")

cat("\nTo use in package, add documentation in R/data.R\n")
