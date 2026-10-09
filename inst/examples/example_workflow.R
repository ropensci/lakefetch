# ==============================================================================
# lakefetch Example Workflow
# ==============================================================================
# This example demonstrates the complete workflow for calculating fetch
# and wave exposure for lake sampling sites.

library(lakefetch)

# ------------------------------------------------------------------------------
# 1. Load Site Data
# ------------------------------------------------------------------------------

# Option A: Load from CSV file
sites <- load_sites("path/to/your_sites.csv")

# Option B: Create manually
sites <- data.frame(
  Site = c("North Bay", "South Shore", "Deep Point", "West Cove"),
  latitude = c(42.1234, 42.1180, 42.1150, 42.1200),
  longitude = c(-79.4500, -79.4400, -79.4550, -79.4650)
)
sites <- load_sites(sites)

# ------------------------------------------------------------------------------
# 2. Get Lake Boundary
# ------------------------------------------------------------------------------

# Option A: Download from OpenStreetMap (automatic)
lake <- get_lake_boundary(sites)

# Option B: Load from local shapefile
# lake <- get_lake_boundary(sites, file = "path/to/lake_boundary.shp")

# ------------------------------------------------------------------------------
# 3. Calculate Fetch
# ------------------------------------------------------------------------------

# Calculate fetch with default options
results <- fetch_calculate(sites, lake)

# Access the results
head(results$results)  # sf object with all fetch data
results$lakes          # lake polygon(s)
results$angles         # angles used for ray-casting

# Key columns in results:
# - fetch_mean: Mean fetch across all directions
# - fetch_max: Maximum fetch (longest open water distance)
# - fetch_effective: Mean of top 3 fetch values
# - orbital_effective: Estimated wave orbital velocity
# - exposure_category: "Sheltered", "Moderate", or "Exposed"

# ------------------------------------------------------------------------------
# 4. Customize Options (Optional)
# ------------------------------------------------------------------------------

# View current options
lakefetch_options()

# Change options before calculation
lakefetch_options(
  buffer_distance_m = 20,       # Larger GPS buffer
  angle_resolution_deg = 10,   # Coarser resolution (faster)
  use_parallel = FALSE         # Disable parallel processing
)

# Reset to defaults
lakefetch_reset_options()

# ------------------------------------------------------------------------------
# 5. Visualize Results
# ------------------------------------------------------------------------------

# Static map of sites
plot_fetch_map(results)

# Bar chart of effective fetch
plot_fetch_bars(results)
# Save plot
ggsave("fetch_barchart.png", width = 12, height = 6)

# Rose diagram for a specific site
plot_fetch_rose(results, "North Bay")

# Create ray geometries for custom visualization
rays <- create_ray_geometries(results)

# Custom map with rays
library(ggplot2)
ggplot() +
  geom_sf(data = results$lakes, fill = "lightblue", alpha = 0.5) +
  geom_sf(data = rays[rays$Site == "North Bay", ],
          aes(color = Distance), linewidth = 0.5) +
  geom_sf(data = results$results, color = "red", size = 3) +
  scale_color_viridis_c(name = "Fetch (m)") +
  theme_minimal()

# ------------------------------------------------------------------------------
# 6. Interactive App
# ------------------------------------------------------------------------------

# Launch Shiny app for interactive exploration
# (Click markers to view fetch rays)
fetch_app(results)

# ------------------------------------------------------------------------------
# 7. Export Results
# ------------------------------------------------------------------------------

# Export to CSV (without geometry)
write.csv(sf::st_drop_geometry(results$results),
          "fetch_results.csv",
          row.names = FALSE)

# Export to GeoPackage (with geometry)
sf::st_write(results$results, "fetch_results.gpkg", append = FALSE)

# Export rays for GIS
sf::st_write(rays, "fetch_rays.gpkg", append = FALSE)

# ------------------------------------------------------------------------------
# 8. Working with Multiple Lakes
# ------------------------------------------------------------------------------

# If your sites span multiple lakes, lakefetch handles this automatically:
# - Sites are assigned to their containing lake polygon
# - Fetch is calculated separately for each lake
# - Results include lake_osm_id and lake_name columns

# Check which lakes were processed
table(results$results$lake_name)

# Filter results by lake
lake1_results <- results$results[results$results$lake_name == "Lake Name", ]

# ------------------------------------------------------------------------------
# 9. NHD Integration (if hydrogeofetch installed)
# ------------------------------------------------------------------------------

# Additional context columns when hydrogeofetch is available:
# - nhd_permanent_id: NHD identifier
# - nhd_gnis_name: Official lake name
# - outlet_dist_m: Distance to lake outlet
# - outlet_bearing: Direction to outlet (N, NE, E, etc.)
# - inlet_nearest_dist_m: Distance to nearest inlet
# - inlet_count: Number of inlets
# - connectivity_class: "Headwater", "Drainage", "Terminal", or "Isolated"
# - outlet_stream_order: Strahler stream order at outlet
# - watershed_area_ha: Watershed area in hectares

# Check if NHD context was added
if ("connectivity_class" %in% names(results$results)) {
  table(results$results$connectivity_class)
}
