# lakefetch

<!-- badges: start -->
[![Status at rOpenSci Software Peer Review](https://badges.ropensci.org/762_status.svg)](https://github.com/ropensci/software-review/issues/762)
[![R-CMD-check](https://github.com/ropensci/lakefetch/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/ropensci/lakefetch/actions/workflows/R-CMD-check.yaml)
[![CRAN status](https://www.r-pkg.org/badges/version/lakefetch)](https://CRAN.R-project.org/package=lakefetch)
[![Project Status: Active](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
[![codecov](https://codecov.io/gh/ropensci/lakefetch/branch/main/graph/badge.svg)](https://app.codecov.io/gh/ropensci/lakefetch)
<!-- badges: end -->

Calculate fetch (open water distance) and wave exposure metrics for freshwater lake sampling sites.

## Motivation

**Fetch** is the unobstructed distance that wind can travel across open water. It is a key physical driver in lakes because it controls wave height, wave energy, and shoreline exposure. Fetch influences sediment resuspension, nutrient cycling, littoral habitat structure, and the distribution of aquatic organisms. Despite its importance, calculating fetch for inland lakes has required either manual GIS work or tools designed for coastal/marine environments that do not handle the irregular shorelines and small spatial scales typical of lakes.

**lakefetch** fills this gap by providing an end-to-end R workflow for freshwater fetch analysis: it downloads lake boundary polygons from OpenStreetMap, calculates directional fetch via a ray-casting algorithm, classifies sites by wave exposure, and optionally integrates weather data to estimate cumulative wave energy. It can also pull hydrological context (outlets, inlets, watershed area, connectivity) from the US National Hydrography Dataset (NHD) via the [nhdplusTools](https://cran.r-project.org/package=nhdplusTools) package. The package is designed for batch processing across many lakes and sites, making it practical for large-scale ecological and limnological studies.

Other R packages calculate fetch for related use cases. [fetchR](https://cran.r-project.org/package=fetchR), [waver](https://cran.r-project.org/package=waver), and [windfetch](https://github.com/blasee/windfetch) target coastal/marine environments and require the user to supply coastline polygons; [lakemorpho](https://cran.r-project.org/package=lakemorpho) computes fetch as one of many morphometric parameters on a single user-supplied lake polygon. lakefetch is distinguished by its lake focus and automatic OpenStreetMap-based boundary download for batch workflows. See [Similar Packages](#similar-packages) below for a detailed comparison.

### Spatial Domain

lakefetch operates on **two-dimensional geographic (curvilinear) coordinates**. Input data are expected as latitude/longitude pairs (WGS84, EPSG:4326) or as `sf` objects in any CRS. Internally, all spatial calculations are performed in a **projected Cartesian coordinate system** (UTM zone automatically determined from site locations) to ensure accurate distance measurements in meters. The package uses the [`sf`](https://r-spatial.github.io/sf/) package for all spatial operations and is compliant with PROJ6+ and WKT2 coordinate reference system representations.

## Installation

Install from CRAN:

```r
install.packages("lakefetch")
```

Or install the development version from GitHub:

```r
# install.packages("remotes")
remotes::install_github("ropensci/lakefetch")
```

## Quick Start

```r
library(lakefetch)

# Load sampling sites from a CSV with latitude/longitude columns
sites <- load_sites(system.file("extdata", "sample_sites.csv", package = "lakefetch"))

# Download lake boundaries from OpenStreetMap
lake <- get_lake_boundary(sites)

# Calculate fetch for all sites
results <- fetch_calculate(sites, lake)

# View results - an sf object with fetch values and exposure categories
results$results
```

## How It Works

1. **Load sites** from a CSV or data frame containing latitude, longitude, and optional site names.
2. **Get lake boundaries** automatically from OpenStreetMap, or load your own shapefiles/geopackages.
3. **Calculate fetch** by casting rays from each site outward at regular angular intervals (default 5 degrees, configurable via `lakefetch_options(angle_resolution_deg = 10)`) and measuring the distance to the nearest shoreline in each direction.
4. **Classify exposure** based on effective fetch: Sheltered (< 2.5 km), Moderate (2.5--5 km), or Exposed (> 5 km). Thresholds are configurable.

For each site, lakefetch returns:

| Metric | Description |
|--------|-------------|
| `fetch_0` ... `fetch_355` | Distance to shore (m) at each compass bearing |
| `fetch_mean` | Mean of all directional fetches |
| `fetch_max` | Maximum directional fetch (longest open water distance) |
| `fetch_effective` | Effective fetch (default: mean of 3 highest directional values) |
| `exposure_class` | Categorical exposure classification |

Three effective fetch methods are available: `"top3"` (default), `"max"`, and `"cosine"` (the SPM/CERC cosine-weighted method from the Shore Protection Manual).

## Features

- **Automatic boundary download**: Downloads lake polygons from OpenStreetMap, with spatial clustering and multi-server fallback for large or geographically spread datasets.
- **Ray-casting fetch**: Measures distance to shore at configurable angular resolution (default 5 degrees, 72 directions).
- **Multi-lake batch processing**: A single CSV can contain sites on many different lakes; boundaries are downloaded and fetch is calculated for each lake automatically.
- **Exposure classification**: Sites are categorized as Sheltered, Moderate, or Exposed based on configurable thresholds.
- **Wave energy estimation**: Empirical wave hindcasting using the Sverdrup-Munk-Bretschneider (SMB) equations with depth-attenuated orbital velocity, based on directional fetch and historical wind data.
- **Depth estimation**: Empirical mean and maximum depth from lake surface area using the global scaling relationship of Cael et al. (2017).
- **NHD integration** (optional, US lakes): Identifies outlets, inlets, stream order, watershed area, and connectivity classification using the National Hydrography Dataset via the nhdplusTools package.
- **Weather integration** (optional): Retrieves historical hourly weather data from the Open-Meteo API and computes windowed summary statistics (wind speed, direction, temperature, precipitation, wave energy) for user-specified time periods before each sampling event.
- **Visualization**: Static plots (fetch maps, bar charts, directional rose diagrams) and two interactive Shiny applications for point-and-click exploration and CSV upload workflows.

## Example Workflow

```r
library(lakefetch)

# Built-in example data
data("adirondack_sites")

# Download lake boundaries from OpenStreetMap
lake <- get_lake_boundary(adirondack_sites)

# Calculate fetch with depth estimation
results <- fetch_calculate(sites = adirondack_sites, lake = lake)

# Visualize
plot_fetch_map(results)      # Map colored by exposure class
plot_fetch_bars(results)     # Bar chart of effective fetch
plot_fetch_rose(results, 1)  # Rose diagram for the first site

# Interactive Shiny app
fetch_app(results)
```

## Key Functions

| Function | Description |
|----------|-------------|
| `load_sites()` | Load and validate sampling sites from CSV or data frame |
| `get_lake_boundary()` | Download lake polygons from OSM or load from local file |
| `fetch_calculate()` | Calculate directional fetch, effective fetch, and exposure class |
| `add_lake_depth()` | Estimate lake depth from surface area (Cael et al. 2017) |
| `add_lake_context()` | Add NHD hydrological context (US lakes; requires nhdplusTools) |
| `add_weather_context()` | Add historical weather and wave energy metrics (requires jsonlite) |
| `plot_fetch_map()` | Map of sites colored by exposure category |
| `plot_fetch_bars()` | Bar chart of effective fetch by site |
| `plot_fetch_rose()` | Directional fetch rose diagram for a single site |
| `fetch_app()` | Interactive Shiny app for exploring fetch results |
| `fetch_app_upload()` | Standalone Shiny app with CSV upload (no coding required) |

## Using Local Boundary Files

If you have your own lake boundary as a shapefile or geopackage:

```r
lake <- get_lake_boundary(sites, file = "my_lake_boundary.gpkg")
results <- fetch_calculate(sites, lake)
```

## Known Limitations

- **Small water bodies not in OpenStreetMap**: Very small lakes and ponds (e.g., prairie ponds, stock tanks) may not be mapped in OpenStreetMap. If no boundary is found, `get_lake_boundary()` will error with a message suggesting that you supply your own boundary file via `get_lake_boundary(sites, file = "your_boundary.gpkg")`. Boundary files can be shapefiles, geopackages, or any format readable by `sf::st_read()`.

- **OpenStreetMap data quality**: Lake boundaries in OSM vary in accuracy and completeness by region. For high-precision studies, consider using authoritative hydrography datasets (e.g., NHD for the US) as boundary sources.

## Similar Packages

Several R packages calculate fetch or wave exposure, but they target different use cases:

| Package | Focus | Key Differences |
|---------|-------|-----------------|
| [fetchR](https://cran.r-project.org/package=fetchR) | General fetch calculation | Requires user-supplied coastline polygons; no automatic boundary download; single-polygon workflow; no batch multi-lake processing. |
| [waver](https://cran.r-project.org/package=waver) | Wave energy for coastal sites | Designed for marine/coastal environments; uses a different fetch algorithm (wedge-based); no lake-specific features like NHD integration or depth estimation. |
| [windfetch](https://github.com/blasee/windfetch) | Wind fetch for coastal environments | Successor to fetchR; coastal focus; no OpenStreetMap integration or multi-lake batch processing. |
| [lakemorpho](https://cran.r-project.org/package=lakemorpho) | Lake morphometry metrics | Calculates fetch as one of many morphometric parameters (shoreline development, max length/width, volume); single-lake focus; requires user-supplied polygon; no wave energy, weather integration, or exposure classification. |

**lakefetch** is distinguished by its focus on freshwater lakes, including: automatic lake boundary download from OpenStreetMap with spatial clustering for large datasets, batch processing of sites across multiple lakes, empirical depth estimation, NHD hydrological context for US lakes, and historical weather/wave energy integration. These features are designed for the common ecological workflow of analyzing field sampling sites across many lakes.

## Example Datasets

The package includes three built-in datasets:

- `adirondack_sites`: Sampling sites from lakes in the Adirondack region of New York
- `wisconsin_lakes`: Sampling sites on three Wisconsin lakes (Mendota, Monona, Geneva)
- `example_lake`: A single lake polygon for testing and examples

## References

- Shore Protection Manual (1984). U.S. Army Corps of Engineers, Coastal Engineering Research Center. 4th Edition.
- Sverdrup, H.U. & Munk, W.H. (1947). Wind, sea, and swell: Theory of relations for forecasting. U.S. Navy Hydrographic Office, Pub. No. 601.
- Cael, B.B., Heathcote, A.J., & Seekell, D.A. (2017). The volume and mean depth of Earth's lakes. *Geophysical Research Letters*, 44(1), 209--218.

## Citation

If you use lakefetch in your research, please cite:

Farrell J (2026). *lakefetch: Calculate Fetch and Wave Exposure for Lake Sampling Points*. R package version 0.1.3, <https://github.com/ropensci/lakefetch>.

Or in BibTeX format:

```bibtex
@Manual{,
  title = {lakefetch: Calculate Fetch and Wave Exposure for Lake Sampling Points},
  author = {Jeremy Lynch Farrell},
  year = {2026},
  note = {R package version 0.1.3},
  url = {https://github.com/ropensci/lakefetch},
}
```

To generate the citation from R:

```r
citation("lakefetch")
```

## AI Assistance Disclosure

This package was developed collaboratively with [Claude](https://claude.ai) (Anthropic). Claude contributed to all aspects of the codebase, including package architecture, function implementation, test writing, documentation, and CI/CD configuration. All code was reviewed and directed by the package author. Claude is credited as co-author on all commits via `Co-Authored-By` tags.

## Contributing

Contributions are welcome. Please see [CONTRIBUTING.md](https://github.com/ropensci/lakefetch/blob/main/CONTRIBUTING.md) for guidelines on reporting bugs, suggesting features, and submitting pull requests.

## Code of Conduct

Please note that the lakefetch project is released with a [Contributor Code of Conduct](https://ropensci.org/code-of-conduct/). By contributing to this project, you agree to abide by its terms.

## License

MIT License
