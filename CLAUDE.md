# lakefetch - Development Status

Last updated: 2026-02-01

## Package Overview

**lakefetch** calculates fetch (open water distance) and wave exposure metrics for freshwater lake sampling sites. It downloads lake boundaries from OpenStreetMap, calculates directional fetch using ray-casting, and provides wave exposure classifications.

**GitHub:** https://github.com/ropensci/lakefetch

## Current Status: AWAITING COLLEAGUE FEEDBACK

All checks pass locally (0 errors, 0 warnings, 1 note). Waiting on feedback from Lucas and Fenly before re-running win-builder/R-hub and submitting to CRAN.

### Recent Changes (2026-02-09, v0.1.1)

1. **Custom column names** (`R/data_loading.R`): `load_sites()` now accepts `lat_col`, `lon_col`, `site_col`, `lake_col` arguments to explicitly specify column names. Useful for datasets like the GLEON Info_Data.csv where the lake name column is just "name".

2. **Progress bars** (`R/fetch_core.R`): Added `txtProgressBar` to fetch calculation and site buffering loops. Only shown in interactive sessions. No new dependencies (uses base R `utils`).

3. **Shiny app performance** (`R/shiny_app.R`): Rose plots and ray geometries are now generated on demand (on marker click) instead of pre-rendered for all sites at startup. Marker clustering enabled for >30 sites. This fixes the crash when using `fetch_app()` with large datasets like the GLEON 433-site dataset.

4. **Invalid coordinate filtering** (`R/data_loading.R`): Added (0,0) coordinate detection and removal with warning.

5. **Optimized OSM downloads** (`R/lake_sources.R`): Refactored `download_lake_osm()` for geographically spread datasets like GLEON (429 sites worldwide). When site spread exceeds 0.5°, sites are grouped into spatial clusters (grid-based at 0.1° resolution via `cluster_sites()`) and each cluster gets a small bbox (~0.1°) with a single `natural=water` Overpass query. This replaces the old single-bbox approach that would cover the entire globe and timeout. Key features: server rotation across 3 Overpass endpoints, 1s rate limiting between queries, automatic retry on server errors, minimum area filter (0.0001 km²) to remove tiny water bodies. Helper functions extracted as top-level: `extract_osm_polys()`, `query_osm_by_name()`, `download_lake_osm_single()`. For small-spread sites (≤0.5°), the original single-bbox behavior is preserved.

### Previous Changes (2026-02-03)

1. **Fixed name-based lake matching** (`R/lake_sources.R`, Pass 3 of `assign_sites_to_lakes()`): Added a distance check (`tolerance_m * 5`, default 500m) so sites aren't silently assigned to a same-named lake far away. Sites that fail the proximity check are skipped with a warning. This fixes the Raquette Lake mismatch bug reported by colleague testing with Adirondack sites.

2. **Clarified Shiny app instructions** (`R/shiny_app.R`, line 496): Changed "Click on a lake" to "Click on a lake polygon to analyze a new point on that lake." per Kait's feedback that the original wording implied any lake on the map could be clicked.

### Next Steps

- Wait for feedback from Lucas and Fenly
- Incorporate any additional changes
- Re-run win-builder and R-hub checks (do NOT re-run until all feedback is in)
- Update `cran-comments.md` if needed
- Submit to CRAN

### Colleague Feedback Tracker

| Colleague | Status | Notes |
|-----------|--------|-------|
| Kait | Received | Wind rose overlay idea (not implementing — out of scope for this package). App click wording fixed. |
| Lucas | Pending | Kait forwarding to him |
| Fenly | Pending | Kait forwarding to them |

### Test Results

| Environment | Result |
|-------------|--------|
| Local Windows 11, R 4.4.1 | ✅ 0 errors, 0 warnings, 1 note |
| R-hub Ubuntu Linux (R-devel) | ✅ Pass |
| R-hub macOS ARM64 (R-devel) | ✅ Pass |
| R-hub Windows (R-devel) | ✅ Pass |
| Win-builder R-devel | ✅ 0 errors, 0 warnings, 2 notes |
| Win-builder R-release | ✅ Submitted |

### Expected NOTEs (acceptable for CRAN)

1. **New submission** with "misspelled" words (`Hydrography`, `NHD`, `OpenStreetMap`, `hydrological`) - these are correct technical terms
2. **`.github` directory** - contains GitHub Actions for CI/CD, standard practice

## Installation (for colleagues testing)

```r
# Install from GitHub
install.packages("remotes")
remotes::install_github("ropensci/lakefetch")

# Or with all optional dependencies
remotes::install_github("ropensci/lakefetch", dependencies = TRUE)
```

## Quick Start

```r
library(lakefetch)

# Option 1: Use built-in example data
data(wisconsin_lakes)
sites <- load_sites(wisconsin_lakes)
lake <- get_lake_boundary(sites)
results <- fetch_calculate(sites, lake)

# Option 2: Use your own CSV with lat/lon columns
sites <- load_sites("your_sites.csv")
lake <- get_lake_boundary(sites)
results <- fetch_calculate(sites, lake)

# View results
print(results$results)

# Interactive Shiny app
fetch_app(results)
```

## Standalone Shiny App (No Coding Required)

Users can run the app without any R coding - just upload a CSV:

```r
library(lakefetch)
fetch_app_upload()
```

This launches a browser-based app where users can:
- Upload a CSV with lat/lon columns
- Automatically download lake boundaries
- View interactive map with fetch rays
- Download results as CSV or GeoPackage

## Key Features

- **Automatic lake boundary download** from OpenStreetMap
- **Multi-lake batch processing** - handles CSV with sites on multiple lakes
- **Directional fetch** - ray-casting at configurable angle resolution (default 5°)
- **Effective fetch methods:**
  - `"top3"` - mean of 3 highest directional fetches (default)
  - `"max"` - maximum directional fetch
  - `"cosine"` - SPM/CERC cosine-weighted method
- **Wave metrics** - orbital velocity using SMB equations with depth attenuation
- **Exposure classification** - Sheltered/Moderate/Exposed (configurable thresholds)
- **Depth estimation** - empirical estimation from lake surface area (Cael et al. 2017)
- **Optional NHD integration** - outlet/inlet detection for US lakes
- **Optional weather integration** - historical wind data from Open-Meteo API
- **Interactive visualization** - Shiny app with Leaflet maps

## Configuration

```r
# View current options
lakefetch_options()

# Change options
lakefetch_options(
  fetch_method = "cosine",           # SPM method
  exposure_sheltered_m = 2000,       # Custom threshold
  exposure_exposed_m = 6000,         # Custom threshold
  default_depth_m = 5,               # For orbital velocity
  angle_resolution_deg = 10          # Faster but less precise
)

# Reset to defaults
lakefetch_reset_options()
```

## CSV Input Format

The CSV file should have:
- **Required:** Columns starting with `lat` and `lon` (e.g., `latitude`, `longitude`)
- **Optional:** `Site` column for site names
- **Optional:** `depth` or `depth_m` column for site-specific depths
- **Optional:** `datetime` column for weather integration

Example:
```
Site,latitude,longitude,depth_m
Site_A,43.42,-73.69,5.5
Site_B,43.43,-73.68,8.2
Site_C,43.41,-73.70,3.1
```

## CRAN Submission (Next Steps)

When ready to submit:

```r
# Final check (optional)
devtools::check()

# Submit to CRAN
devtools::submit_cran()
```

You'll receive:
1. Confirmation email from CRAN (reply to confirm)
2. Review result email (usually 1-5 business days)

## Files for CRAN

- `cran-comments.md` - Notes for CRAN reviewers
- `NEWS.md` - Version changelog
- `README.md` - Package documentation
- `inst/CITATION` - Citation information
- `inst/doc/` - Pre-built vignettes

## Package Structure

```
lakefetch/
├── R/
│   ├── bathymetry.R       # Depth estimation (empirical)
│   ├── data.R             # Built-in datasets documentation
│   ├── data_loading.R     # CSV loading and validation
│   ├── fetch_core.R       # Main fetch calculation
│   ├── globals.R          # Package options
│   ├── lake_sources.R     # OSM boundary download
│   ├── nhd_integration.R  # NHD outlet/inlet detection
│   ├── shiny_app.R        # Interactive Shiny apps
│   ├── visualization.R    # Plotting functions
│   ├── weather_integration.R  # Open-Meteo API
│   └── zzz.R              # Package startup
├── data/                  # Built-in example datasets
├── inst/
│   ├── CITATION           # How to cite the package
│   ├── doc/               # Pre-built vignettes
│   └── examples/          # Example workflows
├── man/                   # Documentation (auto-generated)
├── tests/                 # Unit tests
├── vignettes/             # Vignette source files
├── .github/               # GitHub Actions for CI/CD
├── DESCRIPTION            # Package metadata
├── NAMESPACE              # Exports (auto-generated)
├── README.md              # GitHub readme
├── NEWS.md                # Changelog
└── cran-comments.md       # CRAN submission notes
```

## References

- Shore Protection Manual (1984). U.S. Army Corps of Engineers, CERC. 4th Edition.
- Sverdrup & Munk (1947). Wind, sea, and swell. U.S. Navy Hydrographic Office.
- Cael et al. (2017). Volume and mean depth of Earth's lakes. GRL.
- Mason et al. (2018). Effective fetch maps for the Great Lakes. Scientific Data.

## Contact

Jeremy Lynch Farrell
farrej2@rpi.edu
https://github.com/ropensci/lakefetch
