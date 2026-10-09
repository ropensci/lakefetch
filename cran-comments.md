## Resubmission (v0.1.14)

This resubmission jumps from the currently-published CRAN version (0.1.3) to
0.1.14. In between, the package underwent formal peer review through
rOpenSci ([ropensci/software-review#762](https://github.com/ropensci/software-review/issues/762),
reviewers Jorrit Mesman and Khondula, editor Pakillo), which was approved on
2026-07-20. Versions 0.1.4-0.1.13 are the incremental responses to that
review; none were previously submitted to CRAN. Full details of every
version are in `NEWS.md`; the highlights are:

* Several bug fixes surfaced by review and testing, most notably:
  - Invalid UTM EPSG codes when input was already in a projected CRS
  - Silent (non-warning) failures when sites could not be matched to a lake
  - `plot_fetch_rose()` overplotting after Shiny/ggplot2 device state changes
  - `fetch_mean()`/`fetch_max()`/`fetch_effective()` returning `NaN`/`-Inf`
    instead of `NA` for sites that matched a lake but fell outside its
    polygon (fixed in this release, v0.1.14)
* Robustness/timeout handling improvements for `get_lake_boundary()`'s
  OpenStreetMap downloads (bounded worst-case wait, `total_timeout_s` arg)
* Repository transferred from `jeremylfarrell/lakefetch` to
  `ropensci/lakefetch` following review approval; all links (README,
  DESCRIPTION, man pages, `codemeta.json`, `CITATION`) updated accordingly
* Removal of unused internal helper functions flagged during review
* Suggested dependency `nhdplusTools` replaced by its drop-in successor
  `hydrogeofetch`, at the request of their maintainer, who plans to retire
  `nhdplusTools` from CRAN

## R CMD check results

0 errors | 0 warnings | 0 notes on win-builder R-devel and all R-hub
platforms.

Environment-only NOTEs seen elsewhere, unrelated to package content:

* Win-builder R-release: "Skipping checking math rendering: package 'V8'
  unavailable" (V8 not installed on the check machine).
* Local: "unable to verify current time" (no time-server access).

## Test environments

* Local: Windows 11 x64 (build 26200), R 4.4.1
* Win-builder: R-devel (2026-10-08 r90650 ucrt), R-release (4.6.1 ucrt)
* R-hub: Linux, macOS ARM64, Windows (R-devel)
* GitHub Actions: macOS, Windows, Ubuntu (release, oldrel, devel)

## Downstream dependencies

There are currently no downstream dependencies for this package.

## Notes for CRAN reviewers

### Package purpose

lakefetch calculates fetch (open water distance) and wave exposure metrics
for freshwater lake sampling sites. It addresses a gap in the R ecosystem —
existing fetch packages (fetchR, waver) focus on marine/coastal applications,
while lakefetch is designed specifically for inland lakes with features like:

- Automatic lake boundary download from OpenStreetMap
- Multi-lake batch processing
- Optional NHD (National Hydrography Dataset) integration for US lakes
- Interactive Shiny app for visualization
- Configurable effective fetch methods (top-3, maximum, SPM cosine-weighted)

### API usage

The package makes HTTP requests to:
- OpenStreetMap Overpass API (for lake boundary download)
- Open-Meteo API (for optional historical weather data)

All API calls are wrapped in tryCatch() with informative error messages,
and users can alternatively provide local boundary files.

### URL note

The USGS National Hydrography Dataset URL (https://www.usgs.gov/national-hydrography)
in DESCRIPTION returns HTTP 403 to automated checkers but is accessible in
browsers. This is a known USGS server configuration.
