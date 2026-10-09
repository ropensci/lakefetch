# ==============================================================================
# Package Options and Defaults
# ==============================================================================

#' Default package options
#'
#' @noRd
.lakefetch_defaults <- list(

  buffer_distance_m = 10,
  angle_resolution_deg = 5,

  max_fetch_m = 50000,
  validation_buffer_m = 10,
  default_wind_speed_ms = 10,
  default_depth_m = 10,
  gps_tolerance_m = 100,

  # Effective fetch method: "top3", "max", or "cosine" (SPM/CERC method)
  fetch_method = "top3",


  # Exposure classification thresholds (meters)
  # These are practical defaults for small-to-medium lakes, not based on

  # specific literature. Adjust based on your study system. For context,
  # see Mason et al. (2018) on Great Lakes exposure mapping.
  exposure_sheltered_m = 2500,
  exposure_exposed_m = 5000,

  # Relative exposure classification thresholds (proportion of lake max chord)
  # These classify sites based on the ratio of effective fetch to the lake's
  # maximum possible fetch (longest internal chord).
  exposure_relative_sheltered = 0.25,
  exposure_relative_exposed = 0.50,

  use_parallel = TRUE,

  use_nhd = TRUE
)

#' Get or Set lakefetch Package Options
#'
#' Get or set options that control the behavior of lakefetch functions.
#'
#' @param ... Named arguments to set options. If empty, returns all current options.
#'
#' @return If no arguments, returns a list of all current options.
#'   If arguments provided, sets those options and returns invisible NULL.
#'
#' @details
#' Available options:
#' \describe{
#'   \item{buffer_distance_m}{GPS accuracy buffer in meters (default: 10)}
#'   \item{angle_resolution_deg}{Direction resolution in degrees (default: 5)}
#'   \item{max_fetch_m}{Maximum fetch distance in meters (default: 50000)}
#'   \item{validation_buffer_m}{Shore detection validation buffer (default: 10)}
#'   \item{default_wind_speed_ms}{Default wind speed in m/s (default: 10)}
#'   \item{default_depth_m}{Default water depth in meters (default: 10)}
#'   \item{gps_tolerance_m}{Buffer for matching sites to lakes (default: 100)}
#'   \item{fetch_method}{Effective fetch calculation method: "top3" (mean of 3
#'     highest directional fetches, default), "max" (maximum directional fetch),
#'     or "cosine" (SPM/CERC cosine-weighted average across 9 radials at 6-degree
#'     intervals; see Shore Protection Manual, 1984)}
#'   \item{exposure_sheltered_m}{Fetch threshold below which sites are classified
#'     as "Sheltered" (default: 2500 m). This is a practical default; no universal
#'     standard exists in the literature. Adjust based on your study system.}
#'   \item{exposure_exposed_m}{Fetch threshold above which sites are classified
#'     as "Exposed" (default: 5000 m). Sites between thresholds are "Moderate".
#'     See Mason et al. (2018) for Great Lakes exposure mapping methodology.}
#'   \item{exposure_relative_sheltered}{Proportion of lake maximum fetch below
#'     which sites are classified as "Sheltered" in the relative exposure system
#'     (default: 0.25). Sites are classified relative to the lake's longest
#'     internal chord (maximum possible fetch).}
#'   \item{exposure_relative_exposed}{Proportion of lake maximum fetch above
#'     which sites are classified as "Exposed" in the relative exposure system
#'     (default: 0.50). Sites between thresholds are "Moderate".}
#'   \item{use_parallel}{Use parallel processing for multi-lake (default: TRUE)}
#'   \item{use_nhd}{Use NHD integration if available (default: TRUE)}
#' }
#'
#' @references
#' Shore Protection Manual (1984). U.S. Army Corps of Engineers, Coastal
#' Engineering Research Center. 4th Edition.
#'
#' Mason, L. A., Riseng, C. M., Laber, A. L., & Rutherford, E. S. (2018).
#' Effective fetch and relative exposure index maps for the Laurentian
#' Great Lakes. Scientific Data, 5, 180295.
#'
#' @examples
#' # Get all options
#' lakefetch_options()
#'
#' # Get specific option
#' lakefetch_options()$buffer_distance_m
#'
#' # Set options
#' lakefetch_options(buffer_distance_m = 20, angle_resolution_deg = 10)
#'
#' @export
lakefetch_options <- function(...) {
  args <- list(...)


  if (length(args) == 0) {
    # Return all current options
    opts <- list()
    for (nm in names(.lakefetch_defaults)) {
      opt_name <- paste0("lakefetch.", nm)
      opts[[nm]] <- getOption(opt_name, .lakefetch_defaults[[nm]])
    }
    return(opts)
  }

  # Set options
  for (nm in names(args)) {
    if (!nm %in% names(.lakefetch_defaults)) {
      warning("Unknown option: ", nm)
      next
    }
    opt_name <- paste0("lakefetch.", nm)
    do.call(options, stats::setNames(list(args[[nm]]), opt_name))
  }

  invisible(NULL)
}

#' Reset lakefetch Options to Defaults
#'
#' Reset all lakefetch package options to their default values.
#'
#' @return Invisible NULL
#'
#' @examples
#' lakefetch_reset_options()
#'
#' @export
lakefetch_reset_options <- function() {
  for (nm in names(.lakefetch_defaults)) {
    opt_name <- paste0("lakefetch.", nm)
    do.call(options, stats::setNames(list(.lakefetch_defaults[[nm]]), opt_name))
  }
  invisible(NULL)
}

#' Get a single package option
#'
#' @param name Option name (without "lakefetch." prefix)
#' @return Option value
#' @noRd
get_opt <- function(name) {

  opt_name <- paste0("lakefetch.", name)
  getOption(opt_name, .lakefetch_defaults[[name]])
}

#' Check if hydrogeofetch is available
#'
#' @return Logical indicating if NHD integration is available and enabled
#' @noRd
nhd_available <- function() {
  get_opt("use_nhd") && requireNamespace("hydrogeofetch", quietly = TRUE)
}

#' Check if parallel processing is available
#'
#' @return Logical indicating if parallel processing is available and enabled
#' @noRd
parallel_available <- function() {
  get_opt("use_parallel") && requireNamespace("parallel", quietly = TRUE)
}
