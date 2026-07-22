# ==============================================================================
# Core Fetch Calculation Functions
# ==============================================================================

#' Calculate Fetch for Lake Sampling Sites
#'
#' Main entry point for fetch calculation. Takes sites and lake boundaries,
#' calculates directional fetch using ray-casting, and returns results with
#' exposure metrics.
#'
#' @param sites Data frame or sf object with site locations
#' @param lake Lake boundary data from \code{\link{get_lake_boundary}}
#' @param depth_m Mean water depth in meters for orbital velocity calculation
#'   (used in the SMB wave hindcast equations). Can be a single value (applied
#'   to all sites), a vector (one per site), or NULL to use depth from sites
#'   data or the default from \code{\link{lakefetch_options}}. Mean depth is
#'   preferred over maximum depth as it better represents conditions across
#'   the water column for wave attenuation estimates.
#' @param fetch_method Method for calculating effective fetch. Options:
#'   \describe{
#'     \item{"top3"}{Mean of the 3 highest directional fetch values (default)}
#'     \item{"max"}{Maximum directional fetch value}
#'     \item{"cosine"}{SPM/CERC cosine-weighted average. Uses 9 radials centered
#'       on the direction of maximum fetch at 6-degree intervals, weighted by
#'       cosine of angle from center. Based on Shore Protection Manual (1984).}
#'   }
#'   If NULL, uses the value from \code{\link{lakefetch_options}}.
#' @param add_context Logical; add NHD context if available (default TRUE)
#' @param find_max_fetch Logical; if TRUE, finds the location in each lake with
#'   the maximum possible fetch using a longest-internal-chord algorithm. The
#'   result is returned as a \code{$max_fetch} element in the output list.
#'   Default FALSE.
#'
#' @return A list with elements:
#'   \item{results}{sf object with fetch results for each site}
#'   \item{lakes}{sf object with lake polygons used}
#'   \item{angles}{Vector of angles used for fetch calculation}
#'   \item{max_fetch}{(only if \code{find_max_fetch = TRUE}) sf object with one
#'     row per lake containing the maximum fetch location, chord length (meters),
#'     and chord bearing (degrees)}
#'
#' @details
#' For each site, the function:
#' \enumerate{
#'   \item Assigns the site to its containing lake polygon
#'   \item Buffers the site inward from shore (GPS accuracy adjustment)
#'   \item Casts rays in all directions at specified angle resolution
#'   \item Measures distance to shore in each direction
#'   \item Calculates summary metrics (mean, max, effective fetch)
#'   \item Calculates orbital velocity using depth
#'   \item Derives exposure category (Sheltered/Moderate/Exposed)
#' }
#'
#' Exposure thresholds can be configured via \code{\link{lakefetch_options}}.
#'
#' @references
#' Shore Protection Manual (1984). U.S. Army Corps of Engineers, Coastal
#' Engineering Research Center. 4th Edition.
#'
#' @examplesIf interactive()
#' csv_path <- system.file("extdata", "sample_sites.csv", package = "lakefetch")
#' sites <- load_sites(csv_path)
#' lake <- get_lake_boundary(sites)
#' results <- fetch_calculate(sites, lake)
#'
#' # With explicit depth
#' results <- fetch_calculate(sites, lake, depth_m = 5)
#'
#' # Using cosine-weighted effective fetch (SPM method)
#' results <- fetch_calculate(sites, lake, fetch_method = "cosine")
#'
#' # Access results
#' results$results  # sf with all fetch data
#' results$lakes    # lake polygons
#'
#' # Find the location with maximum fetch in each lake
#' results <- fetch_calculate(sites, lake, find_max_fetch = TRUE)
#' results$max_fetch  # sf with max fetch location per lake
#'
#' @export
fetch_calculate <- function(sites, lake, depth_m = NULL, fetch_method = NULL,
                            add_context = TRUE, find_max_fetch = FALSE) {

  # Extract components from lake data
  all_lakes <- lake$all_lakes
  sites_sf <- lake$sites
  utm_epsg <- lake$utm_epsg

  # Resolve fetch method
  if (is.null(fetch_method)) {
    fetch_method <- get_opt("fetch_method")
  }
  fetch_method <- match.arg(fetch_method, c("top3", "max", "cosine"))
  message("Effective fetch method: ", fetch_method)

  # Resolve depth values
  n_sites <- nrow(sites_sf)
  if (!is.null(depth_m)) {
    # User provided depth
    if (length(depth_m) == 1) {
      depth_vec <- rep(depth_m, n_sites)
    } else if (length(depth_m) == n_sites) {
      depth_vec <- depth_m
    } else {
      stop("depth_m must be a single value or a vector with one value per site")
    }
    message("Using provided depth: ", paste(unique(depth_vec), collapse = ", "), " m")
  } else if ("depth_m" %in% names(sites_sf)) {
    # Depth from sites data
    depth_vec <- sites_sf$depth_m
    depth_vec[is.na(depth_vec)] <- get_opt("default_depth_m")
    message("Using depth from sites data (default for missing: ", get_opt("default_depth_m"), " m)")
  } else {
    # Use default
    depth_vec <- rep(get_opt("default_depth_m"), n_sites)
    message("Using default depth: ", get_opt("default_depth_m"), " m")
  }

  # Assign sites to lakes
  sites_with_lakes <- assign_sites_to_lakes(
    sites_sf,
    all_lakes,
    tolerance_m = get_opt("gps_tolerance_m")
  )

  # Add depth to sites

  sites_with_lakes$depth_m <- depth_vec

  # Calculate fetch (handles multiple lakes)
  fetch_data <- calculate_fetch_multi_lake(
    sites_with_lakes,
    all_lakes,
    utm_epsg,
    fetch_method = fetch_method
  )

  # Add lake context if requested and available
  if (add_context && nhd_available()) {
    fetch_data$results <- add_lake_context(
      fetch_data$results,
      fetch_data$lakes,
      utm_epsg
    )
  }

  # Find max fetch location for each lake if requested
  if (find_max_fetch) {
    message("Finding maximum fetch locations...")
    lake_polys <- fetch_data$lakes
    max_fetch_list <- vector("list", nrow(lake_polys))

    for (i in seq_len(nrow(lake_polys))) {
      lake_poly <- lake_polys[i, ]
      lake_nm <- if ("name" %in% names(lake_poly)) lake_poly$name[1] else
        lake_poly$osm_id[1]
      message("  Finding max fetch for: ",
              ifelse(is.na(lake_nm), "unknown", lake_nm))

      max_fetch_list[[i]] <- find_max_fetch_location(
        lake_poly, utm_epsg, fetch_method = fetch_method, refine = TRUE
      )
    }

    # Remove NULLs and combine
    max_fetch_list <- max_fetch_list[!vapply(max_fetch_list, is.null, logical(1))]
    if (length(max_fetch_list) > 0) {
      fetch_data$max_fetch <- do.call(rbind, max_fetch_list)
    }
  }

  # Include utm_epsg in output so downstream tools (e.g. fetch_app) can use it
  # without having to re-derive it from the CRS object (which can return NA
  # for some PROJ configurations)
  fetch_data$utm_epsg <- utm_epsg

  return(fetch_data)
}

#' Calculate Fetch for Multiple Lakes
#'
#' @param sites_with_lakes sf object with sites assigned to lakes
#' @param all_lakes sf object with all lake polygons
#' @param utm_epsg EPSG code for UTM projection
#' @param fetch_method Method for effective fetch: "top3", "max", or "cosine"
#'
#' @return List with results, lakes, and angles
#' @noRd
calculate_fetch_multi_lake <- function(sites_with_lakes, all_lakes, utm_epsg,
                                        fetch_method = "top3") {

  message("Calculating fetch for multiple lakes...")
  message("  Buffering sites ", get_opt("buffer_distance_m"), "m inward")
  message("  Angle resolution: ", get_opt("angle_resolution_deg"), " degrees")

  # Get unique lakes that have sites
  lakes_with_sites <- unique(sites_with_lakes$lake_osm_id)
  lakes_with_sites <- lakes_with_sites[!is.na(lakes_with_sites)]

  # Warn about unmatched sites - they will receive NA fetch values
  unmatched <- sites_with_lakes[is.na(sites_with_lakes$lake_osm_id), ]
  if (nrow(unmatched) > 0) {
    warning(nrow(unmatched), " site(s) not matched to any lake boundary ",
            "(fetch values will be NA): ",
            paste(unmatched$Site, collapse = ", "),
            ". These lakes may be too small for OpenStreetMap. ",
            "You can supply your own boundary file using: ",
            "get_lake_boundary(sites, file = 'your_boundary.gpkg')")
  }

  if (length(lakes_with_sites) == 0) {
    warning("No sites matched to any lake. All fetch values will be NA.")
    # Return results with all NAs
    angle_res <- get_opt("angle_resolution_deg")
    angles <- seq(0, 360 - angle_res, by = angle_res)
    na_cols <- stats::setNames(
      as.list(rep(NA_real_, length(angles))),
      paste0("fetch_", angles)
    )
    na_results <- sites_with_lakes
    for (col_name in names(na_cols)) {
      na_results[[col_name]] <- NA_real_
    }
    na_results$fetch_mean <- NA_real_
    na_results$fetch_max <- NA_real_
    na_results$fetch_effective <- NA_real_
    na_results$exposure_category <- NA_character_
    na_results$orbital_velocity <- NA_real_

    return(list(
      results = na_results,
      lakes = sf::st_sf(
        osm_id = character(0), name = character(0),
        area_km2 = numeric(0),
        geometry = sf::st_sfc(crs = sf::st_crs(sites_with_lakes))
      ),
      angles = angles
    ))
  }

  message("Processing ", length(lakes_with_sites), " lake(s)...")

  # Define function to process one lake
  process_one_lake <- function(lake_id, all_lakes_data, sites_data, utm_code,
                                eff_fetch_method) {
    lake_poly <- all_lakes_data[which(all_lakes_data$osm_id == lake_id), ]
    lake_sites <- sites_data[which(sites_data$lake_osm_id == lake_id), ]
    lake_nm <- lake_sites$lake_name[1]

    result <- calculate_fetch_single_lake(
      sites = lake_sites,
      lake_polygon = lake_poly,
      utm_epsg = utm_code,
      lake_name = lake_nm,
      lake_osm_id = lake_id,
      fetch_method = eff_fetch_method
    )

    return(result)
  }

  # Process lakes (parallel if multiple lakes and parallel is available)
  use_parallel <- parallel_available() && length(lakes_with_sites) > 1
  cl <- NULL

  if (use_parallel) {
    n_cores <- min(length(lakes_with_sites), max(1, parallel::detectCores() - 1))
    # Respect R CMD check core limits (max 2 simultaneous clusters allowed)
    if (nzchar(Sys.getenv("_R_CHECK_PACKAGE_NAME_"))) {
      n_cores <- min(n_cores, 2L)
    }
    message("Using parallel processing with ", n_cores, " cores for ",
            length(lakes_with_sites), " lakes")

    cl <- tryCatch(
      parallel::makeCluster(n_cores),
      error = function(e) {
        message("Parallel cluster unavailable, falling back to sequential: ", conditionMessage(e))
        NULL
      }
    )
    if (is.null(cl)) use_parallel <- FALSE
  }

  if (use_parallel) {
    # Export required functions and data
    parallel::clusterExport(cl, c(
      "calculate_fetch_single_lake", "get_highres_fetch", "nudge_inward",
      "calc_orbital", "calc_effective_fetch", "get_opt"
    ), envir = environment())

    parallel::clusterExport(cl, c("all_lakes", "sites_with_lakes", "utm_epsg",
                                   "fetch_method", "process_one_lake"),
                             envir = environment())

    parallel::clusterEvalQ(cl, requireNamespace("sf", quietly = TRUE))

    results_list <- parallel::parLapply(cl, lakes_with_sites, function(lid) {
      process_one_lake(lid, all_lakes, sites_with_lakes, utm_epsg, fetch_method)
    })

    parallel::stopCluster(cl)
  } else {
    message("Using sequential processing")
    if (interactive() && length(lakes_with_sites) > 1) {
      pb_lakes <- utils::txtProgressBar(min = 0, max = length(lakes_with_sites), style = 3)
      results_list <- lapply(seq_along(lakes_with_sites), function(j) {
        result <- process_one_lake(lakes_with_sites[j], all_lakes, sites_with_lakes,
                                   utm_epsg, fetch_method)
        utils::setTxtProgressBar(pb_lakes, j)
        result
      })
      close(pb_lakes)
    } else {
      results_list <- lapply(lakes_with_sites, function(lid) {
        process_one_lake(lid, all_lakes, sites_with_lakes, utm_epsg, fetch_method)
      })
    }
  }

  message("Fetch calculation complete.")

  # Combine results from all lakes
  all_results <- do.call(rbind, lapply(results_list, function(x) x$results))
  all_lake_polys <- do.call(rbind, lapply(results_list, function(x) x$lake))
  angles <- results_list[[1]]$angles

  # Add unmatched sites back with NA fetch values
  if (nrow(unmatched) > 0) {
    na_rows <- unmatched
    for (col_name in paste0("fetch_", angles)) {
      na_rows[[col_name]] <- NA_real_
    }
    na_rows$fetch_mean <- NA_real_
    na_rows$fetch_max <- NA_real_
    na_rows$fetch_effective <- NA_real_
    na_rows$exposure_category <- NA_character_
    na_rows$orbital_velocity <- NA_real_

    # Align columns before binding
    missing_in_na <- setdiff(names(all_results), names(na_rows))
    for (col_name in missing_in_na) {
      na_rows[[col_name]] <- NA
    }
    missing_in_results <- setdiff(names(na_rows), names(all_results))
    for (col_name in missing_in_results) {
      all_results[[col_name]] <- NA
    }

    all_results <- rbind(all_results[, names(all_results)],
                          na_rows[, names(all_results)])
    message("  Note: ", nrow(unmatched),
            " unmatched site(s) included with NA fetch values")
  }

  return(list(
    results = all_results,
    lakes = all_lake_polys,
    angles = angles
  ))
}

#' Calculate Fetch for a Single Lake
#'
#' @param sites sf object with site points
#' @param lake_polygon sf object with lake polygon
#' @param utm_epsg EPSG code for UTM projection
#' @param lake_name Character lake name
#' @param lake_osm_id Character OSM ID
#' @param fetch_method Method for effective fetch: "top3", "max", or "cosine"
#'
#' @return List with results, lake, and angles
#' @noRd
calculate_fetch_single_lake <- function(sites, lake_polygon, utm_epsg,
                                         lake_name = NULL, lake_osm_id = NULL,
                                         fetch_method = "top3") {

  n_sites <- nrow(sites)
  if (n_sites == 0) {
    return(NULL)
  }

  message("  Processing ", n_sites, " samples in lake: ",
          ifelse(is.null(lake_name) || is.na(lake_name), lake_osm_id, lake_name))

  # Optimize: identify unique coordinates to avoid recalculating fetch
  coords_all <- sf::st_coordinates(sites)
  coord_key <- paste(round(coords_all[, 1], 6), round(coords_all[, 2], 6), sep = "_")
  unique_coords <- !duplicated(coord_key)
  n_unique <- sum(unique_coords)

  if (n_unique < n_sites) {
    message("    (", n_unique, " unique locations, ", n_sites - n_unique, " repeat samples)")
  }

  # If multiple polygons (duplicates), union them into one
  if (nrow(lake_polygon) > 1) {
    unified_geom <- sf::st_union(lake_polygon)
    lake_polygon <- sf::st_sf(
      data.frame(osm_id = lake_osm_id, name = lake_name, stringsAsFactors = FALSE),
      geometry = sf::st_sfc(unified_geom, crs = utm_epsg)
    )
  }

  # Ensure valid geometry
  if (!all(sf::st_is_valid(lake_polygon))) {
    lake_polygon <- sf::st_make_valid(lake_polygon)
  }

  lake_boundary <- tryCatch({
    sf::st_cast(lake_polygon, "MULTILINESTRING")
  }, error = function(e) {
    tryCatch({
      sf::st_cast(sf::st_boundary(lake_polygon), "MULTILINESTRING")
    }, error = function(e2) {
      sf::st_boundary(lake_polygon)
    })
  })

  # Buffer all sites inward (only unique locations need nudging)
  sites_buffered <- sites
  new_geoms <- vector("list", n_sites)
  nudged_cache <- list()  # Cache nudged positions by coord_key

  show_buffer_pb <- interactive() && n_unique > 5
  if (show_buffer_pb) {
    message("    Buffering sites inward from shore...")
    pb_buf <- utils::txtProgressBar(min = 0, max = n_unique, style = 3)
    pb_buf_count <- 0
  }

  for (i in seq_len(n_sites)) {
    key <- coord_key[i]
    if (is.null(nudged_cache[[key]])) {
      # First time seeing this coordinate - calculate nudge
      nudged_cache[[key]] <- nudge_inward(sf::st_geometry(sites)[i],
                                           lake_boundary,
                                           lake_polygon)
      if (show_buffer_pb) {
        pb_buf_count <- pb_buf_count + 1
        utils::setTxtProgressBar(pb_buf, pb_buf_count)
      }
    }
    new_geoms[[i]] <- nudged_cache[[key]]
  }

  if (show_buffer_pb) close(pb_buf)

  sf::st_geometry(sites_buffered) <- sf::st_sfc(new_geoms, crs = utm_epsg)

  # Calculate fetch (only for unique coordinates)
  angle_res <- get_opt("angle_resolution_deg")
  angles <- seq(0, 360 - angle_res, by = angle_res)

  fetch_cache <- list()  # Cache fetch results by coord_key
  fetch_list <- vector("list", n_sites)

  show_fetch_pb <- interactive() && n_unique > 1
  if (show_fetch_pb) {
    message("    Calculating directional fetch for ", n_unique, " unique locations...")
    pb_fetch <- utils::txtProgressBar(min = 0, max = n_unique, style = 3)
    pb_fetch_count <- 0
  }

  for (i in seq_len(n_sites)) {
    key <- coord_key[i]
    if (is.null(fetch_cache[[key]])) {
      # First time seeing this coordinate - calculate fetch
      fetch_cache[[key]] <- get_highres_fetch(sites_buffered[i, ],
                                               lake_boundary,
                                               lake_polygon,
                                               angles)
      if (show_fetch_pb) {
        pb_fetch_count <- pb_fetch_count + 1
        utils::setTxtProgressBar(pb_fetch, pb_fetch_count)
      }
    }
    fetch_list[[i]] <- fetch_cache[[key]]
  }

  if (show_fetch_pb) close(pb_fetch)

  # Convert to data frame
  fetch_matrix <- do.call(rbind, fetch_list)
  colnames(fetch_matrix) <- paste0("fetch_", angles)
  fetch_df <- as.data.frame(fetch_matrix)

  # Combine with site data
  results <- cbind(sf::st_drop_geometry(sites_buffered), fetch_df)
  results_sf <- sf::st_sf(results, geometry = sf::st_geometry(sites_buffered))

  # Calculate metrics
  fetch_cols <- grep("^fetch_[0-9]", names(results), value = TRUE)
  fetch_values <- as.matrix(results[, fetch_cols])

  # Sites whose (possibly nudged) point still falls outside the lake polygon
  # get NA for every ray (see get_highres_fetch()). This is distinct from the
  # "unmatched to any lake" case: these sites matched a lake within
  # gps_tolerance_m, but sit outside the polygon by more than
  # buffer_distance_m, so nudge_inward() left them in place. rowMeans()/max()
  # with na.rm = TRUE silently turn an all-NA row into NaN/-Inf, so those rows
  # are handled explicitly below to keep the output NA and to warn the user,
  # since this case is otherwise silent.
  all_na_row <- apply(fetch_values, 1, function(x) all(is.na(x)))
  if (any(all_na_row)) {
    warning(sum(all_na_row), " site(s) matched to a lake but fell outside its ",
            "polygon by more than buffer_distance_m (", get_opt("buffer_distance_m"),
            " m), so fetch could not be calculated (values will be NA): ",
            paste(results$Site[all_na_row], collapse = ", "),
            ". This can happen when GPS/OSM coordinates don't perfectly align. ",
            "Consider increasing buffer_distance_m via lakefetch_options(), or ",
            "verify the site coordinates against the lake polygon.")
  }

  results$fetch_mean <- rowMeans(fetch_values, na.rm = TRUE)
  results$fetch_mean[all_na_row] <- NA_real_
  results$fetch_max <- apply(fetch_values, 1, function(x) {
    if (all(is.na(x))) NA_real_ else max(x, na.rm = TRUE)
  })

  # Calculate effective fetch using specified method
  results$fetch_effective <- calc_effective_fetch(fetch_values, angles, fetch_method)

  # Calculate orbital velocity using depth (depth_m column should exist from sites_with_lakes)
  site_depth <- if ("depth_m" %in% names(results)) results$depth_m else get_opt("default_depth_m")
  results$orbital_effective <- calc_orbital(results$fetch_effective, depth_m = site_depth)

  # Exposure classification using configurable absolute thresholds
  sheltered_threshold <- get_opt("exposure_sheltered_m")
  exposed_threshold <- get_opt("exposure_exposed_m")
  results$exposure_category <- ifelse(results$fetch_effective < sheltered_threshold, "Sheltered",
                                      ifelse(results$fetch_effective > exposed_threshold, "Exposed",
                                             "Moderate"))

  # Relative exposure classification (proportion of lake maximum fetch)
  chord <- find_longest_internal_chord(lake_polygon)
  max_chord_m <- if (!is.null(chord)) chord$max_chord_length else NA_real_
  results$lake_max_chord_m <- max_chord_m

  if (!is.na(max_chord_m) && max_chord_m > 0) {
    results$fetch_proportion <- results$fetch_effective / max_chord_m
    rel_sheltered <- get_opt("exposure_relative_sheltered")
    rel_exposed <- get_opt("exposure_relative_exposed")
    results$exposure_relative <- ifelse(results$fetch_proportion < rel_sheltered, "Sheltered",
                                        ifelse(results$fetch_proportion > rel_exposed, "Exposed",
                                               "Moderate"))
  } else {
    results$fetch_proportion <- NA_real_
    results$exposure_relative <- NA_character_
  }

  results_sf <- sf::st_sf(results, geometry = sf::st_geometry(sites_buffered))

  return(list(
    results = results_sf,
    lake = lake_polygon,
    angles = angles
  ))
}

#' Get Directional Fetch via Ray-Casting
#'
#' @param pt sf point geometry
#' @param boundary sf line geometry (lake boundary)
#' @param poly sf polygon geometry (lake)
#' @param angles Numeric vector of angles to cast rays
#'
#' @return Numeric vector of fetch distances
#' @noRd
get_highres_fetch <- function(pt, boundary, poly, angles) {

  max_d <- get_opt("max_fetch_m")
  validation_buffer <- get_opt("validation_buffer_m")

  # If the site is not inside the lake polygon, fetch cannot be calculated
  is_inside <- length(sf::st_intersects(pt, poly)[[1]]) > 0
  if (!is_inside) {
    return(rep(NA_real_, length(angles)))
  }

  coords <- sf::st_coordinates(pt)
  x0 <- coords[1]
  y0 <- coords[2]
  f_dists <- numeric(length(angles))
  pt_crs <- sf::st_crs(pt)

  for (i in seq_along(angles)) {
    deg <- angles[i]
    rad <- deg * (pi / 180)

    x1 <- x0 + max_d * sin(rad)
    y1 <- y0 + max_d * cos(rad)
    ray_geom <- sf::st_sfc(sf::st_linestring(rbind(c(x0, y0), c(x1, y1))), crs = pt_crs)

    # Calculate intersection with error handling
    inter <- tryCatch({
      suppressWarnings(sf::st_intersection(ray_geom, boundary))
    }, error = function(e) {
      sf::st_sfc(crs = pt_crs)
    })

    if (length(inter) == 0) {
      f_dists[i] <- max_d
    } else {
      dists <- sort(as.numeric(sf::st_distance(pt, inter)))
      dists <- dists[dists > 1]

      final_dist <- max_d

      if (length(dists) > 0) {
        final_dist <- dists[1]

        for (d in dists) {
          check_dist <- d + validation_buffer
          if (check_dist >= max_d) break

          xt <- x0 + check_dist * sin(rad)
          yt <- y0 + check_dist * cos(rad)
          test_pt <- sf::st_sfc(sf::st_point(c(xt, yt)), crs = pt_crs)

          is_water <- length(sf::st_intersects(test_pt, poly)[[1]]) > 0

          if (is_water) {
            next
          } else {
            final_dist <- d
            break
          }
        }
      }
      f_dists[i] <- final_dist
    }
  }

  return(f_dists)
}

#' Nudge Point Inward from Shore
#'
#' Buffer a point inward from the nearest shore to account for GPS inaccuracy.
#' Only nudges points that are already close to the shoreline (within the
#' buffer distance threshold). Points far from shore are left at their
#' original location.
#'
#' @param point_geom sf point geometry
#' @param poly_boundary sf line geometry (lake boundary)
#' @param poly_full sf polygon geometry (lake)
#' @param dist Buffer distance in meters
#'
#' @return sf point geometry
#' @noRd
nudge_inward <- function(point_geom, poly_boundary, poly_full, dist = NULL) {

  if (is.null(dist)) {
    dist <- get_opt("buffer_distance_m")
  }

  # Extract the sfg point from sfc if needed (for consistent return type)
  original_point <- if (inherits(point_geom, "sfc")) point_geom[[1]] else point_geom

  # Handle empty or invalid geometries
  if (is.null(poly_boundary) || sf::st_is_empty(poly_boundary) ||
      is.null(poly_full) || sf::st_is_empty(poly_full)) {
    return(original_point)
  }

  is_inside <- length(sf::st_intersects(point_geom, poly_full)[[1]]) > 0

  nearest_on_shore <- tryCatch({
    sf::st_nearest_points(point_geom, poly_boundary)
  }, error = function(e) NULL)

  if (is.null(nearest_on_shore) || length(nearest_on_shore) == 0) {
    return(original_point)
  }

  coords <- sf::st_coordinates(nearest_on_shore)

  if (is.null(coords) || nrow(coords) < 2) {
    return(original_point)
  }

  p_x <- coords[1, 1]
  p_y <- coords[1, 2]
  s_x <- coords[2, 1]
  s_y <- coords[2, 2]

  dx <- p_x - s_x
  dy <- p_y - s_y
  len <- sqrt(dx^2 + dy^2)

  if (len == 0) return(original_point)

  # Only nudge points that are close to the shoreline (within buffer distance)
  # Points far from shore are left at their original location
  if (len > dist) {
    return(original_point)
  }

  if (!is_inside) {
    dx <- -dx
    dy <- -dy
  }

  new_x <- s_x + (dx / len) * dist
  new_y <- s_y + (dy / len) * dist

  return(sf::st_point(c(new_x, new_y)))
}

#' Calculate Effective Fetch
#'
#' Calculate effective fetch using one of three methods.
#'
#' @param fetch_matrix Matrix of directional fetch values (rows = sites, cols = angles)
#' @param angles Numeric vector of angles in degrees
#' @param method Method: "top3", "max", or "cosine"
#'
#' @return Numeric vector of effective fetch values
#' @noRd
calc_effective_fetch <- function(fetch_matrix, angles, method = "top3") {

  if (method == "max") {
    # Simple maximum
    return(apply(fetch_matrix, 1, function(x) {
      if (all(is.na(x))) NA_real_ else max(x, na.rm = TRUE)
    }))

  } else if (method == "top3") {
    # Mean of 3 highest values
    return(apply(fetch_matrix, 1, function(x) {
      if (all(is.na(x))) return(NA_real_)
      mean(sort(x, decreasing = TRUE)[1:3], na.rm = TRUE)
    }))

  } else if (method == "cosine") {
    # SPM/CERC cosine-weighted method
    # Uses 9 radials centered on direction of maximum fetch, at 6-degree intervals
    # Weighted by cosine of angle from center direction
    # Reference: Shore Protection Manual (1984), Equation 3-36

    angle_resolution <- if (length(angles) > 1) angles[2] - angles[1] else 5
    n_radials <- 9
    radial_spacing <- 6  # degrees

    return(apply(fetch_matrix, 1, function(fetch_row) {
      if (all(is.na(fetch_row))) return(NA_real_)
      # Find direction of maximum fetch
      max_idx <- which.max(fetch_row)
      max_angle <- angles[max_idx]

      # Calculate angles for 9 radials centered on max direction
      offsets <- seq(-(n_radials - 1) / 2, (n_radials - 1) / 2) * radial_spacing
      radial_angles <- (max_angle + offsets) %% 360

      # Get fetch values at these angles (interpolate if needed)
      radial_fetches <- vapply(radial_angles, function(a) {
        # Find closest angle in our measurements
        angle_diff <- abs(angles - a)
        angle_diff <- pmin(angle_diff, 360 - angle_diff)  # Handle wrap-around
        closest_idx <- which.min(angle_diff)
        fetch_row[closest_idx]
      }, numeric(1))

      # Cosine weights (in radians)
      cos_weights <- cos(offsets * pi / 180)

      # Weighted average
      sum(radial_fetches * cos_weights) / sum(cos_weights)
    }))
  } else {
    stop("Unknown fetch method: ", method)
  }
}

#' Find the Longest Internal Chord of a Lake Polygon
#'
#' Finds the longest straight line between two boundary points that lies
#' entirely within the polygon. This represents the maximum possible
#' single-direction fetch for any point in the lake.
#'
#' @param polygon sf POLYGON object (single lake)
#' @param n_sample Maximum number of boundary points to sample (default 500).
#'   Higher values give more precision but take longer.
#'
#' @return List with elements:
#'   \item{midpoint}{sf point at the midpoint of the chord}
#'   \item{max_chord_length}{Length of the chord in meters}
#'   \item{bearing}{Bearing of the chord in degrees (0-360)}
#'   \item{endpoints}{2x2 matrix of chord endpoint coordinates}
#'   Returns NULL if polygon has fewer than 3 vertices.
#'
#' @noRd
find_longest_internal_chord <- function(polygon, n_sample = 500) {

  # Extract boundary coordinates (remove L1/L2 index columns)
  coords <- sf::st_coordinates(polygon)
  coords <- coords[, 1:2, drop = FALSE]
  N <- nrow(coords)

  if (N < 3) return(NULL)

  # Remove duplicate closing vertex if present
  if (all(coords[1, ] == coords[N, ])) {
    coords <- coords[-N, , drop = FALSE]
    N <- N - 1
  }

  # Subsample if too many vertices
  if (N > n_sample) {
    idx <- round(seq(1, N, length.out = n_sample))
    coords <- coords[idx, , drop = FALSE]
    N <- n_sample
  }

  # Compute pairwise distance matrix
  dist_mat <- as.matrix(stats::dist(coords))

  # Get upper triangle pairs sorted by distance descending
  pairs <- which(upper.tri(dist_mat), arr.ind = TRUE)
  pair_dists <- dist_mat[pairs]
  ord <- order(pair_dists, decreasing = TRUE)

  crs_val <- sf::st_crs(polygon)

  # Check containment in batches (much faster than one-at-a-time)
  batch_size <- 100
  max_checks <- min(length(ord), 5000)  # Safety cap for degenerate cases

  for (start in seq(1, max_checks, by = batch_size)) {
    end_idx <- min(start + batch_size - 1, max_checks)
    batch_ord <- ord[start:end_idx]

    # Build batch of line segments
    lines_list <- lapply(batch_ord, function(k) {
      i <- pairs[k, 1]
      j <- pairs[k, 2]
      sf::st_linestring(rbind(coords[i, ], coords[j, ]))
    })
    lines_sfc <- sf::st_sfc(lines_list, crs = crs_val)

    # Check which lines are entirely within the polygon
    covered <- tryCatch(
      sf::st_covered_by(lines_sfc, polygon, sparse = FALSE)[, 1],
      error = function(e) rep(FALSE, length(lines_list))
    )

    first_valid <- which(covered)[1]
    if (!is.na(first_valid)) {
      k <- batch_ord[first_valid]
      i <- pairs[k, 1]
      j <- pairs[k, 2]

      midpoint <- sf::st_point(colMeans(coords[c(i, j), ]))
      dx <- coords[j, 1] - coords[i, 1]
      dy <- coords[j, 2] - coords[i, 2]
      bearing <- (atan2(dx, dy) * 180 / pi) %% 360

      return(list(
        midpoint = midpoint,
        max_chord_length = pair_dists[k],
        bearing = bearing,
        endpoints = coords[c(i, j), ]
      ))
    }
  }

  # Fallback: no valid chord found (very degenerate polygon)
  # Use centroid and report NA for chord length
  centroid_coords <- sf::st_coordinates(sf::st_centroid(polygon))
  return(list(
    midpoint = sf::st_point(centroid_coords[1, 1:2]),
    max_chord_length = NA_real_,
    bearing = NA_real_,
    endpoints = NULL
  ))
}

#' Find the Location with Maximum Fetch in a Lake
#'
#' Uses a geometric approach to find the point within a lake that has the
#' highest possible fetch. Computes the longest internal chord of the lake
#' polygon (the longest straight line between two shore points that stays
#' entirely over water). The midpoint of this chord is the maximum-fetch
#' location.
#'
#' @param lake_polygon sf POLYGON object (single lake, UTM CRS)
#' @param utm_epsg EPSG code for UTM projection
#' @param fetch_method Method for effective fetch ("top3", "max", or "cosine").
#'   Only used when \code{refine = TRUE}.
#' @param n_sample Maximum boundary points to sample (default 500)
#' @param refine Logical; if TRUE, runs full directional ray-cast fetch from
#'   the midpoint to compute effective fetch, fetch_max, and fetch_mean.
#'   Default FALSE (returns only chord-based metrics).
#'
#' @return sf object (1 row) with columns:
#'   \item{lake_name}{Name of the lake}
#'   \item{lake_osm_id}{OSM identifier}
#'   \item{max_chord_m}{Length of longest internal chord (meters)}
#'   \item{chord_bearing_deg}{Bearing of the chord (degrees, 0-360)}
#'   \item{fetch_effective}{Effective fetch at midpoint (only if refine=TRUE)}
#'   \item{fetch_max}{Max directional fetch at midpoint (only if refine=TRUE)}
#'   \item{fetch_mean}{Mean directional fetch at midpoint (only if refine=TRUE)}
#'   \item{geometry}{POINT location of maximum fetch}
#'
#' @noRd
find_max_fetch_location <- function(lake_polygon, utm_epsg,
                                     fetch_method = "top3",
                                     n_sample = 500,
                                     refine = FALSE) {

  # Validate geometry
  if (!all(sf::st_is_valid(lake_polygon))) {
    lake_polygon <- sf::st_make_valid(lake_polygon)
  }

  # Phase 1: Find longest internal chord
  chord <- find_longest_internal_chord(lake_polygon, n_sample = n_sample)

  if (is.null(chord)) {
    warning("Could not find internal chord for lake: ",
            if ("name" %in% names(lake_polygon)) lake_polygon$name[1] else "unknown")
    return(NULL)
  }

  # Build result sf
  result_data <- data.frame(
    lake_name = if ("name" %in% names(lake_polygon)) lake_polygon$name[1] else NA_character_,
    lake_osm_id = if ("osm_id" %in% names(lake_polygon)) lake_polygon$osm_id[1] else NA_character_,
    max_chord_m = chord$max_chord_length,
    chord_bearing_deg = chord$bearing,
    stringsAsFactors = FALSE
  )

  result_sf <- sf::st_sf(
    result_data,
    geometry = sf::st_sfc(chord$midpoint, crs = utm_epsg)
  )

  # Phase 2: Optional ray-cast refinement
  if (refine && !is.na(chord$max_chord_length)) {
    lake_boundary <- tryCatch({
      sf::st_cast(lake_polygon, "MULTILINESTRING")
    }, error = function(e) {
      tryCatch({
        sf::st_cast(sf::st_boundary(lake_polygon), "MULTILINESTRING")
      }, error = function(e2) {
        sf::st_boundary(lake_polygon)
      })
    })

    angles <- seq(0, 360 - get_opt("angle_resolution_deg"),
                  by = get_opt("angle_resolution_deg"))

    midpoint_sf <- sf::st_sf(
      geometry = sf::st_sfc(chord$midpoint, crs = utm_epsg)
    )

    fetch_vals <- get_highres_fetch(midpoint_sf, lake_boundary,
                                     lake_polygon, angles)

    eff <- calc_effective_fetch(
      matrix(fetch_vals, nrow = 1), angles, fetch_method
    )[1]

    result_sf$fetch_effective <- eff
    result_sf$fetch_max <- max(fetch_vals)
    result_sf$fetch_mean <- mean(fetch_vals)
  }

  return(result_sf)
}

#' Calculate Orbital Velocity
#'
#' Estimate wave-induced near-bottom orbital velocity based on fetch, wind speed,
#' and water depth using SMB (Sverdrup-Munk-Bretschneider) wave equations.
#'
#' @param fetch_m Fetch distance in meters
#' @param depth_m Water depth in meters
#' @param wind_speed Wind speed in m/s
#'
#' @return Orbital velocity in m/s
#'
#' @details
#' Uses simplified SMB-style equations for fetch-limited wave hindcasting:
#' \itemize{
#'   \item Wave height: H_s = 0.0016 * sqrt(F) * U
#'   \item Wave period: T_p = 0.286 * F^0.33 * U^0.33
#'   \item Orbital velocity: U_b = (pi * H / T) * exp(-k * d)
#' }
#'
#' These simplified coefficients approximate the full SMB equations, which
#' use hyperbolic tangent functions (Shore Protection Manual, 1984, Eq. 3-39).
#' The full deep-water SMB formulation is:
#' H = (U^2 / g) * 0.283 * tanh(0.0125 * (g*F/U^2)^0.42)
#'
#' The simplified form used here is appropriate for small to medium lakes
#' where fetch-limited conditions dominate and computational efficiency
#' is desired. For critical applications, consider implementing the full
#' SPM equations.
#'
#' @references
#' Shore Protection Manual (1984). U.S. Army Corps of Engineers, Coastal
#' Engineering Research Center. 4th Edition. Chapter 3.
#'
#' Sverdrup, H. U., & Munk, W. H. (1947). Wind, sea, and swell: theory of
#' relations for forecasting. U.S. Navy Hydrographic Office, Pub. No. 601.
#'
#' @noRd
calc_orbital <- function(fetch_m, depth_m = NULL, wind_speed = NULL) {
  if (is.null(wind_speed)) {
    wind_speed <- get_opt("default_wind_speed_ms")
  }
  if (is.null(depth_m)) {
    depth_m <- get_opt("default_depth_m")
  }


  # SMB wave hindcast equations
  # Significant wave height (m): H_s = 0.0016 * sqrt(F) * U
  H_sig <- 0.0016 * sqrt(fetch_m) * wind_speed

  # Wave period (s): T_p = 0.286 * F^0.33 * U^0.33
  T_p <- 0.286 * (fetch_m^0.33) * (wind_speed^0.33)

  # Avoid division by zero

  T_p <- pmax(T_p, 0.1)

  # Deep water wavelength: L = 1.56 * T^2
  wavelength <- 1.56 * T_p^2

  # Wave number: k = 2*pi / L
  k <- 2 * pi / wavelength

  # Near-bottom orbital velocity: U_b = (pi * H / T) * exp(-k * d)
  orbital_velocity <- (pi * H_sig / T_p) * exp(-k * depth_m)

  return(orbital_velocity)
}
