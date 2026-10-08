# ==============================================================================
# Lake Boundary Acquisition Functions
# ==============================================================================

#' Get Lake Boundary
#'
#' Get lake boundary polygon(s) either from OpenStreetMap or from a local file.
#'
#' @param sites A data.frame with latitude and longitude columns, or an sf object.
#' @param file Optional file path to a shapefile or geopackage with lake boundaries.
#' @param timeout Integer; Overpass API query timeout in seconds. Default is 90.
#'   Increase for very large lakes (e.g., \code{timeout = 300} for Mälaren or
#'   Great Lakes) or when server load is high.
#' @param simplify_tolerance_m Numeric; if greater than 0, simplify lake
#'   polygons with \code{sf::st_simplify(dTolerance = simplify_tolerance_m)}
#'   (in meters, applied in the UTM projection). Useful for very large or
#'   complex lakes where an exact coastline is not needed and a coarser
#'   polygon greatly speeds up fetch ray-casting. Typical values: 50-500 m
#'   for large lakes (e.g., Mälaren, Vättern). Default is 0 (no simplification).
#' @param total_timeout_s Numeric; soft wall-clock budget in seconds on the
#'   total time \code{get_lake_boundary()} will spend downloading from OSM.
#'   The budget is consulted at natural breakpoints (between Overpass query
#'   types, between clusters for spread-out sites, and between name-filtered
#'   queries) and aborts further work when exceeded. It is NOT a hard cap
#'   on a single \code{osmdata::osmdata_sf()} call: if Overpass returns
#'   HTTP 429, osmdata does its own 60-second-per-retry backoff loop
#'   internally and we cannot safely interrupt that from R without risking
#'   a segfault on Windows. So on a heavily throttled server a single call
#'   may still exceed \code{total_timeout_s} by several minutes. For a hard
#'   ceiling, wrap the call in \code{R.utils::withTimeout()} yourself, or
#'   supply a local boundary file via the \code{file} argument. Default
#'   300 seconds (5 minutes); set to \code{Inf} to disable.
#'
#' @return A list with elements:
#'   \item{all_lakes}{sf object with lake polygons in UTM projection}
#'   \item{sites}{sf object with site points in UTM projection}
#'   \item{utm_epsg}{EPSG code for the UTM projection used}
#'
#' @details
#' If \code{file} is provided, the lake boundary is loaded from the file.
#' Otherwise, the function downloads lake boundaries from OpenStreetMap
#' based on the bounding box of the provided sites.
#'
#' For very large lakes (> ~500 km\eqn{^2}), the default 90-second Overpass
#' API timeout may be exceeded. Use \code{timeout = 300} or higher in those
#' cases. For lakes with very complex shorelines (e.g., Mälaren, Vättern,
#' Võrtsjärv), additionally pass \code{simplify_tolerance_m = 100} (or higher)
#' to coarsen the polygon and speed up downstream fetch calculations.
#'
#' @examplesIf interactive()
#' csv_path <- system.file("extdata", "sample_sites.csv", package = "lakefetch")
#' sites <- load_sites(csv_path)
#' lake_data <- get_lake_boundary(sites)
#'
#' # For very large lakes, increase the timeout
#' lake_data <- get_lake_boundary(sites, timeout = 300)
#'
#' # For large/complex lakes, also coarsen the shoreline
#' lake_data <- get_lake_boundary(sites, timeout = 300,
#'                                simplify_tolerance_m = 100)
#'
#' @export
get_lake_boundary <- function(sites, file = NULL, timeout = 90,
                              simplify_tolerance_m = 0,
                              total_timeout_s = 300) {
  result <- if (!is.null(file)) {
    load_lake_file(sites, file)
  } else {
    download_lake_osm(sites, timeout = timeout,
                       total_timeout_s = total_timeout_s)
  }

  if (isTRUE(simplify_tolerance_m > 0) &&
      !is.null(result$all_lakes) && nrow(result$all_lakes) > 0) {
    message("Simplifying lake polygons (dTolerance = ",
            simplify_tolerance_m, " m)...")
    n_before <- sum(vapply(sf::st_geometry(result$all_lakes),
                           function(g) length(unlist(g)), integer(1))) / 2
    simplified <- sf::st_simplify(result$all_lakes,
                                   dTolerance = simplify_tolerance_m,
                                   preserveTopology = TRUE)
    # Repair any geometries that simplification rendered invalid/empty
    not_empty <- !sf::st_is_empty(simplified)
    simplified <- simplified[not_empty, ]
    bad <- !sf::st_is_valid(simplified)
    if (any(bad)) {
      simplified[bad, ] <- sf::st_make_valid(simplified[bad, ])
    }
    n_after <- sum(vapply(sf::st_geometry(simplified),
                          function(g) length(unlist(g)), integer(1))) / 2
    message("  Vertices: ", round(n_before), " -> ", round(n_after),
            " (", round(100 * (1 - n_after / max(n_before, 1)), 1),
            "% reduction)")
    result$all_lakes <- simplified
  }

  return(result)
}

#' Group Sites into Spatial Clusters
#'
#' Groups sites by proximity using a grid-based approach. Sites within 0.1
#' degrees (~11 km) of each other are placed in the same cluster.
#'
#' @param sites_sf sf object with site points in WGS84
#' @return List of integer vectors, each containing row indices for one cluster
#' @noRd
cluster_sites <- function(sites_sf) {
  coords <- sf::st_coordinates(sites_sf)
  grid_size <- 0.1  # ~11 km

  # Round coordinates to nearest grid cell
  grid_x <- floor(coords[, 1] / grid_size)
  grid_y <- floor(coords[, 2] / grid_size)
  grid_key <- paste(grid_x, grid_y, sep = "_")

  # Group indices by grid cell
  clusters <- split(seq_len(nrow(sites_sf)), grid_key)

  return(unname(clusters))
}

#' Extract Polygons from OSM Response
#'
#' @param osm_data Result from osmdata::osmdata_sf()
#' @return List of standardized sf data frames
#' @noRd
extract_osm_polys <- function(osm_data) {
  result <- list()
  if (!is.null(osm_data$osm_polygons) && nrow(osm_data$osm_polygons) > 0) {
    std_poly <- standardize_osm_sf(osm_data$osm_polygons)
    if (!is.null(std_poly) && nrow(std_poly) > 0) {
      result[[length(result) + 1]] <- std_poly
      message("    Found ", nrow(std_poly), " polygons")
    }
  }
  if (!is.null(osm_data$osm_multipolygons) && nrow(osm_data$osm_multipolygons) > 0) {
    std_mpoly <- standardize_osm_sf(osm_data$osm_multipolygons)
    if (!is.null(std_mpoly) && nrow(std_mpoly) > 0) {
      result[[length(result) + 1]] <- std_mpoly
      message("    Found ", nrow(std_mpoly), " multipolygons")
    }
  }
  return(result)
}

#' Query OSM by Name(s) Using Regex Matching
#'
#' Accepts a single name or vector of names, builds regex OR pattern,
#' and queries Overpass for matching water bodies. Uses substring matching
#' so "Crooked" matches "Crooked Lake".
#'
#' @param bbox Named numeric vector with left, bottom, right, top
#' @param names Character vector of lake names
#' @param overpass_servers Character vector of Overpass API server URLs
#' @param max_attempts Maximum retry attempts
#' @return osmdata result or NULL
#' @noRd
query_osm_by_name <- function(bbox, names, overpass_servers, max_attempts = 3,
                               timeout = 120) {
  # Escape regex special characters in each name, then join with |
  escape_regex <- function(x) {
    gsub("([.|()\\\\+*?\\[\\]^${}])", "\\\\\\1", x, perl = TRUE)
  }
  escaped <- vapply(names, escape_regex, character(1), USE.NAMES = FALSE)
  name_pattern <- paste(escaped, collapse = "|")

  # Overpass returns features whose NODES fall inside the bbox. For a small
  # bbox sitting inside a huge lake (e.g., a single point in Malaren) the
  # lake's polygon nodes are far outside the bbox and nothing comes back.
  # Because the name filter is already very selective, we can safely expand
  # the bbox to a much wider area without risking a flood of unrelated
  # results. Expand to at least 1.5 degrees in each dimension, centered on
  # the original bbox.
  span_x <- as.numeric(bbox["right"] - bbox["left"])
  span_y <- as.numeric(bbox["top"] - bbox["bottom"])
  min_span <- 1.5
  if (is.finite(span_x) && span_x < min_span) {
    cx <- as.numeric(bbox["left"] + bbox["right"]) / 2
    bbox["left"]  <- cx - min_span / 2
    bbox["right"] <- cx + min_span / 2
  }
  if (is.finite(span_y) && span_y < min_span) {
    cy <- as.numeric(bbox["bottom"] + bbox["top"]) / 2
    bbox["bottom"] <- cy - min_span / 2
    bbox["top"]    <- cy + min_span / 2
  }

  # Errors we should NOT retry, because retrying immediately just wastes
  # minutes on osmdata's own 60-second rate-limit backoff:
  #   - "arguments imply differing number of rows" (deterministic osmdata
  #     parse bug on certain Overpass responses)
  #   - HTTP 429 Too Many Requests (server is explicitly asking us to
  #     back off; hammering it faster does not help)
  is_fatal <- function(msg) {
    if (is.null(msg)) return(FALSE)
    grepl("arguments imply differing number of rows", msg, fixed = TRUE) ||
      grepl("429", msg, fixed = TRUE)
  }

  # Wall-clock budget so a completely broken Overpass session can never
  # keep the user waiting more than a couple of minutes.
  outer_start <- Sys.time()
  max_wall_seconds <- 120

  for (attempt in seq_len(max_attempts)) {
    if (as.numeric(difftime(Sys.time(), outer_start, units = "secs")) >
        max_wall_seconds) {
      message("    Aborting: exceeded ", max_wall_seconds,
              "s wall-clock budget for name-filtered query")
      break
    }

    server <- overpass_servers[((attempt - 1) %% length(overpass_servers)) + 1]

    err_msg <- NULL
    result <- tryCatch({
      osmdata::set_overpass_url(server)
      osm_query <- osmdata::opq(bbox = bbox, timeout = timeout)
      osm_query <- osmdata::add_osm_feature(osm_query,
                                             key = "natural", value = "water")
      osm_query <- osmdata::add_osm_feature(osm_query,
                                             key = "name", value = name_pattern,
                                             value_exact = FALSE)
      osmdata::osmdata_sf(osm_query)
    }, error = function(e) {
      err_msg <<- conditionMessage(e)
      NULL
    })

    if (!is.null(result)) {
      osmdata::set_overpass_url(overpass_servers[1])
      return(result)
    }

    if (is_fatal(err_msg)) {
      message("    Overpass error (not retryable here): ", err_msg)
      break
    }

    if (attempt < max_attempts) {
      if (grepl("500|502|503|504|timeout|Timeout",
                err_msg, ignore.case = TRUE)) {
        wait_time <- attempt * 5
        message("    Server error, trying another server in ", wait_time, "s...")
        Sys.sleep(wait_time)
      } else {
        message("    Error: ", err_msg)
      }
    } else {
      message("    Failed after ", max_attempts, " attempts: ", err_msg)
    }
  }
  tryCatch(osmdata::set_overpass_url(overpass_servers[1]), error = function(e) NULL)
  return(NULL)
}

#' Download Water Bodies from OSM for a Single Bounding Box
#'
#' Queries the Overpass API for water bodies within a bounding box. Optionally
#' tries a name-filtered query first to reduce data volume.
#'
#' @param bbox_vec Named numeric vector with left, bottom, right, top
#' @param lake_names Optional character vector of lake names to try first
#' @param overpass_servers Character vector of Overpass API server URLs
#' @param name_only If TRUE, skip broad natural=water fallback when name query
#'   fails. Used in many-cluster mode to reduce API load.
#' @return List of sf data frames with water body polygons
#' @noRd
download_lake_osm_single <- function(bbox_vec, lake_names = NULL,
                                     overpass_servers = NULL,
                                     name_only = FALSE,
                                     timeout = 90,
                                     budget_exceeded = function() FALSE) {
  if (is.null(overpass_servers)) {
    overpass_servers <- c(
      "https://overpass-api.de/api/interpreter",
      "https://overpass.kumi.systems/api/interpreter",
      "https://maps.mail.ru/osm/tools/overpass/api/interpreter"
    )
  }

  # Errors that are deterministic and should not be retried (see the
  # comment in query_osm_by_name for details).
  is_fatal <- function(msg) {
    if (is.null(msg)) return(FALSE)
    grepl("arguments imply differing number of rows", msg, fixed = TRUE) ||
      grepl("429", msg, fixed = TRUE)
  }

  # Helper to query OSM with retries across multiple servers
  query_osm_robust <- function(bbox, key, value, max_attempts = 3) {
    # Wall-clock budget per query type. Combined with the fatal-error
    # short-circuit above, this caps total OSM wait to a couple of minutes
    # even if Overpass is completely broken.
    outer_start <- Sys.time()
    max_wall_seconds <- 90

    for (attempt in seq_len(max_attempts)) {
      if (as.numeric(difftime(Sys.time(), outer_start, units = "secs")) >
          max_wall_seconds) {
        message("    Aborting: exceeded ", max_wall_seconds,
                "s wall-clock budget for ", key, "=", value, " query")
        break
      }

      server <- overpass_servers[((attempt - 1) %% length(overpass_servers)) + 1]

      err_msg <- NULL
      result <- tryCatch({
        osmdata::set_overpass_url(server)
        osm_query <- osmdata::opq(bbox = bbox, timeout = timeout)
        osm_query <- osmdata::add_osm_feature(osm_query, key = key, value = value)
        osmdata::osmdata_sf(osm_query)
      }, error = function(e) {
        err_msg <<- conditionMessage(e)
        NULL
      })

      if (!is.null(result)) {
        osmdata::set_overpass_url(overpass_servers[1])
        return(result)
      }

      if (is_fatal(err_msg)) {
        message("    Overpass error (not retryable here): ", err_msg)
        break
      }

      if (attempt < max_attempts) {
        if (grepl("500|502|503|504|timeout|Timeout",
                  err_msg, ignore.case = TRUE)) {
          wait_time <- attempt * 3
          message("    Server error, trying another server in ", wait_time, "s...")
          Sys.sleep(wait_time)
        } else {
          message("    Error: ", err_msg)
        }
      } else {
        message("    Failed after ", max_attempts, " attempts: ", err_msg)
      }
    }
    tryCatch(osmdata::set_overpass_url(overpass_servers[1]), error = function(e) NULL)
    return(NULL)
  }

  water_list <- list()

  # Try name-filtered queries first if lake names are available
  name_query_sufficient <- FALSE
  if (!is.null(lake_names) && length(lake_names) > 0) {
    lake_names <- unique(lake_names[!is.na(lake_names) & nchar(trimws(lake_names)) > 0])
    if (length(lake_names) > 0) {
      message("    Trying name-filtered query for: ",
              paste(lake_names, collapse = ", "))
      for (lname in lake_names) {
        if (budget_exceeded()) {
          message("    Aborting name queries: total_timeout_s exceeded")
          break
        }
        osm_result <- query_osm_by_name(bbox_vec, lname, overpass_servers,
                                         timeout = max(timeout, 120))
        if (!is.null(osm_result)) {
          name_polys <- extract_osm_polys(osm_result)
          if (length(name_polys) > 0) {
            water_list <- c(water_list, name_polys)
            name_query_sufficient <- TRUE
          }
        }
      }
      if (name_query_sufficient) {
        message("    Name-filtered query found results, skipping broad query")
      }
    }
  }

  # Fall back to broad queries if name-filtered query didn't find anything
  # Skip broad fallback when name_only=TRUE (many-cluster mode to reduce API load)
  if (!name_query_sufficient && !name_only) {
    if (budget_exceeded()) {
      message("    Skipping broad queries: total_timeout_s exceeded")
    } else {
      message("    Querying natural=water...")
      osm_result <- query_osm_robust(bbox_vec, "natural", "water")
      if (!is.null(osm_result)) {
        water_list <- c(water_list, extract_osm_polys(osm_result))
      }
    }

    if (budget_exceeded()) {
      message("    Skipping water=lake query: total_timeout_s exceeded")
    } else {
      message("    Querying water=lake...")
      osm_result <- query_osm_robust(bbox_vec, "water", "lake")
      if (!is.null(osm_result)) {
        water_list <- c(water_list, extract_osm_polys(osm_result))
      }
    }
  } else if (!name_query_sufficient && name_only) {
    message("    No results for name query, skipping broad query (name-only mode)")
  }

  return(water_list)
}

#' Download Lake Boundary from OpenStreetMap
#'
#' Downloads lake boundaries from OpenStreetMap. For geographically spread-out
#' sites, clusters them and queries per-cluster to avoid downloading the entire
#' world's water bodies.
#'
#' @param sites_df Data frame with latitude and longitude columns
#' @param timeout Integer; Overpass API query timeout in seconds (default 90)
#'
#' @return A list with all_lakes, sites, and utm_epsg
#'
#' @noRd
download_lake_osm <- function(sites_df, timeout = 90,
                              total_timeout_s = 300) {

  message("Converting to spatial format...")

  # Disable S2 spherical geometry to avoid topology errors
  sf::sf_use_s2(FALSE)
  on.exit(sf::sf_use_s2(TRUE), add = TRUE)

  # Global wall-clock budget across all query attempts / clusters. If the
  # combined per-query budgets in query_osm_by_name / query_osm_robust plus
  # any per-cluster loops exceed this, we abort and return whatever we've
  # collected so far so the user is not stuck waiting minutes with no
  # feedback.
  overall_start <- Sys.time()
  budget_exceeded <- function() {
    is.finite(total_timeout_s) &&
      as.numeric(difftime(Sys.time(), overall_start, units = "secs")) >
      total_timeout_s
  }

  # Convert to sf object (WGS84). For data.frame input, detect lat/lon
  # columns flexibly (case-insensitive, accepts "lat"/"latitude", "lon"/"long"
  # /"longitude") so users don't have to pre-process with load_sites() if
  # their data already has standard-ish column names.
  if (inherits(sites_df, "sf")) {
    sites_sf <- sf::st_transform(sites_df, 4326)
  } else {
    col_lower <- tolower(names(sites_df))
    lat_idx <- which(col_lower %in% c("latitude", "lat", "y"))[1]
    if (is.na(lat_idx)) lat_idx <- grep("^lat", col_lower)[1]
    lon_idx <- which(col_lower %in% c("longitude", "lon", "long", "lng", "x"))[1]
    if (is.na(lon_idx)) lon_idx <- grep("^lon", col_lower)[1]
    if (is.na(lat_idx) || is.na(lon_idx)) {
      stop("Could not find latitude / longitude columns in input data.frame.\n",
           "  Available columns: ", paste(names(sites_df), collapse = ", "), "\n",
           "  Either rename columns to 'latitude' / 'longitude' or pass the\n",
           "  data through load_sites() first.", call. = FALSE)
    }
    sites_sf <- sf::st_as_sf(sites_df,
                              coords = c(names(sites_df)[lon_idx],
                                         names(sites_df)[lat_idx]),
                              crs = 4326)
  }

  # Overpass servers to try (main server often overloaded)
  overpass_servers <- c(
    "https://overpass-api.de/api/interpreter",
    "https://overpass.kumi.systems/api/interpreter",
    "https://maps.mail.ru/osm/tools/overpass/api/interpreter"
  )

  # Detect lake name column if available
  lake_name_col <- NULL
  for (col in c("lake.name", "lake_name", "lakename", "Lake", "lake", "waterbody")) {
    if (col %in% names(sites_sf)) {
      lake_name_col <- col
      break
    }
  }

  # Create bounding box and check site spread
  bbox <- sf::st_bbox(sites_sf)
  site_spread <- max(bbox["xmax"] - bbox["xmin"], bbox["ymax"] - bbox["ymin"])

  message("Downloading lake boundaries from OpenStreetMap...")

  water_list <- list()

  if (site_spread <= 0.5) {
    # --- Small spread: single bbox query (current behavior) ---
    bbox_buffer <- min(0.05, max(0.01, site_spread * 0.1))
    bbox[1] <- bbox[1] - bbox_buffer
    bbox[2] <- bbox[2] - bbox_buffer
    bbox[3] <- bbox[3] + bbox_buffer
    bbox[4] <- bbox[4] + bbox_buffer

    bbox_vec <- c(bbox["xmin"], bbox["ymin"], bbox["xmax"], bbox["ymax"])
    names(bbox_vec) <- c("left", "bottom", "right", "top")

    message("  Bounding box: [", paste(round(bbox_vec, 4), collapse = ", "), "]")

    # Get lake names for name-filtered query
    cluster_lake_names <- NULL
    if (!is.null(lake_name_col)) {
      cluster_lake_names <- unique(sites_sf[[lake_name_col]])
    }

    water_list <- download_lake_osm_single(bbox_vec, cluster_lake_names,
                                           overpass_servers, timeout = timeout,
                                           budget_exceeded = budget_exceeded)
  } else {
    # --- Large spread ---
    # Fast path: if every site has a known lake name, issue a single
    # name-filtered Overpass query covering the whole site bbox. The name
    # regex is selective enough that returns stay small, and one query is
    # dramatically faster than per-cluster broad queries.
    all_named <- !is.null(lake_name_col) &&
      all(!is.na(sites_sf[[lake_name_col]]) &
          nchar(trimws(as.character(sites_sf[[lake_name_col]]))) > 0)

    if (isTRUE(all_named)) {
      unique_names <- unique(sites_sf[[lake_name_col]])
      message("  Site spread is ", round(site_spread, 1),
              " degrees with all sites named - using single name-filtered query for ",
              length(unique_names), " lake(s)")
      bbox_vec_full <- c(bbox["xmin"], bbox["ymin"],
                         bbox["xmax"], bbox["ymax"])
      names(bbox_vec_full) <- c("left", "bottom", "right", "top")
      name_res <- query_osm_by_name(bbox_vec_full, unique_names,
                                     overpass_servers,
                                     timeout = max(timeout, 120))
      if (!is.null(name_res)) {
        water_list <- c(water_list, extract_osm_polys(name_res))
      }
      tryCatch(osmdata::set_overpass_url(overpass_servers[1]),
               error = function(e) NULL)
    } else {

    # --- Per-cluster small bbox queries (used when lake names are missing) ---
    # Each cluster gets a tiny bbox (~0.1 degree) and a single natural=water
    # query. Small bboxes return quickly with just nearby water bodies.
    clusters <- cluster_sites(sites_sf)
    n_clusters <- length(clusters)

    message("  Site spread is ", round(site_spread, 1),
            " degrees - querying ", n_clusters, " site clusters")

    # Progress bar for interactive sessions
    use_pb <- interactive()
    if (use_pb) {
      pb <- utils::txtProgressBar(min = 0, max = n_clusters, style = 3)
    }
    start_time <- Sys.time()

    # Track failed clusters and rotate servers
    query_count <- 0
    failed_clusters <- integer(0)

    for (ci in seq_along(clusters)) {
      if (budget_exceeded()) {
        message("  Aborting cluster loop: exceeded ", total_timeout_s,
                "s total_timeout_s budget (", ci - 1, "/",
                n_clusters, " clusters processed)")
        break
      }
      cluster_idx <- clusters[[ci]]
      cluster_sf <- sites_sf[cluster_idx, ]

      # Compute small bbox around cluster sites with 0.05 degree buffer
      cl_bbox <- sf::st_bbox(cluster_sf)
      cl_buffer <- 0.05  # ~5.5 km buffer around site(s)
      cl_bbox[1] <- cl_bbox[1] - cl_buffer
      cl_bbox[2] <- cl_bbox[2] - cl_buffer
      cl_bbox[3] <- cl_bbox[3] + cl_buffer
      cl_bbox[4] <- cl_bbox[4] + cl_buffer

      cl_bbox_vec <- c(cl_bbox["xmin"], cl_bbox["ymin"],
                       cl_bbox["xmax"], cl_bbox["ymax"])
      names(cl_bbox_vec) <- c("left", "bottom", "right", "top")

      # Single natural=water query per cluster (small bbox = fast)
      # Try up to 3 attempts across different servers
      cluster_success <- FALSE
      for (attempt in 1:3) {
        if (cluster_success) break
        server_idx <- ((query_count + attempt - 1) %% length(overpass_servers)) + 1
        tryCatch({
          osmdata::set_overpass_url(overpass_servers[server_idx])
          osm_query <- osmdata::opq(bbox = cl_bbox_vec, timeout = 30)
          osm_query <- osmdata::add_osm_feature(osm_query,
                                                 key = "natural", value = "water")
          osm_result <- osmdata::osmdata_sf(osm_query)
          water_list <- c(water_list, extract_osm_polys(osm_result))
          cluster_success <- TRUE
        }, error = function(e) {
          if (attempt < 3) {
            Sys.sleep(2)
          }
        })
      }
      if (!cluster_success) {
        failed_clusters <- c(failed_clusters, ci)
      }

      query_count <- query_count + 1

      # Update progress bar
      if (use_pb) {
        utils::setTxtProgressBar(pb, ci)
      }

      # Rate limit: 1 second between queries to respect Overpass usage policy
      if (ci < n_clusters) {
        Sys.sleep(1)
      }
    }

    if (use_pb) {
      close(pb)
    }

    elapsed <- as.numeric(difftime(Sys.time(), start_time, units = "mins"))
    message("  Download complete in ", round(elapsed, 1), " minutes")

    if (length(failed_clusters) > 0) {
      message("  WARNING: ", length(failed_clusters),
              " cluster(s) failed after 3 attempts: ",
              paste(failed_clusters, collapse = ", "))
      message("  Some lake boundaries may be missing. ",
              "Try running again or provide a local boundary file.")
    }

    # Reset to default server
    tryCatch(osmdata::set_overpass_url(overpass_servers[1]),
             error = function(e) NULL)
    }  # close the !all_named (per-cluster) branch
  }

  # Auto-detect UTM zone from sites. Critical: use sites_sf (already
  # transformed to WGS84 / lon-lat) rather than the raw sites_df, which could
  # be in a projected CRS (e.g., UTM). Using raw projected coordinates here
  # produced garbage EPSG codes like 32683364 from inputs already in UTM.
  centroid_ll <- suppressWarnings(
    sf::st_coordinates(sf::st_centroid(sf::st_union(sites_sf)))
  )
  utm_zone <- floor((centroid_ll[1] + 180) / 6) + 1
  utm_epsg <- ifelse(centroid_ll[2] >= 0,
                     as.numeric(paste0("326", sprintf("%02d", utm_zone))),
                     as.numeric(paste0("327", sprintf("%02d", utm_zone))))

  # Check if we found ANY water bodies
  if (length(water_list) == 0) {
    warning("No water bodies were returned by OpenStreetMap for the given ",
            "site coordinates. This can happen when: (a) the lake is too small ",
            "to be mapped in OSM (e.g., small ponds); (b) all Overpass API ",
            "queries failed (server rate-limiting, timeouts, or a parsing ",
            "error inside the osmdata package). Sites without lake boundaries ",
            "will receive NA fetch values.\n",
            "Workarounds:\n",
            "  - Try again later (Overpass server load varies).\n",
            "  - Supply your own boundary file: ",
            "get_lake_boundary(sites, file = 'your_boundary.gpkg')\n",
            "  - Increase the timeout: get_lake_boundary(sites, timeout = 300)",
            call. = FALSE)

    # Return empty lake set so downstream processing can assign NAs
    sites_utm_temp <- sf::st_transform(sites_sf, utm_epsg)
    empty_lakes <- sf::st_sf(
      osm_id = character(0),
      name = character(0),
      area_km2 = numeric(0),
      geometry = sf::st_sfc(crs = utm_epsg)
    )
    return(list(
      all_lakes = empty_lakes,
      sites = sites_utm_temp,
      utm_epsg = utm_epsg
    ))
  }

  # Combine all water polygons
  message("  Combining results...")
  all_water <- do.call(rbind, water_list)
  message("  Total water bodies found: ", nrow(all_water))

  message("  Auto-detected UTM Zone: ", utm_zone,
          ifelse(centroid_ll[2] >= 0, "N", "S"))

  # Transform EVERYTHING to UTM before spatial operations
  message("  Transforming to UTM for analysis...")
  all_water_utm <- sf::st_transform(all_water, utm_epsg)
  sites_utm <- sf::st_transform(sites_sf, utm_epsg)

  # Process each lake polygon - extract largest part if multipolygon
  message("  Processing lake polygons...")
  processed_lakes <- list()

  for (i in seq_len(nrow(all_water_utm))) {
    tryCatch({
      lake_poly <- all_water_utm[i, ]

      # Skip empty geometries
      if (sf::st_is_empty(lake_poly)) next

      geom_type <- sf::st_geometry_type(lake_poly, by_geometry = FALSE)

      if (geom_type %in% c("MULTIPOLYGON", "GEOMETRYCOLLECTION")) {
        lake_parts <- sf::st_cast(lake_poly, "POLYGON")
        lake_areas <- sf::st_area(lake_parts)
        lake_poly <- lake_parts[which.max(lake_areas), ]
      }

      validity <- sf::st_is_valid(lake_poly)
      if (is.na(validity) || !all(validity)) {
        lake_poly <- sf::st_make_valid(lake_poly)
      }

      processed_lakes[[length(processed_lakes) + 1]] <- lake_poly
    }, error = function(e) {
      # Skip problematic geometries silently
      NULL
    })
  }

  all_lakes_utm <- do.call(rbind, processed_lakes)

  # Calculate areas for reference
  all_lakes_utm$area_km2 <- as.numeric(sf::st_area(all_lakes_utm)) / 1e6

  # Filter out tiny water bodies (< 0.0001 km2 = 100 m2)
  min_area_km2 <- 0.0001
  tiny_mask <- all_lakes_utm$area_km2 < min_area_km2
  n_tiny <- sum(tiny_mask)
  if (n_tiny > 0) {
    message("  Filtered ", n_tiny, " water bodies < ", min_area_km2, " km2")
    all_lakes_utm <- all_lakes_utm[!tiny_mask, ]
  }

  # Deduplicate by osm_id (keep largest polygon if duplicates)
  unique_ids <- unique(all_lakes_utm$osm_id)
  if (length(unique_ids) < nrow(all_lakes_utm)) {
    message("  Deduplicating ", nrow(all_lakes_utm) - length(unique_ids),
            " duplicate lake entries...")
    deduped_list <- lapply(unique_ids, function(id) {
      matches <- all_lakes_utm[all_lakes_utm$osm_id == id, ]
      if (nrow(matches) > 1) {
        matches[which.max(matches$area_km2), ]
      } else {
        matches
      }
    })
    all_lakes_utm <- do.call(rbind, deduped_list)
  }

  message("  Processed ", nrow(all_lakes_utm), " unique lake polygons")
  message("  Total area: ", round(sum(all_lakes_utm$area_km2), 2), " km2")

  return(list(
    all_lakes = all_lakes_utm,
    sites = sites_utm,
    utm_epsg = utm_epsg
  ))
}

#' Standardize and Validate OSM sf Data
#'
#' @param osm_sf sf object from osmdata
#' @return Standardized sf object with osm_id, name, geometry
#' @noRd
standardize_osm_sf <- function(osm_sf) {
  if (is.null(osm_sf) || nrow(osm_sf) == 0) return(NULL)

  # Make geometries valid first
  osm_sf <- sf::st_make_valid(osm_sf)

  # Find columns case-insensitively
  col_names_lower <- tolower(names(osm_sf))
  osm_id_col <- which(col_names_lower == "osm_id")[1]
  name_col <- which(col_names_lower == "name")[1]

  # Extract only essential columns
  result <- data.frame(
    osm_id = if (!is.na(osm_id_col)) as.character(osm_sf[[osm_id_col]]) else NA_character_,
    name = if (!is.na(name_col)) as.character(osm_sf[[name_col]]) else NA_character_,
    stringsAsFactors = FALSE
  )
  result <- sf::st_sf(result, geometry = sf::st_geometry(osm_sf))

  # Remove any invalid geometries
  valid_mask <- sf::st_is_valid(result)
  if (!all(valid_mask)) {
    message("      (Removed ", sum(!valid_mask), " invalid geometries)")
    result <- result[valid_mask, ]
  }

  return(result)
}

#' Load Lake from Local File
#'
#' @param sites_df Data frame with latitude and longitude columns
#' @param lake_file_path Path to shapefile or geopackage
#'
#' @return A list with all_lakes, sites, and utm_epsg
#'
#' @noRd
load_lake_file <- function(sites_df, lake_file_path) {

  message("Loading lake boundary from file: ", lake_file_path)

  # Convert sites to sf
  if (inherits(sites_df, "sf")) {
    sites_sf <- sf::st_transform(sites_df, 4326)
  } else {
    sites_sf <- sf::st_as_sf(sites_df,
                              coords = c("longitude", "latitude"),
                              crs = 4326)
  }

  # Load lake shapefile
  lake_wgs84 <- sf::st_read(lake_file_path, quiet = TRUE)

  # Robustly check if already in WGS84; compare CRS objects rather than
  # relying on the $input string, which varies across sf/PROJ versions
  if (!isTRUE(sf::st_crs(lake_wgs84) == sf::st_crs(4326))) {
    lake_wgs84 <- sf::st_transform(lake_wgs84, 4326)
  }

  # Auto-detect UTM zone
  lake_centroid <- sf::st_coordinates(sf::st_centroid(sf::st_union(lake_wgs84)))
  utm_zone <- floor((lake_centroid[1] + 180) / 6) + 1
  utm_epsg <- ifelse(lake_centroid[2] >= 0,
                     as.numeric(paste0("326", sprintf("%02d", utm_zone))),
                     as.numeric(paste0("327", sprintf("%02d", utm_zone))))

  # Transform to UTM
  lake_utm <- sf::st_transform(lake_wgs84, utm_epsg)
  sites_utm <- sf::st_transform(sites_sf, utm_epsg)

  # Extract largest polygon if needed
  geom_type <- sf::st_geometry_type(lake_utm, by_geometry = FALSE)
  if (any(geom_type %in% c("MULTIPOLYGON", "GEOMETRYCOLLECTION"))) {
    lake_parts <- sf::st_cast(lake_utm, "POLYGON")
    lake_areas <- sf::st_area(lake_parts)
    lake_utm <- lake_parts[which.max(lake_areas), ]
  }

  if (!all(sf::st_is_valid(lake_utm))) {
    lake_utm <- sf::st_make_valid(lake_utm)
  }

  lake_area_km2 <- as.numeric(sf::st_area(lake_utm)) / 1e6
  message("  Lake area: ", round(lake_area_km2, 2), " km2")

  # Add required columns for multi-lake compatibility
  lake_utm$osm_id <- "local_file"
  lake_utm$name <- basename(lake_file_path)
  lake_utm$area_km2 <- lake_area_km2

  return(list(
    all_lakes = lake_utm,
    sites = sites_utm,
    utm_epsg = utm_epsg
  ))
}

#' Assign Sites to Lakes
#'
#' Perform spatial join to assign each site to its containing lake polygon.
#'
#' @param sites_sf sf object with site points
#' @param water_polygons sf object with lake polygons
#' @param tolerance_m Buffer distance in meters for matching sites that fall
#'   just outside lake polygons (e.g., due to GPS noise or coarse OSM
#'   boundaries). Default is the value from \code{lakefetch_options()} (50 m).
#'   For datasets with appreciable GPS error or for very coarsely mapped
#'   shorelines, try 100-500 m.
#'
#' @return sf object with sites and added columns for lake_osm_id, lake_name, lake_area_km2
#'
#' @examplesIf interactive()
#' csv_path <- system.file("extdata", "sample_sites.csv", package = "lakefetch")
#' sites <- load_sites(csv_path)
#' lake_data <- get_lake_boundary(sites)
#'
#' # Assign sites to their containing lakes. Default tolerance (50 m) is
#' # appropriate for sites with accurate coordinates that fall inside the
#' # lake polygon. If your sites are near the shoreline or your GPS error is
#' # larger, increase tolerance_m (e.g., 200-500 m).
#' sites_assigned <- assign_sites_to_lakes(
#'   lake_data$sites,
#'   lake_data$all_lakes,
#'   tolerance_m = 200
#' )
#'
#' # Check assignments
#' table(sites_assigned$lake_name)
#'
#' @export
assign_sites_to_lakes <- function(sites_sf, water_polygons, tolerance_m = NULL) {

  if (is.null(tolerance_m)) {
    tolerance_m <- get_opt("gps_tolerance_m")
  }

  message("Assigning sites to lakes...")

  n_sites <- nrow(sites_sf)
  sites_sf$lake_osm_id <- NA_character_
  sites_sf$lake_name <- NA_character_
  sites_sf$lake_area_km2 <- NA_real_

  # First pass: direct intersection
  message("  Checking direct intersections...")
  sites_in_lakes <- sf::st_join(sites_sf, water_polygons, join = sf::st_intersects, left = TRUE)

  # Handle sites that matched directly
  direct_matches <- !is.na(sites_in_lakes$osm_id)
  if (any(direct_matches)) {
    for (i in which(direct_matches)) {
      site_name <- sites_in_lakes$Site[i]
      if (is.na(sites_sf$lake_osm_id[sites_sf$Site == site_name])) {
        sites_sf$lake_osm_id[sites_sf$Site == site_name] <- sites_in_lakes$osm_id[i]
        sites_sf$lake_name[sites_sf$Site == site_name] <- sites_in_lakes$name[i]
        lake_idx <- which(water_polygons$osm_id == sites_in_lakes$osm_id[i])[1]
        if (!is.na(lake_idx) && "area_km2" %in% names(water_polygons)) {
          sites_sf$lake_area_km2[sites_sf$Site == site_name] <- water_polygons$area_km2[lake_idx]
        }
      }
    }
    message("    ", sum(!is.na(sites_sf$lake_osm_id)), " sites matched directly")
  }

  # Second pass: buffer check for unmatched sites
  # Only match sites that are close to the SHORELINE of a lake (not just near the lake polygon)
  # This prevents assigning points to the wrong lake when multiple lakes are nearby
  unmatched_idx <- which(is.na(sites_sf$lake_osm_id))

  if (length(unmatched_idx) > 0 && tolerance_m > 0) {
    message("  Checking ", length(unmatched_idx),
            " unmatched sites with ", tolerance_m, "m tolerance...")

    for (i in unmatched_idx) {
      site_geom <- sf::st_geometry(sites_sf)[i]

      # Find distance to the BOUNDARY (shoreline) of each lake, not just the polygon
      # This ensures we only match sites that are genuinely close to a lake edge
      shoreline_distances <- vapply(seq_len(nrow(water_polygons)), function(j) {
        lake_boundary <- tryCatch({
          sf::st_boundary(water_polygons[j, ])
        }, error = function(e) {
          sf::st_cast(water_polygons[j, ], "MULTILINESTRING")
        })
        as.numeric(sf::st_distance(site_geom, lake_boundary))
      }, numeric(1))

      # Find lakes within tolerance distance of their shoreline
      within_tolerance <- which(shoreline_distances <= tolerance_m)

      if (length(within_tolerance) > 0) {
        # Pick the lake with the closest shoreline
        closest_idx <- within_tolerance[which.min(shoreline_distances[within_tolerance])]
        lake_match <- water_polygons[closest_idx, ]

        sites_sf$lake_osm_id[i] <- lake_match$osm_id
        sites_sf$lake_name[i] <- lake_match$name
        if ("area_km2" %in% names(lake_match)) {
          sites_sf$lake_area_km2[i] <- lake_match$area_km2
        }
      }
    }

    matched_after_buffer <- sum(!is.na(sites_sf$lake_osm_id))
    matched_before_buffer <- n_sites - length(unmatched_idx)
    buffer_matches <- matched_after_buffer - matched_before_buffer
    message("    ", buffer_matches, " additional sites matched within tolerance")
  }

  # Third pass: name-based matching for remaining unmatched sites
  # If sites have a lake.name or similar column, try to match by name
  unmatched_idx <- which(is.na(sites_sf$lake_osm_id))
  if (length(unmatched_idx) > 0) {
    # Check if sites have a lake name column we can use
    site_lake_col <- NULL
    for (col in c("lake.name", "lake_name", "lakename", "Lake", "lake", "waterbody")) {
      if (col %in% names(sites_sf)) {
        site_lake_col <- col
        break
      }
    }

    if (!is.null(site_lake_col)) {
      message("  Trying name-based matching using '", site_lake_col, "' column...")

      # Get unique lake names from unmatched sites
      unmatched_lake_names <- unique(sites_sf[[site_lake_col]][unmatched_idx])
      unmatched_lake_names <- unmatched_lake_names[!is.na(unmatched_lake_names)]

      # Get OSM lake names (lowercase for comparison)
      osm_names <- tolower(trimws(water_polygons$name))
      osm_names[is.na(osm_names)] <- ""

      for (site_lake_name in unmatched_lake_names) {
        site_lake_lower <- tolower(trimws(site_lake_name))

        # Try exact match first
        exact_match <- which(osm_names == site_lake_lower)

        # If no exact match, try partial/fuzzy matching
        if (length(exact_match) == 0) {
          # Check if OSM name contains site lake name or vice versa
          partial_match <- which(
            grepl(site_lake_lower, osm_names, fixed = TRUE) |
            vapply(osm_names, function(x) grepl(x, site_lake_lower, fixed = TRUE) && nchar(x) > 3, logical(1))
          )
          if (length(partial_match) > 0) {
            exact_match <- partial_match[1]
          }
        }

        if (length(exact_match) > 0) {
          # Found a matching lake - assign all unmatched sites with this lake name
          lake_match <- water_polygons[exact_match[1], ]
          site_indices <- unmatched_idx[sites_sf[[site_lake_col]][unmatched_idx] == site_lake_name]
          name_match_tolerance <- tolerance_m * 5

          matched_count <- 0
          skipped_sites <- character(0)
          for (idx in site_indices) {
            site_dist <- tryCatch(
              as.numeric(sf::st_distance(sf::st_geometry(sites_sf)[idx], lake_match)),
              error = function(e) NA_real_
            )
            if (!is.na(site_dist) && site_dist <= name_match_tolerance) {
              sites_sf$lake_osm_id[idx] <- lake_match$osm_id
              sites_sf$lake_name[idx] <- lake_match$name
              if ("area_km2" %in% names(lake_match)) {
                sites_sf$lake_area_km2[idx] <- lake_match$area_km2
              }
              matched_count <- matched_count + 1
            } else {
              skipped_sites <- c(skipped_sites, sites_sf$Site[idx])
            }
          }
          if (matched_count > 0) {
            message("    Matched ", matched_count, " sites to '", lake_match$name,
                    "' via name matching from '", site_lake_name, "'")
          }
          if (length(skipped_sites) > 0) {
            warning("Skipped ", length(skipped_sites), " site(s) named '", site_lake_name,
                    "': too far from OSM lake '", lake_match$name,
                    "' (>", name_match_tolerance, "m). ",
                    "Site(s) may be on a different water body. ",
                    "Sites: ", paste(skipped_sites, collapse = ", "))
          }
        } else {
          # Report unmatched lake name
          n_unmatched_this_lake <- sum(sites_sf[[site_lake_col]][unmatched_idx] == site_lake_name)
          message("    WARNING: No OSM lake found matching '", site_lake_name,
                  "' (", n_unmatched_this_lake, " sites)")
        }
      }
    }
  }

  # Summary
  matched <- sum(!is.na(sites_sf$lake_osm_id))
  unmatched <- sum(is.na(sites_sf$lake_osm_id))

  message("  Site assignment summary:")
  message("    Matched: ", matched, "/", n_sites)

  if (unmatched > 0) {
    message("    Unmatched: ", unmatched)

    # Provide diagnostic information for unmatched sites
    unmatched_sites <- sites_sf[is.na(sites_sf$lake_osm_id), ]
    unmatched_coords <- sf::st_coordinates(unmatched_sites)

    # Check if unmatched sites have a lake name column
    lake_col <- NULL
    for (col in c("lake.name", "lake_name", "lakename", "Lake", "lake", "waterbody")) {
      if (col %in% names(unmatched_sites)) {
        lake_col <- col
        break
      }
    }

    unmatched_lakes <- NULL
    if (!is.null(lake_col)) {
      unmatched_lakes <- unique(unmatched_sites[[lake_col]])
      message("    Unmatched sites claim to be in: ", paste(unmatched_lakes, collapse = ", "))
    }

    # Show coordinate range of unmatched sites
    message("    Unmatched coordinate range:")
    message("      Lat: ", round(min(unmatched_coords[, 2]), 4), " to ",
            round(max(unmatched_coords[, 2]), 4))
    message("      Lon: ", round(min(unmatched_coords[, 1]), 4), " to ",
            round(max(unmatched_coords[, 1]), 4))

    # Suggest increasing tolerance or checking OSM
    message("    TIP: Try lakefetch_options(gps_tolerance_m = 100) for larger buffer")
    message("    TIP: Check if the lake exists in OpenStreetMap at openstreetmap.org")

    # Promote to a warning so it is surfaced through warnings() and not lost
    # in the message stream. Affected sites get NA fetch downstream, which is
    # easy to miss if the user only skims the console.
    warning(unmatched, " site(s) could not be assigned to any lake polygon",
            if (!is.null(unmatched_lakes))
              paste0(" (claimed lakes: ", paste(unmatched_lakes, collapse = ", "), ")")
            else "",
            ". These sites will receive NA fetch values.",
            " See the diagnostic messages above for coordinate ranges",
            " and try increasing tolerance_m if the sites are near a shoreline.",
            call. = FALSE)
  }

  # Clean up lake names - look up from water_polygons if name is NA
  for (i in which(is.na(sites_sf$lake_name) & !is.na(sites_sf$lake_osm_id))) {
    lake_id <- sites_sf$lake_osm_id[i]
    lake_area <- sites_sf$lake_area_km2[i]

    # First try: Find any polygon with this osm_id that has a name
    matching_polys <- water_polygons[water_polygons$osm_id == lake_id, ]
    if (nrow(matching_polys) > 0) {
      poly_names <- matching_polys$name[!is.na(matching_polys$name) & matching_polys$name != ""]
      if (length(poly_names) > 0) {
        sites_sf$lake_name[i] <- poly_names[1]
        next
      }
    }

    # Second try: Find another lake with very similar area (likely duplicate polygon)
    # This handles cases where OSM has the same lake with different IDs
    if (!is.na(lake_area) && lake_area > 0) {
      area_tolerance <- 0.01  # 1% tolerance
      similar_area_idx <- which(
        abs(water_polygons$area_km2 - lake_area) / lake_area < area_tolerance &
        !is.na(water_polygons$name) &
        water_polygons$name != ""
      )
      if (length(similar_area_idx) > 0) {
        sites_sf$lake_name[i] <- water_polygons$name[similar_area_idx[1]]
      }
    }
  }

  # Final fallback for any still-missing names
  sites_sf$lake_name <- ifelse(
    is.na(sites_sf$lake_name) & !is.na(sites_sf$lake_osm_id),
    paste0("Unknown Lake (ID: ", sites_sf$lake_osm_id, ")"),
    sites_sf$lake_name
  )

  # Show lakes with sites
  if (matched > 0) {
    lake_counts <- table(sites_sf$lake_osm_id, useNA = "no")
    message("  Sites per lake:")
    for (lake_id in names(lake_counts)) {
      lake_nm <- sites_sf$lake_name[sites_sf$lake_osm_id == lake_id][1]
      if (is.na(lake_nm)) lake_nm <- paste0("ID: ", lake_id)
      message("    ", lake_nm, ": ", lake_counts[lake_id], " sites")
    }
  }

  return(sites_sf)
}
