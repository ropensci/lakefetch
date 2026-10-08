# ==============================================================================
# NHD/LAGOS Integration Functions
# ==============================================================================

#' Add Lake Context from NHD
#'
#' Add hydrological context to fetch results using the National Hydrography
#' Dataset (NHD). Includes outlet/inlet locations, watershed area, connectivity
#' classification, and stream order.
#'
#' @param fetch_results sf object with fetch calculation results
#' @param lake_polygons sf object with lake polygons
#' @param utm_epsg EPSG code for UTM projection
#'
#' @return sf object with additional columns for NHD context
#'
#' @details
#' Requires the nhdplusTools package. If not available, returns the input
#' with NA columns added for consistent output format.
#'
#' Added columns include:
#' \itemize{
#'   \item nhd_permanent_id: NHD permanent identifier
#'   \item nhd_gnis_name: GNIS name from NHD
#'   \item nhd_areasqkm: Area in square kilometers from NHD
#'   \item outlet_dist_m: Distance to outlet in meters
#'   \item outlet_bearing: Compass direction to outlet
#'   \item inlet_nearest_dist_m: Distance to nearest inlet
#'   \item inlet_nearest_bearing: Compass direction to nearest inlet
#'   \item inlet_count: Number of inlets
#'   \item connectivity_class: Headwater/Drainage/Terminal/Isolated
#'   \item outlet_stream_order: Strahler stream order at outlet
#'   \item watershed_area_ha: Watershed area in hectares
#'   \item lake_watershed_ratio: Lake area / watershed area
#' }
#'
#' @examplesIf interactive()
#' csv_path <- system.file("extdata", "sample_sites.csv", package = "lakefetch")
#' sites <- load_sites(csv_path)
#' lake <- get_lake_boundary(sites)
#' results <- fetch_calculate(sites, lake)
#' results_with_context <- add_lake_context(results$results, results$lakes, lake$utm_epsg)
#'
#' @export
add_lake_context <- function(fetch_results, lake_polygons, utm_epsg) {

  # Initialize NA columns so output format is consistent
  fetch_results$nhd_permanent_id <- NA_character_
  fetch_results$nhd_gnis_name <- NA_character_
  fetch_results$nhd_areasqkm <- NA_real_
  fetch_results$outlet_dist_m <- NA_real_
  fetch_results$outlet_bearing <- NA_character_
  fetch_results$inlet_nearest_dist_m <- NA_real_
  fetch_results$inlet_nearest_bearing <- NA_character_
  fetch_results$inlet_count <- NA_integer_
  fetch_results$connectivity_class <- NA_character_
  fetch_results$outlet_stream_order <- NA_integer_
  fetch_results$watershed_area_ha <- NA_real_
  fetch_results$lake_watershed_ratio <- NA_real_

  if (!nhd_available()) {
    message("Skipping lake context (nhdplusTools not available)")
    message("Install with: install.packages('nhdplusTools')")
    return(fetch_results)
  }

  message("Adding lake context (NHD integration)...")

  # Skip NHD integration entirely if no lakes were matched. This happens when
  # all sites fail assignment (e.g., a site provided as being in "Long Lake"
  # but coordinates were >500m from any OSM Long Lake polygon). In that case
  # the bbox would contain NAs and nhdplusTools::get_waterbodies() would
  # error out with "!anyNA(x) is not TRUE".
  if (is.null(lake_polygons) || nrow(lake_polygons) == 0) {
    message("  No matched lake polygons - skipping NHD lookup")
    message("Lake context complete.")
    return(fetch_results)
  }

  # Get bounding box for all lakes
  lake_polygons_wgs84 <- sf::st_transform(lake_polygons, 4326)
  bbox <- sf::st_bbox(lake_polygons_wgs84)

  # Defensive check: a bbox with any NA coordinates will fail downstream.
  # This can happen if lake_polygons contains only empty geometries.
  if (anyNA(as.numeric(bbox))) {
    message("  Lake bounding box contains NA - skipping NHD lookup")
    message("Lake context complete.")
    return(fetch_results)
  }

  bbox[1] <- bbox[1] - 0.1
  bbox[2] <- bbox[2] - 0.1
  bbox[3] <- bbox[3] + 0.1
  bbox[4] <- bbox[4] + 0.1

  # Fetch NHD waterbodies for the area
  nhd_waterbodies <- get_nhd_waterbodies(sf::st_as_sfc(bbox))

  # Process each unique lake
  unique_lake_ids <- unique(fetch_results$lake_osm_id)
  unique_lake_ids <- unique_lake_ids[!is.na(unique_lake_ids)]

  for (lake_id in unique_lake_ids) {
    message("  Processing lake: ", lake_id)

    # Get the lake polygon
    lake_idx <- which(lake_polygons$osm_id == lake_id)[1]
    if (is.na(lake_idx)) next

    lake_poly_wgs84 <- sf::st_transform(lake_polygons[lake_idx, ], 4326)

    # Match to NHD
    matched_nhd <- match_lake_to_nhd(lake_poly_wgs84, nhd_waterbodies)

    # Get lake context
    context <- get_lake_context_internal(matched_nhd)

    # Get outlets and inlets
    outlets_inlets <- get_outlets_inlets(matched_nhd, lake_poly_wgs84)

    # Get stream order from outlet flowline
    stream_order <- get_stream_order(outlets_inlets$outlet_flowline)

    # Derive connectivity class
    has_outlet <- !is.null(outlets_inlets$outlet) && nrow(outlets_inlets$outlet) > 0
    has_inlet <- !is.null(outlets_inlets$inlets) && nrow(outlets_inlets$inlets) > 0
    connectivity <- get_connectivity_class(has_outlet, has_inlet)

    # Get watershed area
    watershed_ha <- get_watershed_area(matched_nhd, lake_poly_wgs84)

    # Calculate lake:watershed ratio
    lake_area_ha <- ifelse(!is.na(context$nhd_areasqkm),
                            context$nhd_areasqkm * 100,
                            NA_real_)
    lake_ws_ratio <- ifelse(!is.na(lake_area_ha) && !is.na(watershed_ha) && watershed_ha > 0,
                             lake_area_ha / watershed_ha,
                             NA_real_)

    # Find sites in this lake
    site_idx <- which(fetch_results$lake_osm_id == lake_id)

    # Apply context to all sites in this lake
    for (i in site_idx) {
      fetch_results$nhd_permanent_id[i] <- context$nhd_permanent_id
      fetch_results$nhd_gnis_name[i] <- context$nhd_gnis_name
      fetch_results$nhd_areasqkm[i] <- context$nhd_areasqkm
      fetch_results$connectivity_class[i] <- connectivity
      fetch_results$outlet_stream_order[i] <- stream_order
      fetch_results$watershed_area_ha[i] <- watershed_ha
      fetch_results$lake_watershed_ratio[i] <- lake_ws_ratio

      # Get site location
      site_pt_wgs84 <- sf::st_transform(fetch_results[i, ], 4326)

      # Calculate distance to outlet
      if (has_outlet) {
        outlet_pt <- outlets_inlets$outlet[1, ]
        outlet_calc <- calc_distance_bearing(
          sf::st_geometry(site_pt_wgs84),
          sf::st_geometry(outlet_pt),
          utm_epsg
        )
        fetch_results$outlet_dist_m[i] <- outlet_calc$dist_m
        fetch_results$outlet_bearing[i] <- outlet_calc$bearing
      }

      # Calculate distance to nearest inlet
      if (has_inlet) {
        fetch_results$inlet_count[i] <- nrow(outlets_inlets$inlets)

        inlet_dists <- vapply(seq_len(nrow(outlets_inlets$inlets)), function(j) {
          calc_distance_bearing(
            sf::st_geometry(site_pt_wgs84),
            sf::st_geometry(outlets_inlets$inlets[j, ]),
            utm_epsg
          )$dist_m
        }, numeric(1))

        nearest_inlet_idx <- which.min(inlet_dists)
        nearest_inlet <- outlets_inlets$inlets[nearest_inlet_idx, ]
        inlet_calc <- calc_distance_bearing(
          sf::st_geometry(site_pt_wgs84),
          sf::st_geometry(nearest_inlet),
          utm_epsg
        )
        fetch_results$inlet_nearest_dist_m[i] <- inlet_calc$dist_m
        fetch_results$inlet_nearest_bearing[i] <- inlet_calc$bearing
      } else {
        fetch_results$inlet_count[i] <- 0L
      }
    }
  }

  message("Lake context complete.")
  return(fetch_results)
}

#' Get NHD Waterbodies for a Bounding Box
#'
#' @param bbox_sfc sf bounding box geometry
#' @return sf object with NHD waterbodies or NULL
#' @noRd
get_nhd_waterbodies <- function(bbox_sfc) {
  if (!nhd_available()) {
    return(NULL)
  }

  message("  Fetching NHD waterbodies...")

  tryCatch({
    nhd_waterbodies <- nhdplusTools::get_waterbodies(AOI = bbox_sfc)

    if (is.null(nhd_waterbodies) || nrow(nhd_waterbodies) == 0) {
      message("    No NHD waterbodies found in area")
      return(NULL)
    }

    message("    Found ", nrow(nhd_waterbodies), " NHD waterbodies")
    return(nhd_waterbodies)

  }, error = function(e) {
    message("    Error fetching NHD data: ", conditionMessage(e))
    return(NULL)
  })
}

#' Match a Lake Polygon to NHD via Spatial Intersection
#'
#' @param lake_polygon_wgs84 Lake polygon in WGS84
#' @param nhd_waterbodies sf object with NHD waterbodies
#' @return Matched NHD waterbody or NULL
#' @noRd
match_lake_to_nhd <- function(lake_polygon_wgs84, nhd_waterbodies) {
  if (is.null(nhd_waterbodies) || nrow(nhd_waterbodies) == 0) {
    return(NULL)
  }

  intersects <- sf::st_intersects(lake_polygon_wgs84, nhd_waterbodies)[[1]]

  if (length(intersects) == 0) {
    return(NULL)
  }

  if (length(intersects) > 1) {
    overlaps <- vapply(intersects, function(i) {
      tryCatch({
        inter <- sf::st_intersection(lake_polygon_wgs84, nhd_waterbodies[i, ])
        as.numeric(sf::st_area(inter))
      }, error = function(e) 0)
    }, numeric(1))
    best_match <- intersects[which.max(overlaps)]
  } else {
    best_match <- intersects[1]
  }

  return(nhd_waterbodies[best_match, ])
}

#' Get Outlet and Inlet Locations for a Lake
#'
#' @param lake_nhd Matched NHD waterbody
#' @param lake_polygon_wgs84 Lake polygon in WGS84
#' @return List with outlet, inlets, and outlet_flowline
#' @noRd
get_outlets_inlets <- function(lake_nhd, lake_polygon_wgs84) {
  if (!nhd_available() || is.null(lake_nhd)) {
    return(list(outlet = NULL, inlets = NULL, outlet_flowline = NULL))
  }

  tryCatch({
    # Get flowlines connected to this waterbody
    lake_bbox <- sf::st_bbox(lake_polygon_wgs84)
    lake_bbox[1] <- lake_bbox[1] - 0.02
    lake_bbox[2] <- lake_bbox[2] - 0.02
    lake_bbox[3] <- lake_bbox[3] + 0.02
    lake_bbox[4] <- lake_bbox[4] + 0.02

    flowlines <- nhdplusTools::get_nhdplus(AOI = sf::st_as_sfc(lake_bbox),
                                            realization = "flowline")

    if (is.null(flowlines) || nrow(flowlines) == 0) {
      message("    No flowlines found near lake")
      return(list(outlet = NULL, inlets = NULL, outlet_flowline = NULL))
    }

    # Find flowlines that touch the lake boundary
    lake_boundary <- sf::st_boundary(lake_polygon_wgs84)
    touching <- sf::st_intersects(flowlines, sf::st_buffer(lake_boundary, 0.0001))
    connected_idx <- which(vapply(touching, length, integer(1)) > 0)

    if (length(connected_idx) == 0) {
      message("    No connected flowlines found")
      return(list(outlet = NULL, inlets = NULL, outlet_flowline = NULL))
    }

    connected_flowlines <- flowlines[connected_idx, ]

    # Classify as inlet or outlet based on flow direction
    outlet_list <- list()
    inlet_list <- list()
    outlet_flowline_list <- list()

    for (i in seq_len(nrow(connected_flowlines))) {
      fl <- connected_flowlines[i, ]
      fl_coords <- sf::st_coordinates(fl)

      if (nrow(fl_coords) < 2) next

      start_pt <- sf::st_point(fl_coords[1, 1:2])
      end_pt <- sf::st_point(fl_coords[nrow(fl_coords), 1:2])

      start_in_lake <- sf::st_intersects(sf::st_sfc(start_pt, crs = 4326),
                                          sf::st_buffer(lake_polygon_wgs84, 0.0001))[[1]]
      end_in_lake <- sf::st_intersects(sf::st_sfc(end_pt, crs = 4326),
                                        sf::st_buffer(lake_polygon_wgs84, 0.0001))[[1]]

      if (length(start_in_lake) > 0 && length(end_in_lake) == 0) {
        outlet_list[[length(outlet_list) + 1]] <- start_pt
        outlet_flowline_list[[length(outlet_flowline_list) + 1]] <- fl
      } else if (length(end_in_lake) > 0 && length(start_in_lake) == 0) {
        inlet_list[[length(inlet_list) + 1]] <- end_pt
      }
    }

    outlet_sf <- NULL
    inlet_sf <- NULL
    outlet_flowline <- NULL

    if (length(outlet_list) > 0) {
      outlet_sf <- sf::st_sf(
        type = rep("outlet", length(outlet_list)),
        geometry = sf::st_sfc(outlet_list, crs = 4326)
      )
      outlet_flowline <- outlet_flowline_list[[1]]
    }

    if (length(inlet_list) > 0) {
      inlet_sf <- sf::st_sf(
        type = rep("inlet", length(inlet_list)),
        geometry = sf::st_sfc(inlet_list, crs = 4326)
      )
    }

    message("    Found ", length(outlet_list), " outlet(s) and ",
            length(inlet_list), " inlet(s)")

    return(list(outlet = outlet_sf, inlets = inlet_sf, outlet_flowline = outlet_flowline))

  }, error = function(e) {
    message("    Error finding outlets/inlets: ", conditionMessage(e))
    return(list(outlet = NULL, inlets = NULL, outlet_flowline = NULL))
  })
}

#' Get Stream Order from Outlet Flowline
#'
#' @param outlet_flowline sf flowline object
#' @return Integer stream order or NA
#' @noRd
get_stream_order <- function(outlet_flowline) {
  if (is.null(outlet_flowline)) {
    return(NA_integer_)
  }

  col_names_lower <- tolower(names(outlet_flowline))
  order_cols <- c("streamorde", "stream_order", "strahler", "streamorder")

  for (col in order_cols) {
    if (col %in% col_names_lower) {
      idx <- which(col_names_lower == col)
      return(as.integer(outlet_flowline[[names(outlet_flowline)[idx]]]))
    }
  }

  return(NA_integer_)
}

#' Get Watershed Area Using NLDI Basin Delineation
#'
#' @param lake_nhd Matched NHD waterbody
#' @param lake_polygon_wgs84 Lake polygon in WGS84
#' @return Watershed area in hectares or NA
#' @noRd
get_watershed_area <- function(lake_nhd, lake_polygon_wgs84) {
  if (!nhd_available()) {
    return(NA_real_)
  }

  tryCatch({
    lake_centroid <- sf::st_centroid(lake_polygon_wgs84)
    coords <- sf::st_coordinates(lake_centroid)

    start_point <- sf::st_sfc(sf::st_point(c(coords[1], coords[2])), crs = 4326)

    comid <- tryCatch({
      nhdplusTools::discover_nhdplus_id(point = start_point)
    }, error = function(e) NULL)

    if (is.null(comid) || length(comid) == 0) {
      return(NA_real_)
    }

    basin <- tryCatch({
      nhdplusTools::get_nldi_basin(list(featureSource = "comid",
                                         featureID = as.character(comid)))
    }, error = function(e) NULL)

    if (is.null(basin)) {
      return(NA_real_)
    }

    # Calculate area in hectares (use UTM zone 18N as approximation)
    basin_utm <- sf::st_transform(basin, 32618)
    area_ha <- as.numeric(sf::st_area(basin_utm)) / 10000

    return(area_ha)

  }, error = function(e) {
    return(NA_real_)
  })
}

#' Derive Connectivity Class from Inlet/Outlet Presence
#'
#' @param has_outlet Logical indicating outlet presence
#' @param has_inlet Logical indicating inlet presence
#' @return Character connectivity class
#' @noRd
get_connectivity_class <- function(has_outlet, has_inlet) {
  if (is.na(has_outlet) && is.na(has_inlet)) {
    return(NA_character_)
  }

  has_outlet <- isTRUE(has_outlet)
  has_inlet <- isTRUE(has_inlet)

  if (!has_outlet && !has_inlet) {
    return("Isolated")
  } else if (!has_inlet && has_outlet) {
    return("Headwater")
  } else if (has_inlet && has_outlet) {
    return("Drainage")
  } else if (has_inlet && !has_outlet) {
    return("Terminal")
  } else {
    return(NA_character_)
  }
}

#' Calculate Distance and Bearing Between Two Points
#'
#' @param from_pt sf geometry (point)
#' @param to_pt sf geometry (point)
#' @param utm_crs EPSG code for UTM projection
#' @return List with dist_m, bearing_deg, and bearing (compass)
#' @noRd
calc_distance_bearing <- function(from_pt, to_pt, utm_crs) {
  if (is.null(to_pt) || length(to_pt) == 0) {
    return(list(dist_m = NA_real_, bearing = NA_character_))
  }

  # Transform to UTM for accurate distance
  from_utm <- sf::st_transform(sf::st_sfc(from_pt, crs = 4326), utm_crs)
  to_utm <- sf::st_transform(sf::st_sfc(to_pt, crs = 4326), utm_crs)

  dist_m <- as.numeric(sf::st_distance(from_utm, to_utm))

  # Calculate bearing using WGS84 coordinates
  from_coords <- sf::st_coordinates(sf::st_sfc(from_pt, crs = 4326))
  to_coords <- sf::st_coordinates(sf::st_sfc(to_pt, crs = 4326))

  lon1 <- from_coords[1] * pi / 180
  lat1 <- from_coords[2] * pi / 180
  lon2 <- to_coords[1] * pi / 180
  lat2 <- to_coords[2] * pi / 180

  dlon <- lon2 - lon1
  x <- sin(dlon) * cos(lat2)
  y <- cos(lat1) * sin(lat2) - sin(lat1) * cos(lat2) * cos(dlon)
  bearing_rad <- atan2(x, y)
  bearing_deg <- (bearing_rad * 180 / pi + 360) %% 360

  directions <- c("N", "NE", "E", "SE", "S", "SW", "W", "NW", "N")
  dir_idx <- round(bearing_deg / 45) + 1
  if (dir_idx > 9) dir_idx <- 1
  bearing_compass <- directions[dir_idx]

  return(list(
    dist_m = dist_m,
    bearing_deg = bearing_deg,
    bearing = bearing_compass
  ))
}

#' Get Lake Context from NHD Waterbody
#'
#' @param lake_nhd Matched NHD waterbody
#' @return List with NHD attributes
#' @noRd
get_lake_context_internal <- function(lake_nhd) {
  if (is.null(lake_nhd)) {
    return(list(
      nhd_permanent_id = NA_character_,
      nhd_gnis_name = NA_character_,
      nhd_areasqkm = NA_real_,
      nhd_ftype = NA_character_,
      nhd_fcode = NA_integer_
    ))
  }

  result <- list(
    nhd_permanent_id = NA_character_,
    nhd_gnis_name = NA_character_,
    nhd_areasqkm = NA_real_,
    nhd_ftype = NA_character_,
    nhd_fcode = NA_integer_
  )

  col_names_lower <- tolower(names(lake_nhd))

  if ("permanent_identifier" %in% col_names_lower) {
    idx <- which(col_names_lower == "permanent_identifier")
    result$nhd_permanent_id <- as.character(lake_nhd[[names(lake_nhd)[idx]]])
  }

  if ("gnis_name" %in% col_names_lower) {
    idx <- which(col_names_lower == "gnis_name")
    result$nhd_gnis_name <- as.character(lake_nhd[[names(lake_nhd)[idx]]])
  }

  if ("areasqkm" %in% col_names_lower) {
    idx <- which(col_names_lower == "areasqkm")
    result$nhd_areasqkm <- as.numeric(lake_nhd[[names(lake_nhd)[idx]]])
  }

  if ("ftype" %in% col_names_lower) {
    idx <- which(col_names_lower == "ftype")
    result$nhd_ftype <- as.character(lake_nhd[[names(lake_nhd)[idx]]])
  }

  if ("fcode" %in% col_names_lower) {
    idx <- which(col_names_lower == "fcode")
    result$nhd_fcode <- as.integer(lake_nhd[[names(lake_nhd)[idx]]])
  }

  return(result)
}
