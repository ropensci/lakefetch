# ==============================================================================
# Shiny Interactive App
# ==============================================================================

# Internal helper to reclassify exposure from pre-computed fetch values
# @noRd
reclassify_exposure <- function(results, sheltered_m, exposed_m,
                                 rel_sheltered, rel_exposed) {
  results$exposure_category <- ifelse(
    results$fetch_effective < sheltered_m, "Sheltered",
    ifelse(results$fetch_effective > exposed_m, "Exposed", "Moderate"))
  if ("fetch_proportion" %in% names(results)) {
    results$exposure_relative <- ifelse(
      results$fetch_proportion < rel_sheltered, "Sheltered",
      ifelse(results$fetch_proportion > rel_exposed, "Exposed", "Moderate"))
  }
  results
}

#' Launch Interactive Fetch App
#'
#' Launch a Shiny app for interactive exploration of fetch calculation results.
#' Click on site markers to view fetch rays and detailed information.
#' Click anywhere on the map to analyze a new point.
#'
#' @param fetch_data Results from \code{\link{fetch_calculate}}
#' @param title Optional app title
#'
#' @return Launches a Shiny app (does not return)
#'
#' @details
#' Requires the shiny, leaflet, and base64enc packages (suggested dependencies).
#'
#' The app displays:
#' \itemize{
#'   \item Interactive map with satellite imagery
#'   \item Site markers colored by exposure category
#'   \item Click markers to see fetch rays
#'   \item Popup with rose diagram and metrics
#'   \item Click anywhere on the map to analyze a new point
#' }
#'
#' @examples
#' if (interactive()) {
#'   sites <- load_sites("my_sites.csv")
#'   lake <- get_lake_boundary(sites)
#'   results <- fetch_calculate(sites, lake)
#'   fetch_app(results)
#' }
#'
#' @export
fetch_app <- function(fetch_data, title = NULL) {


  if (!requireNamespace("shiny", quietly = TRUE)) {
    stop("Package 'shiny' is required for the interactive app.\n",
         "Install with: install.packages('shiny')")
  }
  if (!requireNamespace("leaflet", quietly = TRUE)) {
    stop("Package 'leaflet' is required for the interactive app.\n",
         "Install with: install.packages('leaflet')")
  }

  # Determine app title
  if (is.null(title)) {
    unique_lakes <- unique(fetch_data$results$lake_name)
    unique_lakes <- unique_lakes[!is.na(unique_lakes)]
    title <- if (length(unique_lakes) == 1) {
      paste("Fetch Analysis:", unique_lakes[1])
    } else {
      paste("Fetch Analysis:", length(unique_lakes), "Lakes")
    }
  }

  # Store lake data for click analysis
  lakes_utm <- fetch_data$lakes
  # Prefer the stored utm_epsg (added by fetch_calculate) over deriving from
  # the CRS object, which can return NA for some PROJ configurations
  utm_epsg <- fetch_data$utm_epsg
  if (is.null(utm_epsg) || is.na(utm_epsg)) {
    utm_epsg <- sf::st_crs(lakes_utm)$epsg
  }
  n_sites <- nrow(fetch_data$results)
  # Cluster markers when many sites or when sites span a wide geographic area
  # (e.g., sites on lakes across multiple states/countries)
  results_bbox <- sf::st_bbox(sf::st_transform(fetch_data$results, 4326))
  geo_span <- max(results_bbox["xmax"] - results_bbox["xmin"],
                  results_bbox["ymax"] - results_bbox["ymin"])
  # Use isTRUE() to guard against NA (can occur if bbox has NA values)
  use_clustering <- isTRUE(n_sites > 30) || isTRUE(geo_span > 5)
  # For small datasets, pre-render rose plots in popups for best UX
  # For large datasets (>50 sites), generate on demand to avoid long startup
  prerender_roses <- n_sites <= 50

  if (prerender_roses) {
    message("Generating rose diagrams...")
    rose_plots <- vector("list", n_sites)
    for (i in seq_len(n_sites)) {
      rose_plots[[i]] <- make_rose_plot_base64(fetch_data$results[i, ],
                                                fetch_data$results$Site[i])
    }
    fetch_data$results$rose_plot <- unlist(rose_plots)
  }

  # UI
  ui <- shiny::fluidPage(
    shiny::titlePanel(title),
    shiny::sidebarLayout(
      shiny::sidebarPanel(
        width = 3,
        shiny::h4("Instructions"),
        shiny::p("Click any site marker to view fetch rays and rose diagram."),
        shiny::p(shiny::strong("Click anywhere on a lake"), " to analyze a new point."),
        shiny::hr(),
        shiny::h5("Selected Site:"),
        shiny::textOutput("selected_site"),
        shiny::uiOutput("site_details"),
        shiny::conditionalPanel(
          condition = "output.has_click_result == true",
          shiny::hr(),
          shiny::h5("Custom Point Analysis:",
                    style = "color: #B36A00;"),
          shiny::uiOutput("click_results")
        ),
        shiny::hr(),
        shiny::h5("Color Legend:"),
        shiny::uiOutput("legend_text"),
        shiny::hr(),
        shiny::tags$details(
          shiny::tags$summary(shiny::strong("Settings")),
          shiny::tags$br(),
          shiny::tags$em("Display", style = "color: #666;"),
          shiny::selectInput("exposure_mode", "Exposure classification",
                             choices = c("Absolute (meters)" = "absolute",
                                        "Relative (proportion)" = "relative"),
                             selected = "absolute"),
          shiny::conditionalPanel(
            condition = "input.exposure_mode == 'absolute'",
            shiny::numericInput("sheltered_m", "Sheltered threshold (m)",
                                value = 2500, min = 100, max = 50000, step = 100),
            shiny::numericInput("exposed_m", "Exposed threshold (m)",
                                value = 5000, min = 100, max = 50000, step = 100)
          ),
          shiny::conditionalPanel(
            condition = "input.exposure_mode == 'relative'",
            shiny::numericInput("rel_sheltered", "Sheltered proportion",
                                value = 0.25, min = 0.01, max = 0.99, step = 0.05),
            shiny::numericInput("rel_exposed", "Exposed proportion",
                                value = 0.50, min = 0.01, max = 0.99, step = 0.05)
          ),
          shiny::actionButton("apply_display", "Apply Display Settings",
                              class = "btn-sm btn-info"),
          shiny::tags$hr(style = "margin: 10px 0;"),
          shiny::tags$em("Click Analysis", style = "color: #666;"),
          shiny::selectInput("click_method", "Effective fetch method",
                             choices = c("Mean of top 3" = "top3",
                                        "Maximum" = "max",
                                        "SPM cosine-weighted" = "cosine"),
                             selected = "top3"),
          shiny::numericInput("click_depth", "Water depth (m)",
                              value = 10, min = 0.5, max = 100, step = 0.5),
          shiny::numericInput("click_wind", "Wind speed (m/s)",
                              value = 10, min = 0, max = 50, step = 0.5),
          shiny::selectInput("click_angle_res", "Angle resolution",
                             choices = c("1" = "1", "2" = "2",
                                        "5" = "5", "10" = "10"),
                             selected = "5"),
          shiny::numericInput("click_buffer", "Buffer distance (m)",
                              value = 10, min = 1, max = 100, step = 1),
          shiny::numericInput("click_max_fetch", "Max fetch (m)",
                              value = 50000, min = 1000, max = 200000, step = 1000)
        ),
        shiny::hr(),
        shiny::actionButton("clear_click", "Clear Custom Point", class = "btn-sm")
      ),
      shiny::mainPanel(
        leaflet::leafletOutput("map", height = "800px")
      )
    )
  )

  # Server
  server <- function(input, output, session) {

    # Reactive values for display settings
    display_rv <- shiny::reactiveValues(
      results = fetch_data$results,
      sheltered_m = get_opt("exposure_sheltered_m"),
      exposed_m = get_opt("exposure_exposed_m"),
      mode = "absolute"
    )

    # Color palettes
    exposure_pal <- leaflet::colorFactor(
      palette = c("#D55E00", "#E69F00", "#0072B2"),
      levels = c("Exposed", "Moderate", "Sheltered")
    )

    # Reactive ray palette based on current thresholds
    ray_pal_reactive <- shiny::reactive({
      leaflet::colorBin(
        palette = c("#0072B2", "#E69F00", "#D55E00"),
        domain = c(0, max(display_rv$exposed_m * 2, 10000)),
        bins = c(0, display_rv$sheltered_m, display_rv$exposed_m,
                 max(display_rv$exposed_m * 10, 50000))
      )
    })

    # Initial legend text
    output$legend_text <- shiny::renderUI({
      shiny::tagList(
        shiny::p(style = "color: #D55E00; font-weight: bold;",
          sprintf("Exposed: > %.1f km", display_rv$exposed_m / 1000)),
        shiny::p(style = "color: #B36A00; font-weight: bold;",
          sprintf("Moderate: %.1f-%.1f km",
                  display_rv$sheltered_m / 1000, display_rv$exposed_m / 1000)),
        shiny::p(style = "color: #0072B2; font-weight: bold;",
          sprintf("Sheltered: < %.1f km", display_rv$sheltered_m / 1000))
      )
    })

    # Apply display settings - reclassify exposure without recalculation
    shiny::observeEvent(input$apply_display, {
      mode <- input$exposure_mode
      display_rv$mode <- mode

      if (mode == "absolute") {
        s_m <- input$sheltered_m
        e_m <- input$exposed_m
        if (s_m >= e_m) {
          shiny::showNotification("Sheltered threshold must be less than Exposed threshold",
                                   type = "warning")
          return()
        }
        display_rv$sheltered_m <- s_m
        display_rv$exposed_m <- e_m
      } else {
        s_r <- input$rel_sheltered
        e_r <- input$rel_exposed
        if (s_r >= e_r) {
          shiny::showNotification("Sheltered proportion must be less than Exposed proportion",
                                   type = "warning")
          return()
        }
      }

      # Reclassify results
      updated <- reclassify_exposure(
        fetch_data$results,
        sheltered_m = input$sheltered_m,
        exposed_m = input$exposed_m,
        rel_sheltered = input$rel_sheltered,
        rel_exposed = input$rel_exposed
      )
      display_rv$results <- updated

      # Get the active exposure column
      if (mode == "relative" && "exposure_relative" %in% names(updated)) {
        active_col <- updated$exposure_relative
      } else {
        active_col <- updated$exposure_category
      }

      # Update markers via proxy
      results_wgs <- sf::st_transform(updated, 4326)
      leaflet::leafletProxy("map") |>
        leaflet::clearGroup("site_markers") |>
        leaflet::addCircleMarkers(
          data = results_wgs,
          group = "site_markers",
          radius = 6,
          stroke = TRUE, color = "white", weight = 1.5,
          fillOpacity = 0.9,
          fillColor = exposure_pal(active_col),
          layerId = ~Site
        )
    })

    # Base map
    output$map <- leaflet::renderLeaflet({

      results_wgs <- sf::st_transform(display_rv$results, 4326)
      lakes_wgs <- sf::st_transform(fetch_data$lakes, 4326)

      # Build popups - rich with rose plots for small datasets, text-only for large
      if (prerender_roses) {
        # Build outlet/inlet info strings
        outlet_info <- if ("outlet_dist_m" %in% names(results_wgs)) {
          ifelse(!is.na(results_wgs$outlet_dist_m),
                 sprintf("<b>Outlet:</b> %.0f m<br/>", results_wgs$outlet_dist_m), "")
        } else { rep("", nrow(results_wgs)) }
        inlet_info <- if ("inlet_nearest_dist_m" %in% names(results_wgs)) {
          ifelse(!is.na(results_wgs$inlet_nearest_dist_m),
                 sprintf("<b>Nearest Inlet:</b> %.0f m<br/>", results_wgs$inlet_nearest_dist_m), "")
        } else { rep("", nrow(results_wgs)) }

        popup_html <- sprintf(
          "<div style='font-family:sans-serif; width:220px;'>
           <h4 style='margin:0; border-bottom:1px solid #ccc;'>%s</h4>
           <span style='background:#eee; font-size:0.9em;'>%s</span><br/>
           <span style='color:#666; font-size:0.8em;'>%s</span>
           <center><img src='%s' width='100%%' style='margin:5px 0;'/></center>
           <b>Effective Fetch:</b> %.1f km<br/>
           <b>Orbital Velocity:</b> %.3f m/s<br/>
           %s%s
           </div>",
          results_wgs$Site,
          results_wgs$exposure_category,
          ifelse(is.na(results_wgs$lake_name), "", results_wgs$lake_name),
          results_wgs$rose_plot,
          results_wgs$fetch_effective / 1000,
          results_wgs$orbital_effective,
          outlet_info,
          inlet_info
        )
      } else {
        popup_html <- sprintf(
          "<div style='font-family:sans-serif; width:200px;'>
           <h4 style='margin:0; border-bottom:1px solid #ccc;'>%s</h4>
           <span style='background:#eee; font-size:0.9em;'>%s</span><br/>
           <span style='color:#666; font-size:0.8em;'>%s</span><br/>
           <b>Effective Fetch:</b> %.1f km<br/>
           <b>Orbital Velocity:</b> %.3f m/s<br/>
           <em style='font-size:0.8em;'>Click marker for details in sidebar</em>
           </div>",
          results_wgs$Site,
          results_wgs$exposure_category,
          ifelse(is.na(results_wgs$lake_name), "", results_wgs$lake_name),
          results_wgs$fetch_effective / 1000,
          results_wgs$orbital_effective
        )
      }

      m <- leaflet::leaflet(results_wgs) |>
        leaflet::addProviderTiles("Esri.WorldImagery", group = "Imagery") |>
        leaflet::addTiles(group = "OSM") |>
        leaflet::addTiles(
          urlTemplate = paste0("https://basemap.nationalmap.gov/arcgis/rest/",
                                "services/USGSHydroCached/MapServer/tile/",
                                "{z}/{y}/{x}"),
          attribution = "USGS The National Map: National Hydrography Dataset",
          options = leaflet::tileOptions(opacity = 0.8),
          group = "USGS NHD"
        ) |>
        leaflet::addLayersControl(
          baseGroups = c("Imagery", "OSM"),
          overlayGroups = "USGS NHD",
          options = leaflet::layersControlOptions(collapsed = FALSE)
        ) |>
        leaflet::hideGroup("USGS NHD") |>
        leaflet::addPolygons(data = lakes_wgs,
                    fill = FALSE, color = "white",
                    weight = 1, opacity = 0.3)

      if (use_clustering) {
        m <- m |> leaflet::addCircleMarkers(
          radius = 6,
          stroke = TRUE, color = "white", weight = 1.5,
          fillOpacity = 0.9,
          fillColor = ~exposure_pal(exposure_category),
          popup = popup_html,
          layerId = ~Site,
          clusterOptions = leaflet::markerClusterOptions()
        )
      } else {
        m <- m |> leaflet::addCircleMarkers(
          radius = 6,
          stroke = TRUE, color = "white", weight = 1.5,
          fillOpacity = 0.9,
          fillColor = ~exposure_pal(exposure_category),
          popup = popup_html,
          layerId = ~Site
        )
      }

      m |> leaflet::addLegend("bottomright",
                pal = exposure_pal,
                values = ~exposure_category,
                title = "Wave Exposure")
    })

    # Click handler for existing markers - generate rays and rose on demand
    shiny::observeEvent(input$map_marker_click, {
      click <- input$map_marker_click
      site_id <- click$id

      if (is.null(site_id)) return()

      output$selected_site <- shiny::renderText(site_id)

      # Find this site in results
      site_idx <- which(fetch_data$results$Site == site_id)
      if (length(site_idx) == 0) return()

      site_row <- fetch_data$results[site_idx[1], ]

      # Generate ray geometries on demand for this site only
      angles <- fetch_data$angles
      coords <- sf::st_coordinates(site_row)
      x0 <- coords[1]
      y0 <- coords[2]

      ray_lines <- list()
      ray_angles <- integer(0)
      ray_dists <- numeric(0)

      for (angle in angles) {
        col_name <- paste0("fetch_", angle)
        if (!col_name %in% names(site_row)) next
        dist <- as.numeric(sf::st_drop_geometry(site_row)[, col_name])
        if (is.na(dist) || dist <= 0) next

        rad <- angle * (pi / 180)
        x1 <- x0 + dist * sin(rad)
        y1 <- y0 + dist * cos(rad)

        ray_lines[[length(ray_lines) + 1]] <- sf::st_linestring(rbind(c(x0, y0), c(x1, y1)))
        ray_angles <- c(ray_angles, angle)
        ray_dists <- c(ray_dists, dist)
      }

      if (length(ray_lines) > 0) {
        site_rays <- sf::st_sf(
          Angle = ray_angles,
          Distance = ray_dists,
          geometry = sf::st_sfc(ray_lines, crs = sf::st_crs(fetch_data$results))
        )
        site_rays_wgs <- sf::st_transform(site_rays, 4326)

        cur_ray_pal <- ray_pal_reactive()
        leaflet::leafletProxy("map") |>
          leaflet::clearGroup("rays") |>
          leaflet::addPolylines(
            data = site_rays_wgs,
            group = "rays",
            color = ~cur_ray_pal(Distance),
            weight = 2,
            opacity = 0.8,
            popup = ~sprintf("Angle: %d deg<br>Distance: %d m", Angle, round(Distance))
          )
      }

      # Generate rose plot on demand for sidebar
      output$site_details <- shiny::renderUI({
        rose_b64 <- make_rose_plot_base64(site_row, site_id)

        lake_nm <- if (!is.na(site_row$lake_name)) site_row$lake_name else ""
        outlet_text <- if ("outlet_dist_m" %in% names(site_row) && !is.na(site_row$outlet_dist_m)) {
          sprintf("Outlet: %.0f m", site_row$outlet_dist_m)
        } else { NULL }
        inlet_text <- if ("inlet_nearest_dist_m" %in% names(site_row) && !is.na(site_row$inlet_nearest_dist_m)) {
          sprintf("Nearest Inlet: %.0f m", site_row$inlet_nearest_dist_m)
        } else { NULL }

        shiny::tagList(
          if (nzchar(rose_b64)) shiny::tags$img(src = rose_b64, width = "100%"),
          shiny::p(shiny::strong("Lake: "), lake_nm),
          shiny::p(shiny::strong("Effective Fetch: "), sprintf("%.1f km", site_row$fetch_effective / 1000)),
          shiny::p(shiny::strong("Mean Fetch: "), sprintf("%.1f m", site_row$fetch_mean)),
          shiny::p(shiny::strong("Max Fetch: "), sprintf("%.1f m", site_row$fetch_max)),
          shiny::p(shiny::strong("Orbital Velocity: "), sprintf("%.3f m/s", site_row$orbital_effective)),
          shiny::p(shiny::strong("Exposure: "),
            shiny::span(site_row$exposure_category, style = sprintf("color: %s; font-weight: bold;",
              switch(as.character(site_row$exposure_category),
                "Exposed" = "#D55E00",
                "Moderate" = "#E69F00",
                "Sheltered" = "#0072B2"
              )
            ))
          ),
          if (!is.null(outlet_text)) shiny::p(shiny::strong(outlet_text)),
          if (!is.null(inlet_text)) shiny::p(shiny::strong(inlet_text))
        )
      })
    })

    # Store click analysis results
    click_result <- shiny::reactiveVal(NULL)

    # Drives the conditionalPanel that shows the "Custom Point Analysis"
    # sidebar section. Without this, clicking a pre-loaded marker can visually
    # obscure the custom-point result (rose plots are tall) and the user can
    # mistakenly think their custom analysis was discarded.
    output$has_click_result <- shiny::reactive({
      !is.null(click_result())
    })
    shiny::outputOptions(output, "has_click_result", suspendWhenHidden = FALSE)

    # Click handler for map (new point analysis)
    shiny::observeEvent(input$map_click, {
      click <- input$map_click
      if (is.null(click)) return()

      # Create point in WGS84
      click_pt_wgs <- sf::st_sfc(sf::st_point(c(click$lng, click$lat)), crs = 4326)

      # Transform to UTM
      click_pt_utm <- sf::st_transform(click_pt_wgs, utm_epsg)

      # Check if point is inside any lake
      lakes_contain <- sf::st_intersects(click_pt_utm, lakes_utm)[[1]]

      if (length(lakes_contain) == 0) {
        # Point is not in a lake
        output$selected_site <- shiny::renderText("Click is not inside a lake")
        click_result(NULL)
        return()
      }

      # Get the lake polygon
      lake_idx <- lakes_contain[1]
      lake_poly <- lakes_utm[lake_idx, ]
      lake_name <- if (!is.na(lake_poly$name)) lake_poly$name else "Unknown Lake"

      output$selected_site <- shiny::renderText(
        sprintf("Analyzing point in %s...", lake_name)
      )

      # Run fetch calculation for this single point
      tryCatch({
        # Create a minimal site data frame
        site_sf <- sf::st_sf(
          Site = "Custom Point",
          lake_osm_id = lake_poly$osm_id,
          lake_name = lake_name,
          geometry = click_pt_utm
        )

        # Get lake boundary
        lake_boundary <- tryCatch({
          sf::st_cast(lake_poly, "MULTILINESTRING")
        }, error = function(e) {
          sf::st_boundary(lake_poly)
        })

        # Calculate fetch using settings inputs
        angle_res <- as.numeric(input$click_angle_res)
        angles <- seq(0, 360 - angle_res, by = angle_res)

        # Apply settings for this calculation
        lakefetch_options(
          buffer_distance_m = input$click_buffer,
          max_fetch_m = input$click_max_fetch
        )
        on.exit(lakefetch_reset_options(), add = TRUE)

        # Nudge point if needed
        nudged_pt <- nudge_inward(
          click_pt_utm,
          lake_boundary,
          lake_poly,
          input$click_buffer
        )
        nudged_sf <- sf::st_sf(
          Site = "Custom Point",
          geometry = sf::st_sfc(nudged_pt, crs = utm_epsg)
        )

        # Get fetch distances
        fetch_dists <- get_highres_fetch(nudged_sf, lake_boundary, lake_poly, angles)

        # Calculate metrics using settings
        fetch_mean <- mean(fetch_dists, na.rm = TRUE)
        fetch_max <- max(fetch_dists, na.rm = TRUE)
        fetch_matrix <- matrix(fetch_dists, nrow = 1)
        fetch_effective <- calc_effective_fetch(fetch_matrix, angles, input$click_method)[1]
        # Use depth and wind from settings
        orbital <- calc_orbital(fetch_effective, depth_m = input$click_depth,
                                wind_speed = input$click_wind)
        sheltered_m <- display_rv$sheltered_m
        exposed_m <- display_rv$exposed_m
        exposure <- if (fetch_effective < sheltered_m) "Sheltered" else if (fetch_effective > exposed_m) "Exposed" else "Moderate"

        # Create rays for visualization
        pt_coords <- sf::st_coordinates(nudged_sf)
        ray_lines <- lapply(seq_along(angles), function(i) {
          rad <- angles[i] * pi / 180
          end_x <- pt_coords[1] + fetch_dists[i] * sin(rad)
          end_y <- pt_coords[2] + fetch_dists[i] * cos(rad)
          sf::st_linestring(rbind(pt_coords[1:2], c(end_x, end_y)))
        })
        rays_sf <- sf::st_sf(
          Angle = angles,
          Distance = fetch_dists,
          geometry = sf::st_sfc(ray_lines, crs = utm_epsg)
        )
        rays_wgs <- sf::st_transform(rays_sf, 4326)

        # Store results
        click_result(list(
          lake_name = lake_name,
          fetch_mean = fetch_mean,
          fetch_max = fetch_max,
          fetch_effective = fetch_effective,
          orbital = orbital,
          exposure = exposure,
          rays = rays_wgs,
          point = sf::st_transform(nudged_sf, 4326)
        ))

        # Update selected site text
        output$selected_site <- shiny::renderText(
          sprintf("Custom Point in %s", lake_name)
        )

        # Get exposure color
        exp_color <- switch(exposure,
          "Exposed" = "#D55E00",
          "Moderate" = "#E69F00",
          "Sheltered" = "#0072B2"
        )

        # Generate rose diagram for custom point
        # Build a synthetic site_row with fetch_* columns so make_rose_plot_base64 can use it
        fetch_row <- as.data.frame(t(fetch_dists))
        colnames(fetch_row) <- paste0("fetch_", angles)
        fetch_row_sf <- sf::st_sf(fetch_row,
                                   geometry = sf::st_sfc(nudged_pt, crs = utm_epsg))
        rose_b64 <- tryCatch(
          make_rose_plot_base64(fetch_row_sf, lake_name),
          error = function(e) ""
        )

        # Explicitly set output$click_results here (same pattern as marker click handler)
        # so the rose updates correctly even after a prior marker click overrode the binding
        output$click_results <- shiny::renderUI({
          shiny::tagList(
            shiny::hr(),
            shiny::h5("Custom Point Results:"),
            shiny::p(shiny::strong("Lake: "), lake_name),
            shiny::p(shiny::strong("Mean Fetch: "), sprintf("%.1f m", fetch_mean)),
            shiny::p(shiny::strong("Max Fetch: "), sprintf("%.1f m", fetch_max)),
            shiny::p(shiny::strong("Effective Fetch: "),
                     sprintf("%.1f km", fetch_effective / 1000)),
            shiny::p(shiny::strong("Orbital Velocity: "),
                     sprintf("%.3f m/s", orbital)),
            shiny::p(shiny::strong("Exposure: "),
              shiny::span(exposure, style = sprintf("color: %s; font-weight: bold;",
                switch(exposure,
                  "Exposed" = "#D55E00",
                  "Moderate" = "#E69F00",
                  "Sheltered" = "#0072B2"
                )
              ))
            ),
            if (nzchar(rose_b64)) shiny::tags$img(src = rose_b64, width = "100%")
          )
        })

        # Update map with new point and rays
        cur_ray_pal <- ray_pal_reactive()
        leaflet::leafletProxy("map") |>
          leaflet::clearGroup("rays") |>
          leaflet::clearGroup("custom_point") |>
          leaflet::addPolylines(
            data = rays_wgs,
            group = "rays",
            color = ~cur_ray_pal(Distance),
            weight = 2,
            opacity = 0.8,
            popup = ~sprintf("Angle: %d deg<br>Distance: %d m", Angle, round(Distance))
          ) |>
          leaflet::addCircleMarkers(
            data = sf::st_transform(nudged_sf, 4326),
            group = "custom_point",
            radius = 8,
            stroke = TRUE,
            color = "white",
            weight = 2,
            fillColor = exp_color,
            fillOpacity = 0.9,
            popup = sprintf(
              "<b>Custom Point</b><br>Lake: %s<br>Effective Fetch: %.1f km<br>Exposure: %s",
              lake_name, fetch_effective / 1000, exposure
            )
          )

      }, error = function(e) {
        output$selected_site <- shiny::renderText(
          sprintf("Error analyzing point: %s", conditionMessage(e))
        )
        click_result(NULL)
      })
    })

    # Note: output$click_results is set directly inside observeEvent(input$map_click)
    # and observeEvent(input$map_marker_click) so the rose diagram always updates correctly.

    # Clear custom point
    shiny::observeEvent(input$clear_click, {
      click_result(NULL)
      output$selected_site <- shiny::renderText("")
      output$site_details <- shiny::renderUI(NULL)
      leaflet::leafletProxy("map") |>
        leaflet::clearGroup("rays") |>
        leaflet::clearGroup("custom_point")
    })
  }

  shiny::shinyApp(ui, server)
}


#' Launch Interactive Fetch App with File Upload
#'
#' Launch a standalone Shiny app where users can upload a CSV file with GPS
#' coordinates, and the app will automatically download lake boundaries,
#' calculate fetch, and display interactive results.
#'
#' @param title Optional app title (default: "Lake Fetch Calculator")
#'
#' @return Launches a Shiny app (does not return)
#'
#' @details
#' Requires the shiny, leaflet, and base64enc packages (suggested dependencies).
#'
#' The app workflow:
#' \enumerate{
#'   \item Upload a CSV file with latitude/longitude columns
#'   \item App downloads lake boundaries from OpenStreetMap
#'   \item Calculates fetch for all uploaded points
#'   \item Displays interactive map with results
#'   \item Click anywhere on a lake to analyze additional points
#'   \item Download results as CSV or GeoPackage
#' }
#'
#' CSV file requirements:
#' \itemize{
#'   \item Must have columns starting with "lat" and "lon" (case-insensitive)
#'   \item Optional "Site" column for point names
#'   \item Additional columns are preserved in output
#' }
#'
#' @examples
#' if (interactive()) {
#'   # Launch the upload app
#'   fetch_app_upload()
#' }
#'
#' @export
fetch_app_upload <- function(title = "Lake Fetch Calculator") {

  if (!requireNamespace("shiny", quietly = TRUE)) {
    stop("Package 'shiny' is required for the interactive app.\n",
         "Install with: install.packages('shiny')")
  }
  if (!requireNamespace("leaflet", quietly = TRUE)) {
    stop("Package 'leaflet' is required for the interactive app.\n",
         "Install with: install.packages('leaflet')")
  }

  # UI
  ui <- shiny::fluidPage(
    shiny::tags$head(
      shiny::tags$style(shiny::HTML("
        .progress-message { color: #666; font-style: italic; margin: 10px 0; }
        .error-message { color: #D55E00; font-weight: bold; font-weight: bold; }
        .success-message { color: #0072B2; font-weight: bold; font-weight: bold; }
      "))
    ),
    shiny::titlePanel(title),
    shiny::sidebarLayout(
      shiny::sidebarPanel(
        width = 3,
        shiny::conditionalPanel(
          condition = "!output.has_results",
          shiny::h4("Upload GPS Points"),
          shiny::fileInput("file_upload", "Choose CSV File",
                           accept = c("text/csv", ".csv")),
          shiny::helpText("CSV must have columns starting with 'lat' and 'lon'"),
          shiny::helpText("Maximum 50 sites for the web app. For larger datasets,",
                          "use the lakefetch R package locally:",
                          shiny::code("install.packages('lakefetch')")),
          shiny::helpText("Optional: include a 'datetime' column for weather analysis"),
          shiny::tags$hr(style = "margin: 10px 0;"),
          shiny::fileInput("lake_upload",
                           "Optional: lake boundary file",
                           accept = c(".gpkg", ".shp", ".geojson", ".kml",
                                      "application/octet-stream",
                                      "application/geopackage+sqlite3",
                                      "application/json"),
                           multiple = TRUE),
          shiny::helpText("Skip the OSM download by supplying your own lake",
                          "boundary polygon. For shapefiles, upload all of",
                          ".shp, .shx, .dbf, .prj (multi-select)."),
          shiny::hr(),
          shiny::h5("Options"),
          shiny::numericInput("water_depth", "Water depth (m)", value = 5, min = 0.5, max = 100),
          shiny::helpText("Used for orbital velocity calculation"),
          shiny::selectInput("fetch_method", "Effective fetch method",
                             choices = c("Mean of top 3" = "top3",
                                        "Maximum" = "max",
                                        "SPM cosine-weighted" = "cosine"),
                             selected = "top3"),
          shiny::helpText("SPM method uses 9 radials centered on max direction"),
          shiny::checkboxInput("add_nhd", "Add NHD context (outlets/inlets)", value = FALSE),
          shiny::checkboxInput("add_weather", "Add historical weather data", value = FALSE),
          shiny::conditionalPanel(
            condition = "input.add_weather",
            shiny::helpText("Requires 'datetime' column in CSV")
          ),
          shiny::tags$details(
            shiny::tags$summary(shiny::strong("Advanced Settings")),
            shiny::tags$br(),
            shiny::selectInput("angle_res", "Angle resolution",
                               choices = c("1\u00b0" = "1", "2\u00b0" = "2",
                                          "5\u00b0" = "5", "10\u00b0" = "10"),
                               selected = "5"),
            shiny::numericInput("buffer_dist", "Buffer distance (m)",
                                value = 10, min = 1, max = 100, step = 1),
            shiny::numericInput("max_fetch", "Max fetch cap (m)",
                                value = 50000, min = 1000, max = 200000, step = 1000),
            shiny::numericInput("wind_speed", "Wind speed (m/s)",
                                value = 10, min = 0, max = 50, step = 0.5),
            shiny::tags$hr(style = "margin: 10px 0;"),
            shiny::tags$em("Exposure Thresholds", style = "color: #666;"),
            shiny::numericInput("sheltered_m", "Sheltered (m)",
                                value = 2500, min = 100, max = 50000, step = 100),
            shiny::numericInput("exposed_m", "Exposed (m)",
                                value = 5000, min = 100, max = 50000, step = 100),
            shiny::numericInput("rel_sheltered", "Relative sheltered",
                                value = 0.25, min = 0.01, max = 0.99, step = 0.05),
            shiny::numericInput("rel_exposed", "Relative exposed",
                                value = 0.50, min = 0.01, max = 0.99, step = 0.05)
          ),
          shiny::hr(),
          shiny::actionButton("run_analysis", "Run Analysis",
                              class = "btn-primary btn-block",
                              disabled = TRUE),
          shiny::hr(),
          shiny::uiOutput("status_message")
        ),
        shiny::conditionalPanel(
          condition = "output.has_results",
          shiny::h4("Results"),
          shiny::p(shiny::textOutput("results_summary")),
          shiny::hr(),
          shiny::h5("Instructions"),
          shiny::p("Click any marker to view fetch rays."),
          shiny::p(shiny::strong("Click on a lake polygon"), " to analyze a new point on that lake."),
          shiny::hr(),
          shiny::h5("Selected Site:"),
          shiny::textOutput("selected_site"),
          shiny::uiOutput("site_details_upload"),
          shiny::conditionalPanel(
            condition = "output.has_click_result_upload == true",
            shiny::hr(),
            shiny::h5("Custom Point Analysis:",
                      style = "color: #B36A00;"),
            shiny::uiOutput("click_results")
          ),
          shiny::hr(),
          shiny::h5("Color Legend:"),
          shiny::uiOutput("legend_text"),
          shiny::tags$details(
            shiny::tags$summary(shiny::strong("Display Settings")),
            shiny::tags$br(),
            shiny::selectInput("exposure_display", "Exposure classification",
                               choices = c("Absolute (meters)" = "absolute",
                                          "Relative (proportion)" = "relative"),
                               selected = "absolute"),
            shiny::actionButton("apply_display", "Apply Display Settings",
                                class = "btn-sm btn-info")
          ),
          shiny::hr(),
          shiny::h5("Download Results"),
          shiny::downloadButton("download_csv", "Download CSV", class = "btn-sm"),
          shiny::downloadButton("download_gpkg", "Download GeoPackage", class = "btn-sm"),
          shiny::hr(),
          shiny::actionButton("clear_click", "Clear Custom Point", class = "btn-sm"),
          shiny::actionButton("reset_app", "Start Over", class = "btn-sm btn-warning")
        )
      ),
      shiny::mainPanel(
        width = 9,
        shiny::conditionalPanel(
          condition = "!output.has_results",
          shiny::div(
            style = "text-align: center; padding: 100px; color: #999;",
            shiny::h3("Upload a CSV file to begin"),
            shiny::p("Your file should contain latitude and longitude columns."),
            shiny::p("The app will automatically find lakes and calculate fetch.")
          )
        ),
        shiny::conditionalPanel(
          condition = "output.has_results",
          leaflet::leafletOutput("map", height = "800px")
        )
      )
    )
  )

  # Server
  server <- function(input, output, session) {

    # Reactive values
    rv <- shiny::reactiveValues(
      sites = NULL,
      lake_data = NULL,
      fetch_data = NULL,
      click_result = NULL,
      status = NULL,
      error = NULL,
      has_datetime = FALSE,
      has_weather = FALSE
    )

    # Track if we have results
    output$has_results <- shiny::reactive({
      !is.null(rv$fetch_data)
    })
    shiny::outputOptions(output, "has_results", suspendWhenHidden = FALSE)

    # Drives the "Custom Point Analysis" conditionalPanel so the user can
    # see their click-analysis result even after they click a preloaded
    # marker (whose rose plot fills site_details_upload).
    output$has_click_result_upload <- shiny::reactive({
      !is.null(rv$click_result)
    })
    shiny::outputOptions(output, "has_click_result_upload",
                          suspendWhenHidden = FALSE)

    # Handle file upload
    shiny::observeEvent(input$file_upload, {
      req(input$file_upload)

      tryCatch({
        rv$sites <- load_sites(input$file_upload$datapath)

        # Enforce 50-site limit for web app
        if (nrow(rv$sites) > 50) {
          rv$error <- paste0(
            "Your file has ", nrow(rv$sites), " sites. ",
            "The web app supports up to 50 sites. ",
            "For larger datasets, install the lakefetch R package locally: ",
            "install.packages('lakefetch')"
          )
          rv$status <- NULL
          rv$sites <- NULL
          rv$has_datetime <- FALSE
          return()
        }

        # Check if datetime column was detected
        rv$has_datetime <- "datetime" %in% names(rv$sites)

        if (rv$has_datetime) {
          rv$status <- sprintf("Loaded %d sites with datetime. Click 'Run Analysis' to continue.",
                               nrow(rv$sites))
        } else {
          rv$status <- sprintf("Loaded %d sites. Click 'Run Analysis' to continue.",
                               nrow(rv$sites))
        }
        rv$error <- NULL

        # Enable the run button
        shiny::updateActionButton(session, "run_analysis", disabled = FALSE)

      }, error = function(e) {
        rv$error <- sprintf("Error loading file: %s", conditionMessage(e))
        rv$status <- NULL
        rv$sites <- NULL
        rv$has_datetime <- FALSE
      })
    })

    # Status message display
    output$status_message <- shiny::renderUI({
      if (!is.null(rv$error)) {
        shiny::div(class = "error-message", rv$error)
      } else if (!is.null(rv$status)) {
        shiny::div(class = "progress-message", rv$status)
      }
    })

    # Run analysis
    shiny::observeEvent(input$run_analysis, {
      req(rv$sites)

      # Show progress
      shiny::withProgress(message = "Processing...", value = 0, {

        tryCatch({
          # Apply advanced settings before calculation
          lakefetch_options(
            angle_resolution_deg = as.numeric(input$angle_res),
            buffer_distance_m = input$buffer_dist,
            max_fetch_m = input$max_fetch,
            default_wind_speed_ms = input$wind_speed,
            exposure_sheltered_m = input$sheltered_m,
            exposure_exposed_m = input$exposed_m,
            exposure_relative_sheltered = input$rel_sheltered,
            exposure_relative_exposed = input$rel_exposed
          )

          # Step 1: Get lake boundaries. If the user uploaded a boundary
          # file, use it directly and skip the OSM query. Shapefiles come as
          # multiple files (.shp/.shx/.dbf/.prj); we move them into a single
          # temp directory and hand the .shp path to get_lake_boundary().
          if (!is.null(input$lake_upload) && nrow(input$lake_upload) > 0) {
            shiny::incProgress(0.15,
                                detail = "Loading uploaded lake boundary...")
            uploads <- input$lake_upload
            main_path <- if (nrow(uploads) == 1) {
              uploads$datapath[1]
            } else {
              tmp_dir <- tempfile("lake_boundary_"); dir.create(tmp_dir)
              for (i in seq_len(nrow(uploads))) {
                file.copy(uploads$datapath[i],
                          file.path(tmp_dir, uploads$name[i]))
              }
              shp <- list.files(tmp_dir, pattern = "\\.shp$",
                                 full.names = TRUE)[1]
              if (is.na(shp)) uploads$datapath[1] else shp
            }
            rv$lake_data <- get_lake_boundary(rv$sites, file = main_path)
          } else {
            shiny::incProgress(0.15,
                                detail = "Downloading lake boundaries from OSM...")
            rv$lake_data <- get_lake_boundary(rv$sites)
          }

          # Step 2: Calculate fetch
          shiny::incProgress(0.25, detail = "Calculating fetch...")
          rv$fetch_data <- fetch_calculate(rv$sites, rv$lake_data,
                                            depth_m = input$water_depth,
                                            fetch_method = input$fetch_method,
                                            add_context = input$add_nhd)

          # Step 3: Add weather data if requested and datetime available
          rv$has_weather <- FALSE
          if (input$add_weather && rv$has_datetime) {
            shiny::incProgress(0.25, detail = "Fetching historical weather data...")
            tryCatch({
              rv$fetch_data$results <- add_weather_context(
                rv$fetch_data$results,
                datetime_col = "datetime",
                windows_hours = c(24, 72, 168),
                depth_m = input$water_depth
              )
              rv$has_weather <- TRUE
            }, error = function(e) {
              message("Weather data error: ", conditionMessage(e))
            })
          } else {
            shiny::incProgress(0.25)
          }

          # For small datasets, pre-render rose plots for rich popups
          n_result_sites <- nrow(rv$fetch_data$results)
          if (n_result_sites <= 50) {
            shiny::incProgress(0.25, detail = "Generating rose diagrams...")
            rose_plots <- vector("list", n_result_sites)
            for (i in seq_len(n_result_sites)) {
              rose_plots[[i]] <- make_rose_plot_base64(
                rv$fetch_data$results[i, ],
                rv$fetch_data$results$Site[i]
              )
            }
            rv$fetch_data$results$rose_plot <- unlist(rose_plots)
          }

          shiny::incProgress(0.1, detail = "Done!")
          rv$status <- if (rv$has_weather) "Analysis complete with weather data!" else "Analysis complete!"
          rv$error <- NULL

        }, error = function(e) {
          rv$error <- sprintf("Error during analysis: %s", conditionMessage(e))
          rv$fetch_data <- NULL
        })
      })
    })

    # Results summary
    output$results_summary <- shiny::renderText({
      req(rv$fetch_data)
      n_sites <- nrow(rv$fetch_data$results)
      n_lakes <- length(unique(rv$fetch_data$results$lake_osm_id))
      base_text <- sprintf("%d sites across %d lake(s)", n_sites, n_lakes)
      if (rv$has_weather) {
        paste0(base_text, " + weather")
      } else {
        base_text
      }
    })

    # Color palettes
    exposure_pal <- leaflet::colorFactor(
      palette = c("#D55E00", "#E69F00", "#0072B2"),
      levels = c("Exposed", "Moderate", "Sheltered")
    )

    # Reactive ray palette based on thresholds
    ray_pal_reactive <- shiny::reactive({
      s_m <- input$sheltered_m
      e_m <- input$exposed_m
      leaflet::colorBin(
        palette = c("#0072B2", "#E69F00", "#D55E00"),
        domain = c(0, max(e_m * 2, 10000)),
        bins = c(0, s_m, e_m, max(e_m * 10, 50000))
      )
    })

    # Reactive legend text
    output$legend_text <- shiny::renderUI({
      s_m <- input$sheltered_m
      e_m <- input$exposed_m
      shiny::tagList(
        shiny::p(style = "color: #D55E00; font-weight: bold;",
          sprintf("Exposed: > %.1f km", e_m / 1000)),
        shiny::p(style = "color: #B36A00; font-weight: bold;",
          sprintf("Moderate: %.1f-%.1f km", s_m / 1000, e_m / 1000)),
        shiny::p(style = "color: #0072B2; font-weight: bold;",
          sprintf("Sheltered: < %.1f km", s_m / 1000))
      )
    })

    # Apply display settings — reclassify without recalculation
    shiny::observeEvent(input$apply_display, {
      req(rv$fetch_data)
      mode <- input$exposure_display

      s_m <- input$sheltered_m
      e_m <- input$exposed_m
      s_r <- input$rel_sheltered
      e_r <- input$rel_exposed

      if (mode == "absolute" && s_m >= e_m) {
        shiny::showNotification("Sheltered threshold must be less than Exposed",
                                 type = "warning")
        return()
      }
      if (mode == "relative" && s_r >= e_r) {
        shiny::showNotification("Sheltered proportion must be less than Exposed",
                                 type = "warning")
        return()
      }

      # Reclassify
      rv$fetch_data$results <- reclassify_exposure(
        rv$fetch_data$results,
        sheltered_m = s_m, exposed_m = e_m,
        rel_sheltered = s_r, rel_exposed = e_r
      )

      # Get active exposure column
      if (mode == "relative" && "exposure_relative" %in% names(rv$fetch_data$results)) {
        active_col <- rv$fetch_data$results$exposure_relative
      } else {
        active_col <- rv$fetch_data$results$exposure_category
      }

      # Update markers
      results_wgs <- sf::st_transform(rv$fetch_data$results, 4326)
      leaflet::leafletProxy("map") |>
        leaflet::clearGroup("site_markers") |>
        leaflet::addCircleMarkers(
          data = results_wgs,
          group = "site_markers",
          radius = 6,
          stroke = TRUE, color = "white", weight = 1.5,
          fillOpacity = 0.9,
          fillColor = exposure_pal(active_col),
          layerId = ~Site
        )
    })

    # Render map
    output$map <- leaflet::renderLeaflet({
      req(rv$fetch_data)

      results_wgs <- sf::st_transform(rv$fetch_data$results, 4326)
      lakes_wgs <- sf::st_transform(rv$fetch_data$lakes, 4326)
      n_sites <- nrow(results_wgs)
      has_roses <- "rose_plot" %in% names(results_wgs)

      # Rich popups with rose plots for small datasets, text-only for large
      if (has_roses) {
        weather_info <- ""
        if (rv$has_weather && "wind_mean_24h" %in% names(results_wgs)) {
          weather_info <- sprintf(
            "<hr style='margin:5px 0;'/>
             <b>Weather (24h/3d):</b><br/>
             Wind: %.1f / %.1f m/s<br/>
             Wave Energy: %.0f / %.0f<br/>
             Temp: %.1f C | Precip: %.1f mm",
            ifelse(is.na(results_wgs$wind_mean_24h), 0, results_wgs$wind_mean_24h),
            ifelse(is.na(results_wgs$wind_mean_3d), 0, results_wgs$wind_mean_3d),
            ifelse(is.na(results_wgs$wave_energy_24h), 0, results_wgs$wave_energy_24h),
            ifelse(is.na(results_wgs$wave_energy_3d), 0, results_wgs$wave_energy_3d),
            ifelse(is.na(results_wgs$temp_mean_24h), 0, results_wgs$temp_mean_24h),
            ifelse(is.na(results_wgs$precip_total_3d), 0, results_wgs$precip_total_3d)
          )
        }
        popup_html <- sprintf(
          "<div style='font-family:sans-serif; width:220px;'>
           <h4 style='margin:0; border-bottom:1px solid #ccc;'>%s</h4>
           <span style='background:#eee; font-size:0.9em;'>%s</span><br/>
           <span style='color:#666; font-size:0.8em;'>%s</span>
           <center><img src='%s' width='100%%' style='margin:5px 0;'/></center>
           <b>Effective Fetch:</b> %.1f km<br/>
           <b>Orbital Velocity:</b> %.3f m/s<br/>
           %s
           </div>",
          results_wgs$Site,
          results_wgs$exposure_category,
          ifelse(is.na(results_wgs$lake_name), "", results_wgs$lake_name),
          results_wgs$rose_plot,
          results_wgs$fetch_effective / 1000,
          results_wgs$orbital_effective,
          weather_info
        )
      } else {
        popup_html <- sprintf(
          "<div style='font-family:sans-serif; width:200px;'>
           <h4 style='margin:0; border-bottom:1px solid #ccc;'>%s</h4>
           <span style='background:#eee; font-size:0.9em;'>%s</span><br/>
           <span style='color:#666; font-size:0.8em;'>%s</span><br/>
           <b>Effective Fetch:</b> %.1f km<br/>
           <b>Orbital Velocity:</b> %.3f m/s<br/>
           <em style='font-size:0.8em;'>Click marker for details in sidebar</em>
           </div>",
          results_wgs$Site,
          results_wgs$exposure_category,
          ifelse(is.na(results_wgs$lake_name), "", results_wgs$lake_name),
          results_wgs$fetch_effective / 1000,
          results_wgs$orbital_effective
        )
      }

      m <- leaflet::leaflet(results_wgs) |>
        leaflet::addProviderTiles("Esri.WorldImagery", group = "Imagery") |>
        leaflet::addTiles(group = "OSM") |>
        leaflet::addTiles(
          urlTemplate = paste0("https://basemap.nationalmap.gov/arcgis/rest/",
                                "services/USGSHydroCached/MapServer/tile/",
                                "{z}/{y}/{x}"),
          attribution = "USGS The National Map: National Hydrography Dataset",
          options = leaflet::tileOptions(opacity = 0.8),
          group = "USGS NHD"
        ) |>
        leaflet::addLayersControl(
          baseGroups = c("Imagery", "OSM"),
          overlayGroups = "USGS NHD",
          options = leaflet::layersControlOptions(collapsed = FALSE)
        ) |>
        leaflet::hideGroup("USGS NHD") |>
        leaflet::addPolygons(data = lakes_wgs,
                    fill = FALSE, color = "white",
                    weight = 1, opacity = 0.3)

      # Cluster markers when many sites or wide geographic spread
      # Use isTRUE() to guard against NA (can occur if bbox has NA values)
      bbox_wgs <- sf::st_bbox(results_wgs)
      geo_span_upload <- max(bbox_wgs["xmax"] - bbox_wgs["xmin"],
                             bbox_wgs["ymax"] - bbox_wgs["ymin"])
      use_cluster <- isTRUE(n_sites > 30) || isTRUE(geo_span_upload > 5)

      if (use_cluster) {
        m <- m |> leaflet::addCircleMarkers(
          radius = 6,
          stroke = TRUE, color = "white", weight = 1.5,
          fillOpacity = 0.9,
          fillColor = ~exposure_pal(exposure_category),
          popup = popup_html,
          layerId = ~Site,
          clusterOptions = leaflet::markerClusterOptions()
        )
      } else {
        m <- m |> leaflet::addCircleMarkers(
          radius = 6,
          stroke = TRUE, color = "white", weight = 1.5,
          fillOpacity = 0.9,
          fillColor = ~exposure_pal(exposure_category),
          popup = popup_html,
          layerId = ~Site
        )
      }

      m |> leaflet::addLegend("bottomright",
                pal = exposure_pal,
                values = ~exposure_category,
                title = "Wave Exposure")
    })

    # Click handler for existing markers - generate rays and rose on demand
    shiny::observeEvent(input$map_marker_click, {
      req(rv$fetch_data)
      click <- input$map_marker_click
      site_id <- click$id

      if (is.null(site_id)) return()

      output$selected_site <- shiny::renderText(site_id)

      # Find this site in results
      site_idx <- which(rv$fetch_data$results$Site == site_id)
      if (length(site_idx) == 0) return()

      site_row <- rv$fetch_data$results[site_idx[1], ]
      angles <- rv$fetch_data$angles

      # Generate ray geometries on demand for this site only
      coords <- sf::st_coordinates(site_row)
      x0 <- coords[1]
      y0 <- coords[2]

      ray_lines <- list()
      ray_angles <- integer(0)
      ray_dists <- numeric(0)

      for (angle in angles) {
        col_name <- paste0("fetch_", angle)
        if (!col_name %in% names(site_row)) next
        dist <- as.numeric(sf::st_drop_geometry(site_row)[, col_name])
        if (is.na(dist) || dist <= 0) next

        rad <- angle * (pi / 180)
        x1 <- x0 + dist * sin(rad)
        y1 <- y0 + dist * cos(rad)

        ray_lines[[length(ray_lines) + 1]] <- sf::st_linestring(rbind(c(x0, y0), c(x1, y1)))
        ray_angles <- c(ray_angles, angle)
        ray_dists <- c(ray_dists, dist)
      }

      if (length(ray_lines) > 0) {
        site_rays <- sf::st_sf(
          Angle = ray_angles,
          Distance = ray_dists,
          geometry = sf::st_sfc(ray_lines, crs = sf::st_crs(rv$fetch_data$results))
        )
        site_rays_wgs <- sf::st_transform(site_rays, 4326)

        cur_ray_pal <- ray_pal_reactive()
        leaflet::leafletProxy("map") |>
          leaflet::clearGroup("rays") |>
          leaflet::addPolylines(
            data = site_rays_wgs,
            group = "rays",
            color = ~cur_ray_pal(Distance),
            weight = 2,
            opacity = 0.8,
            popup = ~sprintf("Angle: %d deg<br>Distance: %d m", Angle, round(Distance))
          )
      }

      # Generate rose plot for the selected pre-loaded marker. Render into
      # output$site_details_upload (the upper sidebar slot) - NOT into
      # output$click_results, which belongs to the custom-point analysis and
      # would otherwise be clobbered every time the user clicks a marker.
      output$site_details_upload <- shiny::renderUI({
        rose_b64 <- make_rose_plot_base64(site_row, site_id)

        lake_nm <- if (!is.na(site_row$lake_name)) site_row$lake_name else ""

        # Build weather info if available
        weather_tags <- NULL
        if (rv$has_weather && "wind_mean_24h" %in% names(site_row)) {
          weather_tags <- shiny::tagList(
            shiny::hr(),
            shiny::h5("Weather Context:"),
            shiny::p(shiny::strong("Wind (24h): "),
              sprintf("%.1f m/s", ifelse(is.na(site_row$wind_mean_24h), 0, site_row$wind_mean_24h))),
            shiny::p(shiny::strong("Wave Energy (24h): "),
              sprintf("%.0f", ifelse(is.na(site_row$wave_energy_24h), 0, site_row$wave_energy_24h)))
          )
        }

        shiny::tagList(
          if (nzchar(rose_b64)) shiny::tags$img(src = rose_b64, width = "100%"),
          shiny::p(shiny::strong("Lake: "), lake_nm),
          shiny::p(shiny::strong("Effective Fetch: "), sprintf("%.1f km", site_row$fetch_effective / 1000)),
          shiny::p(shiny::strong("Mean Fetch: "), sprintf("%.1f m", site_row$fetch_mean)),
          shiny::p(shiny::strong("Max Fetch: "), sprintf("%.1f m", site_row$fetch_max)),
          shiny::p(shiny::strong("Orbital Velocity: "), sprintf("%.3f m/s", site_row$orbital_effective)),
          shiny::p(shiny::strong("Exposure: "),
            shiny::span(site_row$exposure_category, style = sprintf("color: %s; font-weight: bold;",
              switch(as.character(site_row$exposure_category),
                "Exposed" = "#D55E00",
                "Moderate" = "#E69F00",
                "Sheltered" = "#0072B2"
              )
            ))
          ),
          weather_tags
        )
      })
    })

    # Click handler for map (new point analysis)
    shiny::observeEvent(input$map_click, {
      req(rv$fetch_data)
      click <- input$map_click
      if (is.null(click)) return()

      lakes_utm <- rv$fetch_data$lakes
      utm_epsg <- sf::st_crs(lakes_utm)$epsg

      click_pt_wgs <- sf::st_sfc(sf::st_point(c(click$lng, click$lat)), crs = 4326)
      click_pt_utm <- sf::st_transform(click_pt_wgs, utm_epsg)

      lakes_contain <- sf::st_intersects(click_pt_utm, lakes_utm)[[1]]

      if (length(lakes_contain) == 0) {
        output$selected_site <- shiny::renderText("Click is not inside a lake")
        rv$click_result <- NULL
        return()
      }

      lake_idx <- lakes_contain[1]
      lake_poly <- lakes_utm[lake_idx, ]
      lake_name <- if (!is.na(lake_poly$name)) lake_poly$name else "Unknown Lake"

      output$selected_site <- shiny::renderText(
        sprintf("Analyzing point in %s...", lake_name)
      )

      tryCatch({
        lake_boundary <- tryCatch({
          sf::st_cast(lake_poly, "MULTILINESTRING")
        }, error = function(e) {
          sf::st_boundary(lake_poly)
        })

        angle_res <- as.numeric(input$angle_res)
        angles <- seq(0, 360 - angle_res, by = angle_res)

        nudged_pt <- nudge_inward(
          click_pt_utm, lake_boundary, lake_poly,
          input$buffer_dist
        )
        nudged_sf <- sf::st_sf(
          Site = "Custom Point",
          geometry = sf::st_sfc(nudged_pt, crs = utm_epsg)
        )

        fetch_dists <- get_highres_fetch(nudged_sf, lake_boundary, lake_poly, angles)

        # Calculate metrics using settings
        fetch_mean <- mean(fetch_dists, na.rm = TRUE)
        fetch_max <- max(fetch_dists, na.rm = TRUE)
        fetch_matrix <- matrix(fetch_dists, nrow = 1)
        fetch_effective <- calc_effective_fetch(fetch_matrix, angles, input$fetch_method)[1]
        orbital <- calc_orbital(fetch_effective, depth_m = input$water_depth,
                                wind_speed = input$wind_speed)
        sheltered_m <- input$sheltered_m
        exposed_m <- input$exposed_m
        exposure <- if (fetch_effective < sheltered_m) "Sheltered" else if (fetch_effective > exposed_m) "Exposed" else "Moderate"

        pt_coords <- sf::st_coordinates(nudged_sf)
        ray_lines <- lapply(seq_along(angles), function(i) {
          rad <- angles[i] * pi / 180
          end_x <- pt_coords[1] + fetch_dists[i] * sin(rad)
          end_y <- pt_coords[2] + fetch_dists[i] * cos(rad)
          sf::st_linestring(rbind(pt_coords[1:2], c(end_x, end_y)))
        })
        rays_sf <- sf::st_sf(
          Angle = angles,
          Distance = fetch_dists,
          geometry = sf::st_sfc(ray_lines, crs = utm_epsg)
        )
        rays_wgs <- sf::st_transform(rays_sf, 4326)

        rv$click_result <- list(
          lake_name = lake_name,
          fetch_mean = fetch_mean,
          fetch_max = fetch_max,
          fetch_effective = fetch_effective,
          orbital = orbital,
          exposure = exposure,
          rays = rays_wgs,
          point = sf::st_transform(nudged_sf, 4326)
        )

        output$selected_site <- shiny::renderText(
          sprintf("Custom Point in %s", lake_name)
        )

        exp_color <- switch(exposure,
          "Exposed" = "#D55E00",
          "Moderate" = "#E69F00",
          "Sheltered" = "#0072B2"
        )

        # Generate rose for custom point and explicitly rebind output$click_results
        fetch_row <- as.data.frame(t(fetch_dists))
        colnames(fetch_row) <- paste0("fetch_", angles)
        fetch_row_sf <- sf::st_sf(fetch_row,
                                   geometry = sf::st_sfc(nudged_pt, crs = utm_epsg))
        rose_b64 <- tryCatch(
          make_rose_plot_base64(fetch_row_sf, lake_name),
          error = function(e) ""
        )

        output$click_results <- shiny::renderUI({
          shiny::tagList(
            if (nzchar(rose_b64)) shiny::tags$img(src = rose_b64, width = "100%"),
            shiny::hr(),
            shiny::h5("Custom Point Results:"),
            shiny::p(shiny::strong("Lake: "), lake_name),
            shiny::p(shiny::strong("Effective Fetch: "),
                     sprintf("%.1f km", fetch_effective / 1000)),
            shiny::p(shiny::strong("Mean Fetch: "), sprintf("%.1f m", fetch_mean)),
            shiny::p(shiny::strong("Max Fetch: "), sprintf("%.1f m", fetch_max)),
            shiny::p(shiny::strong("Orbital Velocity: "), sprintf("%.3f m/s", orbital)),
            shiny::p(shiny::strong("Exposure: "),
              shiny::span(exposure, style = sprintf("color: %s; font-weight: bold;",
                exp_color
              ))
            )
          )
        })

        cur_ray_pal <- ray_pal_reactive()
        leaflet::leafletProxy("map") |>
          leaflet::clearGroup("rays") |>
          leaflet::clearGroup("custom_point") |>
          leaflet::addPolylines(
            data = rays_wgs,
            group = "rays",
            color = ~cur_ray_pal(Distance),
            weight = 2,
            opacity = 0.8,
            popup = ~sprintf("Angle: %d deg<br>Distance: %d m", Angle, round(Distance))
          ) |>
          leaflet::addCircleMarkers(
            data = sf::st_transform(nudged_sf, 4326),
            group = "custom_point",
            radius = 8,
            stroke = TRUE,
            color = "white",
            weight = 2,
            fillColor = exp_color,
            fillOpacity = 0.9,
            popup = sprintf(
              "<b>Custom Point</b><br>Lake: %s<br>Effective Fetch: %.1f km<br>Exposure: %s",
              lake_name, fetch_effective / 1000, exposure
            )
          )

      }, error = function(e) {
        output$selected_site <- shiny::renderText(
          sprintf("Error: %s", conditionMessage(e))
        )
        rv$click_result <- NULL
      })
    })

    # Note: output$click_results is set directly inside observeEvent(input$map_click)
    # and observeEvent(input$map_marker_click) so the rose diagram always updates correctly.

    # Download CSV
    output$download_csv <- shiny::downloadHandler(
      filename = function() {
        paste0("fetch_results_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv")
      },
      content = function(file) {
        req(rv$fetch_data)
        results_wgs <- sf::st_transform(rv$fetch_data$results, 4326)
        coords <- sf::st_coordinates(results_wgs)
        output_df <- sf::st_drop_geometry(results_wgs)
        output_df$longitude <- coords[, 1]
        output_df$latitude <- coords[, 2]
        # Remove internal columns not useful for export
        output_df$rose_plot <- NULL
        utils::write.csv(output_df, file, row.names = FALSE)
      }
    )

    # Download GeoPackage. A GeoPackage can hold multiple layers, so we write
    # THREE: the sites (with fetch attributes), the directional fetch rays
    # (one row per direction per site, with distance in meters), and the lake
    # polygons. Users typically want all three for downstream GIS work.
    output$download_gpkg <- shiny::downloadHandler(
      filename = function() {
        paste0("fetch_results_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".gpkg")
      },
      content = function(file) {
        req(rv$fetch_data)
        results_wgs <- sf::st_transform(rv$fetch_data$results, 4326)
        results_wgs$rose_plot <- NULL

        # Layer 1: sites with fetch attributes
        sf::st_write(results_wgs, file, layer = "sites",
                     driver = "GPKG", delete_dsn = TRUE, quiet = TRUE)

        # Layer 2: directional fetch rays (one line per site per direction)
        rays <- tryCatch(
          create_ray_geometries(rv$fetch_data),
          error = function(e) NULL
        )
        if (!is.null(rays) && nrow(rays) > 0) {
          rays_wgs <- sf::st_transform(rays, 4326)
          sf::st_write(rays_wgs, file, layer = "fetch_rays",
                       driver = "GPKG", append = TRUE, quiet = TRUE)
        }

        # Layer 3: lake boundary polygons
        lakes <- rv$fetch_data$lakes
        if (!is.null(lakes) && nrow(lakes) > 0) {
          lakes_wgs <- sf::st_transform(lakes, 4326)
          sf::st_write(lakes_wgs, file, layer = "lakes",
                       driver = "GPKG", append = TRUE, quiet = TRUE)
        }
      }
    )

    # Clear custom point
    shiny::observeEvent(input$clear_click, {
      rv$click_result <- NULL
      output$selected_site <- shiny::renderText("")
      leaflet::leafletProxy("map") |>
        leaflet::clearGroup("rays") |>
        leaflet::clearGroup("custom_point")
    })

    # Reset app
    shiny::observeEvent(input$reset_app, {
      rv$sites <- NULL
      rv$lake_data <- NULL
      rv$fetch_data <- NULL
      rv$click_result <- NULL
      rv$status <- NULL
      rv$error <- NULL
      shiny::updateActionButton(session, "run_analysis", disabled = TRUE)
    })
  }

  shiny::shinyApp(ui, server)
}
