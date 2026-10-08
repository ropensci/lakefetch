# ==============================================================================
# Historical Weather Integration
# ==============================================================================
# Functions to fetch historical weather data and calculate wind/weather metrics
# for lake fetch analysis. Uses Open-Meteo API (free, no registration required).
# ==============================================================================

#' Get Historical Weather for a Location and Time
#'
#' Fetches historical weather data from Open-Meteo API for a specified location
#' and time range.
#'
#' @param lat Latitude
#' @param lon Longitude
#' @param datetime POSIXct datetime of the sample
#' @param days_before Number of days before the sample to fetch (default 7)
#'
#' @return A data frame with hourly weather data
#'
#' @details
#' Uses the Open-Meteo Historical Weather API which provides free access to
#' historical weather data from 1940 to present.
#'
#' @noRd
fetch_weather_history <- function(lat, lon, datetime, days_before = 7) {

  # Calculate date range
  end_date <- as.Date(datetime)
  start_date <- end_date - days_before

  # Build API URL
  base_url <- "https://archive-api.open-meteo.com/v1/archive"

  # Weather variables to fetch
  hourly_vars <- paste(
    "temperature_2m",
    "relative_humidity_2m",
    "precipitation",
    "surface_pressure",
    "cloud_cover",
    "wind_speed_10m",
    "wind_direction_10m",
    "wind_gusts_10m",
    "shortwave_radiation",
    sep = ","
  )

  url <- sprintf(
    "%s?latitude=%.4f&longitude=%.4f&start_date=%s&end_date=%s&hourly=%s&timezone=auto",
    base_url, lat, lon, start_date, end_date, hourly_vars
  )

  # Fetch data
  response <- tryCatch({
    jsonlite::fromJSON(url)
  }, error = function(e) {
    warning("Failed to fetch weather data: ", conditionMessage(e))
    return(NULL)
  })

  if (is.null(response) || is.null(response$hourly)) {
    return(NULL)
  }

  # Convert to data frame
  hourly <- response$hourly
  weather_df <- data.frame(
    datetime = as.POSIXct(hourly$time, format = "%Y-%m-%dT%H:%M", tz = response$timezone),
    temp_c = hourly$temperature_2m,
    humidity_pct = hourly$relative_humidity_2m,
    precip_mm = hourly$precipitation,
    pressure_hpa = hourly$surface_pressure,
    cloud_cover_pct = hourly$cloud_cover,
    wind_speed_ms = hourly$wind_speed_10m,
    wind_dir_deg = hourly$wind_direction_10m,
    wind_gust_ms = hourly$wind_gusts_10m,
    solar_rad_wm2 = hourly$shortwave_radiation,
    stringsAsFactors = FALSE
  )

  return(weather_df)
}


#' Calculate Weather Metrics for Multiple Time Windows
#'
#' Calculates summary statistics for weather variables over multiple time
#' windows preceding a sample time.
#'
#' @param weather_df Data frame from fetch_weather_history
#' @param sample_datetime POSIXct datetime of the sample
#' @param windows_hours Vector of time windows in hours (default c(24, 72, 168))
#'
#' @return A named list of metrics
#'
#' @noRd
calculate_weather_metrics <- function(weather_df, sample_datetime,
                                       windows_hours = c(24, 72, 168)) {

  if (is.null(weather_df) || nrow(weather_df) == 0) {
    return(NULL)
  }

  metrics <- list()

  for (hours in windows_hours) {
    window_name <- if (hours == 24) "24h" else if (hours == 72) "3d" else paste0(hours/24, "d")

    # Filter to time window
    window_start <- sample_datetime - hours * 3600
    window_data <- weather_df[weather_df$datetime >= window_start &
                               weather_df$datetime <= sample_datetime, ]

    if (nrow(window_data) == 0) next

    # Wind metrics
    metrics[[paste0("wind_mean_", window_name)]] <- mean(window_data$wind_speed_ms, na.rm = TRUE)
    metrics[[paste0("wind_max_", window_name)]] <- max(window_data$wind_speed_ms, na.rm = TRUE)
    metrics[[paste0("wind_gust_max_", window_name)]] <- max(window_data$wind_gust_ms, na.rm = TRUE)

    # Dominant wind direction (circular mean)
    metrics[[paste0("wind_dir_dominant_", window_name)]] <- circular_mean_direction(
      window_data$wind_dir_deg,
      window_data$wind_speed_ms
    )

    # Temperature metrics
    metrics[[paste0("temp_mean_", window_name)]] <- mean(window_data$temp_c, na.rm = TRUE)
    metrics[[paste0("temp_min_", window_name)]] <- min(window_data$temp_c, na.rm = TRUE)
    metrics[[paste0("temp_max_", window_name)]] <- max(window_data$temp_c, na.rm = TRUE)

    # Precipitation
    metrics[[paste0("precip_total_", window_name)]] <- sum(window_data$precip_mm, na.rm = TRUE)

    # Other variables (means)
    metrics[[paste0("humidity_mean_", window_name)]] <- mean(window_data$humidity_pct, na.rm = TRUE)
    metrics[[paste0("pressure_mean_", window_name)]] <- mean(window_data$pressure_hpa, na.rm = TRUE)
    metrics[[paste0("cloud_cover_mean_", window_name)]] <- mean(window_data$cloud_cover_pct, na.rm = TRUE)
    metrics[[paste0("solar_rad_mean_", window_name)]] <- mean(window_data$solar_rad_wm2, na.rm = TRUE)
  }

  # Days since major wind event (> 8 m/s sustained)
  strong_wind_idx <- which(weather_df$wind_speed_ms > 8 &
                            weather_df$datetime <= sample_datetime)
  if (length(strong_wind_idx) > 0) {
    last_strong_wind <- max(weather_df$datetime[strong_wind_idx])
    metrics$days_since_strong_wind <- as.numeric(difftime(sample_datetime, last_strong_wind, units = "days"))
  } else {
    metrics$days_since_strong_wind <- NA_real_
  }

  # Instantaneous conditions at sample time
  sample_hour <- weather_df[which.min(abs(weather_df$datetime - sample_datetime)), ]
  if (nrow(sample_hour) > 0) {
    metrics$wind_at_sample <- sample_hour$wind_speed_ms[1]
    metrics$wind_dir_at_sample <- sample_hour$wind_dir_deg[1]
    metrics$temp_at_sample <- sample_hour$temp_c[1]
  }

  return(metrics)
}


#' Calculate Circular Mean of Wind Direction
#'
#' Calculates the wind-speed-weighted circular mean of wind directions.
#'
#' @param directions Vector of wind directions in degrees
#' @param speeds Vector of wind speeds (weights)
#'
#' @return Mean direction in degrees (0-360)
#'
#' @noRd
circular_mean_direction <- function(directions, speeds = NULL) {

  if (length(directions) == 0 || all(is.na(directions))) {
    return(NA_real_)
  }

  # Remove NAs
  valid <- !is.na(directions)
  directions <- directions[valid]

  if (!is.null(speeds)) {
    speeds <- speeds[valid]
    speeds[is.na(speeds)] <- 0
  } else {
    speeds <- rep(1, length(directions))
  }

  # Convert to radians

dir_rad <- directions * pi / 180

  # Calculate weighted vector components
  x <- sum(speeds * cos(dir_rad)) / sum(speeds)
  y <- sum(speeds * sin(dir_rad)) / sum(speeds)

  # Convert back to degrees
  mean_dir <- atan2(y, x) * 180 / pi

  # Normalize to 0-360
  if (mean_dir < 0) mean_dir <- mean_dir + 360

  return(round(mean_dir, 1))
}


#' Calculate Cumulative Wave Energy
#'
#' Calculates cumulative wave energy by combining directional fetch with
#' historical wind direction and speed over a time window.
#'
#' @param fetch_by_direction Named vector of fetch distances by direction (degrees)
#' @param weather_df Data frame from fetch_weather_history
#' @param sample_datetime POSIXct datetime of the sample
#' @param window_hours Time window in hours
#' @param depth_m Water depth in meters (for orbital velocity)
#'
#' @return A list with wave energy metrics
#'
#' @noRd
calculate_cumulative_wave_energy <- function(fetch_by_direction, weather_df,
                                              sample_datetime, window_hours = 72,
                                              depth_m = 5) {

  if (is.null(weather_df) || nrow(weather_df) == 0) {
    return(list(
      cumulative_wave_energy = NA_real_,
      mean_wave_height_m = NA_real_,
      max_wave_height_m = NA_real_,
      mean_orbital_velocity_ms = NA_real_
    ))
  }

  # Filter to time window
  window_start <- sample_datetime - window_hours * 3600
  window_data <- weather_df[weather_df$datetime >= window_start &
                             weather_df$datetime <= sample_datetime, ]

  if (nrow(window_data) == 0) {
    return(list(
      cumulative_wave_energy = NA_real_,
      mean_wave_height_m = NA_real_,
      max_wave_height_m = NA_real_,
      mean_orbital_velocity_ms = NA_real_
    ))
  }

  # Get fetch directions
  fetch_dirs <- as.numeric(gsub("fetch_", "", names(fetch_by_direction)))

  # Calculate wave parameters for each hour
  wave_heights <- numeric(nrow(window_data))
  orbital_velocities <- numeric(nrow(window_data))

  for (i in seq_len(nrow(window_data))) {
    wind_speed <- window_data$wind_speed_ms[i]
    wind_dir <- window_data$wind_dir_deg[i]

    if (is.na(wind_speed) || is.na(wind_dir) || wind_speed < 1) {
      wave_heights[i] <- 0
      orbital_velocities[i] <- 0
      next
    }

    # Find fetch in wind direction (interpolate between available directions)
    dir_diff <- abs(fetch_dirs - wind_dir)
    dir_diff <- pmin(dir_diff, 360 - dir_diff)  # Handle wrap-around
    nearest_dir_idx <- which.min(dir_diff)
    effective_fetch <- fetch_by_direction[nearest_dir_idx]

    if (is.na(effective_fetch) || effective_fetch <= 0) {
      wave_heights[i] <- 0
      orbital_velocities[i] <- 0
      next
    }

    # SMB wave hindcast equations
    # Significant wave height (m)
    Hs <- 0.0016 * sqrt(effective_fetch) * wind_speed
    wave_heights[i] <- Hs

    # Wave period (s) - simplified
    Tp <- 0.286 * sqrt(effective_fetch / 1000) * sqrt(wind_speed)

    # Orbital velocity at bed (m/s)
    if (depth_m > 0 && Tp > 0) {
      wavelength <- 1.56 * Tp^2  # Deep water approximation
      k <- 2 * pi / wavelength
      orbital_velocities[i] <- (pi * Hs / Tp) * exp(-k * depth_m)
    }
  }

  # Cumulative wave energy (proportional to Hs^2 * time)
  cumulative_energy <- sum(wave_heights^2, na.rm = TRUE)

  return(list(
    cumulative_wave_energy = round(cumulative_energy, 2),
    mean_wave_height_m = round(mean(wave_heights, na.rm = TRUE), 3),
    max_wave_height_m = round(max(wave_heights, na.rm = TRUE), 3),
    mean_orbital_velocity_ms = round(mean(orbital_velocities, na.rm = TRUE), 4)
  ))
}


#' Add Weather Context to Fetch Results
#'
#' Adds historical weather metrics and cumulative wave energy to fetch
#' calculation results. For each site, the function queries the Open-Meteo
#' historical-weather API for wind speed and direction in the days leading
#' up to the sample's `datetime`, combines those winds with the site's
#' directional fetch to estimate wave height (Sverdrup-Munk-Bretschneider
#' equations), and integrates wave energy across the requested look-back
#' window(s).
#'
#' @param fetch_results sf object with fetch results (must have datetime column)
#' @param datetime_col Name of the datetime column
#' @param windows_hours Numeric vector of look-back windows in hours over
#'   which to integrate cumulative wave energy. The default
#'   \code{c(24, 72, 168)} produces metrics for the 1-day, 3-day, and 7-day
#'   windows ending at each site's sampling \code{datetime}. For each window
#'   the function adds columns named \code{wave_energy_24h},
#'   \code{wave_energy_72h}, \code{wave_energy_168h}, etc.
#' @param depth_m Water depth for orbital velocity calculation
#'
#' @return sf object with additional weather columns
#'
#' @details
#' The input data must have a datetime column in POSIXct format or a format
#' that can be parsed (ISO 8601, or common date-time formats). Network access
#' to the Open-Meteo API is required; sites are queried sequentially with a
#' short pause between calls to respect the public API rate limit.
#'
#' @examplesIf interactive()
#' csv_path <- system.file("extdata", "sample_sites.csv", package = "lakefetch")
#' sites <- load_sites(csv_path)
#' lake <- get_lake_boundary(sites)
#' results <- fetch_calculate(sites, lake)
#'
#' # Add datetime to results
#' results$results$datetime <- as.POSIXct("2024-07-15 14:00:00")
#'
#' # Add weather context
#' results_with_weather <- add_weather_context(
#'   results$results,
#'   datetime_col = "datetime"
#' )
#'
#' @export
add_weather_context <- function(fetch_results, datetime_col = "datetime",
                                 windows_hours = c(24, 72, 168),
                                 depth_m = NULL) {

  if (!requireNamespace("jsonlite", quietly = TRUE)) {
    stop("Package 'jsonlite' is required for weather data.\n",
         "Install with: install.packages('jsonlite')")
  }

  # Handle depth: use provided depth, per-site depth column, or default
  if (is.null(depth_m)) {
    if ("depth_mean_m" %in% names(fetch_results)) {
      message("Using lake depth from depth_mean_m column")
      use_per_site_depth <- TRUE
    } else {
      message("No depth provided, using default 5m for wave calculations")
      depth_m <- 5
      use_per_site_depth <- FALSE
    }
  } else {
    use_per_site_depth <- FALSE
  }

  # Check for datetime column
  if (!datetime_col %in% names(fetch_results)) {
    stop("Datetime column '", datetime_col, "' not found in results.\n",
         "Available columns: ", paste(names(fetch_results), collapse = ", "))
  }

  # Parse datetime if needed
  dt <- fetch_results[[datetime_col]]
  if (!inherits(dt, "POSIXct")) {
    dt <- tryCatch({
      as.POSIXct(dt)
    }, error = function(e) {
      stop("Could not parse datetime column. Please provide POSIXct or ISO 8601 format.")
    })
  }

  # Get coordinates
  coords <- sf::st_coordinates(sf::st_transform(fetch_results, 4326))

  # Get fetch columns for wave energy calculation
  fetch_cols <- grep("^fetch_[0-9]+$", names(fetch_results), value = TRUE)

  message("Fetching historical weather data for ", nrow(fetch_results), " sites...")

  # Process each site
  all_metrics <- vector("list", nrow(fetch_results))

  for (i in seq_len(nrow(fetch_results))) {
    lat <- coords[i, 2]
    lon <- coords[i, 1]
    sample_dt <- dt[i]

    # Display site_name if available, otherwise Site
    display_name <- if ("site_name" %in% names(fetch_results)) {
      fetch_results$site_name[i]
    } else {
      fetch_results$Site[i]
    }
    message("  Sample ", i, "/", nrow(fetch_results), ": ", display_name)

    # Fetch weather history
    weather <- fetch_weather_history(lat, lon, sample_dt, days_before = max(windows_hours) / 24 + 1)

    if (is.null(weather)) {
      message("    Warning: Could not fetch weather data")
      all_metrics[[i]] <- list()
      next
    }

    # Calculate weather metrics
    metrics <- calculate_weather_metrics(weather, sample_dt, windows_hours)

    # Calculate cumulative wave energy for each window
    if (length(fetch_cols) > 0) {
      fetch_by_dir <- as.numeric(sf::st_drop_geometry(fetch_results[i, fetch_cols]))
      names(fetch_by_dir) <- fetch_cols

      # Get depth for this site
      site_depth <- if (use_per_site_depth) {
        d <- fetch_results$depth_mean_m[i]
        if (is.na(d)) 5 else d  # Default to 5m if NA
      } else {
        depth_m
      }

      for (hours in windows_hours) {
        window_name <- if (hours == 24) "24h" else if (hours == 72) "3d" else paste0(hours/24, "d")
        wave_metrics <- calculate_cumulative_wave_energy(
          fetch_by_dir, weather, sample_dt, hours, site_depth
        )
        metrics[[paste0("wave_energy_", window_name)]] <- wave_metrics$cumulative_wave_energy
        metrics[[paste0("wave_height_mean_", window_name)]] <- wave_metrics$mean_wave_height_m
        metrics[[paste0("wave_height_max_", window_name)]] <- wave_metrics$max_wave_height_m
      }

      # Add orbital velocity for the 3-day window
      wave_3d <- calculate_cumulative_wave_energy(fetch_by_dir, weather, sample_dt, 72, site_depth)
      metrics$orbital_velocity_mean_3d <- wave_3d$mean_orbital_velocity_ms
    }

    all_metrics[[i]] <- metrics

    # Rate limiting - be nice to the API
    Sys.sleep(0.5)
  }

  # Combine all metrics into a data frame
  all_metric_names <- unique(unlist(lapply(all_metrics, names)))

  for (metric_name in all_metric_names) {
    values <- vapply(all_metrics, function(m) {
      if (is.null(m[[metric_name]])) NA_real_ else m[[metric_name]]
    }, numeric(1))
    fetch_results[[metric_name]] <- values
  }

  message("Weather context added successfully!")

  return(fetch_results)
}


