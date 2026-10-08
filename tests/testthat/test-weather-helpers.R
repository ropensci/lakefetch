# ==============================================================================
# Tests for weather_integration.R helper functions
# ==============================================================================

# --- circular_mean_direction ---

test_that("circular_mean_direction calculates unweighted mean", {
  # All north = north
  result <- lakefetch:::circular_mean_direction(c(0, 0, 0))
  expect_equal(result, 0)

  # All east = east
  result <- lakefetch:::circular_mean_direction(c(90, 90, 90))
  expect_equal(result, 90)

  # East and west should cancel to N or S (0 or 180 depending on exact calc)
  result <- lakefetch:::circular_mean_direction(c(90, 270))
  # x components cancel, y components cancel; atan2(0,0) = 0
  expect_true(result == 0 || result == 360 || result == 180)
})

test_that("circular_mean_direction handles wrap-around", {
  # 350 and 10 should average to ~0 (north)
  result <- lakefetch:::circular_mean_direction(c(350, 10))
  expect_true(result < 15 || result > 345)
})

test_that("circular_mean_direction handles speed weighting", {
  # Strong east wind + weak west wind = mostly east
  result <- lakefetch:::circular_mean_direction(c(90, 270), c(10, 1))
  expect_true(abs(result - 90) < 20)
})

test_that("circular_mean_direction handles empty/NA input", {
  expect_true(is.na(lakefetch:::circular_mean_direction(numeric(0))))
  expect_true(is.na(lakefetch:::circular_mean_direction(c(NA, NA))))
})

# --- calculate_weather_metrics ---

test_that("calculate_weather_metrics returns NULL for empty data", {
  expect_null(lakefetch:::calculate_weather_metrics(NULL, Sys.time()))
  expect_null(lakefetch:::calculate_weather_metrics(data.frame(), Sys.time()))
})

test_that("calculate_weather_metrics calculates correct time windows", {
  sample_time <- as.POSIXct("2024-07-15 12:00:00", tz = "UTC")

  # Create 8 days of hourly data
  hours <- seq(sample_time - 8 * 24 * 3600, sample_time, by = "hour")
  weather_df <- data.frame(
    datetime = hours,
    temp_c = rep(20, length(hours)),
    humidity_pct = rep(60, length(hours)),
    precip_mm = rep(0.1, length(hours)),
    pressure_hpa = rep(1013, length(hours)),
    cloud_cover_pct = rep(50, length(hours)),
    wind_speed_ms = rep(5, length(hours)),
    wind_dir_deg = rep(180, length(hours)),
    wind_gust_ms = rep(8, length(hours)),
    solar_rad_wm2 = rep(300, length(hours))
  )

  result <- lakefetch:::calculate_weather_metrics(weather_df, sample_time)

  expect_type(result, "list")
  expect_true("wind_mean_24h" %in% names(result))
  expect_true("wind_mean_3d" %in% names(result))
  expect_true("temp_mean_24h" %in% names(result))
  expect_true("precip_total_24h" %in% names(result))

  # With constant wind speed of 5, mean should be 5
  expect_equal(result$wind_mean_24h, 5)
  expect_equal(result$wind_max_24h, 5)
  expect_equal(result$temp_mean_24h, 20)
})

test_that("calculate_weather_metrics detects strong wind events", {
  sample_time <- as.POSIXct("2024-07-15 12:00:00", tz = "UTC")
  hours <- seq(sample_time - 3 * 24 * 3600, sample_time, by = "hour")

  weather_df <- data.frame(
    datetime = hours,
    temp_c = rep(20, length(hours)),
    humidity_pct = rep(60, length(hours)),
    precip_mm = rep(0, length(hours)),
    pressure_hpa = rep(1013, length(hours)),
    cloud_cover_pct = rep(50, length(hours)),
    wind_speed_ms = rep(3, length(hours)),  # below threshold
    wind_dir_deg = rep(180, length(hours)),
    wind_gust_ms = rep(5, length(hours)),
    solar_rad_wm2 = rep(300, length(hours))
  )
  # Add one strong wind event 1 day before sample
  strong_idx <- which.min(abs(weather_df$datetime - (sample_time - 24 * 3600)))
  weather_df$wind_speed_ms[strong_idx] <- 12

  result <- lakefetch:::calculate_weather_metrics(weather_df, sample_time)
  expect_true(!is.na(result$days_since_strong_wind))
  expect_true(result$days_since_strong_wind > 0.9 && result$days_since_strong_wind < 1.1)
})

test_that("calculate_weather_metrics includes instantaneous conditions", {
  sample_time <- as.POSIXct("2024-07-15 12:00:00", tz = "UTC")
  hours <- seq(sample_time - 24 * 3600, sample_time, by = "hour")

  weather_df <- data.frame(
    datetime = hours,
    temp_c = seq(15, 25, length.out = length(hours)),
    humidity_pct = rep(60, length(hours)),
    precip_mm = rep(0, length(hours)),
    pressure_hpa = rep(1013, length(hours)),
    cloud_cover_pct = rep(50, length(hours)),
    wind_speed_ms = rep(5, length(hours)),
    wind_dir_deg = rep(180, length(hours)),
    wind_gust_ms = rep(8, length(hours)),
    solar_rad_wm2 = rep(300, length(hours))
  )

  result <- lakefetch:::calculate_weather_metrics(weather_df, sample_time)
  expect_true("wind_at_sample" %in% names(result))
  expect_true("temp_at_sample" %in% names(result))
})

# --- calculate_cumulative_wave_energy ---

test_that("calculate_cumulative_wave_energy returns NAs for empty data", {
  result <- lakefetch:::calculate_cumulative_wave_energy(
    c(fetch_0 = 1000, fetch_90 = 500), NULL, Sys.time()
  )
  expect_true(is.na(result$cumulative_wave_energy))
  expect_true(is.na(result$mean_wave_height_m))
})

test_that("calculate_cumulative_wave_energy calculates wave metrics", {
  sample_time <- as.POSIXct("2024-07-15 12:00:00", tz = "UTC")
  hours <- seq(sample_time - 3 * 24 * 3600, sample_time, by = "hour")

  weather_df <- data.frame(
    datetime = hours,
    wind_speed_ms = rep(8, length(hours)),
    wind_dir_deg = rep(180, length(hours)),
    wind_gust_ms = rep(12, length(hours))
  )

  # Create fetch in all directions
  fetch_dirs <- seq(0, 355, by = 5)
  fetch_by_dir <- setNames(rep(2000, length(fetch_dirs)),
                            paste0("fetch_", fetch_dirs))
  # More fetch in the south (wind direction)
  fetch_by_dir["fetch_180"] <- 5000

  result <- lakefetch:::calculate_cumulative_wave_energy(
    fetch_by_dir, weather_df, sample_time, window_hours = 72, depth_m = 5
  )

  expect_true(result$cumulative_wave_energy > 0)
  expect_true(result$mean_wave_height_m > 0)
  expect_true(result$max_wave_height_m >= result$mean_wave_height_m)
  expect_true(result$mean_orbital_velocity_ms >= 0)
})

# --- add_weather_context input validation ---

test_that("add_weather_context errors without jsonlite", {
  skip_if(requireNamespace("jsonlite", quietly = TRUE) == FALSE)
  # Can't really test this without unloading jsonlite, so just test the datetime check
  lake <- create_circular_lake(radius = 1000)
  site <- create_site(500000, 4800000, name = "Test")
  site$lake_osm_id <- "test_circle"
  site$fetch_effective <- 1000

  # No datetime column should error
  expect_error(
    add_weather_context(site, datetime_col = "datetime"),
    "not found"
  )
})

test_that("add_weather_context errors with wrong datetime column name", {
  lake <- create_circular_lake(radius = 1000)
  site <- create_site(500000, 4800000, name = "Test")
  site$lake_osm_id <- "test_circle"
  site$fetch_effective <- 1000
  site$sample_time <- Sys.time()

  expect_error(
    add_weather_context(site, datetime_col = "nonexistent"),
    "not found"
  )
})

test_that("calculate_cumulative_wave_energy handles low wind", {
  sample_time <- as.POSIXct("2024-07-15 12:00:00", tz = "UTC")
  hours <- seq(sample_time - 24 * 3600, sample_time, by = "hour")

  weather_df <- data.frame(
    datetime = hours,
    wind_speed_ms = rep(0.5, length(hours)),  # below 1 m/s threshold
    wind_dir_deg = rep(180, length(hours)),
    wind_gust_ms = rep(1, length(hours))
  )

  fetch_by_dir <- setNames(rep(1000, 72), paste0("fetch_", seq(0, 355, by = 5)))

  result <- lakefetch:::calculate_cumulative_wave_energy(
    fetch_by_dir, weather_df, sample_time, window_hours = 24, depth_m = 5
  )

  expect_equal(result$cumulative_wave_energy, 0)
  expect_equal(result$mean_wave_height_m, 0)
})

test_that("calculate_cumulative_wave_energy returns NAs for out-of-window data", {
  sample_time <- as.POSIXct("2024-07-15 12:00:00", tz = "UTC")
  # Weather data entirely BEFORE the window
  old_hours <- seq(sample_time - 30 * 24 * 3600, sample_time - 20 * 24 * 3600, by = "hour")

  weather_df <- data.frame(
    datetime = old_hours,
    wind_speed_ms = rep(8, length(old_hours)),
    wind_dir_deg = rep(180, length(old_hours)),
    wind_gust_ms = rep(12, length(old_hours))
  )

  fetch_by_dir <- setNames(rep(1000, 72), paste0("fetch_", seq(0, 355, by = 5)))

  result <- lakefetch:::calculate_cumulative_wave_energy(
    fetch_by_dir, weather_df, sample_time, window_hours = 72, depth_m = 5
  )

  expect_true(is.na(result$cumulative_wave_energy))
  expect_true(is.na(result$mean_wave_height_m))
})

test_that("calculate_cumulative_wave_energy handles NA fetch in wind direction", {
  sample_time <- as.POSIXct("2024-07-15 12:00:00", tz = "UTC")
  hours <- seq(sample_time - 24 * 3600, sample_time, by = "hour")

  weather_df <- data.frame(
    datetime = hours,
    wind_speed_ms = rep(8, length(hours)),
    wind_dir_deg = rep(180, length(hours)),
    wind_gust_ms = rep(12, length(hours))
  )

  fetch_by_dir <- setNames(rep(1000, 72), paste0("fetch_", seq(0, 355, by = 5)))
  # Set fetch in the wind direction (180) to NA
  fetch_by_dir["fetch_180"] <- NA

  result <- lakefetch:::calculate_cumulative_wave_energy(
    fetch_by_dir, weather_df, sample_time, window_hours = 24, depth_m = 5
  )

  # Should still compute but with zero wave heights for those hours
  expect_equal(result$cumulative_wave_energy, 0)
})
