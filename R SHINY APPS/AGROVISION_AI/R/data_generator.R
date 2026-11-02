# ============================================================================
# Simulated dataset generator (reproducible). All rows are labelled "Simulated".
# 8 farms, 10 crop plots, 60 observations each (every 2 days) = 600 records.
# Relationships: crop health depends on how close temperature, moisture, pH,
# nutrients, humidity and rainfall are to each crop's reference range;
# yield depends on crop health.  Values are SYNTHETIC, not measured.
# ============================================================================

generate_sample_data <- function(seed = 2025, n_obs = 60, start = as.Date("2025-11-01")) {
  set.seed(seed)
  farms <- data.frame(
    farm_id   = sprintf("F%02d", 1:8),
    farm_name = c("Thanjavur Delta Farm", "Coimbatore Green Acres", "Madurai Sugar Estate",
                  "Salem Groundnut Fields", "Ooty Highland Farm", "Karnal Wheat Farm",
                  "Guntur Cotton Farm", "Krishnagiri Tomato Farm"),
    location  = c("Thanjavur, Tamil Nadu", "Coimbatore, Tamil Nadu", "Madurai, Tamil Nadu",
                  "Salem, Tamil Nadu", "Ooty, Tamil Nadu", "Karnal, Haryana",
                  "Guntur, Andhra Pradesh", "Krishnagiri, Tamil Nadu"),
    latitude  = c(10.78, 11.00, 9.92, 11.66, 11.41, 29.69, 16.31, 12.52),
    longitude = c(79.14, 76.96, 78.12, 78.15, 76.70, 76.99, 80.44, 78.21),
    clim_t = c(29, 27, 30, 28, 17, 18, 31, 26),     # mean air temperature (deg C)
    clim_h = c(78, 62, 70, 60, 75, 55, 60, 65),     # mean humidity (%)
    clim_r = c(7, 3, 5, 3, 3.5, 1.5, 3, 3),         # mean rainfall (mm/day)
    ph = c(6.4, 7.0, 7.2, 6.6, 5.9, 7.3, 7.6, 6.5),
    n  = c(95, 95, 100, 35, 105, 90, 75, 100),
    p  = c(35, 35, 32, 28, 60, 32, 25, 55),
    k  = c(120, 115, 150, 90, 190, 110, 100, 160),
    oc = c(0.9, 0.8, 0.7, 0.5, 1.4, 0.9, 0.6, 0.8),
    stringsAsFactors = FALSE
  )
  plots <- data.frame(
    farm_idx = c(1, 2, 2, 3, 4, 5, 5, 6, 7, 8),
    crop = c("Rice", "Maize", "Cotton", "Sugarcane", "Groundnut", "Potato", "Tomato",
             "Wheat", "Cotton", "Tomato"),
    area = c(12.5, 8, 6.5, 20, 9.5, 5, 3.5, 15, 18, 4.5),
    stringsAsFactors = FALSE
  )

  out <- lapply(seq_len(nrow(plots)), function(j) {
    f <- farms[plots$farm_idx[j], ]
    cr <- plots$crop[j]
    p <- CROP_PROFILE[CROP_PROFILE$crop == cr, ]
    i <- seq_len(n_obs)
    dates <- start + (i - 1) * 2

    temp <- f$clim_t + 2.5 * sin(2 * pi * i / n_obs) + rnorm(n_obs, 0, 1.3)
    hum  <- pmin(98, pmax(25, f$clim_h + rnorm(n_obs, 0, 5) - 0.6 * (temp - f$clim_t)))
    rain <- ifelse(runif(n_obs) < 0.35, rexp(n_obs, rate = 0.35 / f$clim_r), 0)

    # Soil moisture: decays, is refilled by rain, and by occasional irrigation when low
    moist <- numeric(n_obs); m <- 35
    for (t in i) {
      m <- 0.9 * m + 0.9 * rain[t] + 0.8 + rnorm(1, 0, 1)
      if (m < 22 && runif(1) < 0.5) m <- m + 15
      m <- min(62, max(8, m)); moist[t] <- m
    }

    planting <- start - round(runif(1, 0.1, 0.5) * p$duration)
    prog <- pmin(1, as.numeric(dates - planting) / p$duration)

    d <- data.frame(
      farm_id = f$farm_id, farm_name = f$farm_name, location = f$location,
      latitude = f$latitude, longitude = f$longitude, crop_type = cr,
      cultivated_area = plots$area[j], date = dates,
      reading_time = sprintf("06:%02d", sample(c(0, 15, 30, 45), n_obs, TRUE)),
      planting_date = planting,
      soil_ph = f$ph + cumsum(rnorm(n_obs, 0, 0.01)) + rnorm(n_obs, 0, 0.04),
      soil_moisture = moist,
      soil_temperature = temp - 2 + rnorm(n_obs, 0, 0.8),
      nitrogen   = pmax(10, f$n + cumsum(rnorm(n_obs, 0, 1.5)) - 0.12 * i),
      phosphorus = pmax(3,  f$p + cumsum(rnorm(n_obs, 0, 0.5)) - 0.04 * i),
      potassium  = pmax(20, f$k + cumsum(rnorm(n_obs, 0, 1.5)) - 0.10 * i),
      organic_carbon = pmax(0.2, f$oc + rnorm(n_obs, 0, 0.03)),
      temperature = temp, humidity = hum, rainfall = rain,
      wind_speed = pmin(35, pmax(2, rnorm(n_obs, 11, 4))),
      light_intensity = pmin(110000, pmax(8000, rnorm(n_obs, 62000 - 350 * (hum - 60) - 900 * rain, 6000))),
      stringsAsFactors = FALSE
    )
    d$growth_stage <- stage_from_progress(prog)
    # Health = agronomic fit score x plot management factor + noise, with occasional pest/disease dips
    h <- health_score(d) * runif(1, 0.88, 1) + rnorm(n_obs, 0, 2.5) - ifelse(runif(n_obs) < 0.04, 15, 0)
    d$crop_health <- round(pmin(100, pmax(15, h)), 1)
    d$yield <- round(p$base_yield * (0.3 + 0.7 * (d$crop_health / 100)^1.3) * exp(rnorm(n_obs, 0, 0.05)), 2)
    d$data_source <- "Simulated"
    d
  })

  df <- dplyr::bind_rows(out)
  num <- c("soil_ph", "soil_temperature", "organic_carbon", "temperature", "humidity", "rainfall",
           "wind_speed", "nitrogen", "phosphorus", "potassium", "soil_moisture")
  df[num] <- lapply(df[num], round, 2)
  df$light_intensity <- round(df$light_intensity)
  df <- df[order(df$farm_id, df$crop_type, df$date), ALL_COLS]
  rownames(df) <- NULL
  df
}
