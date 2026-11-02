# ============================================================================
# AGROVISION AI - reusable helper functions (no Shiny reactivity in here)
# ============================================================================

`%||%` <- function(a, b) if (is.null(a) || (length(a) == 1 && is.na(a))) b else a

theme_agro <- function(base = 11) {
  ggplot2::theme_minimal(base_size = base) +
    ggplot2::theme(plot.title = ggplot2::element_text(face = "bold", colour = "#1B5E20"),
                   panel.grid.minor = ggplot2::element_blank(),
                   legend.position = "bottom",
                   strip.text = ggplot2::element_text(face = "bold"))
}
pal <- function(n) rep(PAL, length.out = max(n, 1))

# ---------------------------------------------------------------- scoring ---
# 1 inside [lo, hi]; decays linearly to 0 at a distance of 0.75 * (hi - lo)
range_score <- function(x, lo, hi) {
  tol <- pmax(0.75 * (hi - lo), 1e-6)
  pmax(0, 1 - pmax(lo - x, x - hi, 0) / tol)
}

# Agronomic-fit health score (0-100) from environment vs. crop reference ranges
health_score <- function(d) {
  p <- CROP_PROFILE[match(d$crop_type, CROP_PROFILE$crop), ]
  s <- 0.22 * range_score(d$temperature, p$temp_lo, p$temp_hi) +
    0.20 * range_score(d$soil_moisture, p$moist_lo, p$moist_hi) +
    0.14 * range_score(d$soil_ph, p$ph_lo, p$ph_hi) +
    0.14 * range_score(d$nitrogen, p$n_lo, p$n_hi) +
    0.10 * range_score(d$humidity, p$hum_lo, p$hum_hi) +
    0.08 * range_score(d$phosphorus, p$p_lo, p$p_hi) +
    0.08 * range_score(d$potassium, p$k_lo, p$k_hi) +
    0.04 * range_score(d$rainfall, p$rain_lo, p$rain_hi)
  pmin(100, pmax(0, 100 * s))
}

stage_from_progress <- function(prog) {
  as.character(cut(prog, c(-Inf, .1, .4, .6, .85, Inf), labels = STAGES, right = FALSE))
}

# Derived (rule-based, simulated) indicators. These are transparent proxies, NOT image-based detection.
enrich <- function(d) {
  p <- CROP_PROFILE[match(d$crop_type, CROP_PROFILE$crop), ]
  d$plot_id <- paste(d$farm_id, d$crop_type, sep = " | ")
  d$health_class <- factor(as.character(cut(d$crop_health, c(-Inf, 40, 55, 70, 85, Inf),
                           labels = c("Critical", "Poor", "Moderate", "Good", "Excellent"), right = FALSE)),
                           levels = HEALTH_LEVELS)
  d$water_stress_index <- round(pmin(1, pmax(0, 0.7 * pmax(0, (p$moist_lo - d$soil_moisture) / p$moist_lo) +
                                                 0.3 * pmax(0, (d$temperature - p$temp_hi) / 10))), 2)
  d$stress_level <- factor(as.character(cut(d$water_stress_index, c(-Inf, .2, .5, Inf),
                           labels = c("Low", "Moderate", "High"), right = FALSE)), levels = c("Low", "Moderate", "High"))
  d$disease_risk_index <- round(pmin(1, 0.6 * pmax(0, (d$humidity - 70) / 30) +
                                        0.4 * pmax(0, 1 - abs(d$temperature - 26) / 10)), 2)
  d$disease_risk <- factor(as.character(cut(d$disease_risk_index, c(-Inf, .33, .66, Inf),
                           labels = c("Low", "Moderate", "High"), right = FALSE)), levels = c("Low", "Moderate", "High"))
  d$leaf_index <- round(0.2 + 0.7 * d$crop_health / 100, 2)   # NDVI-like proxy derived from the health score
  d$growth_progress <- round(pmin(1, pmax(0, as.numeric(d$date - d$planting_date) / p$duration)), 2)
  d$production_t <- d$yield * d$cultivated_area
  d$yield_index <- round(100 * d$yield / p$base_yield, 1)    # % of the crop's reference yield
  d
}

latest_per_plot <- function(d) {
  d %>% dplyr::group_by(farm_id, crop_type) %>%
    dplyr::slice_max(date, n = 1, with_ties = FALSE) %>% dplyr::ungroup()
}

# Soil quality (0-100) from configurable thresholds
soil_quality <- function(d, th) {
  sc <- cbind(range_score(d$soil_ph, th$ph[1], th$ph[2]),
              pmin(1, d$nitrogen / th$n), pmin(1, d$phosphorus / th$p), pmin(1, d$potassium / th$k),
              range_score(d$organic_carbon, th$oc[1], th$oc[2]),
              range_score(d$soil_moisture, th$moist[1], th$moist[2]))
  round(100 * rowMeans(sc), 1)
}

soil_recs <- function(m, th) {
  r <- character()
  if (m$soil_ph < th$ph[1]) r <- c(r, sprintf("Mean pH %.2f is below %.1f (acidic): liming guided by a lab soil test may help.", m$soil_ph, th$ph[1]))
  if (m$soil_ph > th$ph[2]) r <- c(r, sprintf("Mean pH %.2f is above %.1f (alkaline): organic matter, gypsum or acidifying fertilisers may help; confirm with a soil test.", m$soil_ph, th$ph[2]))
  if (m$nitrogen < th$n) r <- c(r, sprintf("Nitrogen %.0f mg/kg is below %.0f: consider split nitrogen applications, compost or legume rotation.", m$nitrogen, th$n))
  if (m$phosphorus < th$p) r <- c(r, sprintf("Phosphorus %.0f mg/kg is below %.0f: consider phosphate sources placed near the root zone.", m$phosphorus, th$p))
  if (m$potassium < th$k) r <- c(r, sprintf("Potassium %.0f mg/kg is below %.0f: consider potash or potassium-rich organic inputs.", m$potassium, th$k))
  if (m$organic_carbon < th$oc[1]) r <- c(r, sprintf("Organic carbon %.2f%% is low: add compost/residues, reduce tillage, use cover crops.", m$organic_carbon))
  if (m$soil_moisture < th$moist[1]) r <- c(r, sprintf("Soil moisture %.1f%% is below %.0f%%: review irrigation scheduling and mulching.", m$soil_moisture, th$moist[1]))
  if (!length(r)) r <- "All analysed parameters are within the selected thresholds."
  r
}

# ----------------------------------------------------------- data I/O -------
read_any_csv <- function(path) {
  suppressWarnings(readr::read_csv(path, show_col_types = FALSE, progress = FALSE,
                  col_types = readr::cols(.default = readr::col_guess(), farm_id = readr::col_character(),
                                          reading_time = readr::col_character(),
                                          date = readr::col_character(), planting_date = readr::col_character())))
}

read_iot_json <- function(src) {
  # `src` may be a local .json path or a URL returning an array of records with the schema columns.
  x <- jsonlite::fromJSON(src, flatten = TRUE)
  if (is.list(x) && !is.data.frame(x)) x <- x$data %||% x[[1]]
  as.data.frame(x, stringsAsFactors = FALSE)
}

# Validate, clean and standardise any incoming table. Returns list(ok, data, messages)
validate_upload <- function(raw) {
  msgs <- character()
  d <- as.data.frame(raw, stringsAsFactors = FALSE)
  names(d) <- tolower(trimws(names(d)))
  miss <- setdiff(REQUIRED_COLS, names(d))
  if (length(miss)) return(list(ok = FALSE, data = NULL,
                                messages = paste("Missing required columns:", paste(miss, collapse = ", "))))
  for (cl in setdiff(ALL_COLS, names(d))) d[[cl]] <- NA
  for (cl in NUM_COLS) d[[cl]] <- suppressWarnings(as.numeric(d[[cl]]))
  prs <- function(x) as.Date(lubridate::parse_date_time(as.character(x), orders = c("ymd", "dmy", "mdy"), quiet = TRUE))
  d$date <- prs(d$date); d$planting_date <- prs(d$planting_date)
  d$crop_type <- tools::toTitleCase(tolower(trimws(as.character(d$crop_type))))
  d$farm_id <- trimws(as.character(d$farm_id)); d$farm_name <- trimws(as.character(d$farm_name))
  inr <- function(x, lo, hi) is.na(x) | (x >= lo & x <= hi)
  keep <- !is.na(d$date) & d$crop_type %in% CROPS & !is.na(d$cultivated_area) & d$cultivated_area > 0 &
    !is.na(d$farm_id) & nzchar(d$farm_id) & !is.na(d$farm_name) & nzchar(d$farm_name) &
    inr(d$soil_ph, 3, 10) & inr(d$soil_moisture, 0, 100) & inr(d$humidity, 0, 100) &
    inr(d$rainfall, 0, 1000) & inr(d$temperature, -20, 60) & inr(d$nitrogen, 0, 1000) &
    inr(d$phosphorus, 0, 1000) & inr(d$potassium, 0, 2000)
  if (sum(!keep) > 0) msgs <- c(msgs, sprintf("%d row(s) removed (bad date/crop/area/farm or out-of-range values). Supported crops: %s.",
                                              sum(!keep), paste(CROPS, collapse = ", ")))
  d <- d[keep, , drop = FALSE]
  if (!nrow(d)) return(list(ok = FALSE, data = NULL, messages = c(msgs, "No valid rows remain.")))

  # Missing-value handling: crop-wise median, then overall median, then documented default
  n_na <- 0
  for (cl in names(IMPUTE_DEFAULTS)) {
    na <- is.na(d[[cl]]); if (!any(na)) next
    n_na <- n_na + sum(na)
    g <- stats::ave(d[[cl]], d$crop_type, FUN = function(x) stats::median(x, na.rm = TRUE))
    d[[cl]][na] <- g[na]
    na2 <- is.na(d[[cl]])
    if (any(na2)) d[[cl]][na2] <- if (all(is.na(d[[cl]]))) IMPUTE_DEFAULTS[[cl]] else stats::median(d[[cl]], na.rm = TRUE)
  }
  if (n_na > 0) msgs <- c(msgs, sprintf("%d missing numeric value(s) imputed (crop-wise median, then overall median/default).", n_na))

  d$location[is.na(d$location)] <- "Unknown"
  d$reading_time <- ifelse(is.na(d$reading_time), "00:00", substr(as.character(d$reading_time), 1, 5))
  d$data_source <- ifelse(is.na(d$data_source), "Uploaded", as.character(d$data_source))
  dur <- CROP_PROFILE$duration[match(d$crop_type, CROP_PROFILE$crop)]
  first <- stats::ave(as.numeric(d$date), d$farm_id, d$crop_type, FUN = min)
  np <- is.na(d$planting_date)
  d$planting_date[np] <- as.Date(first[np] - round(0.25 * dur[np]), origin = "1970-01-01")
  prog <- pmin(1, pmax(0, as.numeric(d$date - d$planting_date) / dur))
  d$growth_stage <- ifelse(is.na(d$growth_stage) | !(d$growth_stage %in% STAGES), stage_from_progress(prog), d$growth_stage)
  if (any(is.na(d$crop_health))) {
    d$crop_health[is.na(d$crop_health)] <- round(health_score(d)[is.na(d$crop_health)], 1)
    msgs <- c(msgs, "Missing crop_health values were calculated with the agronomic-fit health score.")
  }
  d <- d[, ALL_COLS]
  msgs <- c(msgs, sprintf("%d valid record(s) ready.", nrow(d)))
  list(ok = TRUE, data = d, messages = msgs)
}

load_initial_data <- function(path = file.path("data", "sample_agriculture.csv")) {
  if (!file.exists(path)) {
    dir.create(dirname(path), showWarnings = FALSE, recursive = TRUE)
    readr::write_csv(generate_sample_data(), path)
  }
  v <- validate_upload(read_any_csv(path))
  if (!v$ok) stop(paste(v$messages, collapse = " "))
  v$data
}

# --------------------------------------------------------- machine learning -
MODEL_FEATURES <- c("crop_type", "soil_ph", "soil_moisture", "temperature", "humidity", "rainfall",
                    "nitrogen", "phosphorus", "potassium", "cultivated_area")

prep_features <- function(d, medians, crop_levels = CROPS) {
  x <- as.data.frame(d)[, MODEL_FEATURES, drop = FALSE]
  x$crop_type <- factor(as.character(x$crop_type), levels = crop_levels)       # feature encoding
  for (cl in setdiff(MODEL_FEATURES, "crop_type")) {
    x[[cl]] <- as.numeric(x[[cl]])
    x[[cl]][is.na(x[[cl]])] <- medians[[cl]]                                    # missing value handling
  }
  x$crop_type[is.na(x$crop_type)] <- crop_levels[1]
  x
}

train_yield_model <- function(d, split = 0.8, ntree = 300, seed = 42) {
  d <- d[!is.na(d$yield), ]
  if (nrow(d) < 50) stop("At least 50 records with yield values are required to train the model.")
  num <- setdiff(MODEL_FEATURES, "crop_type")
  medians <- lapply(as.data.frame(d)[num], stats::median, na.rm = TRUE)
  set.seed(seed)                                                                 # reproducibility
  x <- prep_features(d, medians)
  idx <- as.vector(caret::createDataPartition(d$yield, p = split, list = FALSE))
  rf <- randomForest::randomForest(x = x[idx, ], y = d$yield[idx], ntree = ntree, importance = TRUE)
  pred <- as.numeric(stats::predict(rf, x[-idx, ]))
  obs <- d$yield[-idx]
  pr <- caret::postResample(pred, obs)
  metrics <- c(RMSE = unname(pr["RMSE"]), MAE = unname(pr["MAE"]),
               R2 = 1 - sum((obs - pred)^2) / sum((obs - mean(obs))^2))
  imp <- as.data.frame(randomForest::importance(rf)); imp$feature <- rownames(imp)
  list(model = rf, medians = medians, metrics = metrics, importance = imp,
       test = data.frame(actual = obs, predicted = pred, crop = d$crop_type[-idx], farm = d$farm_name[-idx]),
       n_train = length(idx), n_test = length(obs), ntree = ntree, seed = seed, split = split,
       oob_r2 = utils::tail(rf$rsq, 1), trained_at = Sys.time(),
       simulated = any(d$data_source == "Simulated", na.rm = TRUE))
}

predict_yield <- function(mod, d) as.numeric(stats::predict(mod$model, prep_features(d, mod$medians)))

# ----------------------------------------------------------------- alerts ---
build_alerts <- function(d, th = DEFAULT_TH) {
  empty <- data.frame(timestamp = as.POSIXct(character()), severity = character(), farm = character(),
                      crop = character(), type = character(), description = character(),
                      action = character(), stringsAsFactors = FALSE)
  if (!nrow(d)) return(empty)
  ts <- as.POSIXct(paste(d$date, d$reading_time), tz = "UTC", format = "%Y-%m-%d %H:%M")
  mk <- function(cond, sev, type, desc, act) {
    i <- which(cond)
    if (!length(i)) return(NULL)
    data.frame(timestamp = ts[i], severity = sev, farm = d$farm_name[i], crop = d$crop_type[i],
               type = type, description = desc[i], action = act, stringsAsFactors = FALSE)
  }
  z <- function(x) { s <- stats::ave(x, d$plot_id, FUN = function(v) abs(v - mean(v)) / (stats::sd(v) + 1e-9)); s }
  nut <- function(v, mn, nm) rbind(
    mk(v < 0.6 * mn, "Critical", "Low nutrient level", sprintf("%s %.0f mg/kg is far below the %.0f threshold.", nm, v, mn), sprintf("Plan %s fertilisation after a lab soil test.", nm)),
    mk(v >= 0.6 * mn & v < mn, "Warning", "Low nutrient level", sprintf("%s %.0f mg/kg is below the %.0f threshold.", nm, v, mn), sprintf("Monitor %s and consider supplementation.", nm)))
  a <- rbind(
    mk(d$soil_moisture < th$moist_crit, "Critical", "Low soil moisture", sprintf("Soil moisture %.1f%% below critical level %.0f%%.", d$soil_moisture, th$moist_crit), "Assess irrigation immediately."),
    mk(d$soil_moisture >= th$moist_crit & d$soil_moisture < th$moist_warn, "Warning", "Low soil moisture", sprintf("Soil moisture %.1f%% below %.0f%%.", d$soil_moisture, th$moist_warn), "Check irrigation schedule; monitor closely."),
    mk(d$temperature > th$temp_crit, "Critical", "Excessive temperature", sprintf("Air temperature %.1f C above %.0f C.", d$temperature, th$temp_crit), "Consider shading, mulching or irrigation timing changes."),
    mk(d$temperature > th$temp_warn & d$temperature <= th$temp_crit, "Warning", "Excessive temperature", sprintf("Air temperature %.1f C above %.0f C.", d$temperature, th$temp_warn), "Watch for heat stress; avoid midday operations."),
    mk(d$soil_ph < th$ph_lo - 0.5 | d$soil_ph > th$ph_hi + 0.5, "Critical", "Abnormal soil pH", sprintf("Soil pH %.2f far outside %.1f-%.1f.", d$soil_ph, th$ph_lo, th$ph_hi), "Run a lab soil test and plan pH correction."),
    mk((d$soil_ph < th$ph_lo | d$soil_ph > th$ph_hi) & !(d$soil_ph < th$ph_lo - 0.5 | d$soil_ph > th$ph_hi + 0.5), "Warning", "Abnormal soil pH", sprintf("Soil pH %.2f outside %.1f-%.1f.", d$soil_ph, th$ph_lo, th$ph_hi), "Monitor pH and consider amendments."),
    nut(d$nitrogen, th$n_min, "Nitrogen"), nut(d$phosphorus, th$p_min, "Phosphorus"), nut(d$potassium, th$k_min, "Potassium"),
    mk(d$crop_health < th$health_crit | d$water_stress_index >= 0.5, "Critical", "High crop stress",
       sprintf("Crop health %.0f, water stress index %.2f.", d$crop_health, d$water_stress_index), "Inspect the field; review water and nutrient status."),
    mk(d$crop_health >= th$health_crit & d$crop_health < th$health_warn, "Warning", "High crop stress",
       sprintf("Crop health score %.0f is below %.0f.", d$crop_health, th$health_warn), "Scout the plot for stress factors."),
    mk(z(d$soil_moisture) > 3 | z(d$temperature) > 3 | z(d$humidity) > 3, "Informational", "Unusual environmental reading",
       "A reading deviates by more than 3 SD from the plot's own history.", "Verify the sensor and the reading."),
    mk(d$rainfall >= 30, "Informational", "Unusual environmental reading", sprintf("Heavy rainfall %.1f mm.", d$rainfall), "Check drainage; postpone fertiliser/spraying."),
    mk(d$disease_risk_index >= 0.7, "Informational", "Unusual environmental reading", sprintf("Disease-risk proxy %.2f (humid + warm).", d$disease_risk_index), "Scout for fungal disease symptoms.")
  )
  if (is.null(a) || !nrow(a)) return(empty)
  a$severity <- factor(a$severity, levels = c("Critical", "Warning", "Informational"))
  a[order(a$timestamp, decreasing = TRUE), ]
}

# ------------------------------------------------------------- irrigation ---
# Transparent, simplified heuristic (NOT a scientifically validated irrigation prescription)
irrigation_advice <- function(moisture, temp, hum, rain, crop, stage, area = 1) {
  p <- CROP_PROFILE[match(crop, CROP_PROFILE$crop), ]
  kc <- unname(STAGE_KC[stage]); kc[is.na(kc)] <- 0.8
  et0 <- pmax(1, 0.12 * temp + 0.03 * (100 - hum) - 0.5)          # crude reference ET (mm/day)
  etc <- et0 * kc * p$water_factor
  target <- (p$moist_lo + p$moist_hi) / 2
  deficit <- pmax(0, target - moisture) / 100 * 300               # assumes a 300 mm root zone
  need <- round(pmax(0, deficit + etc - 0.8 * rain), 1)
  lvl <- ifelse(moisture < 0.6 * p$moist_lo, 4, ifelse(moisture < 0.85 * p$moist_lo, 3,
                ifelse(moisture < p$moist_lo, 2, 1)))
  lvl <- ifelse(rain >= 15, pmax(1, lvl - 1), lvl)
  lvl <- ifelse(temp >= 35 & lvl == 2, 3, lvl)
  expl <- paste0("Soil moisture ", round(moisture, 1), "% vs reference range ", p$moist_lo, "-", p$moist_hi, "% for ", crop,
                 ". Estimated ETc ", round(etc, 1), " mm/day (ET0 ", round(et0, 1), " x Kc ", kc, " x crop factor ", p$water_factor, "). ",
                 ifelse(rain >= 15, "Heavy rain recorded, so the level was lowered one step. ",
                        ifelse(rain > 0, paste0("Effective rain credit ", round(0.8 * rain, 1), " mm. "), "")),
                 ifelse(temp >= 35, "High temperature increases evaporative demand. ", ""))
  data.frame(level = lvl, status = factor(IRR_LABELS[lvl], levels = IRR_LABELS), et0 = round(et0, 1),
             etc = round(etc, 1), need_mm = need, volume_m3 = round(need * 10 * area),   # 1 mm over 1 ha = 10 m3
             water_stress = round(pmin(1, pmax(0, (p$moist_lo - moisture) / p$moist_lo)), 2),
             explanation = expl, stringsAsFactors = FALSE)
}

# -------------------------------------------------------- crop recommender --
# Explainable algorithm: triangular-style membership of each input against each crop's reference range.
# suitability = 50% environment (temperature, humidity, rainfall) + 50% soil (pH, N, P, K)
recommend_crops <- function(n, p, k, temp, hum, ph, rain) {
  P <- CROP_PROFILE
  m <- data.frame(Temperature = range_score(temp, P$temp_lo, P$temp_hi),
                  Humidity    = range_score(hum, P$hum_lo, P$hum_hi),
                  Rainfall    = range_score(rain, P$rain_lo, P$rain_hi),
                  pH          = range_score(ph, P$ph_lo, P$ph_hi),
                  Nitrogen    = range_score(n, P$n_lo, P$n_hi),
                  Phosphorus  = range_score(p, P$p_lo, P$p_hi),
                  Potassium   = range_score(k, P$k_lo, P$k_hi))
  env <- rowMeans(m[, 1:3]); soil <- rowMeans(m[, 4:7])
  out <- data.frame(crop = P$crop, suitability = round(100 * (0.5 * env + 0.5 * soil), 1),
                    environment = round(100 * env, 1), soil = round(100 * soil, 1),
                    limiting_factor = names(m)[max.col(-as.matrix(m), ties.method = "first")],
                    stringsAsFactors = FALSE)
  out <- cbind(out, round(100 * m))
  out[order(-out$suitability), ]
}

explain_crop <- function(crop, vals) {
  P <- CROP_PROFILE[CROP_PROFILE$crop == crop, ]
  chk <- function(nm, x, lo, hi, u = "") {
    st <- if (x < lo) "below" else if (x > hi) "above" else "within"
    sprintf("%s %s%s is %s the reference range %s-%s%s", nm, x, u, st, lo, hi, u)
  }
  c(chk("Temperature", vals$t, P$temp_lo, P$temp_hi, " C"), chk("Humidity", vals$h, P$hum_lo, P$hum_hi, "%"),
    chk("Rainfall", vals$r, P$rain_lo, P$rain_hi, " mm/day"), chk("Soil pH", vals$ph, P$ph_lo, P$ph_hi),
    chk("Nitrogen", vals$n, P$n_lo, P$n_hi, " mg/kg"), chk("Phosphorus", vals$p, P$p_lo, P$p_hi, " mg/kg"),
    chk("Potassium", vals$k, P$k_lo, P$k_hi, " mg/kg"))
}

sensor_status <- function(x, s) {
  if (is.na(x)) return("No data")
  if (x < s$crit_lo || x > s$crit_hi) "Critical" else if (x < s$warn_lo || x > s$warn_hi) "Warning" else "Normal"
}

# ------------------------------------------------------------- PDF report ---
# Base-graphics PDF: works on Windows without LaTeX / Pandoc.
write_pdf_report <- function(file, d, alerts, irr, preds, mod, sections, title, author, filters_txt) {
  grDevices::pdf(file, width = 8.27, height = 11.69, onefile = TRUE)
  on.exit(grDevices::dev.off(), add = TRUE)
  op <- options(width = 125); on.exit(options(op), add = TRUE)
  reset <- function() graphics::par(mfrow = c(1, 1), oma = c(0, 0, 0, 0), mar = c(5.1, 4.1, 4.1, 2.1))
  txt_page <- function(h, lines, cex = 0.62) {
    reset(); graphics::par(mar = c(0, 0, 0, 0)); graphics::plot.new()
    graphics::text(0.04, 0.97, h, adj = c(0, 1), cex = 1.5, font = 2, col = "#1B5E20")
    graphics::text(0.04, 0.93, paste(lines, collapse = "\n"), adj = c(0, 1), family = "mono", cex = cex)
  }
  tbl <- function(x, n = 50) utils::capture.output(print(utils::head(as.data.frame(x), n), row.names = FALSE))
  chart_page <- function(h, nr) {
    reset(); graphics::par(mfrow = c(nr, 1), oma = c(0, 0, 3, 0), mar = c(9, 4, 3, 1))
  }
  title_it <- function(h) graphics::mtext(h, outer = TRUE, font = 2, cex = 1.3, col = "#1B5E20")

  txt_page(title, c(paste("Prepared by:", author), paste("Generated:", format(Sys.time(), "%Y-%m-%d %H:%M")),
                    "", paste("Filters:", filters_txt), "",
                    sprintf("Records: %d | Farms: %d | Crops: %d | Date range: %s to %s", nrow(d),
                            dplyr::n_distinct(d$farm_id), dplyr::n_distinct(d$crop_type), min(d$date), max(d$date)),
                    paste("Data sources:", paste(unique(d$data_source), collapse = ", ")), "",
                    strwrap(paste("Notice: values labelled 'Simulated' are synthetic demonstration data, not measurements.",
                                  "Health, stress and disease indicators are rule-based proxies, irrigation advice is a simplified",
                                  "heuristic and the yield model is a demonstration model. None are validated agronomic prescriptions."), 110)), 0.75)
  if ("farm" %in% sections) {
    fs <- d %>% dplyr::group_by(farm_name, location, crop_type) %>%
      dplyr::summarise(area_ha = dplyr::first(cultivated_area), records = dplyr::n(), last_reading = max(date), .groups = "drop")
    txt_page("Farm information", tbl(fs))
  }
  if ("crop" %in% sections) {
    cs <- d %>% dplyr::group_by(crop_type) %>% dplyr::summarise(plots = dplyr::n_distinct(plot_id), mean_health = round(mean(crop_health), 1),
                                                                 mean_yield_t_ha = round(mean(yield, na.rm = TRUE), 2), .groups = "drop")
    chart_page("Crop monitoring summary", 2); title_it("Crop monitoring summary")
    graphics::barplot(cs$mean_health, names.arg = cs$crop_type, las = 2, col = "#43A047", ylab = "Mean crop health", main = "Mean crop health by crop")
    graphics::par(mar = c(0, 0, 0, 0)); graphics::plot.new()
    graphics::text(0.02, 0.95, paste(tbl(cs), collapse = "\n"), adj = c(0, 1), family = "mono", cex = 0.7)
  }
  if ("soil" %in% sections) {
    sm <- stats::aggregate(cbind(soil_ph, soil_moisture, nitrogen, phosphorus, potassium, organic_carbon) ~ farm_name, d, mean)
    chart_page("Soil analysis", 2); title_it("Soil analysis")
    graphics::barplot(t(as.matrix(sm[, c("nitrogen", "phosphorus", "potassium")])), beside = TRUE, names.arg = sm$farm_name, las = 2,
                      col = c("#1B5E20", "#8D6E63", "#F9A825"), cex.names = 0.7, main = "Mean N, P, K by farm (mg/kg)",
                      legend.text = c("N", "P", "K"), args.legend = list(x = "topright", cex = 0.7))
    graphics::par(mar = c(0, 0, 0, 0)); graphics::plot.new()
    sm[-1] <- lapply(sm[-1], round, 2)
    graphics::text(0.02, 0.95, paste(tbl(sm), collapse = "\n"), adj = c(0, 1), family = "mono", cex = 0.7)
  }
  if ("env" %in% sections) {
    ag <- stats::aggregate(cbind(temperature, humidity, rainfall) ~ date, d, mean)
    chart_page("Environmental conditions", 3); title_it("Environmental conditions")
    graphics::par(mar = c(4, 4, 3, 1))
    graphics::plot(ag$date, ag$temperature, type = "l", col = "#EF6C00", lwd = 2, xlab = "", ylab = "deg C", main = "Mean air temperature")
    graphics::plot(ag$date, ag$humidity, type = "l", col = "#0277BD", lwd = 2, xlab = "", ylab = "%", main = "Mean humidity")
    graphics::plot(ag$date, ag$rainfall, type = "h", col = "#1B5E20", lwd = 2, xlab = "", ylab = "mm", main = "Mean rainfall")
  }
  if ("health" %in% sections) {
    ct <- table(factor(d$health_class, levels = HEALTH_LEVELS))
    chart_page("Crop health", 2); title_it("Crop health"); graphics::par(mar = c(5, 4, 3, 1))
    graphics::barplot(ct, col = HEALTH_COLORS[names(ct)], main = "Crop health class distribution", ylab = "Records")
    graphics::par(mar = c(0, 0, 0, 0)); graphics::plot.new()
    graphics::text(0.02, 0.95, paste(tbl(data.frame(class = names(ct), records = as.integer(ct))), collapse = "\n"), adj = c(0, 1), family = "mono", cex = 0.8)
  }
  if ("yield" %in% sections) {
    ln <- if (is.null(mod)) "No model trained." else c(
      sprintf("Random Forest regression (%d trees, seed %d), train %d / test %d records.", mod$ntree, mod$seed, mod$n_train, mod$n_test),
      sprintf("Test RMSE = %.3f | MAE = %.3f | R-squared = %.3f | OOB R-squared = %.3f", mod$metrics["RMSE"], mod$metrics["MAE"], mod$metrics["R2"], mod$oob_r2),
      if (mod$simulated) "DEMONSTRATION MODEL trained on simulated data; not for production predictions." else "")
    txt_page("Yield prediction", c(ln, "", if (!is.null(preds)) tbl(preds)))
  }
  if ("irrigation" %in% sections) {
    txt_page("Irrigation recommendations (simplified heuristic)",
             if (is.null(irr)) "No data." else tbl(irr[, c("Farm", "Crop", "Stage", "Moisture", "status", "need_mm", "volume_m3")]))
  }
  if ("alerts" %in% sections) {
    ct <- if (nrow(alerts)) table(alerts$severity) else c(Critical = 0, Warning = 0, Informational = 0)
    top <- if (nrow(alerts)) data.frame(time = format(alerts$timestamp, "%m-%d %H:%M"), sev = as.character(alerts$severity),
                                        farm = substr(alerts$farm, 1, 18), type = alerts$type) else data.frame()
    txt_page("Alerts and recommendations", c(paste(names(ct), as.integer(ct), sep = ": ", collapse = " | "), "", tbl(top, 55)))
  }
  invisible(file)
}
