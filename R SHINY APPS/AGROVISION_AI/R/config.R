# ============================================================================
# AGROVISION AI - configuration (constants shared by all modules)
# Files in R/ are auto-sourced by Shiny (>= 1.5) in alphabetical order.
# ============================================================================

APP_NAME <- "AGROVISION AI"

CROPS  <- c("Rice", "Wheat", "Maize", "Cotton", "Sugarcane", "Groundnut", "Tomato", "Potato")
STAGES <- c("Germination", "Vegetative", "Flowering", "Grain/Fruit Development", "Maturity")

# Simplified stage coefficients (inspired by FAO-56 crop coefficients; NOT a validated prescription)
STAGE_KC <- c(Germination = 0.45, Vegetative = 0.85, Flowering = 1.15,
              `Grain/Fruit Development` = 1.0, Maturity = 0.65)

HEALTH_LEVELS <- c("Excellent", "Good", "Moderate", "Poor", "Critical")
HEALTH_COLORS <- c(Excellent = "#1B5E20", Good = "#66BB6A", Moderate = "#F9A825",
                   Poor = "#EF6C00", Critical = "#C62828")
SEV_COLORS    <- c(Critical = "#C62828", Warning = "#F9A825", Informational = "#0277BD")
IRR_LABELS    <- c("No irrigation required", "Monitor soil moisture",
                   "Irrigation recommended", "Urgent irrigation assessment")
IRR_COLORS    <- c("#2E7D32", "#F9A825", "#EF6C00", "#C62828")
PAL <- c("#1B5E20", "#43A047", "#8D6E63", "#F9A825", "#0277BD", "#A1887F", "#81C784", "#EF6C00")

# Illustrative agronomic reference ranges per crop (generic ballpark values, NOT locally validated).
# rain_* = average daily rainfall in mm; moist_* = soil moisture %; yields in t/ha; duration in days.
CROP_PROFILE <- data.frame(
  crop = CROPS,
  temp_lo  = c(22, 12, 20, 24, 24, 24, 20, 15), temp_hi  = c(32, 24, 30, 34, 34, 32, 28, 22),
  hum_lo   = c(70, 50, 55, 50, 65, 55, 60, 65), hum_hi   = c(90, 70, 75, 75, 85, 75, 80, 85),
  rain_lo  = c(4, 1, 2, 2, 4, 2, 2, 2),         rain_hi  = c(12, 3, 6, 5, 10, 5, 5, 5),
  ph_lo    = c(5.5, 6.0, 5.8, 6.0, 6.0, 5.8, 6.0, 5.2), ph_hi = c(7.0, 7.5, 7.2, 8.0, 7.8, 7.0, 6.8, 6.5),
  n_lo     = c(60, 60, 70, 50, 80, 20, 80, 80),  n_hi     = c(120, 120, 130, 100, 140, 50, 140, 140),
  p_lo     = c(20, 20, 25, 15, 20, 20, 40, 40),  p_hi     = c(50, 45, 50, 35, 45, 40, 80, 80),
  k_lo     = c(80, 70, 80, 80, 100, 60, 120, 150), k_hi   = c(160, 140, 150, 150, 200, 120, 220, 250),
  moist_lo = c(45, 28, 30, 28, 35, 25, 32, 32),  moist_hi = c(60, 42, 45, 42, 50, 40, 48, 48),
  base_yield   = c(5.5, 4.5, 6.5, 2.0, 75, 2.5, 40, 25),
  duration     = c(120, 120, 110, 180, 300, 110, 120, 100),
  water_factor = c(1.2, 0.9, 1.0, 1.0, 1.15, 0.85, 1.0, 0.95),
  stringsAsFactors = FALSE
)

# Sensor definitions: warning / critical limits drive status badges in the monitoring module
SENSORS <- data.frame(
  var   = c("soil_moisture", "soil_temperature", "temperature", "humidity", "rainfall",
            "light_intensity", "soil_ph", "nitrogen", "phosphorus", "potassium"),
  label = c("Soil Moisture", "Soil Temperature", "Air Temperature", "Air Humidity", "Rainfall",
            "Light Intensity", "Soil pH", "Nitrogen (N)", "Phosphorus (P)", "Potassium (K)"),
  unit  = c("%", "\u00b0C", "\u00b0C", "%", "mm", "lux", "pH", "mg/kg", "mg/kg", "mg/kg"),
  icon  = c("tint", "thermometer-half", "sun", "cloud", "cloud-rain", "lightbulb", "flask",
            "seedling", "seedling", "seedling"),
  warn_lo = c(30, 15, 12, 35, -Inf, 20000, 5.8, 40, 15, 80),
  warn_hi = c(65, 35, 35, 90, 40, 90000, 7.8, Inf, Inf, Inf),
  crit_lo = c(20, 10, 5, 25, -Inf, 10000, 5.2, 25, 8, 50),
  crit_hi = c(75, 40, 38, 95, 80, 110000, 8.4, Inf, Inf, Inf),
  stringsAsFactors = FALSE
)

# Data schema
ALL_COLS <- c("farm_id", "farm_name", "location", "latitude", "longitude", "crop_type",
              "cultivated_area", "date", "reading_time", "planting_date", "soil_ph", "soil_moisture",
              "soil_temperature", "nitrogen", "phosphorus", "potassium", "organic_carbon",
              "temperature", "humidity", "rainfall", "wind_speed", "light_intensity",
              "crop_health", "growth_stage", "yield", "data_source")
REQUIRED_COLS <- c("farm_id", "farm_name", "crop_type", "cultivated_area", "date", "soil_ph",
                   "soil_moisture", "nitrogen", "phosphorus", "potassium", "temperature",
                   "humidity", "rainfall")
NUM_COLS <- c("latitude", "longitude", "cultivated_area", "soil_ph", "soil_moisture",
              "soil_temperature", "nitrogen", "phosphorus", "potassium", "organic_carbon",
              "temperature", "humidity", "rainfall", "wind_speed", "light_intensity",
              "crop_health", "yield")
# Fallback values used to impute missing numeric readings when a whole column is absent
IMPUTE_DEFAULTS <- list(soil_ph = 6.5, soil_moisture = 35, soil_temperature = 24, nitrogen = 80,
                        phosphorus = 30, potassium = 120, organic_carbon = 0.8, temperature = 26,
                        humidity = 65, rainfall = 2, wind_speed = 10, light_intensity = 50000)

# Default alert thresholds (editable in the Alerts module)
DEFAULT_TH <- list(moist_crit = 20, moist_warn = 30, temp_warn = 35, temp_crit = 38,
                   ph_lo = 6.0, ph_hi = 7.5, n_min = 50, p_min = 20, k_min = 100,
                   health_warn = 55, health_crit = 40)

# Variables offered in the visualization module
VIZ_NUM <- c("soil_ph", "soil_moisture", "soil_temperature", "nitrogen", "phosphorus", "potassium",
             "organic_carbon", "temperature", "humidity", "rainfall", "wind_speed", "light_intensity",
             "crop_health", "yield", "yield_index", "cultivated_area", "water_stress_index", "disease_risk_index",
             "growth_progress")
VIZ_CAT <- c("farm_name", "crop_type", "growth_stage", "health_class", "plot_id")
