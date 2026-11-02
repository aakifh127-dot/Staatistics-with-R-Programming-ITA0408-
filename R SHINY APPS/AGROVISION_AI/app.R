# ============================================================================
# AGROVISION AI - Smart Agriculture Intelligence Platform
# Run with:  shiny::runApp()   (working directory = AGROVISION_AI)
# ============================================================================
suppressPackageStartupMessages({
  library(shiny); library(shinydashboard); library(shinyWidgets)
  library(plotly); library(dplyr); library(tidyr); library(readr); library(ggplot2)
  library(DT); library(leaflet); library(lubridate); library(corrplot); library(writexl); library(jsonlite)
})
for (pkg in c("randomForest", "caret", "forecast"))
  if (!requireNamespace(pkg, quietly = TRUE)) stop("Please install the package '", pkg, "' (see install_packages.R)")

# Fallback if R/ was not auto-sourced (Shiny < 1.5)
if (!exists("CROP_PROFILE")) for (f in sort(list.files("R", pattern = "\\.R$", full.names = TRUE))) source(f)
for (dname in c("data", "models", "reports")) dir.create(dname, showWarnings = FALSE)

INIT <- load_initial_data()            # sample dataset (generated on first run if the CSV is missing)

# ------------------------------------------------------------ UI helpers ----
kpi <- function(value, label, icon_name, sub = NULL) {
  div(class = "kpi-card", div(class = "kpi-icon", icon(icon_name)),
      div(class = "kpi-body", div(class = "kpi-value", value), div(class = "kpi-label", label),
          if (!is.null(sub)) div(class = "kpi-sub", sub)))
}
plotbox <- function(title, id, width = 6, h = "320px")
  box(title = title, width = width, status = "success", solidHeader = TRUE, plotlyOutput(id, height = h))
dtbox <- function(title, id, width = 12)
  box(title = title, width = width, status = "success", solidHeader = TRUE, DT::DTOutput(id))
dt <- function(df, n = 10, ...) DT::datatable(df, rownames = FALSE, class = "compact stripe hover",
                                              options = list(pageLength = n, scrollX = TRUE), ...)
pl <- function(p, tip = NULL) plotly::config(plotly::ggplotly(p, tooltip = tip), displaylogo = FALSE)
need_data <- function(d) validate(need(nrow(d) > 0, "No data for the selected filters."))
nz <- function(x, d) if (is.null(x) || length(x) == 0 || is.na(x)) d else x
disclaimer <- function(...) div(class = "note-box", icon("info-circle"), " ", ...)

weather_condition <- function(t, r, h)
  dplyr::case_when(r >= 15 ~ "Heavy rain", r >= 3 ~ "Rainy", t >= 35 ~ "Hot", h >= 85 ~ "Humid", t <= 15 ~ "Cool", TRUE ~ "Fair")

# Generic chart builder for the visualization module
make_chart <- function(d, type, x, y, color, agg, bins, size_var, trend) {
  col <- if (identical(color, "none")) NULL else color
  d$.grp <- if (is.null(col)) "All" else as.character(d[[col]])
  n_g <- dplyr::n_distinct(d$.grp)
  fn <- switch(agg, Mean = mean, Median = stats::median, Sum = sum, Max = max, Min = min)
  fa <- function(v) fn(v, na.rm = TRUE)
  sc <- list(scale_color_manual(values = pal(n_g)), scale_fill_manual(values = pal(n_g)))
  lg <- col %||% ""
  if (type %in% c("Line chart", "Time-series")) {
    validate(need(is.numeric(d[[y]]), "Y variable must be numeric."))
    a <- d %>% group_by(across(all_of(c(x, ".grp")))) %>% summarise(v = fa(.data[[y]]), .groups = "drop")
    p <- ggplot(a, aes(x = .data[[x]], y = v, color = .grp, group = .grp)) + geom_line(linewidth = 0.8) + sc +
      labs(x = x, y = paste(agg, y), color = lg) + theme_agro()
    if (type == "Time-series" && inherits(a[[x]], "Date"))
      p <- p + geom_smooth(method = "lm", formula = y ~ x, se = FALSE, linetype = "dashed", linewidth = 0.5)
    pl(p)
  } else if (type == "Bar chart") {
    validate(need(is.numeric(d[[y]]), "Y variable must be numeric."))
    a <- d %>% group_by(across(all_of(c(x, ".grp")))) %>% summarise(v = fa(.data[[y]]), .groups = "drop")
    a$.x <- as.character(a[[x]])
    pl(ggplot(a, aes(.x, v, fill = .grp)) + geom_col(position = "dodge") + sc +
         labs(x = x, y = paste(agg, y), fill = lg) + theme_agro() + theme(axis.text.x = element_text(angle = 45, hjust = 1)))
  } else if (type == "Pie chart") {
    validate(need(is.numeric(d[[y]]), "Y variable must be numeric."))
    a <- d %>% group_by(.x = as.character(.data[[x]])) %>% summarise(v = fa(.data[[y]]), .groups = "drop")
    plotly::config(plot_ly(a, labels = ~.x, values = ~v, type = "pie", marker = list(colors = pal(nrow(a)))), displaylogo = FALSE)
  } else if (type == "Histogram") {
    validate(need(is.numeric(d[[x]]), "X variable must be numeric for a histogram."))
    pl(ggplot(d, aes(.data[[x]], fill = .grp)) + geom_histogram(bins = bins, alpha = 0.8, position = "identity") + sc +
         labs(x = x, y = "Count", fill = lg) + theme_agro())
  } else if (type == "Scatter plot") {
    validate(need(is.numeric(d[[x]]) && is.numeric(d[[y]]), "Both X and Y must be numeric."))
    p <- ggplot(d, aes(.data[[x]], .data[[y]], color = .grp)) + geom_point(alpha = 0.7) + sc + labs(x = x, y = y, color = lg) + theme_agro()
    if (isTRUE(trend)) p <- p + geom_smooth(method = "lm", formula = y ~ x, se = FALSE, linewidth = 0.6)
    pl(p)
  } else if (type == "Box plot") {
    validate(need(is.numeric(d[[y]]), "Y variable must be numeric."))
    pl(ggplot(d, aes(as.factor(.data[[x]]), .data[[y]], fill = .grp)) + geom_boxplot(alpha = 0.8) + sc +
         labs(x = x, y = y, fill = lg) + theme_agro() + theme(axis.text.x = element_text(angle = 45, hjust = 1)))
  } else if (type == "Correlation heatmap") {
    m <- d[, intersect(VIZ_NUM, names(d))]; m <- m[, sapply(m, function(v) stats::sd(v, na.rm = TRUE) > 0), drop = FALSE]
    validate(need(ncol(m) >= 2 && nrow(m) >= 3, "Not enough variation for a correlation matrix."))
    cm <- round(stats::cor(m, use = "pairwise.complete.obs"), 2)
    plotly::config(plot_ly(z = cm, x = colnames(cm), y = rownames(cm), type = "heatmap", zmin = -1, zmax = 1,
                           colors = grDevices::colorRamp(c("#8D6E63", "#FFFFFF", "#2E7D32"))), displaylogo = FALSE)
  } else {   # Multivariable comparison: x, y, colour and bubble size
    validate(need(is.numeric(d[[x]]) && is.numeric(d[[y]]) && is.numeric(d[[size_var]]), "X, Y and size variables must be numeric."))
    pl(ggplot(d, aes(.data[[x]], .data[[y]], color = .grp, size = .data[[size_var]])) + geom_point(alpha = 0.55) + sc +
         scale_size(range = c(1, 7)) + labs(x = x, y = y, color = lg, size = size_var) + theme_agro())
  }
}

farm_map <- function(d) {
  f <- d %>% filter(!is.na(latitude), !is.na(longitude)) %>%
    group_by(farm_id, farm_name, location, latitude, longitude) %>%
    summarise(crops = paste(sort(unique(crop_type)), collapse = ", "), health = mean(crop_health),
              moisture = mean(soil_moisture), .groups = "drop")
  ar <- latest_per_plot(d) %>% group_by(farm_id) %>% summarise(area = sum(cultivated_area), .groups = "drop")
  f <- left_join(f, ar, by = "farm_id")
  f$cls <- as.character(cut(f$health, c(-Inf, 40, 55, 70, 85, Inf), labels = c("Critical", "Poor", "Moderate", "Good", "Excellent"), right = FALSE))
  f$colr <- unname(HEALTH_COLORS[f$cls])
  m <- leaflet(f) %>% addTiles()
  if (nrow(f)) m <- m %>% addCircleMarkers(~longitude, ~latitude, radius = ~6 + sqrt(area) * 2, color = ~colr, fillColor = ~colr, fillOpacity = 0.7,
        popup = ~sprintf("<b>%s</b><br>%s<br>Crops: %s<br>Area: %.1f ha<br>Mean health: %.1f (%s)<br>Mean soil moisture: %.1f%%", farm_name, location, crops, area, health, cls, moisture)) %>%
    addLegend("bottomright", colors = unname(HEALTH_COLORS), labels = names(HEALTH_COLORS), title = "Mean crop health")
  m
}

# --------------------------------------------------------------------- UI ----
ui <- dashboardPage(
  skin = "green",
  dashboardHeader(title = tags$span(icon("leaf"), "AGROVISION AI"), titleWidth = 270),
  dashboardSidebar(
    width = 270,
    sidebarMenu(id = "tabs",
      menuItem("Executive Dashboard", tabName = "dash", icon = icon("chart-line")),
      menuItem("Farm Monitoring (IoT)", tabName = "mon", icon = icon("satellite-dish")),
      menuItem("Crop Health", tabName = "health", icon = icon("heartbeat")),
      menuItem("Soil Analytics", tabName = "soil", icon = icon("flask")),
      menuItem("Weather Intelligence", tabName = "weather", icon = icon("cloud-sun")),
      menuItem("ML Yield Prediction", tabName = "ml", icon = icon("brain")),
      menuItem("Smart Irrigation", tabName = "irr", icon = icon("tint")),
      menuItem("Crop Recommendation", tabName = "rec", icon = icon("seedling")),
      menuItem("Data Visualization", tabName = "viz", icon = icon("chart-bar")),
      menuItem("Farm Management", tabName = "farm", icon = icon("tractor")),
      menuItem("Alerts", tabName = "alerts", icon = icon("bell")),
      menuItem("Reports", tabName = "reports", icon = icon("file-pdf")),
      menuItem("About & Architecture", tabName = "about", icon = icon("info-circle"))),
    div(class = "sidebar-filters",
        h5("GLOBAL FILTERS"),
        pickerInput("f_farm", "Farm", choices = sort(unique(INIT$farm_name)), selected = sort(unique(INIT$farm_name)),
                    multiple = TRUE, options = pickerOptions(actionsBox = TRUE, selectedTextFormat = "count > 2")),
        pickerInput("f_crop", "Crop", choices = sort(unique(INIT$crop_type)), selected = sort(unique(INIT$crop_type)),
                    multiple = TRUE, options = pickerOptions(actionsBox = TRUE, selectedTextFormat = "count > 2")),
        dateRangeInput("f_date", "Date range", start = min(INIT$date), end = max(INIT$date)))
  ),
  dashboardBody(
    tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")),
    uiOutput("data_banner"),
    tabItems(
      # ---------------- Module 1: Executive dashboard ----------------
      tabItem("dash",
        fluidRow(column(3, uiOutput("kpi_farms")), column(3, uiOutput("kpi_area")),
                 column(3, uiOutput("kpi_crops")), column(3, uiOutput("kpi_moist"))),
        fluidRow(column(3, uiOutput("kpi_temp")), column(3, uiOutput("kpi_ph")),
                 column(3, uiOutput("kpi_yield")), column(3, uiOutput("kpi_health"))),
        fluidRow(plotbox("Crop distribution (cultivated area)", "dash_crop"),
                 plotbox("Farm-wise production comparison (latest estimate, tonnes)", "dash_prod")),
        fluidRow(plotbox("Soil moisture trend", "dash_moist"), plotbox("Temperature and humidity", "dash_th")),
        fluidRow(plotbox("Crop health distribution", "dash_health"),
                 plotbox("Yield performance index (% of reference yield)", "dash_perf")),
        fluidRow(dtbox("Recent sensor alerts", "dash_alerts", 6), dtbox("Farm activity summary", "dash_activity", 6))),
      # ---------------- Module 2: Monitoring ----------------
      tabItem("mon",
        fluidRow(box(width = 12, status = "success", solidHeader = TRUE, title = "Sensor panel (SIMULATED readings - no hardware connected)",
                     materialSwitch("live_on", "Live-style simulation (refresh every 5 s)", value = TRUE, status = "success"),
                     uiOutput("sensor_cards"))),
        fluidRow(box(title = "Sensor time series", width = 6, status = "success", solidHeader = TRUE,
                     selectInput("mon_var", "Sensor", choices = setNames(SENSORS$var, SENSORS$label)),
                     plotlyOutput("mon_ts", height = "300px")),
                 box(title = "Sensor comparison (standardised z-scores)", width = 6, status = "success", solidHeader = TRUE,
                     selectizeInput("mon_cmp_vars", "Sensors to compare", choices = setNames(SENSORS$var, SENSORS$label),
                                    selected = c("soil_moisture", "temperature", "humidity"), multiple = TRUE),
                     plotlyOutput("mon_cmp", height = "300px"))),
        fluidRow(box(title = "Connect real sensors (IoT adapter)", width = 4, status = "success", solidHeader = TRUE,
                     p("Readings in JSON (array of records using the dataset column names) are validated and appended. ",
                       "Any gateway that can POST/export JSON can be plugged in via ", code("read_iot_json()"), " in ", code("R/helpers.R"), "."),
                     fileInput("iot_file", "Upload JSON sensor payload", accept = ".json"),
                     textInput("iot_url", "...or fetch from a JSON endpoint URL"), actionButton("iot_fetch", "Fetch and append", icon = icon("download"))),
                 dtbox("Historical sensor data", "mon_hist", 8))),
      # ---------------- Module 3: Crop health ----------------
      tabItem("health",
        fluidRow(column(12, disclaimer("Leaf index, water-stress and disease-risk values are transparent rule-based proxies derived from the data. ",
                                       "No satellite imagery or computer-vision disease detection is used."))),
        fluidRow(plotbox("Crop health class distribution", "ch_dist"), plotbox("Growth progress by plot (% of crop cycle)", "ch_growth")),
        fluidRow(plotbox("Water stress levels by crop", "ch_stress"), plotbox("Crop-wise performance (mean health)", "ch_perf")),
        fluidRow(plotbox("Farm-wise crop comparison (mean health)", "ch_compare"), plotbox("Crop health trend", "ch_trend")),
        fluidRow(dtbox("Latest crop status per plot", "ch_tbl"))),
      # ---------------- Module 4: Soil ----------------
      tabItem("soil",
        fluidRow(box(title = "Thresholds", width = 12, status = "success", solidHeader = TRUE,
          fluidRow(column(2, selectInput("soil_crop", "Threshold basis", c("Custom", CROPS))),
                   column(2, numericInput("th_ph_lo", "pH min", 6.0, step = 0.1)), column(2, numericInput("th_ph_hi", "pH max", 7.5, step = 0.1)),
                   column(1, numericInput("th_n", "N min", 50)), column(1, numericInput("th_p", "P min", 20)),
                   column(1, numericInput("th_k", "K min", 100)), column(2, numericInput("th_oc", "Organic C min (%)", 0.5, step = 0.1))),
          disclaimer("Recommendations depend on crop type, soil context and lab-tested values. Thresholds are configurable ",
                     "(or taken from a generic crop reference profile) and are not validated local prescriptions."))),
        fluidRow(plotbox("Nutrient levels by farm (dashed = threshold)", "soil_nutr", 8), plotbox("Soil quality score by farm", "soil_q", 4)),
        fluidRow(box(title = "Soil parameter comparison", width = 6, status = "success", solidHeader = TRUE,
                     selectInput("soil_cmp_var", "Parameter", c("soil_ph", "soil_moisture", "soil_temperature", "nitrogen", "phosphorus", "potassium", "organic_carbon")),
                     plotlyOutput("soil_cmp", height = "280px")),
                 plotbox("Nutrient deficiency indicators (% of readings below threshold)", "soil_def")),
        fluidRow(plotbox("Soil suitability for the planted crop (reference profile)", "soil_suit"),
                 box(title = "Correlation heatmap", width = 6, status = "success", solidHeader = TRUE, plotOutput("soil_corr", height = "340px"))),
        fluidRow(box(title = "Agricultural recommendations", width = 5, status = "success", solidHeader = TRUE,
                     uiOutput("soil_rec_farm_ui"), uiOutput("soil_recs")),
                 dtbox("Farm-wise soil comparison", "soil_tbl", 7))),
      # ---------------- Module 5: Weather ----------------
      tabItem("weather",
        fluidRow(plotbox("Temperature trend", "w_temp"), plotbox("Humidity trend", "w_hum")),
        fluidRow(plotbox("Rainfall distribution", "w_rain"), plotbox("Rainfall comparison by farm", "w_rainfarm")),
        fluidRow(plotbox("Wind speed", "w_wind"), plotbox("Seasonal (monthly) patterns", "w_season")),
        fluidRow(plotbox("Weather condition analysis", "w_cond"),
                 box(title = "Weather impact on crops", width = 6, status = "success", solidHeader = TRUE,
                     fluidRow(column(6, selectInput("w_var", "Weather variable", c("temperature", "humidity", "rainfall", "wind_speed", "light_intensity"))),
                              column(6, selectInput("w_y", "Crop response", c("crop_health", "yield_index")))),
                     plotlyOutput("w_impact", height = "240px"))),
        fluidRow(dtbox("Correlation of weather variables with crop response", "w_cor"))),
      # ---------------- Module 6: ML ----------------
      tabItem("ml",
        fluidRow(box(title = "Random Forest yield model", width = 4, status = "success", solidHeader = TRUE,
                     sliderInput("ml_split", "Training share", 0.6, 0.9, 0.8, 0.05),
                     numericInput("ml_ntree", "Number of trees", 300, 50, 1000, 50),
                     numericInput("ml_seed", "Random seed", 42),
                     actionButton("ml_train", "Train / retrain model", icon = icon("play"), class = "btn-success"),
                     hr(), uiOutput("ml_summary")),
                 column(8, uiOutput("ml_kpis"), plotbox("Actual vs predicted yield (test set)", "ml_avp", 12, "300px"))),
        fluidRow(plotbox("Feature importance (%IncMSE)", "ml_imp"),
                 box(title = "Single prediction", width = 6, status = "success", solidHeader = TRUE,
                     fluidRow(column(4, selectInput("ml_crop", "Crop", CROPS)), column(4, numericInput("ml_area", "Area (ha)", 5, 0.1)),
                              column(4, numericInput("ml_ph", "Soil pH", 6.5, step = 0.1))),
                     fluidRow(column(4, numericInput("ml_moist", "Moisture (%)", 35)), column(4, numericInput("ml_temp", "Temp (C)", 27)),
                              column(4, numericInput("ml_hum", "Humidity (%)", 70))),
                     fluidRow(column(3, numericInput("ml_rain", "Rain (mm)", 3)), column(3, numericInput("ml_n", "N", 90)),
                              column(3, numericInput("ml_p", "P", 30)), column(3, numericInput("ml_k", "K", 120))),
                     uiOutput("ml_pred_out"))),
        fluidRow(dtbox("Predicted yield per plot (latest reading, current filters)", "ml_pred_tbl", 6),
                 box(title = "Time-series forecast (ARIMA)", width = 6, status = "success", solidHeader = TRUE,
                     fluidRow(column(6, selectInput("fc_var", "Variable", c("temperature", "humidity", "rainfall", "soil_moisture", "crop_health", "yield_index"))),
                              column(6, sliderInput("fc_h", "Horizon (steps)", 3, 30, 10))),
                     plotlyOutput("fc_plot", height = "260px")))),
      # ---------------- Module 7: Irrigation ----------------
      tabItem("irr",
        fluidRow(box(title = "Irrigation what-if calculator", width = 5, status = "success", solidHeader = TRUE,
                     fluidRow(column(6, sliderInput("ir_moist", "Soil moisture (%)", 5, 70, 28)), column(6, sliderInput("ir_temp", "Temperature (C)", 5, 45, 32))),
                     fluidRow(column(6, sliderInput("ir_hum", "Humidity (%)", 20, 100, 55)), column(6, sliderInput("ir_rain", "Rainfall (mm)", 0, 80, 0))),
                     fluidRow(column(4, selectInput("ir_crop", "Crop", CROPS)), column(5, selectInput("ir_stage", "Growth stage", STAGES, selected = "Vegetative")),
                              column(3, numericInput("ir_area", "Area (ha)", 1, 0.1))),
                     uiOutput("ir_result")),
                 box(title = "Water demand vs rainfall (current filters)", width = 7, status = "success", solidHeader = TRUE, plotlyOutput("ir_ts", height = "380px"))),
        fluidRow(plotbox("Estimated irrigation volume by plot (m3)", "ir_vol"), plotbox("Water stress indicator by plot", "ir_wsi")),
        fluidRow(dtbox("Farm-wise irrigation comparison and explanations", "ir_tbl"))),
      # ---------------- Module 8: Recommendation ----------------
      tabItem("rec",
        fluidRow(box(title = "Soil and climate inputs", width = 4, status = "success", solidHeader = TRUE,
                     fluidRow(column(4, numericInput("rc_n", "N", 90)), column(4, numericInput("rc_p", "P", 35)), column(4, numericInput("rc_k", "K", 120))),
                     fluidRow(column(6, numericInput("rc_t", "Temp (C)", 27)), column(6, numericInput("rc_h", "Humidity (%)", 70))),
                     fluidRow(column(6, numericInput("rc_ph", "Soil pH", 6.5, step = 0.1)), column(6, numericInput("rc_r", "Rainfall (mm/day)", 4, step = 0.5))),
                     actionButton("rc_load", "Load averages from filtered data", icon = icon("database")),
                     br(), br(), disclaimer("Explainable rule-based scoring against generic reference ranges (50% climate, 50% soil). ",
                                             "Not validated for any specific region; confirm with local agronomy advice.")),
                 plotbox("Crop suitability score", "rc_bar", 8, "340px")),
        fluidRow(dtbox("Recommended crops", "rc_tbl", 7), box(title = "Why this recommendation?", width = 5, status = "success", solidHeader = TRUE, uiOutput("rc_explain"))),
        fluidRow(box(title = "Compare crops", width = 12, status = "success", solidHeader = TRUE,
                     selectizeInput("rc_cmp", "Crops", CROPS, selected = c("Rice", "Maize", "Cotton"), multiple = TRUE, options = list(maxItems = 4)),
                     plotlyOutput("rc_radar", height = "420px")))),
      # ---------------- Module 9: Visualization ----------------
      tabItem("viz",
        fluidRow(box(title = "Chart builder", width = 3, status = "success", solidHeader = TRUE,
                     selectInput("viz_type", "Chart type", c("Line chart", "Bar chart", "Pie chart", "Histogram", "Scatter plot", "Box plot",
                                                              "Correlation heatmap", "Multivariable comparison", "Time-series")),
                     selectInput("viz_x", "X variable", c("date", VIZ_CAT, VIZ_NUM), selected = "date"),
                     selectInput("viz_y", "Y variable", VIZ_NUM, selected = "soil_moisture"),
                     selectInput("viz_color", "Group / compare by", c("none", "farm_name", "crop_type", "growth_stage", "health_class"), selected = "farm_name"),
                     selectInput("viz_agg", "Aggregation", c("Mean", "Median", "Sum", "Max", "Min")),
                     sliderInput("viz_bins", "Histogram bins", 5, 60, 20),
                     selectInput("viz_size", "Bubble size (multivariable)", VIZ_NUM, selected = "crop_health"),
                     checkboxInput("viz_trend", "Add trend line (scatter)", TRUE),
                     pickerInput("viz_stage", "Filter: growth stage", STAGES, selected = STAGES, multiple = TRUE, options = pickerOptions(actionsBox = TRUE)),
                     pickerInput("viz_health", "Filter: health class", HEALTH_LEVELS, selected = HEALTH_LEVELS, multiple = TRUE, options = pickerOptions(actionsBox = TRUE)),
                     downloadButton("dl_viz", "Download visualised data (CSV)", class = "btn-success")),
                 tabBox(width = 9,
                        tabPanel("Chart", plotlyOutput("viz_plot", height = "520px")),
                        tabPanel("Farm map", leafletOutput("viz_map", height = "520px")),
                        tabPanel("Statistical summary", selectInput("viz_group", "Group summary by", c("None", VIZ_CAT)), DT::DTOutput("viz_summary")),
                        tabPanel("Interactive table", DT::DTOutput("viz_data_tbl"))))),
      # ---------------- Module 10: Farm management ----------------
      tabItem("farm",
        fluidRow(
          box(title = "Add farm / crop plot", width = 4, status = "success", solidHeader = TRUE,
              textInput("fm_name", "Farm name"), textInput("fm_loc", "Location"),
              fluidRow(column(6, numericInput("fm_lat", "Latitude", 11.0, step = 0.01)), column(6, numericInput("fm_lon", "Longitude", 77.0, step = 0.01))),
              fluidRow(column(6, selectInput("fm_crop", "Crop", CROPS)), column(6, numericInput("fm_area", "Area (ha)", 5, 0.1))),
              fluidRow(column(6, dateInput("fm_plant", "Planting date", Sys.Date() - 30)), column(6, selectInput("fm_stage", "Growth stage", STAGES, selected = "Vegetative"))),
              strong("Initial field readings"),
              fluidRow(column(3, numericInput("fm_ph", "pH", 6.5, step = 0.1)), column(3, numericInput("fm_moist", "Moist %", 35)),
                       column(3, numericInput("fm_temp", "Temp", 27)), column(3, numericInput("fm_hum", "Hum %", 65))),
              fluidRow(column(3, numericInput("fm_n", "N", 80)), column(3, numericInput("fm_p", "P", 30)),
                       column(3, numericInput("fm_k", "K", 120)), column(3, numericInput("fm_rain", "Rain", 2))),
              actionButton("fm_add", "Add record", icon = icon("plus"), class = "btn-success")),
          box(title = "Edit / delete", width = 4, status = "success", solidHeader = TRUE,
              selectInput("fm_sel", "Select farm - crop plot", choices = NULL),
              textInput("fm_e_name", "Farm name"), textInput("fm_e_loc", "Location"),
              fluidRow(column(6, numericInput("fm_e_lat", "Latitude", 0, step = 0.01)), column(6, numericInput("fm_e_lon", "Longitude", 0, step = 0.01))),
              fluidRow(column(6, numericInput("fm_e_area", "Area (ha)", 1, 0.1)), column(6, dateInput("fm_e_plant", "Planting date"))),
              selectInput("fm_e_stage", "Current growth stage", STAGES),
              actionButton("fm_save", "Save changes", icon = icon("save"), class = "btn-success"),
              actionButton("fm_del", "Delete farm", icon = icon("trash"), class = "btn-danger")),
          box(title = "Upload / export data", width = 4, status = "success", solidHeader = TRUE,
              fileInput("fm_file", "Upload CSV dataset", accept = ".csv"),
              radioButtons("fm_mode", NULL, c("Append to current data" = "append", "Replace current data" = "replace"), inline = TRUE),
              uiOutput("fm_up_msg"), actionButton("fm_apply", "Apply uploaded data", icon = icon("upload"), class = "btn-success"),
              hr(), downloadButton("dl_template", "CSV template"), downloadButton("dl_export_csv", "Export CSV"),
              downloadButton("dl_export_xlsx", "Export Excel"), br(), br(),
              actionButton("fm_reset", "Reset to sample dataset", icon = icon("undo")))),
        fluidRow(dtbox("Farms and plots", "fm_tbl", 7), dtbox("Upload preview (validated)", "fm_up_prev", 5))),
      # ---------------- Module 11: Alerts ----------------
      tabItem("alerts",
        fluidRow(uiOutput("al_kpis")),
        fluidRow(box(title = "Alert thresholds", width = 12, status = "success", solidHeader = TRUE, collapsible = TRUE,
          fluidRow(column(1, numericInput("al_mc", "Moist crit", 20)), column(1, numericInput("al_mw", "Moist warn", 30)),
                   column(1, numericInput("al_tw", "Temp warn", 35)), column(1, numericInput("al_tc", "Temp crit", 38)),
                   column(1, numericInput("al_pl", "pH low", 6.0, step = 0.1)), column(1, numericInput("al_ph", "pH high", 7.5, step = 0.1)),
                   column(1, numericInput("al_n", "N min", 50)), column(1, numericInput("al_p", "P min", 20)),
                   column(1, numericInput("al_k", "K min", 100)), column(1, numericInput("al_hw", "Health warn", 55)),
                   column(1, numericInput("al_hc", "Health crit", 40))))),
        fluidRow(plotbox("Alerts by type and severity", "al_type"), plotbox("Alert timeline", "al_time")),
        fluidRow(box(title = "Alert history", width = 12, status = "success", solidHeader = TRUE,
                     checkboxGroupInput("al_sev", "Severity", c("Critical", "Warning", "Informational"), selected = c("Critical", "Warning", "Informational"), inline = TRUE),
                     DT::DTOutput("al_tbl"), br(), downloadButton("dl_alerts", "Download alerts (CSV)")))),
      # ---------------- Module 12: Reports ----------------
      tabItem("reports",
        fluidRow(box(title = "Report builder", width = 5, status = "success", solidHeader = TRUE,
                     textInput("rp_title", "Report title", "AGROVISION AI - Agricultural Monitoring Report"),
                     textInput("rp_author", "Prepared by", "Farm Analyst"),
                     checkboxGroupInput("rp_sections", "Sections (PDF)",
                                        c("Farm information" = "farm", "Crop monitoring summary" = "crop", "Soil analysis" = "soil",
                                          "Environmental conditions" = "env", "Crop health" = "health", "Yield prediction" = "yield",
                                          "Irrigation recommendations" = "irrigation", "Alerts and recommendations" = "alerts"),
                                        selected = c("farm", "crop", "soil", "env", "health", "yield", "irrigation", "alerts")),
                     downloadButton("dl_pdf", "PDF report", class = "btn-success"), downloadButton("dl_xlsx", "Excel workbook"),
                     downloadButton("dl_csv", "CSV data"), downloadButton("dl_json", "JSON data")),
                 box(title = "Report preview (current selection)", width = 7, status = "success", solidHeader = TRUE, uiOutput("rp_preview")))),
      # ---------------- About ----------------
      tabItem("about",
        box(title = "About AGROVISION AI", width = 12, status = "success", solidHeader = TRUE,
            h4("Architecture"),
            p("Data layer: one validated table (sample CSV, uploads, manual entries, IoT JSON) -> enrich() derives indicators -> global filters -> module reactives."),
            p("Replace the simulated feed by sending real readings through ", code("read_iot_json()"), " or by loading a CSV with the same columns; ",
              "weather APIs can be added by returning a data frame with temperature, humidity, rainfall and wind_speed columns."),
            h4("Transparency"),
            tags$ul(tags$li("The bundled dataset is synthetic and labelled 'Simulated'. The ML model trained on it is a demonstration model."),
                    tags$li("Health, stress and disease indicators are rule-based proxies. No image-based detection is performed."),
                    tags$li("Irrigation and crop recommendations use simplified, transparent heuristics and are not validated prescriptions."),
                    tags$li("Model metrics are always computed on the held-out test set.")))
      )
    )
  )
)

# ------------------------------------------------------------------ SERVER ----
server <- function(input, output, session) {
  rv <- reactiveValues(data = INIT)
  model_rv <- reactiveVal(tryCatch(train_yield_model(INIT), error = function(e) NULL))
  base <- reactive(enrich(rv$data))

  # ----- global filters -----
  fd <- reactive({
    d <- base()
    d <- d[d$farm_name %in% input$f_farm & d$crop_type %in% input$f_crop, ]
    dr <- input$f_date
    if (length(dr) == 2 && !anyNA(dr)) d <- d[d$date >= dr[1] & d$date <= dr[2], ]
    d
  })
  observeEvent(rv$data, {
    d <- rv$data; req(nrow(d) > 0)
    fm <- sort(unique(d$farm_name)); cr <- sort(unique(d$crop_type))
    updatePickerInput(session, "f_farm", choices = fm, selected = fm)
    updatePickerInput(session, "f_crop", choices = cr, selected = cr)
    updateDateRangeInput(session, "f_date", start = min(d$date), end = max(d$date), min = min(d$date), max = max(d$date))
    u <- unique(d[, c("farm_id", "farm_name", "crop_type")])
    updateSelectInput(session, "fm_sel", choices = setNames(paste(u$farm_id, u$crop_type, sep = "||"), paste0(u$farm_name, " - ", u$crop_type)))
  }, ignoreInit = FALSE)

  output$data_banner <- renderUI({
    src <- unique(rv$data$data_source)
    if (any(src == "Simulated")) div(class = "sim-banner", icon("exclamation-triangle"),
      " SIMULATED DATA - the sensor, weather and yield values shown are synthetic demonstration data, not real hardware or field measurements.")
  })

  alert_th <- reactive(list(moist_crit = nz(input$al_mc, 20), moist_warn = nz(input$al_mw, 30), temp_warn = nz(input$al_tw, 35),
                            temp_crit = nz(input$al_tc, 38), ph_lo = nz(input$al_pl, 6), ph_hi = nz(input$al_ph, 7.5),
                            n_min = nz(input$al_n, 50), p_min = nz(input$al_p, 20), k_min = nz(input$al_k, 100),
                            health_warn = nz(input$al_hw, 55), health_crit = nz(input$al_hc, 40)))
  alerts_df <- reactive(build_alerts(fd(), alert_th()))
  alert_display <- function(a) data.frame(Time = format(a$timestamp, "%Y-%m-%d %H:%M"), Severity = a$severity, Farm = a$farm, Crop = a$crop,
                                          Type = a$type, Description = a$description, `Recommended action` = a$action, check.names = FALSE)
  sev_style <- function(x) DT::formatStyle(x, "Severity", backgroundColor = DT::styleEqual(c("Critical", "Warning", "Informational"), c("#FFCDD2", "#FFF9C4", "#BBDEFB")))

  pred_df <- reactive({
    m <- model_rv(); lp <- latest_per_plot(fd())
    if (is.null(m) || !nrow(lp)) return(NULL)
    pr <- predict_yield(m, lp)
    data.frame(Farm = lp$farm_name, Crop = lp$crop_type, Area_ha = lp$cultivated_area, Observed_yield_t_ha = lp$yield,
               Predicted_yield_t_ha = round(pr, 2), Predicted_production_t = round(pr * lp$cultivated_area, 1))
  })
  irr_df <- reactive({
    lp <- latest_per_plot(fd()); if (!nrow(lp)) return(NULL)
    a <- irrigation_advice(lp$soil_moisture, lp$temperature, lp$humidity, lp$rainfall, lp$crop_type, lp$growth_stage, lp$cultivated_area)
    cbind(data.frame(Farm = lp$farm_name, Crop = lp$crop_type, Stage = lp$growth_stage, Area_ha = lp$cultivated_area, Moisture = lp$soil_moisture), a)
  })

  # =================== Module 1: Executive dashboard ===================
  output$kpi_farms <- renderUI({ d <- fd(); kpi(n_distinct(d$farm_id), "Farms monitored", "tractor") })
  output$kpi_area <- renderUI({ d <- fd(); kpi(sprintf("%.1f ha", sum(latest_per_plot(d)$cultivated_area)), "Cultivated area", "map") })
  output$kpi_crops <- renderUI({ d <- fd(); kpi(n_distinct(d$crop_type), "Crops monitored", "seedling") })
  output$kpi_moist <- renderUI({ d <- fd(); kpi(sprintf("%.1f %%", mean(d$soil_moisture)), "Avg soil moisture", "tint") })
  output$kpi_temp <- renderUI({ d <- fd(); kpi(sprintf("%.1f \u00b0C", mean(d$temperature)), "Avg temperature", "thermometer-half") })
  output$kpi_ph <- renderUI({ d <- fd(); kpi(sprintf("%.2f", mean(d$soil_ph)), "Avg soil pH", "flask") })
  output$kpi_yield <- renderUI({
    p <- pred_df()
    kpi(if (is.null(p)) "n/a" else sprintf("%.0f t", sum(p$Predicted_production_t)), "Predicted production", "chart-line",
        if (!is.null(model_rv()) && model_rv()$simulated) "demo model" else NULL)
  })
  output$kpi_health <- renderUI({
    d <- fd(); h <- mean(d$crop_health)
    kpi(sprintf("%.1f / 100", h), "Crop health score", "heartbeat", as.character(cut(h, c(-Inf, 40, 55, 70, 85, Inf), HEALTH_LEVELS[c(5, 4, 3, 2, 1)], right = FALSE)))
  })
  output$dash_crop <- renderPlotly({
    d <- fd(); need_data(d)
    a <- latest_per_plot(d) %>% group_by(crop_type) %>% summarise(area = sum(cultivated_area), .groups = "drop")
    plotly::config(plot_ly(a, labels = ~crop_type, values = ~area, type = "pie", hole = 0.4, marker = list(colors = pal(nrow(a)))), displaylogo = FALSE)
  })
  output$dash_prod <- renderPlotly({
    d <- fd(); need_data(d)
    a <- latest_per_plot(d) %>% mutate(prod = coalesce(production_t, 0))
    pl(ggplot(a, aes(farm_name, prod, fill = crop_type, text = paste(crop_type, round(prod, 1), "t"))) + geom_col() +
         scale_fill_manual(values = pal(n_distinct(a$crop_type))) + labs(x = NULL, y = "Tonnes", fill = "Crop") + theme_agro() +
         theme(axis.text.x = element_text(angle = 30, hjust = 1)), "text")
  })
  output$dash_moist <- renderPlotly({
    d <- fd(); need_data(d)
    a <- d %>% group_by(date, farm_name) %>% summarise(v = mean(soil_moisture), .groups = "drop")
    pl(ggplot(a, aes(date, v, color = farm_name)) + geom_line() + scale_color_manual(values = pal(n_distinct(a$farm_name))) +
         labs(x = NULL, y = "Soil moisture (%)", color = "Farm") + theme_agro())
  })
  output$dash_th <- renderPlotly({
    d <- fd(); need_data(d)
    a <- d %>% group_by(date) %>% summarise(t = mean(temperature), h = mean(humidity), .groups = "drop")
    plot_ly(a, x = ~date) %>% add_lines(y = ~t, name = "Temperature (\u00b0C)", line = list(color = "#EF6C00")) %>%
      add_lines(y = ~h, name = "Humidity (%)", yaxis = "y2", line = list(color = "#0277BD")) %>%
      layout(yaxis = list(title = "\u00b0C"), yaxis2 = list(overlaying = "y", side = "right", title = "%"), legend = list(orientation = "h")) %>%
      plotly::config(displaylogo = FALSE)
  })
  output$dash_health <- renderPlotly({
    d <- fd(); need_data(d)
    a <- d %>% count(health_class, .drop = FALSE)
    pl(ggplot(a, aes(health_class, n, fill = health_class)) + geom_col() + scale_fill_manual(values = HEALTH_COLORS) +
         labs(x = NULL, y = "Records") + theme_agro() + theme(legend.position = "none"))
  })
  output$dash_perf <- renderPlotly({
    d <- fd(); need_data(d)
    a <- d %>% group_by(crop_type) %>% summarise(v = mean(yield_index, na.rm = TRUE), .groups = "drop")
    pl(ggplot(a, aes(crop_type, v)) + geom_col(fill = "#2E7D32") + geom_hline(yintercept = 100, linetype = "dashed", colour = "#8D6E63") +
         labs(x = NULL, y = "% of reference yield") + theme_agro())
  })
  output$dash_alerts <- DT::renderDT({
    a <- alerts_df(); validate(need(nrow(a) > 0, "No alerts for the selected filters."))
    sev_style(dt(alert_display(head(a, 50))[, c("Time", "Severity", "Farm", "Type", "Recommended action")], 6))
  })
  output$dash_activity <- DT::renderDT({
    d <- fd(); need_data(d)
    dt(d %>% group_by(Farm = farm_name) %>% summarise(Crops = paste(unique(crop_type), collapse = ", "), Records = n(),
         `Last reading` = max(date), `Mean health` = round(mean(crop_health), 1), `Latest stage` = growth_stage[which.max(date)], .groups = "drop"), 6)
  })

  # =================== Module 2: Monitoring ===================
  tick <- reactiveTimer(5000)
  live_vals <- reactive({
    live <- isTRUE(input$live_on); if (live) tick()
    lp <- latest_per_plot(fd()); validate(need(nrow(lp) > 0, "No data for the selected filters."))
    v <- sapply(SENSORS$var, function(s) mean(lp[[s]], na.rm = TRUE))
    if (live) v <- v * (1 + rnorm(length(v), 0, 0.015))
    v
  })
  output$sensor_cards <- renderUI({
    v <- live_vals()
    fluidRow(lapply(seq_len(nrow(SENSORS)), function(i) {
      s <- SENSORS[i, ]; st <- sensor_status(v[i], s)
      column(3, div(class = paste0("sensor-card status-", tolower(st)), div(class = "sensor-head", icon(s$icon), s$label),
                    div(class = "sensor-val", format(round(v[i], if (s$var %in% c("light_intensity", "nitrogen", "phosphorus", "potassium")) 0 else 1), big.mark = ","), span(class = "sensor-unit", s$unit)),
                    span(class = "badge-status", st), span(class = "sim-tag", "simulated")))
    }))
  })
  output$mon_ts <- renderPlotly({
    d <- fd(); need_data(d); v <- input$mon_var; s <- SENSORS[SENSORS$var == v, ]
    a <- d %>% group_by(date, farm_name) %>% summarise(val = mean(.data[[v]]), .groups = "drop")
    p <- ggplot(a, aes(date, val, color = farm_name)) + geom_line() + scale_color_manual(values = pal(n_distinct(a$farm_name))) +
      labs(x = NULL, y = paste0(s$label, " (", s$unit, ")"), color = "Farm") + theme_agro()
    if (is.finite(s$warn_lo)) p <- p + geom_hline(yintercept = s$warn_lo, linetype = "dashed", colour = "#F9A825")
    if (is.finite(s$warn_hi)) p <- p + geom_hline(yintercept = s$warn_hi, linetype = "dashed", colour = "#F9A825")
    pl(p)
  })
  output$mon_cmp <- renderPlotly({
    d <- fd(); need_data(d); validate(need(length(input$mon_cmp_vars) > 0, "Select at least one sensor."))
    a <- d %>% select(date, all_of(input$mon_cmp_vars)) %>% pivot_longer(-date) %>% group_by(name) %>%
      mutate(z = (value - mean(value)) / (sd(value) + 1e-9)) %>% group_by(date, name) %>% summarise(z = mean(z), .groups = "drop")
    pl(ggplot(a, aes(date, z, color = name)) + geom_line() + scale_color_manual(values = pal(n_distinct(a$name))) +
         labs(x = NULL, y = "z-score", color = "Sensor") + theme_agro())
  })
  output$mon_hist <- DT::renderDT({
    d <- fd(); need_data(d)
    dt(d %>% arrange(desc(date)) %>% select(date, farm_name, crop_type, all_of(SENSORS$var)) %>% head(500), 8)
  })

  # =================== Data ingestion (shared) ===================
  ingest <- function(raw, mode = "append", src = "Uploaded CSV") {
    v <- validate_upload(raw)
    if (!v$ok) { showNotification(paste(v$messages, collapse = " "), type = "error", duration = 8); return(invisible(FALSE)) }
    nd <- v$data; nd$data_source <- ifelse(nd$data_source == "Uploaded", src, nd$data_source)
    rv$data <- if (mode == "replace") nd else distinct(bind_rows(rv$data, nd))
    showNotification(paste(tail(v$messages, 1), "Data updated."), type = "message")
    invisible(TRUE)
  }
  observeEvent(input$iot_file, {
    res <- tryCatch(read_iot_json(input$iot_file$datapath), error = function(e) { showNotification(paste("JSON error:", conditionMessage(e)), type = "error"); NULL })
    if (!is.null(res)) ingest(res, "append", "IoT JSON import")
  })
  observeEvent(input$iot_fetch, {
    req(nzchar(trimws(input$iot_url)))
    res <- tryCatch(read_iot_json(trimws(input$iot_url)), error = function(e) { showNotification(paste("Endpoint error:", conditionMessage(e)), type = "error"); NULL })
    if (!is.null(res)) ingest(res, "append", "IoT endpoint")
  })

  # =================== Module 3: Crop health ===================
  output$ch_dist <- renderPlotly({
    d <- fd(); need_data(d)
    a <- d %>% count(crop_type, health_class, .drop = FALSE)
    pl(ggplot(a, aes(crop_type, n, fill = health_class)) + geom_col() + scale_fill_manual(values = HEALTH_COLORS) +
         labs(x = NULL, y = "Records", fill = "Class") + theme_agro())
  })
  output$ch_growth <- renderPlotly({
    d <- fd(); need_data(d)
    a <- latest_per_plot(d)
    pl(ggplot(a, aes(reorder(plot_id, growth_progress), 100 * growth_progress, fill = growth_stage, text = paste(growth_stage))) + geom_col() + coord_flip() +
         scale_fill_manual(values = setNames(pal(5), STAGES)) + labs(x = NULL, y = "% of crop cycle", fill = "Stage") + theme_agro(), "text")
  })
  output$ch_stress <- renderPlotly({
    d <- fd(); need_data(d)
    pl(ggplot(d, aes(crop_type, water_stress_index, fill = crop_type)) + geom_boxplot(alpha = 0.8) + scale_fill_manual(values = pal(n_distinct(d$crop_type))) +
         labs(x = NULL, y = "Water stress index (0-1)") + theme_agro() + theme(legend.position = "none"))
  })
  output$ch_perf <- renderPlotly({
    d <- fd(); need_data(d)
    a <- d %>% group_by(crop_type) %>% summarise(h = mean(crop_health), .groups = "drop")
    pl(ggplot(a, aes(reorder(crop_type, h), h)) + geom_col(fill = "#43A047") + coord_flip() + labs(x = NULL, y = "Mean crop health") + theme_agro())
  })
  output$ch_compare <- renderPlotly({
    d <- fd(); need_data(d)
    a <- d %>% group_by(farm_name, crop_type) %>% summarise(h = mean(crop_health), .groups = "drop")
    pl(ggplot(a, aes(farm_name, h, fill = crop_type)) + geom_col(position = "dodge") + scale_fill_manual(values = pal(n_distinct(a$crop_type))) +
         labs(x = NULL, y = "Mean crop health", fill = "Crop") + theme_agro() + theme(axis.text.x = element_text(angle = 30, hjust = 1)))
  })
  output$ch_trend <- renderPlotly({
    d <- fd(); need_data(d)
    a <- d %>% group_by(date, crop_type) %>% summarise(h = mean(crop_health), .groups = "drop")
    pl(ggplot(a, aes(date, h, color = crop_type)) + geom_line() + scale_color_manual(values = pal(n_distinct(a$crop_type))) +
         labs(x = NULL, y = "Crop health", color = "Crop") + theme_agro())
  })
  output$ch_tbl <- DT::renderDT({
    d <- fd(); need_data(d)
    t <- latest_per_plot(d) %>% transmute(Farm = farm_name, Crop = crop_type, Stage = growth_stage, `Growth %` = round(100 * growth_progress),
           `Leaf index (proxy)` = leaf_index, `Water stress` = water_stress_index, `Disease risk` = as.character(disease_risk),
           `Health score` = crop_health, Status = as.character(health_class))
    dt(t) %>% DT::formatStyle("Status", backgroundColor = DT::styleEqual(HEALTH_LEVELS, c("#C8E6C9", "#DCEDC8", "#FFF9C4", "#FFE0B2", "#FFCDD2")))
  })

  # =================== Module 4: Soil ===================
  soil_th <- reactive({
    cr <- input$soil_crop %||% "Custom"
    if (cr != "Custom") { p <- CROP_PROFILE[CROP_PROFILE$crop == cr, ]
      list(ph = c(p$ph_lo, p$ph_hi), n = p$n_lo, p = p$p_lo, k = p$k_lo, oc = c(nz(input$th_oc, 0.5), 1.5), moist = c(p$moist_lo, p$moist_hi)) }
    else list(ph = c(nz(input$th_ph_lo, 6), nz(input$th_ph_hi, 7.5)), n = nz(input$th_n, 50), p = nz(input$th_p, 20), k = nz(input$th_k, 100),
              oc = c(nz(input$th_oc, 0.5), 1.5), moist = c(25, 50))
  })
  soil_d <- reactive({ d <- fd(); if (nrow(d)) d$soil_quality <- soil_quality(d, soil_th()); d })
  output$soil_nutr <- renderPlotly({
    d <- soil_d(); need_data(d); th <- soil_th()
    a <- d %>% group_by(farm_name) %>% summarise(Nitrogen = mean(nitrogen), Phosphorus = mean(phosphorus), Potassium = mean(potassium), .groups = "drop") %>%
      pivot_longer(-farm_name, names_to = "nutrient", values_to = "v")
    thr <- data.frame(nutrient = c("Nitrogen", "Phosphorus", "Potassium"), t = c(th$n, th$p, th$k))
    pl(ggplot(a, aes(farm_name, v)) + geom_col(fill = "#2E7D32") + geom_hline(data = thr, aes(yintercept = t), linetype = "dashed", colour = "#C62828") +
         facet_wrap(~nutrient, scales = "free_y") + labs(x = NULL, y = "mg/kg") + theme_agro() + theme(axis.text.x = element_text(angle = 40, hjust = 1)))
  })
  output$soil_q <- renderPlotly({
    d <- soil_d(); need_data(d)
    a <- d %>% group_by(farm_name) %>% summarise(q = mean(soil_quality), .groups = "drop")
    pl(ggplot(a, aes(reorder(farm_name, q), q, fill = q)) + geom_col() + coord_flip() + scale_fill_gradient(low = "#EF6C00", high = "#1B5E20", limits = c(0, 100)) +
         labs(x = NULL, y = "Soil quality score (0-100)") + theme_agro() + theme(legend.position = "none"))
  })
  output$soil_cmp <- renderPlotly({
    d <- soil_d(); need_data(d)
    pl(ggplot(d, aes(farm_name, .data[[input$soil_cmp_var]], fill = farm_name)) + geom_boxplot(alpha = 0.8) + scale_fill_manual(values = pal(n_distinct(d$farm_name))) +
         labs(x = NULL, y = input$soil_cmp_var) + theme_agro() + theme(legend.position = "none", axis.text.x = element_text(angle = 40, hjust = 1)))
  })
  output$soil_def <- renderPlotly({
    d <- soil_d(); need_data(d); th <- soil_th()
    a <- d %>% group_by(farm_name) %>% summarise(Nitrogen = 100 * mean(nitrogen < th$n), Phosphorus = 100 * mean(phosphorus < th$p),
            Potassium = 100 * mean(potassium < th$k), `Organic C` = 100 * mean(organic_carbon < th$oc[1]),
            pH = 100 * mean(soil_ph < th$ph[1] | soil_ph > th$ph[2]), .groups = "drop") %>% pivot_longer(-farm_name)
    pl(ggplot(a, aes(name, farm_name, fill = value, text = paste0(round(value), "%"))) + geom_tile(colour = "white") +
         geom_text(aes(label = round(value)), size = 3) + scale_fill_gradient(low = "#E8F5E9", high = "#C62828", limits = c(0, 100)) +
         labs(x = NULL, y = NULL, fill = "% below") + theme_agro(), "text")
  })
  output$soil_suit <- renderPlotly({
    d <- fd(); need_data(d); p <- CROP_PROFILE[match(d$crop_type, CROP_PROFILE$crop), ]
    d$s <- 100 * (range_score(d$soil_ph, p$ph_lo, p$ph_hi) + range_score(d$nitrogen, p$n_lo, p$n_hi) + range_score(d$phosphorus, p$p_lo, p$p_hi) +
                    range_score(d$potassium, p$k_lo, p$k_hi) + range_score(d$soil_moisture, p$moist_lo, p$moist_hi)) / 5
    a <- d %>% group_by(plot = paste(farm_name, "-", crop_type)) %>% summarise(s = mean(s), .groups = "drop")
    pl(ggplot(a, aes(reorder(plot, s), s)) + geom_col(fill = "#8D6E63") + coord_flip() + labs(x = NULL, y = "Suitability (0-100)") + theme_agro())
  })
  output$soil_corr <- renderPlot({
    d <- fd(); validate(need(nrow(d) >= 5, "Not enough data."))
    m <- d[, c("soil_ph", "soil_moisture", "soil_temperature", "nitrogen", "phosphorus", "potassium", "organic_carbon", "crop_health", "yield_index")]
    m <- m[, sapply(m, function(v) sd(v, na.rm = TRUE) > 0), drop = FALSE]; validate(need(ncol(m) >= 2, "Not enough variation."))
    corrplot::corrplot(cor(m, use = "pairwise.complete.obs"), method = "color", type = "upper", addCoef.col = "black", number.cex = 0.6, tl.col = "black",
                       tl.cex = 0.8, col = colorRampPalette(c("#8D6E63", "white", "#2E7D32"))(200))
  })
  output$soil_tbl <- DT::renderDT({
    d <- soil_d(); need_data(d)
    dt(d %>% group_by(Farm = farm_name) %>% summarise(pH = mean(soil_ph), Moisture = mean(soil_moisture), `Soil temp` = mean(soil_temperature), N = mean(nitrogen),
         P = mean(phosphorus), K = mean(potassium), `Organic C` = mean(organic_carbon), `Quality score` = mean(soil_quality), .groups = "drop") %>%
         mutate(across(where(is.numeric), ~ round(.x, 2))), 8)
  })
  output$soil_rec_farm_ui <- renderUI(selectInput("soil_rec_farm", "Recommendations for", c("All filtered farms", sort(unique(fd()$farm_name)))))
  output$soil_recs <- renderUI({
    d <- fd(); req(input$soil_rec_farm); need_data(d)
    if (input$soil_rec_farm != "All filtered farms") d <- d[d$farm_name == input$soil_rec_farm, ]
    validate(need(nrow(d) > 0, "No data."))
    m <- as.list(colMeans(d[, c("soil_ph", "nitrogen", "phosphorus", "potassium", "organic_carbon", "soil_moisture")]))
    tagList(tags$ul(lapply(soil_recs(m, soil_th()), tags$li)),
            tags$small("Indicative only: depends on crop type, soil context and laboratory soil tests."))
  })

  # =================== Module 5: Weather ===================
  daily_by_farm <- function(v, ylab) {
    d <- fd(); need_data(d)
    a <- d %>% group_by(date, farm_name) %>% summarise(val = mean(.data[[v]]), .groups = "drop")
    pl(ggplot(a, aes(date, val, color = farm_name)) + geom_line() + scale_color_manual(values = pal(n_distinct(a$farm_name))) +
         labs(x = NULL, y = ylab, color = "Farm") + theme_agro())
  }
  output$w_temp <- renderPlotly(daily_by_farm("temperature", "Temperature (\u00b0C)"))
  output$w_hum <- renderPlotly(daily_by_farm("humidity", "Humidity (%)"))
  output$w_wind <- renderPlotly(daily_by_farm("wind_speed", "Wind speed (km/h)"))
  output$w_rain <- renderPlotly({
    d <- fd(); need_data(d)
    pl(ggplot(d, aes(rainfall)) + geom_histogram(bins = 30, fill = "#0277BD", alpha = 0.85) + labs(x = "Rainfall per reading (mm)", y = "Count") + theme_agro())
  })
  output$w_rainfarm <- renderPlotly({
    d <- fd(); need_data(d)
    pl(ggplot(d, aes(farm_name, rainfall, fill = farm_name)) + geom_boxplot(alpha = 0.8) + scale_fill_manual(values = pal(n_distinct(d$farm_name))) +
         labs(x = NULL, y = "Rainfall (mm)") + theme_agro() + theme(legend.position = "none", axis.text.x = element_text(angle = 40, hjust = 1)))
  })
  output$w_season <- renderPlotly({
    d <- fd(); need_data(d)
    a <- d %>% mutate(month = floor_date(date, "month")) %>% group_by(month) %>%
      summarise(Temperature = mean(temperature), Humidity = mean(humidity), Rainfall = mean(rainfall), .groups = "drop") %>% pivot_longer(-month)
    pl(ggplot(a, aes(month, value, color = name)) + geom_line() + geom_point() + facet_wrap(~name, scales = "free_y") +
         scale_color_manual(values = pal(3)) + labs(x = NULL, y = NULL) + theme_agro() + theme(legend.position = "none"))
  })
  output$w_cond <- renderPlotly({
    d <- fd(); need_data(d); d$cond <- weather_condition(d$temperature, d$rainfall, d$humidity)
    a <- d %>% count(cond, farm_name)
    pl(ggplot(a, aes(cond, n, fill = farm_name)) + geom_col() + scale_fill_manual(values = pal(n_distinct(a$farm_name))) +
         labs(x = NULL, y = "Records", fill = "Farm") + theme_agro())
  })
  output$w_impact <- renderPlotly({
    d <- fd(); need_data(d)
    pl(ggplot(d, aes(.data[[input$w_var]], .data[[input$w_y]], color = crop_type)) + geom_point(alpha = 0.5) +
         geom_smooth(method = "lm", formula = y ~ x, se = FALSE, linewidth = 0.6) + scale_color_manual(values = pal(n_distinct(d$crop_type))) +
         labs(x = input$w_var, y = input$w_y, color = "Crop") + theme_agro())
  })
  output$w_cor <- DT::renderDT({
    d <- fd(); need_data(d); vars <- c("temperature", "humidity", "rainfall", "wind_speed", "light_intensity")
    cr <- function(v, y) { if (sd(d[[v]]) == 0 || sd(d[[y]], na.rm = TRUE) == 0 || nrow(d) < 3) NA else round(cor(d[[v]], d[[y]], use = "complete.obs"), 3) }
    dt(data.frame(Variable = vars, `Correlation with crop health` = sapply(vars, cr, y = "crop_health"),
                  `Correlation with yield index` = sapply(vars, cr, y = "yield_index"), check.names = FALSE, row.names = NULL), 6)
  })

  # =================== Module 6: ML ===================
  observeEvent(input$ml_train, {
    withProgress(message = "Training Random Forest...", value = 0.5, {
      res <- tryCatch(train_yield_model(rv$data, input$ml_split, input$ml_ntree, nz(input$ml_seed, 42)),
                      error = function(e) { showNotification(paste("Training failed:", conditionMessage(e)), type = "error", duration = 8); NULL })
      if (!is.null(res)) { model_rv(res); try(saveRDS(res, file.path("models", "yield_rf.rds")), silent = TRUE); showNotification("Model trained and saved to models/yield_rf.rds") }
    })
  })
  output$ml_kpis <- renderUI({
    m <- model_rv(); validate(need(!is.null(m), "No model available. Train the model first."))
    fluidRow(column(3, kpi(sprintf("%.3f", m$metrics["RMSE"]), "RMSE (t/ha)", "ruler")), column(3, kpi(sprintf("%.3f", m$metrics["MAE"]), "MAE (t/ha)", "ruler-horizontal")),
             column(3, kpi(sprintf("%.3f", m$metrics["R2"]), "R-squared (test)", "bullseye")), column(3, kpi(sprintf("%d / %d", m$n_train, m$n_test), "Train / test rows", "table")))
  })
  output$ml_summary <- renderUI({
    m <- model_rv(); req(m); p <- pred_df()
    tagList(if (m$simulated) disclaimer(strong("Demonstration model: "), "trained on simulated data. Reliable production predictions require real historical agricultural data."),
            tags$ul(tags$li(sprintf("%d trees, seed %d, %.0f%% training split (stratified by yield).", m$ntree, m$seed, 100 * m$split)),
                    tags$li(sprintf("Out-of-bag R-squared: %.3f. All test metrics are computed on the held-out test set.", m$oob_r2)),
                    tags$li("Rows come from repeated readings of the same plots, so a random split can be optimistic for new farms."),
                    if (!is.null(p)) tags$li(sprintf("Predicted production for current filters: %.1f t.", sum(p$Predicted_production_t)))))
  })
  output$ml_avp <- renderPlotly({
    m <- model_rv(); validate(need(!is.null(m), "No model available."))
    t <- m$test
    pl(ggplot(t, aes(actual, predicted, color = crop, text = farm)) + geom_point(alpha = 0.75) + geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
         scale_color_manual(values = setNames(pal(8), CROPS)) + labs(x = "Actual yield (t/ha)", y = "Predicted yield (t/ha)", color = "Crop") + theme_agro(), c("x", "y", "text"))
  })
  output$ml_imp <- renderPlotly({
    m <- model_rv(); validate(need(!is.null(m), "No model available."))
    i <- m$importance; i$v <- i[["%IncMSE"]]
    pl(ggplot(i, aes(reorder(feature, v), v)) + geom_col(fill = "#2E7D32") + coord_flip() + labs(x = NULL, y = "%IncMSE (permutation importance)") + theme_agro())
  })
  output$ml_pred_tbl <- DT::renderDT({ p <- pred_df(); validate(need(!is.null(p), "No predictions available.")); dt(p, 8) })
  output$ml_pred_out <- renderUI({
    m <- model_rv(); validate(need(!is.null(m), "Train the model first."))
    nd <- data.frame(crop_type = input$ml_crop, soil_ph = nz(input$ml_ph, 6.5), soil_moisture = nz(input$ml_moist, 35), temperature = nz(input$ml_temp, 27),
                     humidity = nz(input$ml_hum, 70), rainfall = nz(input$ml_rain, 3), nitrogen = nz(input$ml_n, 90), phosphorus = nz(input$ml_p, 30),
                     potassium = nz(input$ml_k, 120), cultivated_area = nz(input$ml_area, 5))
    x <- prep_features(nd, m$medians)
    ind <- stats::predict(m$model, x, predict.all = TRUE)$individual
    q <- stats::quantile(ind, c(0.1, 0.9)); mu <- mean(ind)
    div(class = "irr-result", style = "border-left:6px solid #2E7D32", h4(sprintf("Predicted yield: %.2f t/ha", mu)),
        p(sprintf("Tree-ensemble spread (10th-90th percentile): %.2f - %.2f t/ha. Expected production: %.1f t.", q[1], q[2], mu * nd$cultivated_area)))
  })
  output$fc_plot <- renderPlotly({
    d <- fd(); need_data(d)
    a <- d %>% group_by(date) %>% summarise(v = mean(.data[[input$fc_var]], na.rm = TRUE), .groups = "drop") %>% arrange(date)
    validate(need(nrow(a) >= 15, "At least 15 dated observations are needed for forecasting."))
    step <- max(1, as.numeric(stats::median(diff(a$date))))
    f <- tryCatch(forecast::forecast(forecast::auto.arima(stats::ts(a$v)), h = input$fc_h, level = 80), error = function(e) NULL)
    validate(need(!is.null(f), "The ARIMA model could not be fitted."))
    fut <- data.frame(date = max(a$date) + step * seq_len(input$fc_h), v = as.numeric(f$mean), lo = as.numeric(f$lower), hi = as.numeric(f$upper))
    pl(ggplot() + geom_line(data = a, aes(date, v), colour = "#1B5E20") + geom_ribbon(data = fut, aes(date, ymin = lo, ymax = hi), fill = "#81C784", alpha = 0.4) +
         geom_line(data = fut, aes(date, v), colour = "#EF6C00", linetype = "dashed") + labs(x = NULL, y = input$fc_var, title = "History (green) and 80% forecast band (orange)") + theme_agro())
  })

  # =================== Module 7: Irrigation ===================
  output$ir_result <- renderUI({
    req(input$ir_crop, input$ir_stage)
    r <- irrigation_advice(input$ir_moist, input$ir_temp, input$ir_hum, input$ir_rain, input$ir_crop, input$ir_stage, nz(input$ir_area, 1))
    div(class = "irr-result", style = paste0("border-left:6px solid ", IRR_COLORS[r$level]), h4(as.character(r$status)),
        p(r$explanation), tags$ul(tags$li(sprintf("Estimated water requirement: %.1f mm (about %s m3 for %.1f ha).", r$need_mm, format(r$volume_m3, big.mark = ","), nz(input$ir_area, 1))),
                                  tags$li(sprintf("Water stress indicator: %.2f (0 = none, 1 = severe).", r$water_stress))),
        tags$small("Simplified heuristic - not a scientifically validated irrigation prescription. Verify with local agronomic guidance."))
  })
  output$ir_ts <- renderPlotly({
    d <- fd(); need_data(d)
    a <- irrigation_advice(d$soil_moisture, d$temperature, d$humidity, d$rainfall, d$crop_type, d$growth_stage, 1)
    d$etc <- a$etc
    g <- d %>% group_by(date) %>% summarise(`Crop water use ETc (mm/day)` = mean(etc), `Rainfall (mm)` = mean(rainfall), .groups = "drop") %>% pivot_longer(-date)
    pl(ggplot(g, aes(date, value, color = name)) + geom_line() + scale_color_manual(values = c("#EF6C00", "#0277BD")) + labs(x = NULL, y = "mm", color = NULL) + theme_agro())
  })
  output$ir_vol <- renderPlotly({
    r <- irr_df(); validate(need(!is.null(r), "No data."))
    pl(ggplot(r, aes(paste(Farm, Crop, sep = " - "), volume_m3, fill = status)) + geom_col() + coord_flip() + scale_fill_manual(values = setNames(IRR_COLORS, IRR_LABELS), drop = FALSE) +
         labs(x = NULL, y = "m3", fill = NULL) + theme_agro())
  })
  output$ir_wsi <- renderPlotly({
    r <- irr_df(); validate(need(!is.null(r), "No data."))
    pl(ggplot(r, aes(paste(Farm, Crop, sep = " - "), water_stress)) + geom_col(fill = "#0277BD") + coord_flip() + labs(x = NULL, y = "Water stress indicator") + theme_agro())
  })
  output$ir_tbl <- DT::renderDT({
    r <- irr_df(); validate(need(!is.null(r), "No data."))
    dt(r %>% transmute(Farm, Crop, Stage, `Moisture %` = Moisture, `ETc mm/day` = etc, `Need mm` = need_mm, `Volume m3` = volume_m3, Status = as.character(status), Explanation = explanation), 8) %>%
      DT::formatStyle("Status", backgroundColor = DT::styleEqual(IRR_LABELS, c("#C8E6C9", "#FFF9C4", "#FFE0B2", "#FFCDD2")))
  })

  # =================== Module 8: Crop recommendation ===================
  observeEvent(input$rc_load, {
    d <- fd(); req(nrow(d) > 0)
    updateNumericInput(session, "rc_n", value = round(mean(d$nitrogen))); updateNumericInput(session, "rc_p", value = round(mean(d$phosphorus)))
    updateNumericInput(session, "rc_k", value = round(mean(d$potassium))); updateNumericInput(session, "rc_t", value = round(mean(d$temperature), 1))
    updateNumericInput(session, "rc_h", value = round(mean(d$humidity))); updateNumericInput(session, "rc_ph", value = round(mean(d$soil_ph), 2))
    updateNumericInput(session, "rc_r", value = round(mean(d$rainfall), 1))
  })
  rec <- reactive(recommend_crops(nz(input$rc_n, 0), nz(input$rc_p, 0), nz(input$rc_k, 0), nz(input$rc_t, 25), nz(input$rc_h, 60), nz(input$rc_ph, 6.5), nz(input$rc_r, 3)))
  output$rc_bar <- renderPlotly({
    r <- rec(); r$crop <- factor(r$crop, levels = rev(r$crop))
    pl(ggplot(r, aes(crop, suitability, fill = suitability, text = paste0("Env ", environment, " | Soil ", soil))) + geom_col() + coord_flip() +
         scale_fill_gradient(low = "#D7CCC8", high = "#1B5E20", limits = c(0, 100)) + labs(x = NULL, y = "Suitability score (0-100)") + theme_agro() + theme(legend.position = "none"), "text")
  })
  output$rc_tbl <- DT::renderDT({ r <- rec(); names(r)[1:5] <- c("Crop", "Suitability", "Environmental compat.", "Soil compat.", "Limiting factor"); dt(r, 8) })
  output$rc_explain <- renderUI({
    r <- rec(); top <- r$crop[1]
    vals <- list(t = nz(input$rc_t, 25), h = nz(input$rc_h, 60), r = nz(input$rc_r, 3), ph = nz(input$rc_ph, 6.5), n = nz(input$rc_n, 0), p = nz(input$rc_p, 0), k = nz(input$rc_k, 0))
    tagList(h4(sprintf("%s - suitability %.1f", top, r$suitability[1])), tags$ul(lapply(explain_crop(top, vals), tags$li)),
            p(sprintf("Main limiting factor: %s. Score = 50%% climate (temperature, humidity, rainfall) + 50%% soil (pH, N, P, K).", r$limiting_factor[1])),
            tags$small("Reference ranges are generic and not validated for a specific region."))
  })
  output$rc_radar <- renderPlotly({
    r <- rec(); validate(need(length(input$rc_cmp) > 0, "Select crops to compare."))
    th <- c("Temperature", "Humidity", "Rainfall", "pH", "Nitrogen", "Phosphorus", "Potassium")
    p <- plot_ly(type = "scatterpolar", mode = "lines")
    for (cr in input$rc_cmp) { v <- as.numeric(r[r$crop == cr, th]); p <- add_trace(p, r = c(v, v[1]), theta = c(th, th[1]), name = cr, fill = "toself") }
    p %>% layout(polar = list(radialaxis = list(range = c(0, 100), title = "compatibility %")), legend = list(orientation = "h")) %>% plotly::config(displaylogo = FALSE)
  })

  # =================== Module 9: Visualization ===================
  viz_data <- reactive({ d <- fd(); d[d$growth_stage %in% input$viz_stage & d$health_class %in% input$viz_health, ] })
  output$viz_plot <- renderPlotly({
    d <- viz_data(); need_data(d)
    make_chart(d, input$viz_type, input$viz_x, input$viz_y, input$viz_color, input$viz_agg, input$viz_bins, input$viz_size, input$viz_trend)
  })
  output$viz_map <- renderLeaflet({ d <- fd(); validate(need(nrow(d) > 0, "No data.")); farm_map(d) })
  output$viz_summary <- DT::renderDT({
    d <- viz_data(); need_data(d)
    num <- intersect(VIZ_NUM, names(d)); g <- input$viz_group
    long <- d %>% select(all_of(c(if (g != "None") g, num))) %>% pivot_longer(all_of(num), names_to = "Variable")
    if (g != "None") long <- group_by(long, .data[[g]])
    s <- long %>% group_by(Variable, .add = TRUE) %>% summarise(n = sum(!is.na(value)), Mean = mean(value, na.rm = TRUE), SD = sd(value, na.rm = TRUE),
          Min = min(value, na.rm = TRUE), Q1 = quantile(value, .25, na.rm = TRUE), Median = median(value, na.rm = TRUE), Q3 = quantile(value, .75, na.rm = TRUE),
          Max = max(value, na.rm = TRUE), .groups = "drop") %>% mutate(across(where(is.numeric), ~ round(.x, 2)))
    dt(s, 12)
  })
  output$viz_data_tbl <- DT::renderDT({ d <- viz_data(); need_data(d); dt(d, 10, filter = "top") })
  output$dl_viz <- downloadHandler(filename = function() paste0("agrovision_visualised_data_", Sys.Date(), ".csv"),
                                   content = function(file) readr::write_csv(viz_data(), file))

  # =================== Module 10: Farm management ===================
  output$fm_tbl <- DT::renderDT({
    d <- rv$data; validate(need(nrow(d) > 0, "No farm records."))
    dt(d %>% group_by(`Farm ID` = farm_id, Farm = farm_name, Location = location, Crop = crop_type) %>%
         summarise(`Area (ha)` = first(cultivated_area), Planted = first(planting_date), `Current stage` = growth_stage[which.max(date)],
                   Records = n(), `Last reading` = max(date), Source = paste(unique(data_source), collapse = ", "), .groups = "drop"), 8)
  })
  observeEvent(input$fm_sel, {
    req(input$fm_sel); k <- strsplit(input$fm_sel, "\\|\\|")[[1]]
    r <- rv$data[rv$data$farm_id == k[1] & rv$data$crop_type == k[2], ]; req(nrow(r) > 0); r <- r[which.max(r$date), ]
    updateTextInput(session, "fm_e_name", value = r$farm_name); updateTextInput(session, "fm_e_loc", value = r$location)
    updateNumericInput(session, "fm_e_lat", value = nz(r$latitude, 0)); updateNumericInput(session, "fm_e_lon", value = nz(r$longitude, 0))
    updateNumericInput(session, "fm_e_area", value = r$cultivated_area); updateDateInput(session, "fm_e_plant", value = r$planting_date)
    updateSelectInput(session, "fm_e_stage", selected = r$growth_stage)
  })
  observeEvent(input$fm_add, {
    nm <- trimws(input$fm_name)
    if (!nzchar(nm) || nz(input$fm_area, 0) <= 0) return(showNotification("Farm name and a positive area are required.", type = "error"))
    d <- rv$data; ex <- d[d$farm_name == nm, ]
    id <- if (nrow(ex)) ex$farm_id[1] else sprintf("F%02d", max(c(0, suppressWarnings(as.integer(sub("^F", "", unique(d$farm_id))))), na.rm = TRUE) + 1)
    raw <- data.frame(farm_id = id, farm_name = nm, location = if (nzchar(trimws(input$fm_loc))) input$fm_loc else if (nrow(ex)) ex$location[1] else "Unknown",
                      latitude = if (nrow(ex)) ex$latitude[1] else input$fm_lat, longitude = if (nrow(ex)) ex$longitude[1] else input$fm_lon,
                      crop_type = input$fm_crop, cultivated_area = input$fm_area, date = as.character(Sys.Date()), reading_time = format(Sys.time(), "%H:%M"),
                      planting_date = as.character(input$fm_plant), soil_ph = input$fm_ph, soil_moisture = input$fm_moist, nitrogen = input$fm_n,
                      phosphorus = input$fm_p, potassium = input$fm_k, temperature = input$fm_temp, humidity = input$fm_hum, rainfall = input$fm_rain,
                      growth_stage = input$fm_stage, stringsAsFactors = FALSE)
    ingest(raw, "append", "Manual entry")
  })
  observeEvent(input$fm_save, {
    req(input$fm_sel); k <- strsplit(input$fm_sel, "\\|\\|")[[1]]
    if (!nzchar(trimws(input$fm_e_name)) || nz(input$fm_e_area, 0) <= 0) return(showNotification("Name and a positive area are required.", type = "error"))
    d <- rv$data; fa <- d$farm_id == k[1]; pl_i <- fa & d$crop_type == k[2]
    d$farm_name[fa] <- trimws(input$fm_e_name); d$location[fa] <- input$fm_e_loc; d$latitude[fa] <- input$fm_e_lat; d$longitude[fa] <- input$fm_e_lon
    d$cultivated_area[pl_i] <- input$fm_e_area; d$planting_date[pl_i] <- input$fm_e_plant
    last <- pl_i & d$date == max(d$date[pl_i]); d$growth_stage[last] <- input$fm_e_stage
    rv$data <- d; showNotification("Farm details updated.")
  })
  observeEvent(input$fm_del, {
    req(input$fm_sel); k <- strsplit(input$fm_sel, "\\|\\|")[[1]]
    showModal(modalDialog(title = "Delete farm", sprintf("Delete ALL records of farm %s (every crop plot)? This cannot be undone.", k[1]),
                          footer = tagList(modalButton("Cancel"), actionButton("fm_del_ok", "Delete", class = "btn-danger"))))
  })
  observeEvent(input$fm_del_ok, {
    k <- strsplit(input$fm_sel, "\\|\\|")[[1]]; rv$data <- rv$data[rv$data$farm_id != k[1], ]; removeModal(); showNotification("Farm deleted.")
  })
  up_raw <- reactive({ req(input$fm_file); tryCatch(read_any_csv(input$fm_file$datapath), error = function(e) NULL) })
  up_val <- reactive({ r <- up_raw(); if (is.null(r)) list(ok = FALSE, data = NULL, messages = "The file could not be read as CSV.") else validate_upload(r) })
  output$fm_up_msg <- renderUI({ if (is.null(input$fm_file)) return(NULL); v <- up_val(); div(class = if (v$ok) "msg-ok" else "msg-err", lapply(v$messages, function(m) p(m))) })
  output$fm_up_prev <- DT::renderDT({ v <- up_val(); validate(need(v$ok, "Upload a valid CSV to preview it here.")); dt(v$data, 6) })
  observeEvent(input$fm_apply, {
    if (is.null(input$fm_file)) return(showNotification("Choose a CSV file first.", type = "warning"))
    r <- up_raw(); if (!is.null(r)) ingest(r, input$fm_mode, "Uploaded CSV")
  })
  observeEvent(input$fm_reset, { rv$data <- load_initial_data(); showNotification("Sample dataset restored.") })
  output$dl_template <- downloadHandler(filename = function() "agrovision_template.csv",
                                        content = function(file) readr::write_csv(head(INIT[, ALL_COLS], 5), file))
  output$dl_export_csv <- downloadHandler(filename = function() paste0("agrovision_data_", Sys.Date(), ".csv"), content = function(file) readr::write_csv(rv$data, file))
  output$dl_export_xlsx <- downloadHandler(filename = function() paste0("agrovision_data_", Sys.Date(), ".xlsx"), content = function(file) writexl::write_xlsx(list(Data = rv$data), file))

  # =================== Module 11: Alerts ===================
  output$al_kpis <- renderUI({
    a <- alerts_df(); n <- function(s) sum(a$severity == s)
    fluidRow(column(4, kpi(n("Critical"), "Critical alerts", "exclamation-circle")), column(4, kpi(n("Warning"), "Warnings", "exclamation-triangle")),
             column(4, kpi(n("Informational"), "Informational", "info-circle")))
  })
  output$al_type <- renderPlotly({
    a <- alerts_df(); validate(need(nrow(a) > 0, "No alerts for the selected filters."))
    pl(ggplot(a, aes(type, fill = severity)) + geom_bar() + coord_flip() + scale_fill_manual(values = SEV_COLORS) + labs(x = NULL, y = "Alerts", fill = NULL) + theme_agro())
  })
  output$al_time <- renderPlotly({
    a <- alerts_df(); validate(need(nrow(a) > 0, "No alerts for the selected filters."))
    a$day <- as.Date(a$timestamp)
    pl(ggplot(a, aes(day, fill = severity)) + geom_bar() + scale_fill_manual(values = SEV_COLORS) + labs(x = NULL, y = "Alerts per day", fill = NULL) + theme_agro())
  })
  al_view <- reactive({ a <- alerts_df(); a[a$severity %in% input$al_sev, ] })
  output$al_tbl <- DT::renderDT({ a <- al_view(); validate(need(nrow(a) > 0, "No alerts match the selection.")); sev_style(dt(alert_display(a), 10)) })
  output$dl_alerts <- downloadHandler(filename = function() paste0("agrovision_alerts_", Sys.Date(), ".csv"), content = function(file) readr::write_csv(alert_display(al_view()), file))

  # =================== Module 12: Reports ===================
  filters_txt <- reactive(sprintf("Farms: %s | Crops: %s | Dates: %s to %s", paste(input$f_farm, collapse = ", "), paste(input$f_crop, collapse = ", "), input$f_date[1], input$f_date[2]))
  output$rp_preview <- renderUI({
    d <- fd(); validate(need(nrow(d) > 0, "No data for the selected filters."))
    a <- alerts_df(); m <- model_rv()
    tags$ul(tags$li(filters_txt()), tags$li(sprintf("%d records, %d farms, %d crops, %.1f ha cultivated.", nrow(d), n_distinct(d$farm_id), n_distinct(d$crop_type), sum(latest_per_plot(d)$cultivated_area))),
            tags$li(sprintf("Mean crop health %.1f | soil moisture %.1f%% | temperature %.1f C | soil pH %.2f.", mean(d$crop_health), mean(d$soil_moisture), mean(d$temperature), mean(d$soil_ph))),
            tags$li(sprintf("Alerts: %d critical, %d warning, %d informational.", sum(a$severity == "Critical"), sum(a$severity == "Warning"), sum(a$severity == "Informational"))),
            tags$li(if (is.null(m)) "No yield model trained." else sprintf("Yield model test RMSE %.2f, MAE %.2f, R-squared %.3f.", m$metrics["RMSE"], m$metrics["MAE"], m$metrics["R2"])))
  })
  output$dl_csv <- downloadHandler(filename = function() paste0("agrovision_report_data_", Sys.Date(), ".csv"), content = function(file) readr::write_csv(fd(), file))
  output$dl_xlsx <- downloadHandler(filename = function() paste0("agrovision_report_", Sys.Date(), ".xlsx"), content = function(file) {
    d <- fd(); m <- model_rv(); a <- alerts_df()
    sheets <- list(Filtered_Data = d, Farm_Summary = d %>% group_by(farm_name, crop_type) %>% summarise(records = n(), mean_health = mean(crop_health), mean_moisture = mean(soil_moisture),
                                                                                                        mean_temp = mean(temperature), mean_ph = mean(soil_ph), .groups = "drop"),
                   Alerts = alert_display(a))
    if (!is.null(irr_df())) sheets$Irrigation <- irr_df()
    if (!is.null(pred_df())) sheets$Yield_Predictions <- pred_df()
    if (!is.null(m)) sheets$Model_Metrics <- data.frame(Metric = c("RMSE", "MAE", "R2", "OOB_R2", "Train_rows", "Test_rows"), Value = c(m$metrics, m$oob_r2, m$n_train, m$n_test))
    writexl::write_xlsx(sheets, file)
  })
  output$dl_json <- downloadHandler(filename = function() paste0("agrovision_report_", Sys.Date(), ".json"), content = function(file) {
    d <- fd(); jsonlite::write_json(list(meta = list(generated = format(Sys.time()), filters = filters_txt(), records = nrow(d)), data = d),
                                    file, pretty = TRUE, auto_unbox = TRUE, na = "null", dataframe = "rows")
  })
  output$dl_pdf <- downloadHandler(filename = function() paste0("agrovision_report_", Sys.Date(), ".pdf"), content = function(file) {
    d <- fd(); if (!nrow(d)) stop("No data for the selected filters.")
    withProgress(message = "Building PDF report...", value = 0.5, {
      write_pdf_report(file, d, alerts_df(), irr_df(), pred_df(), model_rv(), input$rp_sections, input$rp_title, input$rp_author, filters_txt())
    })
    try(file.copy(file, file.path("reports", paste0("report_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".pdf"))), silent = TRUE)
  })
}

shinyApp(ui, server)
