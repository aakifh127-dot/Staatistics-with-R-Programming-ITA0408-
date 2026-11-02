# AGROVISION AI - Smart Agriculture Intelligence Platform (R Shiny)

Academic demonstration of an integrated R data-representation framework for crop monitoring.
**All bundled data is simulated** (labelled `Simulated` in the data and in the UI banner).

## Install and run (Windows / macOS / Linux)
1. Install R (>= 4.1) and RStudio.
2. Open the `AGROVISION_AI` folder as the working directory.
3. `source("install_packages.R")`  (installs all packages; free, no API keys)
4. `shiny::runApp()`

On first run the app generates `data/sample_agriculture.csv` (600 records, 8 farms, 8 crops) if it does not exist.
`source("generate_data.R")` regenerates it manually. Map tiles need internet access (OpenStreetMap); everything else works offline.

## Project structure
```
app.R                 UI + server (12 modules)
R/config.R            constants: crop reference profiles, sensors, schema, thresholds
R/data_generator.R    reproducible simulated dataset (seed 2025)
R/helpers.R           scoring, validation/imputation, Random Forest, alerts, irrigation, recommender, PDF report
www/custom.css        theme
data/ models/ reports/  dataset, saved model (yield_rf.rds), PDF copies
```

## Architecture
Data table (sample CSV / upload / manual entry / IoT JSON) -> `validate_upload()` (type coercion, range checks, imputation) -> `enrich()` (health class, stress and disease-risk proxies, growth progress) -> global filters (farm, crop, dates) -> module reactives -> plots, tables, downloads.
**Future IoT/weather:** send JSON records with the dataset column names via the Monitoring tab (file or URL), or edit `read_iot_json()`; weather APIs only need to return temperature, humidity, rainfall, wind_speed.

## Modules
1. Executive Dashboard - 8 KPIs, crop distribution, production, trends, health, alerts, activity.
2. Farm Monitoring - simulated live sensor cards with status, time series, z-score comparison, JSON ingestion.
3. Crop Health - classification (Excellent...Critical), growth progress, water stress, disease-risk proxy.
4. Soil Analytics - configurable thresholds, nutrient charts, deficiency heatmap, quality score, correlation, recommendations.
5. Weather Intelligence - trends, rainfall, wind, monthly patterns, conditions, impact on crops.
6. ML Yield Prediction - Random Forest (caret stratified split, seed, imputation, factor encoding), RMSE/MAE/R2 from the test set, importance, single prediction, ARIMA forecast.
7. Smart Irrigation - what-if calculator and farm-wise comparison with explanations (simplified heuristic).
8. Crop Recommendation - explainable range-membership scoring, radar comparison.
9. Data Visualization - 9 chart types with dynamic variables, filters, map, statistics, CSV download.
10. Farm Management - add/edit/delete, CSV upload with validation, template, export.
11. Alerts - Critical/Warning/Informational with editable thresholds, history, download.
12. Reports - PDF (base graphics, no LaTeX needed), Excel, CSV, JSON from the current filters.

## Honest limitations
Simulated data; yield model is a demonstration model (random row split on repeated plot readings can be optimistic); health/disease/leaf indicators are proxies, not image analysis; irrigation and crop recommendation use simplified generic reference values, not validated prescriptions.

## Testing checklist
- `shiny::runApp()` starts without errors; dashboard KPIs change when farm/crop/date filters change.
- Monitoring: cards refresh every 5 s; toggling the switch stops jitter.
- ML: click *Train*; RMSE/MAE/R2 update; `models/yield_rf.rds` is created; same seed gives same metrics.
- Farm Management: add a farm (appears in filters), edit it, delete it; upload `agrovision_template.csv` (append) and a CSV with a missing column (error shown).
- Alerts: lower "Moist warn" threshold and confirm counts change.
- Reports: download PDF, XLSX, CSV, JSON; PDF reflects filters.
