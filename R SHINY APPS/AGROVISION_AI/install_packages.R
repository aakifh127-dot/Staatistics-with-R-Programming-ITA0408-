# Run once:  source("install_packages.R")
pkgs <- c("shiny", "shinydashboard", "shinyWidgets", "ggplot2", "plotly", "DT", "dplyr", "tidyr",
          "readr", "leaflet", "randomForest", "caret", "corrplot", "lubridate", "forecast",
          "writexl", "jsonlite")
missing <- setdiff(pkgs, rownames(installed.packages()))
if (length(missing)) install.packages(missing, repos = "https://cloud.r-project.org")
cat("All packages available.\n")
