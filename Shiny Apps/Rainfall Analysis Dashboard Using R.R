# RainSense AI | Complete Light-Theme Rainfall Dashboard
# Save as app.R and click Run App in RStudio.

required <- c('shiny', 'plotly', 'dplyr', 'lubridate', 'jsonlite', 'forecast', 'DT')
for (p in required) {
  if (!requireNamespace(p, quietly = TRUE)) install.packages(p, repos = 'https://cloud.r-project.org')
}
library(shiny)
library(plotly)
library(dplyr)
library(lubridate)
# jsonlite functions are accessed with jsonlite:: to avoid masking shiny::validate
library(forecast)
library(DT)

cities <- data.frame(
  city = c('Chennai', 'Vellore', 'Coimbatore', 'Madurai', 'Puducherry',
           'Bengaluru', 'Mumbai', 'Delhi'),
  lat = c(13.083, 12.917, 11.017, 9.925, 11.934, 12.972, 19.076, 28.614),
  lon = c(80.270, 79.133, 76.956, 78.120, 79.830, 77.595, 72.878, 77.209)
)

make_demo <- function() {
  set.seed(42)
  dates <- seq(as.Date('2021-01-01'), as.Date('2025-12-31'), by = 'day')
  mo <- month(dates)
  chance <- ifelse(mo %in% 10:12, .48, ifelse(mo %in% 6:9, .32, .16))
  amount <- ifelse(mo %in% 10:12, 11, ifelse(mo %in% 6:9, 7, 4))
  rain <- ifelse(runif(length(dates)) < chance,
                 rgamma(length(dates), shape = 1.6, scale = amount), 0)
  data.frame(date = dates, rainfall = round(rain, 1))
}

fetch_history <- function(city) {
  location <- cities[cities$city == city, , drop = FALSE]
  endpoint <- paste0(
    'https://archive-api.open-meteo.com/v1/archive?latitude=', location$lat,
    '&longitude=', location$lon,
    '&start_date=2020-01-01&end_date=', Sys.Date() - 7,
    '&daily=precipitation_sum&timezone=Asia%2FKolkata'
  )
  response <- jsonlite::fromJSON(endpoint)
  if (is.null(response$daily) || is.null(response$daily$time) ||
      is.null(response$daily$precipitation_sum)) stop('API returned no daily precipitation data.')
  df <- data.frame(date = as.Date(response$daily$time),
                   rainfall = as.numeric(response$daily$precipitation_sum))
  df <- df[!is.na(df$date) & !is.na(df$rainfall) & df$rainfall >= 0, ]
  if (nrow(df) == 0) stop('No valid daily records returned.')
  df
}

clean_upload <- function(path) {
  x <- read.csv(path, stringsAsFactors = FALSE, check.names = FALSE)
  names(x) <- trimws(tolower(names(x)))
  if (!all(c('date', 'rainfall') %in% names(x)))
    stop('CSV must contain columns named date and rainfall.')
  dates <- as.Date(x$date, format = '%Y-%m-%d')
  vals <- suppressWarnings(as.numeric(x$rainfall))
  if (any(is.na(dates)) || any(is.na(vals)) || any(vals < 0))
    stop('Invalid CSV. Use YYYY-MM-DD dates and nonnegative numerical rainfall in mm.')
  data.frame(date = dates, rainfall = vals) |>
    group_by(date) |>
    summarise(rainfall = sum(rainfall), .groups = 'drop') |>
    arrange(date)
}

plot_style <- function(p) {
  p |>
    layout(
      paper_bgcolor = '#FFFFFF', plot_bgcolor = '#FFFFFF',
      font = list(family = 'Arial, sans-serif', color = '#334155', size = 12),
      margin = list(l = 60, r = 25, t = 25, b = 55),
      xaxis = list(automargin = TRUE, gridcolor = '#F1F5F9', zeroline = FALSE),
      yaxis = list(automargin = TRUE, gridcolor = '#E2E8F0', zeroline = FALSE),
      hovermode = 'x unified'
    ) |>
    config(displaylogo = FALSE, responsive = TRUE)
}

metric <- function(label, value_id, icon, color) {
  div(class = 'metric',
      div(class = 'metric-icon', style = paste0('background:', color, '16;color:', color), icon),
      div(class = 'metric-label', label),
      div(class = 'metric-value', textOutput(value_id, inline = TRUE)))
}

chart_card <- function(title, id, height = '330px') {
  div(class = 'chart-card', h3(title), plotlyOutput(id, height = height))
}

ui <- fluidPage(
  tags$head(
    tags$meta(name = 'viewport', content = 'width=device-width, initial-scale=1'),
    tags$style(HTML('
      body {background:#F3F7FC !important; color:#152238 !important; font-family:Segoe UI,Arial,sans-serif;}
      .container-fluid {padding:0 !important;}
      .topbar {background:#FFFFFF; border-bottom:1px solid #E2E8F0; padding:19px 30px; display:flex; align-items:center; justify-content:space-between;}
      .brand {font-size:25px; font-weight:800; color:#1559C1;}
      .brand span {color:#16233A;}
      .tagline {font-size:13px; color:#64748B; margin-top:3px;}
      .top-pill {background:#EAF5FF; color:#1D62B8; font-weight:600; border-radius:30px; padding:9px 15px;}
      .app-body {padding:23px 28px 30px;}
      .sidebar-box {background:#FFFFFF; border:1px solid #E2E8F0; border-radius:17px; padding:22px; box-shadow:0 4px 22px #14315B09;}
      .sidebar-box h3 {margin-top:0; font-weight:750; font-size:18px; color:#142844;}
      .sidebar-box label {color:#475569; font-weight:650; font-size:13px;}
      .sidebar-box .btn {width:100%; border-radius:9px; font-weight:650; margin-bottom:8px;}
      .btn-primary {background:#2563EB; border-color:#2563EB;}
      .btn-primary:hover {background:#1D4ED8;}
      .source-note {padding:11px; background:#F1F7FF; color:#33557D; border-radius:9px; font-size:12px; overflow-wrap:anywhere;}
      .content-title {font-size:25px; font-weight:800; margin:2px 0 7px; color:#142844;}
      .content-sub {color:#64748B; font-size:14px; margin-bottom:22px;}
      .nav-tabs {border:0; background:#FFFFFF; border-radius:12px; padding:9px; margin-bottom:18px; box-shadow:0 3px 16px #14315B0A; display:flex; flex-wrap:wrap; gap:4px;}
      .nav-tabs>li>a {color:#64748B !important; border:0 !important; border-radius:9px !important; font-weight:650; padding:11px 15px; margin:0;}
      .nav-tabs>li.active>a,.nav-tabs>li.active>a:hover,.nav-tabs>li.active>a:focus {background:#E9F1FF !important; color:#1D4ED8 !important; border:0 !important;}
      .nav-tabs>li>a:hover {background:#F2F6FC;}
      .metric-grid {display:grid; grid-template-columns:repeat(4,minmax(0,1fr)); gap:15px; margin-bottom:16px;}
      .metric {background:#FFFFFF; border:1px solid #E2E8F0; border-radius:14px; padding:18px; min-height:138px; box-shadow:0 3px 14px #14315B08;}
      .metric-icon {display:inline-block; font-size:17px; padding:6px 11px; border-radius:9px; font-weight:750; margin-bottom:10px;}
      .metric-label {font-size:13px; color:#64748B; font-weight:600;}
      .metric-value {font-size:24px; color:#142844; font-weight:800; margin-top:6px; word-break:break-word;}
      .chart-card {background:#FFFFFF; border:1px solid #E2E8F0; border-radius:15px; padding:19px 19px 10px; margin-bottom:16px; box-shadow:0 3px 16px #14315B08; min-width:0;}
      .chart-card h3 {font-size:17px; font-weight:750; margin:0 0 12px; color:#1A3154;}
      .chart-grid {display:grid; grid-template-columns:1fr 1fr; gap:16px;}
      .info-note {background:#EAF5FF; color:#315477; border-left:4px solid #3B82F6; padding:13px; margin-bottom:18px; border-radius:7px;}
      .shiny-plot-output,.plotly,.js-plotly-plot {width:100% !important;}
      .dataTables_wrapper {overflow-x:auto;}
      @media (max-width:1100px) {.metric-grid {grid-template-columns:repeat(2,minmax(0,1fr));}.chart-grid{grid-template-columns:1fr;}}
      @media (max-width:650px) {.app-body{padding:12px;}.topbar{padding:15px;}.brand{font-size:20px;}.top-pill{display:none;}.metric-grid{grid-template-columns:1fr 1fr;gap:8px;}.metric{padding:12px;}.metric-value{font-size:19px;}.nav-tabs>li>a{padding:8px 10px;font-size:12px;}}
    '))
  ),
  div(class = 'topbar',
      div(div(class = 'brand', 'RainSense ', span('AI')),
          div(class = 'tagline', 'Rainfall intelligence & climate analytics')),
      div(class = 'top-pill', 'R Shiny Dashboard')),
  div(class = 'app-body',
      fluidRow(
        column(3,
          div(class = 'sidebar-box',
              h3('Dashboard Controls'),
              selectInput('city', 'Location', cities$city, selected = 'Chennai'),
              actionButton('load', 'Load Real Rainfall Data', class = 'btn-primary'),
              fileInput('file', 'Or upload a CSV', accept = '.csv'),
              tags$small('Required CSV columns: date, rainfall (mm). Date format: YYYY-MM-DD.'),
              hr(),
              dateRangeInput('dates', 'Date range',
                             start = '2021-01-01', end = '2025-12-31'),
              hr(),
              sliderInput('roof', 'Roof area (m²)', 10, 1000, 100, step = 10),
              sliderInput('eff', 'Collection efficiency (%)', 40, 95, 80),
              hr(),
              div(class = 'source-note', strong('Data source'), textOutput('source')),
              br(),
              downloadButton('download', 'Download Filtered CSV')
          )
        ),
        column(9,
          h1(class = 'content-title', 'Rainfall Analytics Dashboard'),
          p(class = 'content-sub', 'Explore patterns, anomalies, forecasts and harvesting opportunities.'),
          tabsetPanel(id = 'page',
            tabPanel('Overview',
              div(class = 'metric-grid',
                  metric('Total rainfall', 'total', '☁', '#2563EB'),
                  metric('Mean daily rainfall', 'mean', '◉', '#0891B2'),
                  metric('Rainy days', 'rainydays', '☂', '#059669'),
                  metric('Maximum daily rainfall', 'maximum', '↗', '#EA580C')),
              chart_card('Daily Rainfall Trend', 'trend', '350px'),
              div(class = 'chart-grid',
                  chart_card('Average Rainfall by Month', 'monthly', '310px'),
                  chart_card('Rainfall Intensity Distribution', 'distribution', '310px'))
            ),
            tabPanel('Historical Analysis',
              div(class = 'info-note', 'All charts update automatically when the selected date range changes.'),
              chart_card('Yearly Rainfall Comparison', 'yearly', '340px'),
              chart_card('Monthly Rainfall Heatmap', 'heatmap', '360px'),
              chart_card('Rainfall by Season', 'seasonal', '310px')
            ),
            tabPanel('AI Prediction',
              div(class = 'info-note', 'ARIMA forecasts monthly totals from continuous historical observations. Prediction intervals are estimates, not guarantees.'),
              sliderInput('horizon', 'Months to predict', 3, 12, 6),
              actionButton('predict', 'Generate Forecast', class = 'btn-primary'),
              br(), br(),
              chart_card('Observed and Forecast Monthly Rainfall', 'forecastchart', '390px'),
              div(class = 'chart-card', h3('Forecast Table'), DTOutput('forecasttable'))
            ),
            tabPanel('Climate Intelligence',
              div(class = 'info-note', 'Annual anomalies are compared with the mean of complete calendar years in the available filtered data. These are descriptive indicators, not official alerts.'),
              chart_card('Annual Rainfall Anomalies (%)', 'anomalychart', '360px'),
              div(class = 'chart-card', h3('Yearly Anomaly Classification'), DTOutput('anomalytable'))
            ),
            tabPanel('Water Harvesting',
              div(class = 'metric-grid', metric('Potential water collection', 'harvest', '💧', '#059669')),
              div(class = 'info-note', 'Potential collection (liters) = rainfall (mm) × roof area (m²) × efficiency. This is a theoretical estimate before additional system losses.'),
              chart_card('Potential Collection by Month', 'harvestchart', '390px')
            ),
            tabPanel('Dataset',
              div(class = 'chart-card', h3('Filterable Rainfall Records'), DTOutput('datatable')),
              div(class = 'chart-card', h3('Data Summary'), verbatimTextOutput('summary'))
            )
          )
        )
      )
  )
)

server <- function(input, output, session) {
  raw <- reactiveVal(make_demo())
  source_name <- reactiveVal('SYNTHETIC DEMO DATA (not observed rainfall)')

  observeEvent(input$load, {
    tryCatch({
      withProgress(message = 'Fetching Open-Meteo data...', value = 0.5, {
        new_data <- fetch_history(input$city)
        raw(new_data)
        source_name(paste('Open-Meteo Historical API —', input$city))
        updateDateRangeInput(session, 'dates',
                             start = min(new_data$date), end = max(new_data$date),
                             min = min(new_data$date), max = max(new_data$date))
      })
      showNotification('Real rainfall data loaded.', type = 'message')
    }, error = function(e) showNotification(
      paste('Download failed:', conditionMessage(e)), type = 'error', duration = 9))
  })

  observeEvent(input$file, {
    tryCatch({
      new_data <- clean_upload(input$file$datapath)
      raw(new_data)
      source_name(paste('Uploaded CSV —', input$file$name))
      updateDateRangeInput(session, 'dates', start = min(new_data$date),
                           end = max(new_data$date), min = min(new_data$date),
                           max = max(new_data$date))
      showNotification('CSV loaded successfully.', type = 'message')
    }, error = function(e) showNotification(
      conditionMessage(e), type = 'error', duration = 9))
  })

  output$source <- renderText(source_name())

  data <- reactive({
    req(input$dates)
    x <- raw() |>
      filter(date >= as.Date(input$dates[1]), date <= as.Date(input$dates[2])) |>
      arrange(date)
    shiny::validate(shiny::need(nrow(x) > 0, 'No rainfall data in the selected date range.'))
    x
  })

  monthly_data <- reactive({
    data() |>
      mutate(month_date = floor_date(date, 'month')) |>
      group_by(month_date) |>
      summarise(rainfall = sum(rainfall), .groups = 'drop') |>
      arrange(month_date)
  })

  yearly_data <- reactive({
    data() |>
      mutate(year = year(date)) |>
      group_by(year) |>
      summarise(rainfall = sum(rainfall), days = n(), .groups = 'drop')
  })

  output$total <- renderText(sprintf('%.1f mm', sum(data()$rainfall)))
  output$mean <- renderText(sprintf('%.2f mm', mean(data()$rainfall)))
  output$rainydays <- renderText(sum(data()$rainfall > 0))
  output$maximum <- renderText(sprintf('%.1f mm', max(data()$rainfall)))

  output$trend <- renderPlotly({
    x <- data()
    plot_style(plot_ly(x, x = ~date, y = ~rainfall, type = 'scatter', mode = 'lines',
                      line = list(color = '#2563EB', width = 1.7),
                      fill = 'tozeroy', fillcolor = 'rgba(37,99,235,0.12)',
                      name = 'Rainfall', hovertemplate = '%{x}<br>%{y:.1f} mm<extra></extra>') |>
                 layout(yaxis = list(title = 'Daily rainfall (mm)')))
  })

  output$monthly <- renderPlotly({
    x <- monthly_data() |>
      mutate(month_num = month(month_date)) |>
      group_by(month_num) |>
      summarise(rainfall = mean(rainfall), .groups = 'drop') |>
      arrange(month_num)
    x$label <- month.abb[x$month_num]
    plot_style(plot_ly(x, x = ~label, y = ~rainfall, type = 'bar',
                      marker = list(color = '#38BDF8'), name = 'Monthly average') |>
                 layout(yaxis = list(title = 'Average monthly total (mm)')))
  })

  output$distribution <- renderPlotly({
    x <- data() |>
      mutate(group = case_when(rainfall == 0 ~ 'No rain', rainfall < 2.5 ~ 'Light',
                               rainfall < 15 ~ 'Moderate', rainfall < 65 ~ 'Heavy',
                               TRUE ~ 'Very heavy')) |>
      count(group)
    plot_ly(x, labels = ~group, values = ~n, type = 'pie', hole = .55,
            marker = list(colors = c('#94A3B8', '#38BDF8', '#0EA5E9', '#2563EB', '#1E40AF')),
            textinfo = 'label+percent') |>
      layout(paper_bgcolor = '#FFFFFF', font = list(color = '#334155'),
             margin = list(l = 15, r = 15, b = 15, t = 15), showlegend = FALSE) |>
      config(displaylogo = FALSE, responsive = TRUE)
  })

  output$yearly <- renderPlotly({
    x <- yearly_data()
    plot_style(plot_ly(x, x = ~factor(year), y = ~rainfall, type = 'bar',
                      marker = list(color = '#3B82F6'),
                      hovertemplate = '%{x}: %{y:.1f} mm<extra></extra>') |>
                 layout(xaxis = list(title = 'Year'),
                        yaxis = list(title = 'Recorded rainfall (mm)')))
  })

  output$heatmap <- renderPlotly({
    x <- monthly_data() |>
      mutate(yr = year(month_date), mo = month(month_date))
    years <- sort(unique(x$yr))
    z <- matrix(NA_real_, nrow = length(years), ncol = 12)
    for (i in seq_len(nrow(x))) {
      z[match(x$yr[i], years), x$mo[i]] <- x$rainfall[i]
    }
    plot_ly(x = month.abb, y = as.character(years), z = z, type = 'heatmap',
            colorscale = list(c(0, '#EFF6FF'), c(.5, '#60A5FA'), c(1, '#1D4ED8')),
            colorbar = list(title = 'mm'),
            hovertemplate = '%{y} %{x}<br>%{z:.1f} mm<extra></extra>') |>
      layout(paper_bgcolor = '#FFFFFF', plot_bgcolor = '#FFFFFF',
             font = list(color = '#334155'),
             margin = list(l = 60, r = 40, t = 20, b = 55),
             xaxis = list(title = 'Month'), yaxis = list(title = 'Year', type = 'category')) |>
      config(displaylogo = FALSE, responsive = TRUE)
  })

  output$seasonal <- renderPlotly({
    x <- data() |>
      mutate(season = case_when(month(date) %in% 6:9 ~ 'Southwest monsoon',
                                month(date) %in% 10:12 ~ 'Northeast monsoon',
                                TRUE ~ 'Other months')) |>
      group_by(season) |>
      summarise(rainfall = sum(rainfall), .groups = 'drop')
    plot_style(plot_ly(x, x = ~season, y = ~rainfall, type = 'bar',
                      marker = list(color = '#0EA5E9'),
                      hovertemplate = '%{x}<br>%{y:.1f} mm<extra></extra>') |>
                 layout(yaxis = list(title = 'Total rainfall (mm)'),
                        xaxis = list(title = 'Season')))
  })

  prediction <- eventReactive(input$predict, {
    x <- monthly_data()
    req(nrow(x) > 0)
    expected <- seq(min(x$month_date), max(x$month_date), by = 'month')
    shiny::validate(shiny::need(length(expected) >= 24, 'At least 24 months of rainfall history are required.'),
             shiny::need(length(expected) == nrow(x),
                  'Missing months detected. Upload a continuous dataset to forecast.'))
    # Also reject incomplete first/last months to avoid interpreting partial totals as full months.
    original <- data()
    all_dates <- seq(min(expected), max(expected) %m+% months(1) - days(1), by = 'day')
    shiny::validate(shiny::need(all(all_dates %in% original$date),
                  'Forecast requires complete daily records for every month. Choose full calendar months.'))
    series <- ts(x$rainfall, frequency = 12,
                 start = c(year(min(x$month_date)), month(min(x$month_date))))
    model <- forecast::auto.arima(series)
    f <- forecast::forecast(model, h = input$horizon, level = 95)
    future <- seq(max(expected) %m+% months(1), by = 'month', length.out = input$horizon)
    data.frame(date = future, predicted = pmax(0, as.numeric(f$mean)),
               lower = pmax(0, as.numeric(f$lower[, 1])),
               upper = pmax(0, as.numeric(f$upper[, 1])))
  })

  output$forecastchart <- renderPlotly({
    shiny::validate(shiny::need(input$predict > 0, 'Click Generate Forecast to train a model.'))
    f <- prediction()
    x <- monthly_data()
    p <- plot_ly()
    p <- add_lines(p, data = x, x = ~month_date, y = ~rainfall,
                   name = 'Observed', line = list(color = '#2563EB', width = 2))
    p <- add_ribbons(p, data = f, x = ~date, ymin = ~lower, ymax = ~upper,
                     name = '95% interval', fillcolor = 'rgba(245,158,11,0.20)',
                     line = list(color = 'transparent'))
    p <- add_lines(p, data = f, x = ~date, y = ~predicted,
                   name = 'Forecast', line = list(color = '#EA580C', width = 3))
    plot_style(p |> layout(yaxis = list(title = 'Monthly rainfall (mm)')))
  })

  output$forecasttable <- renderDT({
    shiny::validate(shiny::need(input$predict > 0, 'Generate a forecast first.'))
    x <- prediction()
    x[-1] <- lapply(x[-1], function(v) round(v, 1))
    datatable(x, rownames = FALSE, options = list(pageLength = 6, dom = 'tip'))
  })

  anomalies <- reactive({
    x <- yearly_data() |>
      filter(days >= 365)
    shiny::validate(shiny::need(nrow(x) >= 2,
                  'At least two complete calendar years are needed for anomalies.'))
    baseline <- mean(x$rainfall)
    x |>
      mutate(anomaly_pct = if (baseline > 0) 100 * (rainfall - baseline) / baseline else NA_real_,
             status = case_when(is.na(anomaly_pct) ~ 'Unavailable',
                                anomaly_pct > 20 ~ 'Above reference',
                                anomaly_pct < -20 ~ 'Below reference',
                                TRUE ~ 'Near reference'))
  })

  output$anomalychart <- renderPlotly({
    x <- anomalies()
    plot_style(plot_ly(x, x = ~factor(year), y = ~anomaly_pct, type = 'bar',
                      marker = list(color = ifelse(x$anomaly_pct < 0, '#F97316', '#10B981')),
                      hovertemplate = '%{x}<br>%{y:.1f}%<extra></extra>') |>
                 layout(xaxis = list(title = 'Year'),
                        yaxis = list(title = 'Anomaly from dataset mean (%)')))
  })

  output$anomalytable <- renderDT({
    x <- anomalies()
    x$rainfall <- round(x$rainfall, 1)
    x$anomaly_pct <- round(x$anomaly_pct, 1)
    datatable(x, rownames = FALSE, options = list(pageLength = 7, dom = 'tip'))
  })

  output$harvest <- renderText({
    paste0(format(round(sum(data()$rainfall) * input$roof * input$eff / 100),
                  big.mark = ','), ' L')
  })

  output$harvestchart <- renderPlotly({
    x <- monthly_data() |>
      mutate(liters = rainfall * input$roof * input$eff / 100)
    plot_style(plot_ly(x, x = ~month_date, y = ~liters, type = 'bar',
                      marker = list(color = '#10B981'),
                      hovertemplate = '%{x}<br>%{y:,.0f} L<extra></extra>') |>
                 layout(yaxis = list(title = 'Estimated collection (L)')))
  })

  output$datatable <- renderDT({
    datatable(data(), filter = 'top', rownames = FALSE,
              options = list(pageLength = 10, scrollX = TRUE))
  })
  output$summary <- renderPrint({
    cat('Data source:', source_name(), '\n')
    cat('Date range:', as.character(min(data()$date)), 'to', as.character(max(data()$date)), '\n')
    cat('Records:', nrow(data()), '\n\n')
    print(summary(data()$rainfall))
  })
  output$download <- downloadHandler(
    filename = function() paste0('rainsense_', Sys.Date(), '.csv'),
    content = function(file) write.csv(data(), file, row.names = FALSE)
  )
}

shinyApp(ui, server)
