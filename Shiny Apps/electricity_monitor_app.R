# Smart Electricity Consumption Monitor - R Shiny
# Save as app.R and click Run App in RStudio.
if (!requireNamespace("shiny", quietly = TRUE)) install.packages("shiny")
if (!requireNamespace("ggplot2", quietly = TRUE)) install.packages("ggplot2")
library(shiny)
library(ggplot2)

calculate_bill <- function(units) {
  stopifnot(is.numeric(units), length(units) == 1, is.finite(units), units >= 0)
  min(units, 100) * 2 + min(max(units - 100, 0), 100) * 3 + max(units - 200, 0) * 5
}

empty_data <- function() {
  data.frame(Household = integer(), Units = numeric(), Bill = numeric(), Status = character(), stringsAsFactors = FALSE)
}

# Demonstration data includes an explicit loop and break, per assignment requirements.
make_sample_data <- function() {
  example_units <- c(80, 150, 280, 90, -1)
  result <- empty_data()
  for (units in example_units) {
    if (units == -1) break
    status <- if (units > 250) "High Consumption" else "Normal"
    result <- rbind(result, data.frame(Household = nrow(result) + 1L, Units = units,
                                      Bill = calculate_bill(units), Status = status))
  }
  result
}

money <- function(x) paste0("Rs. ", format(round(x, 2), big.mark = ",", nsmall = 2, trim = TRUE))

ui <- fluidPage(
  tags$head(tags$meta(name = "viewport", content = "width=device-width, initial-scale=1"),
    tags$style(HTML('
      body {background:#f3f7fc; color:#172b4d; font-family:"Segoe UI",Arial,sans-serif;}
      .container-fluid {padding:22px 28px; max-width:1700px;}
      .app-title {font-size:31px;font-weight:800;letter-spacing:-.5px;margin:0;color:#15294b;}
      .subtitle {color:#66788e;font-size:15px;margin:7px 0 22px;}
      .side-card,.dash-card,.metric {background:white;border:1px solid #e4eaf3;border-radius:20px;box-shadow:0 6px 22px rgba(25,58,100,.04);}
      .side-card {padding:22px;margin-bottom:18px;}
      .dash-card {padding:21px 24px;margin-bottom:20px;}
      .metric {padding:20px 21px;min-height:142px;margin-bottom:18px;}
      .metric-label {color:#657790;font-size:14px;font-weight:600;margin-bottom:13px;}
      .metric-value {color:#183c79;font-size:29px;font-weight:800;line-height:1.12;overflow-wrap:anywhere;}
      .metric-badge {font-size:12px;font-weight:700;border-radius:9px;padding:5px 9px;background:#e8f0ff;color:#4164d9;display:inline-block;margin-bottom:14px;}
      .metric.alert .metric-badge {background:#fff0ed;color:#dc563f;}
      .metric.alert .metric-value {color:#d64039;}
      .side-title {font-size:21px;font-weight:750;margin:0 0 20px;}
      .section-title {font-size:19px;font-weight:750;color:#18345d;margin:0 0 16px;}
      .form-group label {font-weight:650;color:#354965;}
      .form-control {border-radius:10px;min-height:41px;border-color:#dce3ef;}
      .btn {border-radius:10px;font-weight:650;padding:10px 13px;}
      .btn-primary {background:#4365e9;border-color:#4365e9;}
      .btn-success {background:#1fa97b;border-color:#1fa97b;}
      .btn-outline {background:#eef3fc;border:1px solid #dfe7f5;color:#365070;}
      .hint {color:#75859b;font-size:13px;line-height:1.5;}
      .state-message {background:#edf4ff;border-radius:11px;padding:12px;color:#27518a;margin-top:14px;}
      .nav-tabs {border-bottom:0;background:white;border-radius:15px;padding:8px;margin-bottom:20px;border:1px solid #e4eaf3;}
      .nav-tabs>li>a {color:#60738b;font-weight:700;border:0!important;border-radius:11px!important;margin-right:5px;}
      .nav-tabs>li.active>a,.nav-tabs>li.active>a:focus,.nav-tabs>li.active>a:hover {background:#e8efff!important;color:#355ad3!important;}
      table {background:white;} .table>thead>tr>th {color:#536881;background:#f2f6fc;border-bottom:0;}
      .table>tbody>tr>td {vertical-align:middle;} .table-responsive {border:0;}
      .high-note {background:#fff2f0;color:#b73931;border-radius:12px;padding:12px 16px;font-weight:650;}
      .normal-note {background:#effbf4;color:#166a47;border-radius:12px;padding:12px 16px;font-weight:650;}
      .tiny {font-size:12px;color:#7b899c;}
      @media(max-width:900px){.container-fluid{padding:15px}.app-title{font-size:25px}.metric{min-height:120px}.metric-value{font-size:24px}}
    '))
  ),
  div(class = "app-title", "⚡ Smart Electricity Consumption Monitor"),
  div(class = "subtitle", "Interactive household billing • Usage insights • High-consumption alerts"),
  fluidRow(
    column(width = 3,
      div(class = "side-card",
        div(class = "side-title", "Dashboard Controls"),
        numericInput("units", "Electricity consumed (units)", value = 120, min = -1, step = 1),
        actionButton("add", "＋ Add Household", class = "btn-primary", width = "100%"),
        br(), br(),
        fluidRow(
          column(6, actionButton("sample", "Sample Data", class = "btn-outline", width = "100%")),
          column(6, actionButton("clear", "Clear All", class = "btn-outline", width = "100%"))
        ),
        div(class = "state-message", textOutput("message")),
        tags$hr(),
        tags$strong("Tariff rates"),
        tags$p(class = "hint", "First 100 units: Rs. 2/unit"),
        tags$p(class = "hint", "Next 100 units: Rs. 3/unit"),
        tags$p(class = "hint", "Above 200 units: Rs. 5/unit"),
        tags$p(class = "hint", "Over 250 units triggers an alert."),
        tags$hr(),
        tags$p(class = "hint", "Enter -1 and click Add Household to stop entries. Clear All or Sample Data re-enables input."),
        downloadButton("export", "Download CSV", class = "btn-outline", width = "100%")
      )
    ),
    column(width = 9,
      tabsetPanel(id = "tabs", type = "tabs",
        tabPanel("Overview", br(),
          fluidRow(
            column(3, div(class = "metric", div(class = "metric-badge", "HOUSEHOLDS"), div(class = "metric-label", "Total households"), div(class = "metric-value", textOutput("total")))),
            column(3, div(class = "metric", div(class = "metric-badge", "REVENUE"), div(class = "metric-label", "Total electricity bill"), div(class = "metric-value", textOutput("revenue")))),
            column(3, div(class = "metric", div(class = "metric-badge", "AVERAGE"), div(class = "metric-label", "Average units"), div(class = "metric-value", textOutput("average")))),
            column(3, div(class = "metric alert", div(class = "metric-badge", "ALERTS"), div(class = "metric-label", "High consumers"), div(class = "metric-value", textOutput("high"))))
          ),
          div(class = "dash-card", div(class = "section-title", "Household Electricity Consumption"), plotOutput("usage_plot", height = "340px")),
          fluidRow(
            column(6, div(class = "dash-card", div(class = "section-title", "Electricity Bill Comparison"), plotOutput("bill_plot", height = "290px"))),
            column(6, div(class = "dash-card", div(class = "section-title", "Consumption Alerts"), uiOutput("alert_panel")))
          )
        ),
        tabPanel("Household Records", br(),
          div(class = "dash-card", div(class = "section-title", "Household Billing Records"), tableOutput("records"), tags$p(class = "tiny", "Sample records are provided for demonstration; add or remove households using the left panel."))
        ),
        tabPanel("Tariff Calculator", br(),
          div(class = "dash-card", div(class = "section-title", "Live Bill Calculator"),
            numericInput("calc_units", "Units to calculate", value = 280, min = 0, step = 1),
            h3(textOutput("calc_total")),
            tableOutput("calc_breakdown"),
            tags$p(class = "hint", "Progressive slab billing: only the units in each slab receive that rate."),
            uiOutput("calc_alert")
          )
        )
      )
    )
  )
)

server <- function(input, output, session) {
  households <- reactiveVal(make_sample_data())
  stopped <- reactiveVal(FALSE)
  notice <- reactiveVal("Sample data loaded: 4 households. Add another or enter -1 to stop.")

  observeEvent(input$add, {
    units <- input$units
    if (stopped()) {
      notice("Entry stopped. Click Clear All or Sample Data to restart.")
      return()
    }
    if (is.null(units) || length(units) != 1 || !is.finite(units)) {
      notice("Please enter a valid number of units.")
      return()
    }
    if (units == -1) {
      stopped(TRUE)
      notice("Stopped: -1 received. Existing records remain available.")
      return()
    }
    if (units < 0) {
      notice("Units cannot be negative (except -1 to stop).")
      return()
    }
    old <- households()
    new <- data.frame(Household = nrow(old) + 1L, Units = units,
                      Bill = calculate_bill(units),
                      Status = if (units > 250) "High Consumption" else "Normal")
    households(rbind(old, new))
    notice(if (units > 250) paste("HIGH CONSUMPTION ALERT: Household", nrow(old) + 1L, "used", units, "units.")
           else paste("Household", nrow(old) + 1L, "added successfully."))
  })
  observeEvent(input$sample, {
    households(make_sample_data())
    stopped(FALSE)
    notice("Sample data restored: 80, 150, 280 and 90 units.")
  })
  observeEvent(input$clear, {
    households(empty_data())
    stopped(FALSE)
    notice("All records cleared. You can start adding households.")
  })

  output$message <- renderText(notice())
  output$total <- renderText(nrow(households()))
  output$revenue <- renderText(money(sum(households()$Bill)))
  output$average <- renderText({
    d <- households()
    if (nrow(d) == 0) "0 units" else paste0(round(mean(d$Units), 1), " units")
  })
  output$high <- renderText(sum(households()$Units > 250))

  output$usage_plot <- renderPlot({
    d <- households()
    shiny::validate(shiny::need(nrow(d) > 0, "No households yet. Add one to display the chart."))
    d$Label <- factor(paste0("Household ", d$Household), levels = paste0("Household ", d$Household))
    ggplot(d, aes(Label, Units, fill = Status)) +
      geom_col(width = .58) + geom_hline(yintercept = 250, linetype = "dashed", color = "#d64949", linewidth = .8) +
      scale_fill_manual(values = c("Normal" = "#5379ed", "High Consumption" = "#f06b69")) +
      labs(x = NULL, y = "Electricity units", fill = NULL, caption = "Dashed line = 250-unit alert threshold") +
      theme_minimal(base_size = 13) + theme(legend.position = "top", panel.grid.major.x = element_blank(), plot.caption = element_text(color = "#76879d"))
  })
  output$bill_plot <- renderPlot({
    d <- households()
    shiny::validate(shiny::need(nrow(d) > 0, "No households yet."))
    d$Label <- factor(paste0("H", d$Household), levels = paste0("H", d$Household))
    ggplot(d, aes(Label, Bill)) + geom_col(fill = "#20b79b", width = .6) +
      labs(x = "Household", y = "Bill (Rs.)") + theme_minimal(base_size = 12) +
      theme(panel.grid.major.x = element_blank())
  })
  output$alert_panel <- renderUI({
    d <- households()
    high <- d[d$Units > 250, , drop = FALSE]
    if (nrow(high) == 0) return(div(class = "normal-note", "No high-consumption households."))
    tagList(
      div(class = "high-note", paste(nrow(high), "household(s) above 250 units.")),
      tags$br(),
      lapply(seq_len(nrow(high)), function(i) {
        tags$p(tags$strong(paste0("Household ", high$Household[i], ": ")),
               paste0(high$Units[i], " units • ", money(high$Bill[i])))
      })
    )
  })
  output$records <- renderTable({
    d <- households()
    if (nrow(d) == 0) return(data.frame(Message = "No household data. Add a household."))
    d$Bill <- vapply(d$Bill, money, character(1))
    d
  }, striped = TRUE, hover = TRUE, bordered = FALSE, spacing = "m", width = "100%")

  output$calc_total <- renderText({
    u <- input$calc_units
    if (is.null(u) || !is.finite(u) || u < 0) return("Enter non-negative units")
    paste("Estimated bill:", money(calculate_bill(u)))
  })
  output$calc_breakdown <- renderTable({
    u <- input$calc_units
    shiny::validate(shiny::need(!is.null(u) && is.finite(u) && u >= 0, "Enter valid units."))
    amounts <- c(min(u, 100), min(max(u - 100, 0), 100), max(u - 200, 0))
    data.frame(Slab = c("First 100 units", "Next 100 units", "Above 200 units"),
               Units = amounts, Rate = c(2, 3, 5), Amount = vapply(amounts * c(2, 3, 5), money, character(1)))
  }, striped = TRUE, bordered = FALSE)
  output$calc_alert <- renderUI({
    u <- input$calc_units
    if (is.null(u) || !is.finite(u) || u < 0) return(NULL)
    if (u > 250) div(class = "high-note", "High-consumption alert: usage exceeds 250 units.")
    else div(class = "normal-note", "Consumption is within the 250-unit alert threshold.")
  })
  output$export <- downloadHandler(
    filename = function() "electricity_households.csv",
    content = function(file) write.csv(households(), file, row.names = FALSE)
  )
}

shinyApp(ui, server)
