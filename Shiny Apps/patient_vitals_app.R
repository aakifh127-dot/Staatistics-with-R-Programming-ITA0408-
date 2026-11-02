if (!requireNamespace("shiny", quietly = TRUE)) install.packages("shiny")
library(shiny)

# Demonstrates while(), if-else, and break with the -1 sentinel.
process_readings <- function(values) {
  result <- data.frame(Reading = integer(), HeartRate = numeric(),
                       Classification = character(), Flag = character(),
                       stringsAsFactors = FALSE)
  i <- 1
  while (i <= length(values)) {
    rate <- values[i]
    if (rate == -1) break
    if (is.na(rate) || !is.finite(rate) || rate < 0) {
      i <- i + 1
      next
    }
    if (rate < 60) {
      classification <- "Below reference range"
      flag <- "Clinical review"
    } else if (rate > 100) {
      classification <- "Above reference range"
      flag <- "Clinical review"
    } else {
      classification <- "Within reference range"
      flag <- "Not flagged"
    }
    result[nrow(result) + 1, ] <- list(nrow(result) + 1, rate,
                                        classification, flag)
    i <- i + 1
  }
  result
}

sample_values <- c(72, 110, 58, 85, 105, -1)

ui <- fluidPage(
  tags$head(tags$style(HTML("
    body { background:#f4f7fd; color:#162b49; font-family:'Segoe UI',Arial,sans-serif; }
    .container-fluid { max-width:1500px; padding:22px 28px; }
    .hero { background:linear-gradient(110deg,#ffffff,#eaf3ff); border:1px solid #e0e9f6;
            border-radius:22px; padding:25px 30px; margin-bottom:20px; }
    .eyebrow { color:#3f67e8; font-size:12px; letter-spacing:1.4px; font-weight:800; }
    .hero h2 { font-weight:800; color:#16325a; margin:8px 0; }
    .hero p { color:#65758c; margin:0; font-size:15px; }
    .panel-card { background:white; border:1px solid #e3eaf5; border-radius:18px;
                  box-shadow:0 7px 28px rgba(34,64,113,.045); padding:22px; margin-bottom:20px; }
    .panel-card h3 { font-size:19px; font-weight:750; margin:0 0 18px; color:#203959; }
    .metric { background:white; border:1px solid #e3eaf5; border-radius:17px; padding:19px;
              margin-bottom:18px; min-height:124px; }
    .metric .label { font-size:13px; font-weight:650; color:#63758f; }
    .metric .number { font-size:32px; font-weight:800; margin-top:12px; color:#1b3763; }
    .metric.alert .number { color:#df4854; }
    .metric.ok .number { color:#159b78; }
    .btn { border-radius:10px; font-weight:650; padding:9px 15px; }
    .btn-primary { background:#4265e8; border-color:#4265e8; }
    .btn-danger { background:#eb5b6a; border-color:#eb5b6a; }
    .form-control { border-radius:9px; }
    .status { color:#3b577b; font-size:14px; padding:10px 12px; background:#edf4ff;
              border-radius:10px; margin:10px 0; }
    .note { color:#6a7890; font-size:12px; line-height:1.6; }
    .table>thead>tr>th { background:#f0f5ff; color:#36527b; border-bottom:0; }
    .table>tbody>tr>td { vertical-align:middle; }
    .alert-row { background:#fff1f2 !important; }
    @media(max-width:767px) { .container-fluid { padding:12px; } .hero { padding:19px; } }
  "))),
  div(class="hero",
      div(class="eyebrow", "HOSPITAL MONITORING • R SHINY"),
      h2("Patient Vital-Sign Monitoring"),
      p("Heart-rate analytics, reference-range flags and interactive reading history")),
  fluidRow(
    column(3, div(class="panel-card",
      h3("Monitoring Controls"),
      numericInput("rate", "Heart rate (bpm)", value=72, min=-1, step=1),
      actionButton("add", "Add Reading", class="btn-primary", width="100%"),
      br(), br(),
      actionButton("stop", "Stop (-1)", class="btn-warning", width="100%"),
      br(), br(),
      actionButton("reset", "Reset Sample Data", class="btn-danger", width="100%"),
      div(class="status", textOutput("status")),
      p(class="note", "Enter -1 or use Stop to end collection. After stopping, reset to start again."),
      tags$hr(),
      downloadButton("download", "Download CSV", class="btn-default"),
      tags$hr(),
      p(class="note", "Programming example only. The 60–100 bpm reference range is not a diagnosis or substitute for clinician assessment.")
    )),
    column(9,
      fluidRow(
        column(4, div(class="metric", div(class="label", "TOTAL VALID READINGS"),
                      div(class="number", textOutput("total")))),
        column(4, div(class="metric alert", div(class="label", "FLAGGED FOR REVIEW"),
                      div(class="number", textOutput("flagged")))),
        column(4, div(class="metric ok", div(class="label", "WITHIN REFERENCE RANGE"),
                      div(class="number", textOutput("normal"))))
      ),
      div(class="panel-card", h3("Heart-Rate Trend"),
          plotOutput("trend", height="320px")),
      fluidRow(
        column(7, div(class="panel-card", h3("Reading History"),
                      tableOutput("history"))),
        column(5, div(class="panel-card", h3("Flagged Readings"),
                      tableOutput("alerts"),
                      p(class="note", "Flagged: below 60 or above 100 bpm.")))
      )
    )
  )
)

server <- function(input, output, session) {
  values <- reactiveVal(sample_values)
  stopped <- reactiveVal(FALSE)
  message <- reactiveVal("Sample readings loaded: 72, 110, 58, 85, 105. 3 flagged.")

  records <- reactive(process_readings(values()))

  observeEvent(input$add, {
    if (stopped()) { message("Collection has stopped. Reset to continue."); return() }
    rate <- input$rate
    if (is.null(rate) || length(rate) != 1 || is.na(rate) || !is.finite(rate)) {
      message("Enter a valid numeric heart rate."); return()
    }
    if (rate == -1) {
      values(c(values()[values() != -1], -1))
      stopped(TRUE)
      message("Sentinel -1 received. Collection stopped.")
      return()
    }
    if (rate < 0) { message("Negative readings other than -1 are invalid."); return() }
    # Remove any trailing demonstration sentinel before appending.
    old <- values()
    if (length(old) && tail(old, 1) == -1) old <- head(old, -1)
    values(c(old, rate, -1))
    message(paste0("Reading ", rate, " bpm added.",
                   if (rate < 60 || rate > 100) " Flagged for clinical review." else " Within reference range."))
  })

  observeEvent(input$stop, {
    stopped(TRUE)
    message("Collection stopped using sentinel -1.")
  })

  observeEvent(input$reset, {
    values(sample_values)
    stopped(FALSE)
    updateNumericInput(session, "rate", value=72)
    message("Sample data restored. Entry is enabled.")
  })

  output$status <- renderText(message())
  output$total <- renderText(nrow(records()))
  output$flagged <- renderText(sum(records()$Flag == "Clinical review"))
  output$normal <- renderText(sum(records()$Flag == "Not flagged"))

  output$trend <- renderPlot({
    d <- records()
    if (nrow(d) == 0) { plot.new(); text(.5,.5,"No readings available yet"); return() }
    par(mar=c(4,4,2,1), bg="white")
    plot(d$Reading, d$HeartRate, type="n", xlab="Reading number", ylab="Heart rate (bpm)",
         ylim=range(c(d$HeartRate, 60, 100)) + c(-8, 8), xaxt="n", bty="n")
    axis(1, at=d$Reading)
    abline(h=c(60,100), col="#e8a04a", lty=2, lwd=2)
    grid(nx=NA, ny=NULL, col="#ecf0f6")
    lines(d$Reading, d$HeartRate, col="#4568e8", lwd=2)
    points(d$Reading, d$HeartRate, pch=19, cex=1.5,
           col=ifelse(d$Flag == "Clinical review", "#e14c61", "#159b78"))
    legend("topright", legend=c("Within range", "Flagged", "Reference boundaries"),
           col=c("#159b78", "#e14c61", "#e8a04a"), pch=c(19,19,NA),
           lty=c(NA,NA,2), bty="n", cex=.85)
  })

  output$history <- renderTable({
    d <- records()
    if (!nrow(d)) return(data.frame(Message="No readings available"))
    names(d) <- c("#", "BPM", "Classification", "Review status")
    d
  }, striped=TRUE, bordered=FALSE, spacing="s")

  output$alerts <- renderTable({
    d <- records()
    d <- d[d$Flag == "Clinical review", c("Reading", "HeartRate", "Classification"), drop=FALSE]
    if (!nrow(d)) return(data.frame(Message="No flagged readings"))
    names(d) <- c("#", "BPM", "Reason")
    d
  }, striped=TRUE, spacing="s")

  output$download <- downloadHandler(
    filename=function() "patient_heart_rate_readings.csv",
    content=function(file) write.csv(records(), file, row.names=FALSE)
  )
}

shinyApp(ui, server)
