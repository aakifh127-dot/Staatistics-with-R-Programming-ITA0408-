# Delivery Route Distance Calculator - R Shiny
# Run this file as app.R in RStudio.
if (!requireNamespace("shiny", quietly = TRUE)) install.packages("shiny")
library(shiny)

recursive_distance <- function(segments, index = 1L) {
  if (index > length(segments)) return(0)  # Base case
  segments[index] + recursive_distance(segments, index + 1L)
}

parse_distances <- function(text) {
  clean <- trimws(text)
  clean <- sub("^c\\s*\\(", "", clean)
  clean <- sub("\\)\\s*$", "", clean)
  if (!nzchar(trimws(clean))) stop("Enter at least one route segment.")
  pieces <- trimws(strsplit(clean, ",", fixed = TRUE)[[1]])
  if (any(!nzchar(pieces))) stop("Enter numbers separated by commas, without empty values.")
  values <- suppressWarnings(as.numeric(pieces))
  if (any(!is.finite(values))) stop("All segments must be valid, finite numbers.")
  if (any(values < 0)) stop("Negative distances are not allowed.")
  if (length(values) > 500) stop("For this recursion exercise, limit routes to 500 segments to avoid stack-depth errors.")
  values
}

ui <- fluidPage(
  tags$head(tags$meta(name = "viewport", content = "width=device-width, initial-scale=1"),
    tags$style(HTML('
      body {background:#f4f7fc;color:#162a4a;font-family:Segoe UI,Arial,sans-serif;}
      .container-fluid {max-width:1600px;padding:26px 30px;}
      .hero {background:linear-gradient(110deg,#2457ce,#1598c2);color:white;padding:27px 30px;border-radius:22px;margin-bottom:22px;}
      .hero h1 {font-size:30px;font-weight:750;margin:0 0 8px;}
      .hero p {font-size:15px;opacity:.93;margin:0;}
      .card {background:white;border:1px solid #e3eaf5;border-radius:18px;padding:22px;margin-bottom:20px;box-shadow:0 6px 22px rgba(28,58,110,.055);}
      .card h3 {font-weight:700;color:#18365d;margin-top:0;margin-bottom:16px;font-size:20px;}
      .kpi {min-height:145px;}
      .kpi .label {color:#64748b;font-size:14px;font-weight:600;}
      .kpi .value {font-size:30px;font-weight:800;margin-top:12px;color:#2457ce;}
      .kpi.green .value {color:#149c77;}
      .kpi.amber .value {color:#e08a19;}
      .help {color:#6b7c95;font-size:13px;line-height:1.6;}
      .form-control {border-radius:10px;border-color:#d8e2f0;}
      .btn {border-radius:10px;font-weight:650;padding:10px 17px;}
      .btn-primary {background:#3162df;border-color:#3162df;}
      .btn-default {background:#eef3fb;border-color:#dbe6f5;color:#254878;}
      .status-ok {background:#e7f8ef;color:#08794f;padding:12px 14px;border-radius:10px;font-weight:650;}
      .status-error {background:#fff0ef;color:#b3261e;padding:12px 14px;border-radius:10px;font-weight:650;}
      .table>thead>tr>th {background:#eff5ff;color:#264c7b;border-bottom:0;}
      .table>tbody>tr>td {border-color:#edf1f7;}
      @media (max-width:800px) {.container-fluid{padding:14px}.hero h1{font-size:23px}.kpi{min-height:110px}}
    '))
  ),
  div(class="hero", h1("Delivery Route Distance Calculator"),
      p("Recursive route analysis • Distance validation • Segment insights")),
  fluidRow(
    column(4, div(class="card", h3("Route Controls"),
      textInput("distances", "Segment distances (km)", value="12, 8, 15, 10, 5", placeholder="12, 8, 15, 10, 5"),
      p(class="help", "Enter comma-separated values or R-style c(12, 8, 15, 10, 5). Zero is allowed; negative values are rejected."),
      actionButton("calculate", "Calculate Route", class="btn-primary"),
      actionButton("sample", "Restore Sample"), br(),br(),
      uiOutput("feedback"), hr(),
      p(class="help", "The dashboard begins with example data. Click Calculate Route after editing distances."),
      downloadButton("download", "Export Segments CSV")
    )),
    column(8,
      fluidRow(
        column(4, div(class="card kpi", div(class="label", "Recursive total"), div(class="value",textOutput("recursive_kpi",inline=TRUE)))),
        column(4, div(class="card kpi green", div(class="label", "Built-in sum() total"), div(class="value",textOutput("sum_kpi",inline=TRUE)))),
        column(4, div(class="card kpi amber", div(class="label", "Route segments"), div(class="value",textOutput("count_kpi",inline=TRUE))))
      ),
      div(class="card",h3("Distance by Route Segment"),plotOutput("segment_plot",height="290px")),
      div(class="card",h3("Calculation Verification"),tableOutput("comparison"))
    )
  ),
  fluidRow(
    column(7,div(class="card",h3("Segment Breakdown"),tableOutput("segment_table"))),
    column(5,div(class="card",h3("How Recursion Works"),
      p("The function adds the current segment to the sum of all remaining segments."),
      tags$pre("distance(v, i) = v[i] + distance(v, i + 1)\ndistance(v, i) = 0 when i > length(v)"),
      h4("Base case"),p("When the index passes the final segment, the function returns 0. This stops further calls and prevents infinite recursion."),
      h4("Long-route limitation"),p("Each segment adds a function call. Very long routes can exceed R's recursion or expression stack limit. Use sum() for large routes."),
      p(class="help", "This demo caps input at 500 segments to reduce the risk of deep-recursion errors.")
    ))
  )
)

server <- function(input, output, session) {
  data <- reactiveVal(c(12, 8, 15, 10, 5))
  error_message <- reactiveVal("")

  observeEvent(input$calculate, {
    tryCatch({
      values <- parse_distances(input$distances)
      data(values)
      error_message("")
    }, error=function(e) error_message(conditionMessage(e)))
  })
  observeEvent(input$sample, {
    updateTextInput(session,"distances",value="12, 8, 15, 10, 5")
    data(c(12, 8, 15, 10, 5))
    error_message("")
  })

  output$feedback <- renderUI({
    if (nzchar(error_message())) div(class="status-error",paste("Error:",error_message()))
    else div(class="status-ok","Route ready — all distances are valid.")
  })

  results <- reactive({
    v <- data()
    recursive <- recursive_distance(v)
    builtin <- sum(v)
    list(v=v, recursive=recursive, builtin=builtin,
         match=isTRUE(all.equal(recursive,builtin,tolerance=1e-8)))
  })

  fmt <- function(x) format(round(x,2),trim=TRUE,scientific=FALSE,big.mark=",")
  output$recursive_kpi <- renderText(paste0(fmt(results()$recursive)," km"))
  output$sum_kpi <- renderText(paste0(fmt(results()$builtin)," km"))
  output$count_kpi <- renderText(length(results()$v))
  output$comparison <- renderTable({
    r <- results()
    data.frame(Method=c("Recursive function","R sum() function","Comparison"),
      Result=c(paste(fmt(r$recursive),"km"),paste(fmt(r$builtin),"km"),
               if (r$match) "MATCH" else "DIFFERENCE"),check.names=FALSE)
  },striped=TRUE,bordered=FALSE)
  segments <- reactive({
    v <- results()$v
    data.frame(Segment=paste0("Segment ",seq_along(v)),
               `Distance (km)`=v,`Cumulative (km)`=cumsum(v),check.names=FALSE)
  })
  output$segment_table <- renderTable(segments(),striped=TRUE,bordered=FALSE,digits=2)
  output$segment_plot <- renderPlot({
    v <- results()$v
    op <- par(mar=c(4.8,4.2,1,0.8))
    on.exit(par(op))
    bars <- barplot(v, names.arg=paste0("S",seq_along(v)), col="#4b78e6",border=NA,
                    ylab="Distance (km)",xlab="Route segments",las=1,
                    ylim=c(0,max(1,max(v))*1.18),cex.names=if(length(v)>30)0.6 else 0.9)
    if(length(v)<=30) text(bars,v,labels=fmt(v),pos=3,cex=.8,col="#35537c")
    abline(h=0,col="#dce4f0")
  })
  output$download <- downloadHandler(
    filename=function() "delivery_route_segments.csv",
    content=function(file) write.csv(segments(),file,row.names=FALSE)
  )
}
shinyApp(ui,server)
