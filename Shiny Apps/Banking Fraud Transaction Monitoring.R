
library(shiny)
library(bslib)
library(plotly)
library(DT)

ui <- fluidPage(
  theme = bs_theme(
    version = 5,
    bg = "#F5F7FC",
    fg = "#172B4D",
    primary = "#2563EB"
  ),
  
  tags$head(tags$style(HTML("
    body {
      background: #F5F7FC;
      font-family: 'Segoe UI', Arial, sans-serif;
    }
    .header {
      background: linear-gradient(120deg,#173B80,#3478E8);
      color: white;
      padding: 28px;
      border-radius: 18px;
      margin: 20px 0;
      box-shadow: 0 6px 20px #D5DFEE;
    }
    .panel-card {
      background: white;
      padding: 22px;
      border-radius: 16px;
      margin-bottom: 20px;
      box-shadow: 0 4px 16px #E2E8F2;
    }
    .metric {
      background: white;
      padding: 22px;
      border-radius: 15px;
      margin-bottom: 16px;
      border-left: 5px solid #2563EB;
      box-shadow: 0 4px 16px #E2E8F2;
      min-height: 135px;
    }
    .metric-label {
      color: #64748B;
      font-size: 14px;
      font-weight: 600;
    }
    .metric-value {
      font-size: 27px;
      font-weight: bold;
      margin-top: 12px;
      overflow-wrap: anywhere;
    }
    .btn-primary {
      background: #2563EB;
      border: none;
      border-radius: 10px;
      padding: 11px 20px;
      width: 100%;
      font-weight: 600;
    }
    .alert-card {
      background: #FFF1F0;
      border-left: 5px solid #EF4444;
      border-radius: 10px;
      padding: 16px;
      color: #A52828;
    }
    .success-card {
      background: #ECFDF5;
      border-left: 5px solid #16A34A;
      border-radius: 10px;
      padding: 16px;
      color: #166534;
    }
    .info-card {
      background: #EFF6FF;
      padding: 15px;
      border-radius: 10px;
      color: #2855A1;
    }
    .form-control {
      border-radius: 10px;
    }
    .download-btn {
      width: 100%;
      border-radius: 10px;
      padding: 10px;
      background: #E8F0FF;
      color: #2563EB;
    }
    .section-title {
      font-size: 19px;
      font-weight: 700;
      margin-bottom: 16px;
    }
  "))),
  
  div(
    class = "header",
    h2("BankShield - Fraud Intelligence"),
    h4("Banking Transaction Monitoring Dashboard"),
    p("Smart Transaction Analysis | Suspicious Activity Detection | Financial Insights")
  ),
  
  fluidRow(
    column(
      3,
      div(
        class = "panel-card",
        h4("Transaction Control"),
        
        textAreaInput(
          "transactions",
          "Enter Transaction Amounts",
          value = "20000,65000,45000,-200,90000,12000,75000,30000,-1",
          rows = 6
        ),
        
        numericInput(
          "threshold",
          "Fraud Review Threshold (Rs.)",
          value = 50000,
          min = 0,
          step = 1000
        ),
        
        div(
          class = "info-card",
          strong("Monitoring Rules"),
          tags$ul(
            tags$li("Above Rs. 50,000: Flagged by default"),
            tags$li("Negative values: Skipped"),
            tags$li("-1: End monitoring"),
            tags$li("Valid values: Added to total")
          )
        ),
        
        br(),
        
        actionButton(
          "analyze",
          "Analyze Transactions",
          class = "btn-primary"
        ),
        
        br(), br(),
        
        downloadButton(
          "download",
          "Download Transaction Report",
          class = "download-btn"
        )
      ),
      
      div(
        class = "panel-card",
        h4("System Information"),
        p("System: BankShield Analytics"),
        p("Detection: Rule-Based"),
        p("Programming Language: R"),
        p("Framework: Shiny"),
        p("Processing: while() Loop"),
        p("Control: next and break")
      )
    ),
    
    column(
      9,
      fluidRow(
        column(
          3,
          div(
            class = "metric",
            div(class = "metric-label", "PROCESSED TRANSACTIONS"),
            div(class = "metric-value", textOutput("processed"))
          )
        ),
        
        column(
          3,
          div(
            class = "metric",
            style = "border-left-color:#F59E0B;",
            div(class = "metric-label", "FLAGGED TRANSACTIONS"),
            div(class = "metric-value", textOutput("flagged"))
          )
        ),
        
        column(
          3,
          div(
            class = "metric",
            style = "border-left-color:#16A34A;",
            div(class = "metric-label", "TOTAL TRANSACTION VALUE"),
            div(class = "metric-value", textOutput("total"))
          )
        ),
        
        column(
          3,
          div(
            class = "metric",
            style = "border-left-color:#EF4444;",
            div(class = "metric-label", "INVALID TRANSACTIONS"),
            div(class = "metric-value", textOutput("invalid"))
          )
        )
      ),
      
      div(
        class = "panel-card",
        div(class = "section-title", "Security Monitoring Status"),
        uiOutput("riskAlert")
      ),
      
      fluidRow(
        column(
          7,
          div(
            class = "panel-card",
            div(class = "section-title", "Transaction Amount Analysis"),
            plotlyOutput("transactionPlot", height = "330px")
          )
        ),
        
        column(
          5,
          div(
            class = "panel-card",
            div(class = "section-title", "Transaction Risk Distribution"),
            plotlyOutput("riskPie", height = "330px")
          )
        )
      ),
      
      div(
        class = "panel-card",
        div(class = "section-title", "Transaction Monitoring Report"),
        DTOutput("transactionTable")
      ),
      
      fluidRow(
        column(
          6,
          div(
            class = "panel-card",
            div(class = "section-title", "Flagged Transaction Details"),
            DTOutput("flaggedTable")
          )
        ),
        
        column(
          6,
          div(
            class = "panel-card",
            div(class = "section-title", "Financial Security Insights"),
            uiOutput("insights")
          )
        )
      ),
      
      div(
        class = "panel-card",
        div(class = "section-title", "Final Monitoring Summary"),
        verbatimTextOutput("summary")
      )
    )
  )
)

server <- function(input, output, session) {
  
  analyze_transactions <- function(values, threshold) {
    
    i <- 1
    total <- 0
    flagged <- 0
    invalid <- 0
    processed <- 0
    
    report <- data.frame(
      ID = integer(),
      Amount = numeric(),
      Status = character(),
      stringsAsFactors = FALSE
    )
    
    while (i <= length(values)) {
      
      amount <- values[i]
      i <- i + 1
      
      if (!is.na(amount) && amount == -1) {
        break
      }
      
      if (is.na(amount) || !is.finite(amount) || amount < 0) {
        invalid <- invalid + 1
        next
      }
      
      processed <- processed + 1
      total <- total + amount
      
      if (amount > threshold) {
        status <- "Flagged for Review"
        flagged <- flagged + 1
      } else {
        status <- "Normal"
      }
      
      report <- rbind(
        report,
        data.frame(
          ID = processed,
          Amount = amount,
          Status = status
        )
      )
    }
    
    list(
      report = report,
      processed = processed,
      total = total,
      flagged = flagged,
      invalid = invalid,
      threshold = threshold
    )
  }
  
  results <- eventReactive(
    input$analyze,
    {
      values <- suppressWarnings(
        as.numeric(
          trimws(
            unlist(strsplit(input$transactions, ","))
          )
        )
      )
      
      analyze_transactions(values, input$threshold)
    },
    ignoreNULL = FALSE
  )
  
  output$processed <- renderText({
    results()$processed
  })
  
  output$flagged <- renderText({
    results()$flagged
  })
  
  output$invalid <- renderText({
    results()$invalid
  })
  
  output$total <- renderText({
    paste0(
      "Rs. ",
      format(
        results()$total,
        big.mark = ",",
        scientific = FALSE,
        trim = TRUE
      )
    )
  })
  
  output$riskAlert <- renderUI({
    
    x <- results()
    
    if (x$flagged > 0) {
      
      div(
        class = "alert-card",
        h4("High-Value Transaction Alert"),
        p(
          paste(
            x$flagged,
            "transaction(s) exceeded the monitoring threshold of Rs.",
            format(x$threshold, big.mark = ","),
            "and require further investigation."
          )
        )
      )
      
    } else {
      
      div(
        class = "success-card",
        h4("No High-Value Transactions Flagged"),
        p("All processed transactions are within the configured threshold.")
      )
    }
  })
  
  output$transactionPlot <- renderPlotly({
    
    x <- results()$report
    
    validate(
      need(nrow(x) > 0, "No valid transactions available.")
    )
    
    colors <- ifelse(
      x$Status == "Normal",
      "#3B82F6",
      "#F59E0B"
    )
    
    plot_ly(
      x = x$ID,
      y = x$Amount,
      type = "bar",
      marker = list(color = colors),
      text = paste(
        "Transaction:", x$ID,
        "<br>Amount: Rs.", x$Amount,
        "<br>Status:", x$Status
      ),
      hoverinfo = "text"
    ) %>%
      layout(
        xaxis = list(title = "Transaction Number", dtick = 1),
        yaxis = list(title = "Transaction Amount (Rs.)"),
        plot_bgcolor = "white",
        paper_bgcolor = "white",
        showlegend = FALSE,
        shapes = list(
          list(
            type = "line",
            x0 = 0,
            x1 = 1,
            xref = "paper",
            y0 = results()$threshold,
            y1 = results()$threshold,
            line = list(
              color = "#EF4444",
              dash = "dash",
              width = 2
            )
          )
        )
      )
  })
  
  output$riskPie <- renderPlotly({
    
    x <- results()
    
    counts <- c(
      Normal = x$processed - x$flagged,
      Flagged = x$flagged
    )
    
    validate(
      need(sum(counts) > 0, "No valid transactions available.")
    )
    
    plot_ly(
      labels = names(counts),
      values = as.numeric(counts),
      type = "pie",
      hole = 0.6,
      marker = list(
        colors = c("#22C55E", "#F59E0B")
      ),
      textinfo = "label+percent"
    ) %>%
      layout(
        showlegend = TRUE,
        paper_bgcolor = "white"
      )
  })
  
  output$transactionTable <- renderDT({
    
    x <- results()$report
    
    datatable(
      x,
      rownames = FALSE,
      filter = "top",
      options = list(
        pageLength = 7,
        scrollX = TRUE
      )
    ) %>%
      formatCurrency(
        "Amount",
        currency = "₹",
        digits = 2
      ) %>%
      formatStyle(
        "Status",
        color = styleEqual(
          c("Normal", "Flagged for Review"),
          c("#15803D", "#B45309")
        ),
        fontWeight = "bold"
      )
  })
  
  output$flaggedTable <- renderDT({
    
    x <- results()$report
    x <- x[x$Status == "Flagged for Review", , drop = FALSE]
    
    datatable(
      x,
      rownames = FALSE,
      options = list(
        pageLength = 5,
        dom = "tip",
        scrollX = TRUE
      )
    )
  })
  
  output$insights <- renderUI({
    
    x <- results()
    
    rate <- if (x$processed > 0) {
      round(x$flagged / x$processed * 100, 2)
    } else {
      0
    }
    
    average <- if (x$processed > 0) {
      round(x$total / x$processed, 2)
    } else {
      0
    }
    
    highest <- if (x$processed > 0) {
      max(x$report$Amount)
    } else {
      0
    }
    
    tagList(
      h4("Transaction Statistics"),
      p(strong("Flagged Transaction Rate: "), paste0(rate, "%")),
      p(strong("Average Transaction: "), paste0("Rs. ", average)),
      p(strong("Highest Transaction: "), paste0("Rs. ", highest)),
      p(strong("Normal Transactions: "), x$processed - x$flagged),
      p(strong("Invalid Transactions: "), x$invalid),
      tags$hr(),
      p(
        style = "color:#64748B;",
        "Flagged transactions should be reviewed using additional banking security checks."
      )
    )
  })
  
  output$summary <- renderText({
    
    x <- results()
    
    paste0(
      "BANK TRANSACTION MONITORING REPORT\n",
      "-------------------------------------------\n",
      "Valid Transactions: ", x$processed, "\n",
      "Flagged Transactions: ", x$flagged, "\n",
      "Normal Transactions: ", x$processed - x$flagged, "\n",
      "Invalid Transactions: ", x$invalid, "\n",
      "Total Transaction Value: Rs. ", x$total, "\n",
      "Review Threshold: Rs. ", x$threshold, "\n",
      "Monitoring Status: Completed\n"
    )
  })
  
  output$download <- downloadHandler(
    
    filename = function() {
      paste0("BankShield_Report_", Sys.Date(), ".csv")
    },
    
    content = function(file) {
      write.csv(
        results()$report,
        file,
        row.names = FALSE
      )
    }
  )
}

shinyApp(ui, server)
