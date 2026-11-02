
library(shiny)
library(bslib)
library(plotly)
library(DT)

ui <- fluidPage(
  theme = bs_theme(
    version = 5,
    bg = "#F5F8FC",
    fg = "#1E293B",
    primary = "#2563EB"
  ),
  
  tags$head(tags$style(HTML("
    body {
      background:#F5F8FC;
      font-family:'Segoe UI',Arial,sans-serif;
    }
    .header {
      background:linear-gradient(120deg,#164E83,#3B82F6);
      color:white;
      padding:30px;
      border-radius:18px;
      margin:20px 0;
      box-shadow:0 5px 18px #DCE5F2;
    }
    .panel-card {
      background:white;
      padding:24px;
      border-radius:17px;
      margin-bottom:20px;
      box-shadow:0 4px 16px #E1E8F2;
    }
    .metric {
      background:white;
      padding:22px;
      border-radius:16px;
      margin-bottom:18px;
      min-height:135px;
      border-left:5px solid #2563EB;
      box-shadow:0 4px 16px #E1E8F2;
    }
    .metric-label {
      font-size:13px;
      color:#64748B;
      font-weight:600;
    }
    .metric-value {
      font-size:29px;
      font-weight:750;
      margin-top:12px;
      overflow-wrap:anywhere;
    }
    .section-title {
      font-size:19px;
      font-weight:700;
      margin-bottom:16px;
    }
    .btn-primary {
      width:100%;
      border:0;
      background:#2563EB;
      border-radius:10px;
      padding:12px;
      font-weight:650;
    }
    .form-control {
      border-radius:10px;
    }
    .info-box {
      background:#EFF6FF;
      color:#2855A1;
      border-radius:11px;
      padding:16px;
      margin-top:15px;
    }
    .success-box {
      background:#ECFDF5;
      border-left:5px solid #16A34A;
      border-radius:12px;
      padding:18px;
      color:#166534;
    }
    .warning-box {
      background:#FFF7ED;
      border-left:5px solid #F59E0B;
      border-radius:12px;
      padding:18px;
      color:#9A560C;
    }
    .footer {
      text-align:center;
      color:#94A3B8;
      padding:20px;
      font-size:12px;
    }
  "))),
  
  div(
    class="header",
    h2("RoutePulse | Logistics Intelligence"),
    h4("Smart Delivery Route Distance Analytics"),
    p("Recursive Calculation | Iterative Comparison | Route Performance Monitoring")
  ),
  
  fluidRow(
    column(
      3,
      div(
        class="panel-card",
        h4("Route Configuration"),
        
        textAreaInput(
          "distances",
          "Route Segment Distances (km)",
          value="12,18,25,10,15,20",
          rows=5
        ),
        
        numericInput(
          "cost",
          "Estimated Cost per km (Rs.)",
          value=12,
          min=0,
          step=1
        ),
        
        numericInput(
          "speed",
          "Average Vehicle Speed (km/h)",
          value=40,
          min=1,
          step=5
        ),
        
        div(
          class="info-box",
          strong("Calculation Rules"),
          tags$ul(
            tags$li("All distances must be non-negative"),
            tags$li("Recursive function uses a base case"),
            tags$li("Iterative function uses a loop"),
            tags$li("Both results are compared")
          )
        ),
        
        br(),
        
        actionButton(
          "calculate",
          "Analyze Delivery Route",
          class="btn-primary"
        ),
        
        br(), br(),
        
        downloadButton(
          "download",
          "Download Route Report"
        )
      ),
      
      div(
        class="panel-card",
        h4("Algorithm Information"),
        p("Language: R Programming"),
        p("Framework: Shiny"),
        p("Algorithm 1: Recursion"),
        p("Algorithm 2: Iteration"),
        p("Time Complexity: O(n)"),
        p("Recursive Space: O(n)"),
        p("Iterative Space: O(1)")
      )
    ),
    
    column(
      9,
      fluidRow(
        column(
          3,
          div(
            class="metric",
            div(class="metric-label","TOTAL DISTANCE"),
            div(class="metric-value",textOutput("totalDistance"))
          )
        ),
        
        column(
          3,
          div(
            class="metric",
            style="border-left-color:#16A34A;",
            div(class="metric-label","ROUTE SEGMENTS"),
            div(class="metric-value",textOutput("segments"))
          )
        ),
        
        column(
          3,
          div(
            class="metric",
            style="border-left-color:#F59E0B;",
            div(class="metric-label","ESTIMATED COST"),
            div(class="metric-value",textOutput("costValue"))
          )
        ),
        
        column(
          3,
          div(
            class="metric",
            style="border-left-color:#8B5CF6;",
            div(class="metric-label","ESTIMATED HOURS"),
            div(class="metric-value",textOutput("travelTime"))
          )
        )
      ),
      
      div(
        class="panel-card",
        div(class="section-title","Route Calculation Status"),
        uiOutput("routeStatus")
      ),
      
      fluidRow(
        column(
          7,
          div(
            class="panel-card",
            div(class="section-title","Distance by Route Segment"),
            plotlyOutput("segmentPlot",height="350px")
          )
        ),
        
        column(
          5,
          div(
            class="panel-card",
            div(class="section-title","Segment Distance Distribution"),
            plotlyOutput("distancePie",height="350px")
          )
        )
      ),
      
      div(
        class="panel-card",
        div(class="section-title","Cumulative Route Progress"),
        plotlyOutput("cumulativePlot",height="330px")
      ),
      
      fluidRow(
        column(
          6,
          div(
            class="panel-card",
            div(class="section-title","Algorithm Comparison"),
            DTOutput("comparisonTable")
          )
        ),
        
        column(
          6,
          div(
            class="panel-card",
            div(class="section-title","Performance Insights"),
            uiOutput("insights")
          )
        )
      ),
      
      div(
        class="panel-card",
        div(class="section-title","Delivery Route Segment Report"),
        DTOutput("routeTable")
      ),
      
      div(
        class="panel-card",
        div(class="section-title","Final Logistics Analysis Report"),
        verbatimTextOutput("summary")
      )
    )
  ),
  
  div(
    class="footer",
    "RoutePulse Analytics | Smart Logistics Route Monitoring"
  )
)

server <- function(input,output,session) {
  
  recursive_distance <- function(x,n) {
    if(n==0) {
      return(0)
    }
    
    x[n]+recursive_distance(x,n-1)
  }
  
  iterative_distance <- function(x) {
    total <- 0
    
    for(i in seq_along(x)) {
      total <- total+x[i]
    }
    
    return(total)
  }
  
  results <- eventReactive(
    input$calculate,
    {
      raw <- trimws(unlist(strsplit(
        input$distances,",",fixed=TRUE
      )))
      
      distances <- suppressWarnings(as.numeric(raw))
      
      validate(
        need(
          length(distances)>0 &&
            all(!is.na(distances) & is.finite(distances)),
          "Enter valid numeric route distances."
        ),
        need(
          all(distances>=0),
          "Negative route distances are not allowed."
        ),
        need(
          length(distances)<=400,
          "For safe recursion, enter at most 400 segments."
        ),
        need(
          !is.na(input$cost) &&
            is.finite(input$cost) && input$cost>=0,
          "Cost per kilometer must be non-negative."
        ),
        need(
          !is.na(input$speed) &&
            is.finite(input$speed) && input$speed>0,
          "Vehicle speed must be greater than zero."
        )
      )
      
      n <- length(distances)
      
      recursive_time <- system.time({
        recursive_total <- recursive_distance(distances,n)
      })["elapsed"]
      
      iterative_time <- system.time({
        iterative_total <- iterative_distance(distances)
      })["elapsed"]
      
      cumulative <- cumsum(distances)
      
      report <- data.frame(
        Segment=paste("Segment",seq_len(n)),
        Distance_km=distances,
        Cumulative_km=cumulative
      )
      
      matched <- isTRUE(all.equal(
        recursive_total,iterative_total,
        tolerance=1e-8
      ))
      
      list(
        report=report,
        distances=distances,
        recursive=recursive_total,
        iterative=iterative_total,
        recursive_time=as.numeric(recursive_time),
        iterative_time=as.numeric(iterative_time),
        matched=matched,
        n=n,
        cost=recursive_total*input$cost,
        hours=recursive_total/input$speed
      )
    },
    ignoreNULL=FALSE
  )
  
  output$totalDistance <- renderText({
    paste0(round(results()$recursive,2)," km")
  })
  
  output$segments <- renderText({
    results()$n
  })
  
  output$costValue <- renderText({
    paste0(
      "Rs. ",
      format(
        round(results()$cost,2),
        big.mark=",",
        scientific=FALSE
      )
    )
  })
  
  output$travelTime <- renderText({
    paste0(round(results()$hours,2)," hrs")
  })
  
  output$routeStatus <- renderUI({
    x <- results()
    
    if(x$matched) {
      div(
        class="success-box",
        h4("Route Calculation Successful"),
        p(paste(
          "Recursive and iterative algorithms both calculated",
          round(x$recursive,2),
          "km. Their results match."
        ))
      )
    } else {
      div(
        class="warning-box",
        h4("Calculation Difference Detected"),
        p("The two algorithms produced different results.")
      )
    }
  })
  
  output$segmentPlot <- renderPlotly({
    x <- results()$report
    
    plot_ly(
      x=x$Segment,
      y=x$Distance_km,
      type="bar",
      marker=list(color="#3B82F6"),
      text=paste0(round(x$Distance_km,2)," km"),
      textposition="outside"
    ) %>%
      layout(
        xaxis=list(title="Route Segment"),
        yaxis=list(title="Distance (km)"),
        plot_bgcolor="white",
        paper_bgcolor="white",
        showlegend=FALSE,
        margin=list(t=35)
      )
  })
  
  output$distancePie <- renderPlotly({
    x <- results()$report
    
    validate(
      need(
        sum(x$Distance_km)>0,
        "Enter positive distances for distribution."
      )
    )
    
    plot_ly(
      labels=x$Segment,
      values=x$Distance_km,
      type="pie",
      hole=0.55,
      textinfo="percent",
      marker=list(
        colors=c(
          "#2563EB","#16A34A","#F59E0B",
          "#8B5CF6","#06B6D4","#EC4899"
        )
      )
    ) %>%
      layout(
        showlegend=TRUE,
        paper_bgcolor="white"
      )
  })
  
  output$cumulativePlot <- renderPlotly({
    x <- results()$report
    
    plot_ly(
      x=seq_len(nrow(x)),
      y=x$Cumulative_km,
      type="scatter",
      mode="lines+markers",
      line=list(color="#16A34A",width=4),
      marker=list(size=9),
      text=paste0(
        x$Segment,
        "<br>Total: ",
        x$Cumulative_km," km"
      ),
      hoverinfo="text"
    ) %>%
      layout(
        xaxis=list(title="Segment Number",dtick=1),
        yaxis=list(title="Cumulative Distance (km)"),
        plot_bgcolor="white",
        paper_bgcolor="white"
      )
  })
  
  output$comparisonTable <- renderDT({
    x <- results()
    
    comparison <- data.frame(
      Algorithm=c("Recursive","Iterative"),
      Distance_km=c(x$recursive,x$iterative),
      Execution_Time_ms=c(
        x$recursive_time*1000,
        x$iterative_time*1000
      ),
      Time_Complexity=c("O(n)","O(n)"),
      Extra_Space=c("O(n)","O(1)")
    )
    
    datatable(
      comparison,
      rownames=FALSE,
      options=list(
        dom="t",
        scrollX=TRUE
      )
    ) %>%
      formatRound(
        c("Distance_km","Execution_Time_ms"),
        digits=3
      )
  })
  
  output$routeTable <- renderDT({
    datatable(
      results()$report,
      rownames=FALSE,
      filter="top",
      options=list(
        pageLength=8,
        scrollX=TRUE
      )
    )
  })
  
  output$insights <- renderUI({
    x <- results()
    
    longest <- which.max(x$distances)
    shortest <- which.min(x$distances)
    
    tagList(
      h4("Route Performance"),
      p(strong("Longest Segment: "),
        paste0("Segment ",longest)),
      p(strong("Longest Distance: "),
        paste0(max(x$distances)," km")),
      p(strong("Shortest Segment: "),
        paste0("Segment ",shortest)),
      p(strong("Shortest Distance: "),
        paste0(min(x$distances)," km")),
      p(strong("Recursive Execution: "),
        paste0(round(x$recursive_time*1000,3)," ms")),
      p(strong("Iterative Execution: "),
        paste0(round(x$iterative_time*1000,3)," ms")),
      tags$hr(),
      p(style="color:#64748B;",
        "Both methods require O(n) time. Iteration generally uses less memory and is more suitable for very large routes. Single-run execution times may be too small for meaningful benchmarking.")
    )
  })
  
  output$summary <- renderText({
    x <- results()
    
    paste0(
      "LOGISTICS DELIVERY ROUTE ANALYSIS\n",
      "-------------------------------------------\n",
      "Route Segments: ",x$n,"\n",
      "Recursive Distance: ",round(x$recursive,2)," km\n",
      "Iterative Distance: ",round(x$iterative,2)," km\n",
      "Results Match: ",x$matched,"\n",
      "Estimated Travel Time: ",round(x$hours,2)," hrs\n",
      "Estimated Travel Cost: Rs. ",round(x$cost,2),"\n",
      "Recursive Time: ",round(x$recursive_time*1000,3)," ms\n",
      "Iterative Time: ",round(x$iterative_time*1000,3)," ms\n",
      "Analysis Status: Completed"
    )
  })
  
  output$download <- downloadHandler(
    filename=function() {
      paste0("RoutePulse_Report_",Sys.Date(),".csv")
    },
    content=function(file) {
      write.csv(
        results()$report,
        file,
        row.names=FALSE
      )
    }
  )
}

shinyApp(ui,server)
