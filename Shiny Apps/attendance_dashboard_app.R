if (!requireNamespace("shiny", quietly = TRUE)) install.packages("shiny")
if (!requireNamespace("ggplot2", quietly = TRUE)) install.packages("ggplot2")
library(shiny)
library(ggplot2)

sample_students <- data.frame(
  Student = c("Arun", "Priya", "Kavin"),
  Attended = c(85, 72, 40),
  Total = c(100, 100, 50),
  stringsAsFactors = FALSE
)

process_attendance <- function(records) {
  result <- data.frame(Student=character(), Attended=numeric(), Total=numeric(),
                       Percentage=numeric(), Status=character(), stringsAsFactors=FALSE)
  skipped <- character()
  for (i in seq_len(nrow(records))) {
    name <- trimws(as.character(records$Student[i]))
    attended <- suppressWarnings(as.numeric(records$Attended[i]))
    total <- suppressWarnings(as.numeric(records$Total[i]))
    if (is.na(name) || !nzchar(name) || is.na(attended) || is.na(total) ||
        !is.finite(attended) || !is.finite(total) || attended < 0 ||
        total <= 0 || attended > total || attended %% 1 != 0 || total %% 1 != 0) {
      skipped <- c(skipped, paste("Row", i))
      next
    }
    percentage <- attended / total * 100
    if (percentage >= 80) {
      status <- "Eligible"
    } else {
      status <- "Not Eligible"
    }
    result <- rbind(result, data.frame(Student=name, Attended=attended,
      Total=total, Percentage=round(percentage, 2), Status=status))
  }
  list(valid=result, skipped=skipped)
}

ui <- fluidPage(
  tags$head(tags$style(HTML('
    body {background:#f4f7fd; color:#172b4d; font-family: "Segoe UI", Arial, sans-serif;}
    .wrap {max-width:1400px; margin:0 auto; padding:18px 6px;}
    .top {background:linear-gradient(110deg,#255bd9,#5b80ed);color:white;
      padding:24px 30px;border-radius:20px;margin-bottom:22px;}
    .top h2 {color:white;font-weight:750;margin:0 0 8px 0;}
    .top p {color:#eaf0ff;margin:0;}
    .panel-card {background:white;border:1px solid #e4eaf3;border-radius:18px;
      padding:22px;box-shadow:0 6px 22px rgba(24,52,110,.055);margin-bottom:19px;}
    .panel-card h3 {font-size:19px;font-weight:700;margin:0 0 18px 0;color:#1a315b;}
    .metric {background:white;padding:20px;border:1px solid #e4eaf3;border-radius:18px;
      box-shadow:0 6px 22px rgba(24,52,110,.055);margin-bottom:20px;}
    .metric-label {font-size:14px;color:#63748e;font-weight:600;}
    .metric-value {font-size:30px;font-weight:800;color:#204ac5;min-height:43px;}
    .btn-primary {background:#4166e9;border-color:#4166e9;border-radius:9px;font-weight:650;}
    .btn-default {border-radius:9px;}
    .form-control {border-radius:9px;}
    .table {background:white;} .table>thead>tr>th {background:#edf3ff;color:#26447b;}
    .table>tbody>tr>td {vertical-align:middle;}
    .hint {color:#66758b;font-size:13px;}
    .nav-pills>li.active>a,.nav-pills>li.active>a:hover {background:#4268ec;border-radius:10px;}
    .nav-pills>li>a {font-weight:600;color:#3b5275;}
  '))),
  div(class="wrap",
    div(class="top", h2("Student Attendance & Exam Eligibility"),
      p("Interactive attendance monitoring | Minimum required attendance: 80%")),
    fluidRow(
      column(3, div(class="metric", div(class="metric-label", "Total valid students"),
                    div(class="metric-value", textOutput("total_students", inline=TRUE)))),
      column(3, div(class="metric", div(class="metric-label", "Eligible students"),
                    div(class="metric-value", textOutput("eligible_count", inline=TRUE)))),
      column(3, div(class="metric", div(class="metric-label", "Below 80%"),
                    div(class="metric-value", textOutput("not_eligible_count", inline=TRUE)))),
      column(3, div(class="metric", div(class="metric-label", "Average attendance"),
                    div(class="metric-value", textOutput("avg_attendance", inline=TRUE))))
    ),
    fluidRow(
      column(4, div(class="panel-card", h3("Add Student"),
        textInput("name", "Student name", placeholder="Enter student name"),
        numericInput("attended", "Classes attended", value=85, min=0, step=1),
        numericInput("total_classes", "Total classes conducted", value=100, min=1, step=1),
        actionButton("add", "Add student", class="btn-primary"),
        actionButton("reset", "Restore sample data"),
        br(),br(), textOutput("notice"),
        p(class="hint", "Invalid records are skipped during processing using next.")
      )),
      column(8, div(class="panel-card", h3("Attendance Comparison"),
        plotOutput("attendance_chart", height="330px")))
    ),
    tabsetPanel(type="pills",
      tabPanel("All Students", br(), div(class="panel-card", h3("Student Attendance Records"),
        tableOutput("all_table"))),
      tabPanel("Below 80%", br(), div(class="panel-card", h3("Students Not Eligible for Exams"),
        tableOutput("below_table"), textOutput("below_message"))),
      tabPanel("Insights", br(), div(class="panel-card", h3("Eligibility by Student"),
        plotOutput("status_chart", height="300px"),
        p(class="hint", "Students need at least 80% attendance to be eligible.")))
    )
  )
)

server <- function(input, output, session) {
  records <- reactiveVal(sample_students)
  notice <- reactiveVal("Sample data loaded: Arun, Priya and Kavin.")
  processed <- reactive(process_attendance(records()))
  valid <- reactive(processed()$valid)

  observeEvent(input$add, {
    new_record <- data.frame(Student=as.character(input$name),
      Attended=as.numeric(input$attended), Total=as.numeric(input$total_classes),
      stringsAsFactors=FALSE)
    check <- process_attendance(new_record)
    if (nrow(check$valid) == 0) {
      notice("Invalid input skipped. Enter a name and valid whole-number attendance values (0 <= attended <= total, total > 0).")
      return()
    }
    records(rbind(records(), new_record))
    notice(paste("Added", trimws(input$name), "successfully."))
    updateTextInput(session, "name", value="")
  })

  observeEvent(input$reset, {
    records(sample_students)
    notice("Sample data restored.")
  })

  output$notice <- renderText(notice())
  output$total_students <- renderText(nrow(valid()))
  output$eligible_count <- renderText(sum(valid()$Status == "Eligible"))
  output$not_eligible_count <- renderText(sum(valid()$Status == "Not Eligible"))
  output$avg_attendance <- renderText(if (nrow(valid()) == 0) "0%" else
    paste0(round(mean(valid()$Percentage),1), "%"))

  output$all_table <- renderTable({
    x <- valid()
    names(x) <- c("Student", "Attended", "Total", "Attendance (%)", "Exam Status")
    x
  }, striped=TRUE, bordered=FALSE, spacing="m")

  output$below_table <- renderTable({
    x <- valid()[valid()$Status == "Not Eligible", , drop=FALSE]
    names(x) <- c("Student", "Attended", "Total", "Attendance (%)", "Exam Status")
    x
  }, striped=TRUE, spacing="m")
  output$below_message <- renderText({
    if (!any(valid()$Status == "Not Eligible")) "All students meet the 80% requirement." else "These students are below the minimum 80% attendance requirement."
  })

  output$attendance_chart <- renderPlot({
    x <- valid()
    if (nrow(x)==0) {plot.new();text(.5,.5,"No valid students");return()}
    ggplot(x, aes(x=reorder(Student, Percentage), y=Percentage, fill=Status)) +
      geom_col(width=.6) +
      geom_hline(yintercept=80, color="#e74c58", linetype="dashed", linewidth=1) +
      geom_text(aes(label=paste0(Percentage,"%")), hjust=-.15, size=4) +
      coord_flip() +
      scale_fill_manual(values=c("Eligible"="#3b77ed", "Not Eligible"="#f07378")) +
      scale_y_continuous(limits=c(0,110), breaks=seq(0,100,20)) +
      labs(x=NULL,y="Attendance (%)",fill=NULL) +
      theme_minimal(base_size=13) +
      theme(legend.position="bottom",panel.grid.major.y=element_blank())
  })

  output$status_chart <- renderPlot({
    x <- valid()
    if (nrow(x)==0) {plot.new();text(.5,.5,"No data");return()}
    counts <- as.data.frame(table(factor(x$Status,
      levels=c("Eligible", "Not Eligible"))))
    names(counts) <- c("Status","Count")
    ggplot(counts, aes(Status,Count,fill=Status)) +
      geom_col(width=.5) + geom_text(aes(label=Count),vjust=-.4,size=6) +
      scale_fill_manual(values=c("Eligible"="#3b77ed", "Not Eligible"="#f07378")) +
      scale_y_continuous(limits=c(0,max(counts$Count)+1)) +
      labs(x=NULL,y="Number of students") +
      theme_minimal(base_size=13) + theme(legend.position="none")
  })
}

shinyApp(ui=ui, server=server)
