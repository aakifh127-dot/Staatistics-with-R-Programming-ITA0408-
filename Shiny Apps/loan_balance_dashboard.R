# Banking System - Recursive Loan Balance Calculator
# Save as app.R and run in RStudio.
if (!requireNamespace('shiny', quietly = TRUE)) install.packages('shiny')
if (!requireNamespace('ggplot2', quietly = TRUE)) install.packages('ggplot2')
library(shiny)
library(ggplot2)

money <- function(x) paste0('₹', formatC(x, format = 'f', digits = 2, big.mark = ','))

# The base case ends recursion at the requested month or when fully repaid.
recursive_schedule <- function(balance, rate, payment, months, month = 0L) {
  current <- data.frame(Month = month, Balance = round(balance, 2))
  if (month >= months || balance <= 0) return(current)
  new_balance <- max(0, balance * (1 + rate) - payment)
  rbind(current, recursive_schedule(new_balance, rate, payment, months, month + 1L))
}

iterative_schedule <- function(balance, rate, payment, months) {
  out <- data.frame(Month = 0L, Balance = round(balance, 2))
  if (months == 0 || balance == 0) return(out)
  for (m in seq_len(months)) {
    balance <- max(0, balance * (1 + rate) - payment)
    out <- rbind(out, data.frame(Month = m, Balance = round(balance, 2)))
    if (balance <= 0) break
  }
  out
}

example <- list(loan = 10000, interest = 1, payment = 1000, months = 3)

ui <- fluidPage(
  tags$head(tags$style(HTML('
  body{background:#f4f7fd;color:#172b4d;font-family:Segoe UI,Arial,sans-serif}
  .container-fluid{max-width:1540px;padding:22px 27px}
  .hero{background:linear-gradient(110deg,#e8f0ff,#f7fbff);border:1px solid #dae7ff;padding:25px 30px;border-radius:22px;margin-bottom:19px}
  .hero h2{font-weight:800;margin:0;color:#15356b}.hero p{color:#627490;margin:8px 0 0}
  .panelx{background:white;border:1px solid #e3eaf3;border-radius:19px;padding:24px;margin-bottom:18px;box-shadow:0 8px 28px rgba(27,55,100,.045)}
  .panelx h3{font-size:19px;font-weight:750;margin-top:0;color:#173969}
  .metric{background:white;border:1px solid #e2eaf5;border-radius:17px;padding:21px;margin-bottom:16px;min-height:125px;box-shadow:0 5px 20px rgba(30,64,100,.04)}
  .metric .labelx{color:#71809a;font-size:13px;font-weight:600}.metric .num{font-size:26px;font-weight:800;color:#183865;margin-top:11px;overflow-wrap:anywhere}
  .btn-primary{background:#4666e7;border-color:#4666e7;border-radius:9px;font-weight:700;padding:10px 18px}
  .btn-default{border-radius:9px;padding:10px 18px}.form-control{border-radius:9px;border-color:#d8e1ef;height:42px}
  .control-label{color:#365070;font-weight:650}.note{font-size:13px;color:#71809a}
  .status{background:#eaf7f0;border:1px solid #c8edd8;border-radius:10px;padding:14px 17px;color:#226746;font-weight:650}
  .warn{background:#fff3e7;border:1px solid #ffd8a9;border-radius:10px;padding:14px 17px;color:#9a5208;font-weight:650}
  .nav-tabs{border:0;margin-bottom:18px}.nav-tabs>li>a{color:#65758f;border:0;border-radius:10px;font-weight:700}
  .nav-tabs>li.active>a,.nav-tabs>li.active>a:focus{color:#385ddd;background:#eaf0ff;border:0}
  .table>thead>tr>th{background:#eff4fd;color:#325178;border:0}.table>tbody>tr>td{border-color:#edf1f7}
  @media(max-width:768px){.container-fluid{padding:12px}.hero{padding:20px}.metric{min-height:95px}}
  '))),
  div(class='hero', h2('LoanWise | Banking Loan Monitor'),
      p('Recursive loan calculations • Repayment tracking • Interest analysis')),
  fluidRow(
    column(3, div(class='panelx', h3('Loan Parameters'),
      numericInput('loan','Initial Loan Amount (₹)',value=10000,min=0,step=500),
      numericInput('interest','Monthly Interest Rate (%)',value=1,min=0,max=100,step=0.1),
      numericInput('payment','Fixed Monthly Payment (₹)',value=1000,min=0,step=100),
      numericInput('months','Number of Months',value=3,min=0,max=120,step=1),
      actionButton('calculate','Calculate Loan',class='btn-primary',width='100%'),br(),br(),
      actionButton('reset','Reset to Sample',width='100%'),
      hr(),p(class='note','Example: ₹10,000 loan • 1% monthly interest • ₹1,000 monthly payment • 3 months.'),
      downloadButton('export','Download CSV',class='btn-default')
    )),
    column(9,
      fluidRow(
        column(4,div(class='metric',div(class='labelx','Initial Loan'),div(class='num',textOutput('initial',inline=TRUE)))),
        column(4,div(class='metric',div(class='labelx','Remaining Balance'),div(class='num',textOutput('remaining',inline=TRUE)))),
        column(4,div(class='metric',div(class='labelx','Months Processed'),div(class='num',textOutput('months_done',inline=TRUE))))
      ),
      uiOutput('status'),br(),
      div(class='panelx',tabsetPanel(
        tabPanel('Overview',br(),h3('Loan Balance Over Time'),plotOutput('loanplot',height='330px'),
          fluidRow(column(6,h4('Recursive Result'),textOutput('recursive_result')),
                   column(6,h4('Iterative Result'),textOutput('iterative_result')))),
        tabPanel('Month-by-Month',br(),h3('Repayment Schedule'),tableOutput('schedule')),
        tabPanel('Comparison & Analysis',br(),h3('Recursive vs Iterative'),tableOutput('comparison'),
          h4('How recursion terminates'),p('The base case returns immediately when the number of months has been reached or the remaining balance is zero. Every recursive call advances the month by one, preventing an infinite series of calls.'),
          h4('Recursion vs iteration'),p('Both methods implement the same formula. Recursion uses one function call per month, while iteration uses a for loop. Iteration is preferable for long schedules because recursion consumes call-stack space. This example limits the duration to 120 months.'))
      ))
    )
  )
)

server <- function(input,output,session) {
  params <- reactiveVal(example)
  observeEvent(input$calculate,{
    vals <- c(input$loan,input$interest,input$payment,input$months)
    if(length(vals)!=4 || any(!is.finite(vals)) || any(vals < 0) || input$interest > 100 || input$months > 120 || input$months != floor(input$months)) {
      showNotification('Enter valid nonnegative values. Interest: 0–100%; duration: whole months, 0–120.',type='error',duration=7)
      return()
    }
    params(list(loan=input$loan,interest=input$interest,payment=input$payment,months=as.integer(input$months)))
  })
  observeEvent(input$reset,{
    updateNumericInput(session,'loan',value=example$loan)
    updateNumericInput(session,'interest',value=example$interest)
    updateNumericInput(session,'payment',value=example$payment)
    updateNumericInput(session,'months',value=example$months)
    params(example)
  })
  records <- reactive({
    p <- params()
    rec <- recursive_schedule(p$loan,p$interest/100,p$payment,p$months)
    iter <- iterative_schedule(p$loan,p$interest/100,p$payment,p$months)
    list(rec=rec,iter=iter)
  })
  output$initial <- renderText(money(params()$loan))
  output$remaining <- renderText(money(tail(records()$rec$Balance,1)))
  output$months_done <- renderText(max(records()$rec$Month))
  output$status <- renderUI({
    p <- params(); last <- tail(records()$rec$Balance,1)
    initial_interest <- p$loan * p$interest/100
    if(p$loan > 0 && p$payment <= initial_interest && p$months > 0 && last > 0) {
      div(class='warn',strong('Warning: Payment does not exceed first-month interest. '),
        paste0('Interest starts at ',money(initial_interest),'/month. The balance will not decrease under the stated fixed rate/payment assumptions.'))
    } else if(last == 0) {
      div(class='status','Loan fully repaid within the selected period.')
    } else {
      div(class='status','Calculation complete. The loan still has a remaining balance at the selected month.')
    }
  })
  output$recursive_result <- renderText(money(tail(records()$rec$Balance,1)))
  output$iterative_result <- renderText(money(tail(records()$iter$Balance,1)))
  output$loanplot <- renderPlot({
    df <- records()$rec
    ggplot(df,aes(x=Month,y=Balance))+
      geom_area(fill='#c7d8ff',alpha=.65)+
      geom_line(color='#4768e8',linewidth=1.25)+
      geom_point(color='#244dbd',size=2.5)+
      scale_y_continuous(labels=function(x) paste0('₹',format(x,big.mark=',',scientific=FALSE,trim=TRUE)))+
      scale_x_continuous(breaks=pretty(df$Month,n=8))+
      labs(x='Month',y='Remaining balance',subtitle='Balance after each monthly payment')+
      theme_minimal(base_size=12)+theme(panel.grid.minor=element_blank(),plot.subtitle=element_text(color='#74829a'))
  })
  output$schedule <- renderTable({
    df <- records()$rec
    names(df) <- c('Month','Remaining Balance')
    df[['Remaining Balance']] <- vapply(df[['Remaining Balance']],money,character(1))
    df
  },striped=TRUE,hover=TRUE,spacing='m')
  output$comparison <- renderTable({
    rec <- records()$rec; iter <- records()$iter
    data.frame(Method=c('Recursive function','Iterative for loop'),
      `Remaining Balance`=c(money(tail(rec$Balance,1)),money(tail(iter$Balance,1))),
      `Months Calculated`=c(max(rec$Month),max(iter$Month)),check.names=FALSE)
  },striped=TRUE)
  output$export <- downloadHandler(
    filename=function(){'loan_repayment_schedule.csv'},
    content=function(file){write.csv(records()$rec,file,row.names=FALSE)}
  )
}
shinyApp(ui,server)
