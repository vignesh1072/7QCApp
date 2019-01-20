library(shiny)
library(shinydashboard)
library(plotly)
library(qcc)

MyRaneData <- read.csv(file="E:/R-demo/01 Nov 2018.csv", encoding = "UTF-8", header=TRUE, sep=",")

myRaneDf <- data.frame(MyRaneData)


ui <- dashboardPage(
  dashboardHeader(title = "Rane"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
      menuItem("Widgets", icon = icon("th"), tabName = "widgets",
               badgeLabel = "new", badgeColor = "green"),
      tags$style(HTML("
                      .btn {
                      color: #b65e1a;
                      border: 2px #b65e1a solid;
                      }
                      .title{
                      background-color:#375a7f;
                      }
                      .btn:hover {
                      color: #fff;
                      background-color: #b65e1a;
                      }
                      .btn-default.active, .btn-default:active, .open > .dropdown-toggle.btn-default {
                      color: #fff;
                      background-color: #b65e1a;
                      border-color: #b65e1a;
                      }
                      #sidebar {
                      background-color: #506e9e;
                      }
                      #dd1 {
                      color: #ffffff;
                      
                      }	
                      .dd2 {
                      color: #ffffff;
                      font-size:14px;
                      
                      }
                      .pad{
                      padding-bottom:10px;
                      }
                      #totalIndustry{
                      color:#ffffff;
                      font-size:14px;
                      font-weight:700;
                      background-color:#333333;
                      }
                      #totalRetail{
                      background-color:#070e96;
                      font-size:14px;
                      color:#ffffff;
                      font-weight:700;
                      }
                      #msOfYear{
                      background-color:#333333;
                      color:#ffffff;
                      font-size:14px;
                      font-weight:700;
                      }
                      #tmuIndustry{
                      background-color:#070e96;
                      color:#ffffff;
                      font-size:14px;
                      font-weight:700;
                      }
                      #tmuRetail{
                      background-color:#333333;
                      color:#ffffff;
                      font-size:14px;
                      font-weight:700;
                      }
                      #tmumsOfYear{
                      background-color:#070e96;
                      color:#ffffff;
                      font-size:14px;
                      font-weight:700;
                      }
                      #dis{
                      background-color:#f5c6cb;
                      color:#32383e;
                      }
                      
                      ")),
      tags$div(class='dd2',					
               selectizeInput(
                 "gear", 'Gear No', choices = "", multiple = FALSE,
                 options = list(
                   placeholder = 'Gear NO')
               ))      
      )
    ),
  dashboardBody(
    fluidRow(
      box(
        title = "Contrl Chart- Leaked pressure Drop", status = "primary", solidHeader = TRUE,
        collapsible = TRUE,
        plotOutput("controlChart")
      ),
      box(
        title = "Control Chart -CW leak", status = "primary", solidHeader = TRUE,
        collapsible = TRUE,
        plotOutput("controlChartCW")
      )
      ,
      box(
        title = "Control Chart -CW leak", status = "primary", solidHeader = TRUE,
        collapsible = TRUE,
        plotOutput("controlChartHist")
      )
      ,
      box(
        title = "Histogram -Leaked pressure drop", status = "primary", solidHeader = TRUE,
        collapsible = TRUE,
        plotlyOutput("histleaked")
      )
      ,
      box(
        title = "Histogram -Torque", status = "primary", solidHeader = TRUE,
        collapsible = TRUE,
        plotlyOutput("histTorque")
      )
      
    )
  )
)

server <- shinyServer(function(input, output, session) {
  
  updateSelectizeInput(session, "gear", choices = MyRaneData$Gear_No,
                       server = TRUE)
  output$controlChart <- renderPlot({
    qcc(data = myRaneDf$NPD..bar., # The dataset
        type = "xbar.one", # The chart type (in this case it lets qcc know that n = 1)
        plot = TRUE) # Display the plot
    
  })
  output$controlChartCW <- renderPlot({
    
    qcc(data = myRaneDf$Leak.Pr.CW..bar., # The dataset
        type = "xbar.one", # The chart type (in this case it lets qcc know that n = 1)
        plot = TRUE) # Display the plot
    
  })
  output$controlChartHist <- renderPlot({		
    test <- qcc.groups(myRaneDf$Leak.Pr.CW..bar., sample.int(116))
    q <- qcc(test, type="xbar", plot=FALSE)
    process.capability(q, spec.limits=c(0,2.5))
    
  })
  output$histleaked <- renderPlotly({
    p <- plot_ly(x = MyRaneData$NPD..bar.,
                 type = "histogram"
    )
  })
  output$histTorque <- renderPlotly({
    
    p <- plot_ly(x = MyRaneData$Torque.Bal.1..Nm.,
                 type = "histogram"
    )
  })
  
  
})

#Run app! 
shinyApp(ui = ui, server = server)

