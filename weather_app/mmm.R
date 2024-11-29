library(shiny)
library(promises)
library(future)
library(DT)
library(plotly)
library(chron)

plan(multisession)

testAsyncProcess <- function(x){
  start <- Sys.time()
  Sys.sleep(x)
  end <- Sys.time()
  result <- data.frame(
    start = as.character(times(strftime(start,"%H:%M:%S"))),
    end   = as.character(times(strftime(end,  "%H:%M:%S"))),
    duration = round(end - start,1)
  )
  return(result)
}

ui <- fluidPage(
  titlePanel("async test app"),
  sidebarLayout(
    sidebarPanel(
      width = 12,
      fluidRow(
        column(
          3, 
          actionButton("SimulateAsyncProcesses", "Simulate async processes"), 
          style = 'margin-top:25px'
        ),
        column(
          4, 
          DTOutput("ProcessInfo")
        )
      )
    ),
    mainPanel(
      width = 12,
      fluidRow(
        column(
          2, 
          actionButton("GenerateDataToPlot", "Generate data to plot")
        ),
        column(8, offset = 1, plotlyOutput("GeneratedHeatMap"))
      )
    )
  )
)

server <- function(input, output, session) {
  
  
  DataToPlot <- eventReactive(input$GenerateDataToPlot, {
    matrix(runif(100), nrow = 10, ncol = 10)
  })
  
  processInfo <- reactiveVal()
  
  processInfo2 <- eventReactive(input$SimulateAsyncProcesses, {
    future_promise(testAsyncProcess(10)) %...>% {processInfo(.)}
  })
  
  output$ProcessInfo            <- renderDT({
    req(processInfo2())
    datatable(processInfo(), rownames = FALSE, options = list(dom = 't'))
  })
  
  output$GeneratedHeatMap       <- renderPlotly({
    req(DataToPlot())
    plot_ly(z = DataToPlot(), type = "heatmap")
  })
}

shinyApp(ui = ui, server = server)