library(shiny)
library(readr)
library(tidyverse)
library(broom)
library(magrittr)
library(DT)
#
HearingLoss <- read_delim(
  file = "HearingLoss.txt", 
  "\t", escape_double = FALSE, trim_ws = TRUE) |> 
  select(Age, starts_with("HearingLoss"))

ui <- fluidPage(
  tags$head(
    # Note the wrapping of the string in HTML()
    tags$style(HTML("
    @import url('https://fonts.cdnfonts.com/css/jetbrains-mono-2');
    body {
      font-family: 'JetBrains Mono', monospace;
      
      
      height: 100vh;
      line-height: 2.20rem;
    }
    h2 {
        font-size: 3rem;
        font-weight: 700;
        line-height: calc(2* var(--line-height));
        text-transform: uppercase;
        text-align: center;
        left: 0px;
        right: 0px;
        
        display: flex;
        align-items: center;
        justify-content: center
        
    }
    "))),
  titlePanel("Hearing Loss Frequencies"),
  fluidRow(
    column(6,
           selectInput(
             label = "Right Hand", inputId =  'rhs', 
             choices = "Age")
    ),
    
    column(6,
           selectInput(
             label = "Left Hand", inputId =  'lhs', 
             choices = c('HearingLoss250','HearingLoss500','HearingLoss1000','HearingLoss2000',
                         'HearingLoss4000','HearingLoss8000'), multiple = T)
    )
  ),
  # verbatimTextOutput("data_summary"),
  # verbatimTextOutput('ml_summary'),
  DT::dataTableOutput("data_summary"),
  DT::dataTableOutput('ml_summary'),
  # uiOutput('test')
  fluidRow(
    column(6, plotOutput('log_p_plot')),
    column(6, plotOutput('my_summary_table'))
  )
)

server <- function(input, output, session) {
  model_output <- reactive({
    models = list()
    for (i in input$lhs){
      for (j in input$rhs){
        models[[paste(i, "vs", j)]] <- lm(as.formula(paste(i, "~", j)), data = HearingLoss)
      }
    }
    models
  })
  #
  model_table_1 <- reactive({
    validate(
      need(length(model_output()) > 0, message = "NO MODEL YET, SELECT RHS VARIABLE")
    )
    model_tibble = lapply(model_output(), glance, simplify = F)
    print(model_tibble)
    table <- dplyr::bind_rows(model_tibble, .id = "model") #|> 
    # mutate(
    #   HERTZ = str_squish(str_sub(model,12,15))) #|> 
    #
    # levels(table$HERTZ) <- as.factor(table$HERTZ)
    
    table
  })
  #
  model_table_2 <- reactive({
    validate(
      need(length(model_output()) > 0, message = "NO MODEL YET, SELECT RHS VARIABLE")
    )
    model_tibble = lapply(model_output(), tidy, simplify = F)
    table <- dplyr::bind_rows(model_tibble, .id = "model") |> 
      select(model, term, estimate) |>
      pivot_wider(
        id_cols = term, names_from = model, values_from = estimate
      )
  })
  #
  # output$test <- renderUI({
  #   tbl_regression(model_table_1())
  # })
  # output$data_summary <- DT::renderDataTable({
  #   stargazer::stargazer(HearingLoss,summary = TRUE, type="html")
  # })
  output$data_summary <- DT::renderDataTable({
    # validate(
    #   need(nrow(model_table_2())>0, message = "NO DATA")
    # )
    model_table_2() |> 
      datatable(
        options = list(
          dom = 't',
          initComplete = JS(
            "function(settings, json) {",
            "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});",
            "}")), rownames = F)
  })
  output$ml_summary <- DT::renderDataTable({
    # validate(
    #   need(nrow(model_table_1())>0, message = "NO DATA")
    # )
    model_table_1() |> 
      datatable(
        options = list(
          dom = 't',
          initComplete = JS(
            "function(settings, json) {",
            "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});",
            "}")), rownames = F)
  })
  
  output$log_p_plot <- renderPlot({
    # validate(
    #   need(nrow(model_table_1())>0, message = "NO DATA")
    # )
    # print(colnames(model_table_1()))
    model_table_1() |> 
      mutate(
        HERTZ = str_squish(str_sub(model,12,15))
      ) |>
      select(HERTZ, `p.value`) |> 
      mutate(
        neg_log10 = -log10(p.value) #negative log
      ) |>  
      ggplot(aes(y = reorder(HERTZ, neg_log10), x = neg_log10)) + 
      geom_dotplot(binaxis='y', stackdir='center') +
      labs(x = "-log10(p-value)", y = "Frequencies",
           title = "Name") + 
      theme_light() + 
      theme(
        plot.title = element_text(hjust = 0.5, size = 15)
      )
  })
  #
  output$my_summary_table <- renderPlot({
    # validate(
    #   need(nrow(HearingLoss)>0, message = "NO DATA")
    # )
    b <- boxplot(
      HearingLoss,
               xaxt="n",border = "white",col = "black",
               boxwex = 0.3,medlwd=1,whiskcol="black",
               staplecol="black",outcol="red",cex=0.3,outpch=19,
               main="Hearing Loss")
    axis(side=1,at=1:length(b$names),
         labels=paste(b$names,"\n(n=",b$n,")",sep=""),
         mgp=c(3,2,0))
  })
  #
}

shinyApp(ui, server)