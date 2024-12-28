library(shiny)
library(readr)
library(tidyverse)
library(broom)
library(magrittr)
library(DT)
library(caret)
library(pROC)

library(bslib)
#
HearingLoss <- read_delim(
  file = "HearingLoss.txt", 
  "\t", escape_double = FALSE, trim_ws = TRUE) |> 
  select(-ID) |> drop_na() #, starts_with("HearingLoss"))
#
HearingLoss$Diagnosis <- as.factor(HearingLoss$Diagnosis)
#
lhs_ch = colnames(HearingLoss) |> str_subset("Diagnosis", negate = T)
#
ui <- page_sidebar(
  tags$head(
    # Note the wrapping of the string in HTML()
    tags$style(HTML("
        @import url('https://fonts.cdnfonts.com/css/jetbrains-mono-2');
        body {
          font-family: 'JetBrains Mono', monospace;
          height: 100vh;
          line-height: 2.20rem;
        }
        h1 {
            font-size: 3rem;
            font-weight: 700;
            line-height: calc(2* var(--line-height));
            text-transform: uppercase;
            text-align: center;
            left: 0px;
            right: 0px;

            display: flex;
            align-items: center;
            justify-content: center}
        "))),
  title = "Hearing Loss Diagnosis Classification",
  
  sidebar = sidebar(
    bg = "white",
    accordion(
      accordion_panel(
        title = "Variable Selection",
        list(
          selectInput(
            label = "Right Hand", inputId =  'rhs', 
            choices = "Diagnosis")
          ,
          selectInput(
            label = "Left Hand", inputId =  'lhs',
            choices = lhs_ch, 
            multiple = T),
          sliderInput( 
            label = "Train Proportion", inputId =  'train_prop',
            min = 0.5, max = 0.9, 
            value = c(0.75) 
          )
        )
        
      )
    )),
  accordion(
    # open = NULL
    accordion_panel(
      "Variable Importance",
      plotOutput('data_var_imp_plot')
    ),
    accordion_panel(
      "Model Perfomance",
        verbatimTextOutput('ml_summary')
    )
  )
)
server <- function(input, output, session) {
  #
  train_index = reactive({
    set.seed(0711)
    inTraining <- createDataPartition(HearingLoss$Diagnosis, p = input$train_prop, list = FALSE)
  })
  #
  train_data <- reactive({
    req(input$lhs)
    training <- HearingLoss[ train_index(), c(input$rhs, input$lhs)]
    training
  })
  #
  test_data <- reactive({
    req(input$lhs)
    testing  <- HearingLoss[-train_index(), c(input$rhs, input$lhs)]
    testing
  })
  #
  model_output <- reactive({
    #
    set.seed(0711)
    rf_model <- caret::train(
      Diagnosis ~ ., data = train_data(), 
      method = "rf",
      trControl = trainControl("cv", number = 10)
    )
    #
    rf_model
  })
  #
  test_pred_data <- reactive({
    test_pred = model_output() |> 
      predict(test_data())
  })
  #
  test_pred_data_prob <- reactive({
    test_pred = model_output() |> 
      predict(test_data(), type = 'prob')
  })
  #
  model_perfomance_table <- reactive({
    confusionMatrix(test_pred_data(),test_data()$Diagnosis)
  })
  #
  output$ml_summary <- renderPrint({
    model_perfomance_table()
    })
  #
  output$data_var_imp_plot <- renderPlot({
    vip::vip(model_output(), num_features = 25, bar = FALSE) +
      theme_bw() +
      theme(
        plot.title = element_blank(),
        axis.title = element_blank(),
        axis.text = element_text(face = 'bold', size = rel(2))
      )
    })
}

shinyApp(ui, server)