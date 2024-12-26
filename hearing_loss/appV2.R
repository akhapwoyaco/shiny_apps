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
  title = "Hearing Loss Linear Regression",
  
  sidebar = sidebar(
    bg = "white",
    accordion(
      accordion_panel(
        title = "Regression Vars",
        list(
          selectInput(
            label = "Right Hand", inputId =  'rhs', 
            choices = "Age")
          ,
          selectInput(
            label = "Left Hand", inputId =  'lhs',
            choices = c(
              'HearingLoss250','HearingLoss500','HearingLoss1000', 
              'HearingLoss2000', 'HearingLoss4000','HearingLoss8000'), 
            multiple = T)
        )
        
      )
    )),
  accordion(
    open = c("Data Summary"),
    accordion_panel(
      "Data Summary",
      plotOutput('data_box_plot')
    ),
    accordion_panel(
      "Model Coefficients",
      DT::dataTableOutput("model_coeff")
      ),
    accordion_panel(
      "Model Perfomance",
      DT::dataTableOutput('ml_summary')
    ),
    accordion_panel(
      "Log P-Value",
      plotOutput('log_p_plot')
    )
    
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
  model_perfomance_table <- reactive({
    model_tibble = lapply(model_output(), glance, simplify = F)
    table <- dplyr::bind_rows(model_tibble, .id = "model")
    table |> 
      select(model,r.squared, p.value, AIC, BIC, nobs) 
  })
  #
  model_coef_table <- reactive({
    model_tibble = lapply(model_output(), tidy, simplify = F)
    table <- dplyr::bind_rows(model_tibble, .id = "model") |> 
      select(model, term, estimate) |>
      pivot_wider(
        id_cols = term, names_from = model, values_from = estimate
      ) |> 
      mutate_if(is.numeric, round, digits = 2)
  })
  #
  output$model_coeff <- DT::renderDataTable({
    model_coef_table() |> 
      datatable(
        options = list(
          dom = 't',
          initComplete = JS(
            "function(settings, json) {",
            "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});",
            "}")), rownames = F) #|>
      # formatRound(digits = 2)
  })
  output$ml_summary <- DT::renderDataTable({
    model_perfomance_table() |>
      mutate_if(is.numeric, round, digits = 4) |> 
      datatable(
        options = list(
          dom = 't',
          initComplete = JS(
            "function(settings, json) {",
            "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});",
            "}")), rownames = F)
  })
  
  output$log_p_plot <- renderPlot({
    # print(colnames(model_perfomance_table()))
    model_perfomance_table() |> 
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
        plot.title = element_blank()#text(hjust = 0.5, size = 15)
      )
  })
  #
  output$data_box_plot <- renderPlot({
    b <- boxplot(
      HearingLoss,
      xaxt = "n", border = "white", col = "black", 
      boxwex = 0.3, medlwd = 1, whiskcol = "black", 
      staplecol = "black", outcol = "red", cex = 0.3, 
      outpch=19#,
      # main="Hearing Loss"
      )
    axis(
      side = 1, at = 1:length(b$names), 
      labels = paste(b$names,"\n(n=",b$n,")", sep = ""), 
      mgp = c(3,2,0))
  })
}

shinyApp(ui, server)