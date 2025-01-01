library(shiny)
library(readr)
library(bslib)
library(dplyr)
library(lubridate)
library(ggplot2)
library(tidyr)
library(DT)
#

# TODO
# 
#'clean colnames 
clean_col_names <- function(data, index, ...){
  names(data)[index] <- janitor::make_clean_names(names(data)[index], ...)
  data
}
key_health_metrics <- read_csv("data/KeyHealthMetrics.csv") |>
  # clean first 3 colnames
  clean_col_names(1:3)
#'
#'Health Grouping
#'
health_grouping <- unique(key_health_metrics$grouping)
#' 
#' County
county_name <- unique(key_health_metrics$organisation_unit)
#' 
#' Data
#'  
data_grouping_name <- unique(key_health_metrics$data)
#'
#'
#'
ui <- fluidPage(
  tags$head(
    # Note the wrapping of the string in HTML()
    tags$style(HTML("
    @import url('https://fonts.cdnfonts.com/css/jetbrains-mono-2');
    body {
      font-family: 'JetBrains Mono', monospace;
      background-color: black;
      color: white;
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
        background-color: black;
        display: flex;
        align-items: center;
        justify-content: center
        
    }
    label {
      font-size: 2rem;
      font-weight: 700;
        
        display: flex;
        max-width: 100%;
        margin-bottom: 5px;
        font-weight: 700;
        justify-content: center;
    }
    .row {
      background-color: black;
      display: flex;
      align-items: center;
      justify-content: center;
      flex-direction: row;
      
    }
    #county_input, #health_grouping_input, #data_grouping_name_input, #download_image {
      background-color: black;
      display: flex;
      align-items: center;
      justify-content: center;
      flex-direction: row;
      
      font-size: 2rem;
      font-weight: 700;
      
      background-color: black;
      border: 0.5px solid #fff;
      padding: inherit; /*give padding below selected*/
      display: inline-block;
      
      
      
      position: relative;
      z-index: 1;
      box-sizing: border-box;
      box-shadow: none;
      border-radius: 0px;
      color: white;

    }
    .shiny-input-container:not(.shiny-input-container-inline) {
      width: 100% !important; /*Yeap Definitely wanted the drop downs to be centerd*/
    }
    "))),
  titlePanel("Health Metrics"),
  fluidPage(
    fluidRow(
      #style = "display: flex; flex-wrap:nowrap; gap: 1ch; width: calc(round(down, 100%,(1ch* var(--grid-cells)) -(1ch* var(--grid-cells) - 1))); margin-bottom: var(--line-height);",
      column(4,
             selectInput(
               inputId = 'county_input', 
               label = 'County', selectize = F,
               choices = county_name),
             style="margin: 0 auto;display: block;"),
      column(4, 
             uiOutput('health_grouping_input_o'),
             style="margin: 0 auto;display: block;"
      ),
      column(4,
             uiOutput('data_grouping_name_input_o'),
             style="margin: 0 auto;display: block;" # fixes and ensure the dropdown is centred
      )
    ),
    br(),
    fluidRow(
      plotOutput(outputId = 'data_plot_example', 
                 height = '650px', width = '90%')),
    br(),
    HTML(
      '<p>The full source code is here: <a href = "https://github.com/akhapwoyaco/shiny_apps/blob/main/weather_app/app.R" 
      target="_blank">https://github.com/akhapwoyaco/shiny_apps/blob/main/weather_app/</a>
      </a>
      </p>
      <p>Inspiration: <a href = "https://owickstrom.github.io/the-monospace-web/#introduction"
      target="_blank">The Monospace Web/</a>
      </a>
      </p>
      '
    ),
    br(),
    div(
      style = "gap: 1ch; display: flex;
      align-items: center;
      justify-content: center; width: calc(round(down, 100%,(1ch* var(--grid-cells)) -(1ch* var(--grid-cells) - 1))); margin-bottom: var(--line-height);",
      downloadButton(outputId = "download_image", label = "Download Image")
    ),
    br(),
    br(),
    div(
      class = "footer",
      style='height:50px;background:gray54;margin-top:auto;margin-bottom:auto;
                    text-align:center;',
      HTML(
        '<footer class="footer">
              Copyright &copy; 2024 &nbsp;
              Github Account: <a href="https://github.com/akhapwoyaco"
              target="_blank">akhapwoyaco</a>
              </footer>'
      ))
  )
)


server <- function(input, output, session) {
  #' We have the county name, now get data on county
  county_data <- reactive({
    key_health_metrics |>
      filter(
        organisation_unit %in% input$county_input) |>
      select(!(organisation_unit))
  })
  #' get unique groupings for grouping and update selector
  output$health_grouping_input_o <- renderUI(
    {req(input$county_input)
      #
      grouping_unique <- county_data() |>
        distinct(grouping) |> pull()
      selectInput(
        label = 'Grouping', selectize = F,
        inputId = 'health_grouping_input',
        choices = c(grouping_unique))
      #
    })
  #' subset grouping from county data
  group_data <- reactive({
    req(input$county_input, input$health_grouping_input)
    county_data() |>
      filter(
        grouping %in% input$health_grouping_input) |>
      select(!(grouping))
  })
  #' Observe the grouping input and with it get distinct data levels
  #' and update selector 
  output$data_grouping_name_input_o <- renderUI(
    {
      req(input$county_input, input$health_grouping_input)
      req(group_data())
      data_unique <- group_data() |>
        distinct(data) |> pull()
      
      selectInput( 
        label = 'Data', selectize = F,
        inputId = 'data_grouping_name_input',
        choices = c(data_unique))
    })
  #' 
  #' subset data 
  group_county_data <- reactive({
    req(input$data_grouping_name_input)
    group_data() |>
      filter(data %in% input$data_grouping_name_input) |>
      select(!(data))
  })
  #'
  final_plot_data <- reactive({
    # ensure data is well filtered before getting here
    validate(
      need(nrow(group_county_data()) > 0, message = F)
    )
    group_county_data() |>
      pivot_longer(
        cols = everything(),
        names_to = 'Date', values_to = 'values'
      ) |> drop_na(values) |>
      mutate(
        Date = lubridate::my(Date)
      ) |>
      mutate(
        year_s = lubridate::year(Date)
      ) 
  })
  #' 
  ggplot_out <- reactive({
    req(input$data_grouping_name_input)
    final_plot_data() |> 
      ggplot(aes(x = Date, y = values)) + 
      geom_line() + geom_point() + 
      scale_x_date(date_labels = '%m', date_breaks = 'month') +  
      scale_y_continuous(labels = scales::label_comma()) +
      labs(
        title = paste(
          input$county_input, ": ", gsub(pattern = "\\d+-", "",input$health_grouping_input),
          " ", 
          sep = ''),
        subtitle = input$data_grouping_name_input, 
        caption = 
          "Center for Epidemiological Modelling and Analsyis: https://cema.africa/kenyahealthdatatrends\nhttps://github.com/akhapwoyaco"
      ) +
      facet_grid(.~year_s, space = 'free_x',
                 scales = 'free_x', switch = 'x'
      ) +
      theme_bw() + 
      theme(
        strip.placement = 'outside',
        axis.title = element_blank(),
        axis.text.x = element_text(face = 'bold', size = rel(1.2)),
        axis.text.y = element_text(face = 'bold', size = rel(1.2), angle = 90, hjust = 0.5),
        panel.spacing = unit(0, 'cm'), 
        plot.title = element_text(hjust = 0.5, face = 'bold', size = rel(1.5)),
        plot.subtitle = element_text(hjust = 0.5, face = 'bold', size = rel(1.2)),
        plot.caption = element_text(hjust = 0.5, face = 'bold', size = rel(1.2))
      )
  })
  #
  output$data_plot_example <- renderPlot({
    ggplot_out()
  })
  
  # download plot via handler
  output$download_image <- downloadHandler(
    filename = function(){
      # supply state names and paste on filetype
      paste(
        paste0(
          input$county_input, 
          input$health_grouping_input, 
          input$data_grouping_name_input,
          collapse = '_'),
        '.jpeg', sep = ''
      )
    },
    content = function(file){
      ggsave(
        plot = ggplot_out(), filename = file, 
        width = 35, height = 25, units = "cm", dpi = 450
      )
    }
  )
  # output$data_table_example <- renderDT({
  #   final_plot_data()
  # })
  #
}

shinyApp(ui, server)