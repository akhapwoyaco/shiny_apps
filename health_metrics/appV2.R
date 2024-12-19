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
# I want a mono layout similar to https://owickstrom.github.io/the-monospace-web/
# even for the dropdowns
# Clear ERRORS Warning: Error in combine_vars: Faceting variables must have at least one value.
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
    :root {
    --font-family: 'JetBrains Mono', monospace;
    --line-height: 2.20rem;
    --border-thickness: 2px;
    --background-color: black;
    }
    body {
        background-color: black;
        color: white;
        height: 100vh;
    }
    h2 {
        font-size: 2rem;
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
    }
    .form-group .shiny-input-container {
      background-color: black;
    }
    .selectize-input {
      /*background-color: black;*/
      /*border: 1px solid #fff;*/
      /*padding: 6px 12px;*/
      display: inline-block;
      width: 100%;
      position: relative;
      z-index: 1;
      box-sizing: border-box;
      box-shadow: none;
      border-radius: 0px;
      color: white;
    }
    .selectize-input.full {
      background: black;
    }
    .selectize-input .selectize-control.single .selectize-input.input-active{
      color: black;
      background: green;
    }
    .selectize-input .item {
      color: white;
      border: 1px solid black;
      background: black !important;
      width: fit-content;
      line-height: 33.333px;
    }
    .shiny-input-select .selectize-control .single .plugin-selectize-plugin-a11y {
      backgroud: green; !important
    }
    .selectize-dropdown-content {
      background-color: black;
      border: 0.5px solid #fff;
      padding: 6px 12px;
      display: inline-block;
      width: 100%;
      position: relative;
      z-index: 1;
      box-sizing: border-box;
      box-shadow: none;
      border-radius: 0px;
      color: white;
    }
    "))),
  # theme = bs_theme(
  #   bg = "#101010", 
  #   fg = "#FDF7F7", 
  #   primary = "#ED79F9", 
  #   base_font = font_google("Prompt"),
  #   code_font = font_google("JetBrains Mono")
  # ),
  titlePanel("Health Metrics"),
  fluidPage(
    fluidRow(
      style = "display: flex; gap: 1ch; width: calc(round(down, 100%,(1ch* var(--grid-cells)) -(1ch* var(--grid-cells) - 1))); margin-bottom: var(--line-height);",
      column(4,
             selectInput(
               inputId = 'county_input', 
               label = 'County', 
               choices = county_name)),
      column(4, 
             selectInput(
               inputId = 'health_grouping_input', 
               label = 'Grouping', 
               choices = NULL)),
      column(4,
             selectInput(
               inputId = 'data_grouping_name_input', 
               label = 'Data', 
               choices = NULL
             ))
    ),
    br(),
    fluidRow(
      plotOutput(outputId = 'data_plot_example', 
                 height = '650px', width = '90%')),
    br(),
    # fluidRow(
    #   style = "background: green; ",
    #   selectizeInput(
    #     inputId = 'data_grouping_name_input', 
    #     label = 'Data', 
    #     choices = 1:3
    #   )#dataTableOutput(outputId = 'data_table_example')
    #   ),
    br(),
    div(
      style = "gap: 1ch; display: flex;
      align-items: center;
      justify-content: center; width: calc(round(down, 100%,(1ch* var(--grid-cells)) -(1ch* var(--grid-cells) - 1))); margin-bottom: var(--line-height);",
      downloadButton(outputId = "download_image", label = "Download Image")
    ),
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
  # )
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
  observeEvent(input$county_input,{
    req(input$county_input)
    #
    grouping_unique <- county_data() |>
      distinct(grouping) |> pull()
    # print(grouping_unique)
    updateSelectInput(
      session = session,
      inputId = 'health_grouping_input',
      choices = c(grouping_unique))
    #
  })
  #' subset grouping from county data
  group_data <- reactive({
    req(input$health_grouping_input)
    county_data() |>
      filter(
        grouping %in% input$health_grouping_input) |>
      select(!(grouping))
  })
  #' Observe the grouping input and with it get distinct data levels
  #' and update selector
  observeEvent(input$health_grouping_input, {
    req(input$health_grouping_input)
    req(group_data())
    data_unique <- group_data() |>
      distinct(data) |> pull()
    
    updateSelectInput(
      session = session, inputId = 'data_grouping_name_input',
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
    # View(group_county_data())
    group_county_data() |>
      pivot_longer(
        cols = everything(),
        names_to = 'Date', values_to = 'values'
      ) |>
      mutate(
        Date = as.Date(paste('1', Date, sep = ' '), '%d %B %Y')
      )
  })
  #' 
  ggplot_out <- reactive({
    req(final_plot_data())
    # View(final_plot_data())
    final_plot_data() |> 
      ggplot(aes(x = Date, y = values)) + 
      geom_line() + geom_point() + 
      scale_x_date(date_labels = '%m', date_breaks = 'month') +  
      facet_grid(~year(Date), space = 'free_x', 
                 scales = 'free_x', switch = 'x') + 
      theme_bw() + 
      theme(
        strip.placement = 'outside',
        axis.title = element_blank(),
        axis.text.x = element_text(face = 'bold'),
        axis.text.y = element_text(face = 'bold', angle = 90, hjust = 0.5),
        panel.spacing = unit(0, 'cm')
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
        width = 30, height = 25, units = "cm", dpi = 450
      )
    }
  )
  # output$data_table_example <- renderDT({
  #   final_plot_data()
  # })
  #
}

shinyApp(ui, server)