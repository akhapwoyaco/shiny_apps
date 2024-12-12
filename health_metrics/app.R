library(shiny)
library(readr)
library(bslib)
library(dplyr)
library(lubridate)
library(ggplot2)
library(tidyr)
library(DT)
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
  theme = bs_theme(
    bg = "#101010", 
    fg = "#FDF7F7", 
    primary = "#ED79F9", 
    base_font = font_google("Prompt"),
    code_font = font_google("JetBrains Mono")
  ),
  #' TAB 1
  tabsetPanel(
    id = 'tabs',
    tabPanel(
      title = "Introduction",
      fluidPage(
        br(),
        br(),
        helpText(
          tags$h4('Data Acquisition:'),
          'The data was acquired from the Center for Epidemiological Modelling and Analsyis:', 
          tags$a("Key Health Metrics", 
                 href = "https://cema.africa/kenyahealthdatatrends"),
          tags$br()  ),
        br(),
        br(),
        helpText(
          'The analysis is guided by the desire to learn the R 
              programming language, statistical data analysis 
              and master shiny apps.',
          paste0(
            'The KeyHealthMetrics data contains ', '1', 
            ' observations, and ', '1', ' features on ....')),
        br(),
        br(),
        helpText(
          # include the html for lista and bold text
          tags$h4('Data:'),
          tags$li(
            "Selection is made by the radio buttons on top right box for associations 
            desired based on the publisher."), 
          tags$li("Select Inputs define the features to get 
            associations by"),
          tags$h4("Plots: "),
          tags$li(
            'Add Resource for Mosaic Plot Interpretation and Download Button Functionality')
        )
      )
    ),
    tabPanel(
      title = "Key Health Metrics",
      fluidPage(
        fluidRow(
          column(4,
                 selectInput(
                   inputId = 'county_input', 
                   label = 'County', 
                   choices = county_name)),
          column(4,
                 selectizeInput(
                   inputId = 'health_grouping_input', 
                   label = 'Grouping', 
                   choices = NULL)),
          column(4,
                 selectizeInput(
                   inputId = 'data_grouping_name_input', 
                   label = 'Data', 
                   choices = NULL
                 ))),
        fluidRow(
          plotOutput(outputId = 'data_plot_example', 
                     height = '650px', width = '90%')),
        br(),
        # fluidRow(
        #   dataTableOutput(outputId = 'data_table_example')),
        # br()
        fluidRow(
          column(
            width = 12, 
            downloadButton(outputId = "download_image", label = "Download Image")
          )
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
    )
  )
)  

server <- function(input, output, session) {
  #' We have the county name, now get data on county
  county_data <- reactive({
    key_health_metrics |>
      filter(
        organisation_unit == input$county_input) |>
      select(!(organisation_unit))
  })
  #' get unique groupings for grouping and update selector
  observeEvent(input$county_input,{
    req(input$county_input)
    #
    grouping_unique <- county_data() |>
      distinct(grouping) |> pull()
    # print(grouping_unique)
    updateSelectizeInput(
      session = session,
      inputId = 'health_grouping_input',
      choices = c(grouping_unique), 
      server = TRUE)
    #
  })
  #' subset grouping from county data
  group_data <- reactive({
    req(input$health_grouping_input)
    county_data() |>
      filter(
        grouping == input$health_grouping_input) |>
      select(!(grouping))
  })
  #' Observe the grouping input and with it get distinct data levels
  #' and update selector
  observeEvent(input$health_grouping_input, {
    req(input$health_grouping_input)
    req(group_data())
    data_unique <- group_data() |>
      distinct(data) |> pull()
    
    updateSelectizeInput(
      session = session, inputId = 'data_grouping_name_input',
      choices = c(data_unique), 
      server = T)
  })
  #' 
  #' subset data 
  group_county_data <- reactive({
    req(input$data_grouping_name_input)
    group_data() |>
      filter(data == input$data_grouping_name_input) |>
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
    final_plot_data() |> 
      ggplot(aes(x = Date, y = values)) + 
      geom_line() + geom_point() + 
      scale_x_date(date_labels = '%b', date_breaks = 'month') +  
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