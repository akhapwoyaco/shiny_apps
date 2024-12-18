library(tidyverse)
library(readr)
library(janitor)
library(shiny)
#
Doing_Business_Dataset <- read_csv(
  file = "archive/Doing Business Dataset.csv", 
  na = c(".."), n_max = 37248) |>
  clean_names() |>
  mutate(
    series_name2 = gsub(pattern = "\\(.*", replacement = "", series_name) |>
      trimws(),
    series_name3 = gsub(pattern = ".*\\(", replacement = "", series_name) |>
      trimws() |> gsub(pattern = "\\).*", replacement = "")
  )
#
country_unique = unique(Doing_Business_Dataset$country_code)
#
ui <- fluidPage(
  tags$head(
    # Note the wrapping of the string in HTML()
    tags$style(HTML("
      h2 {
        text-align: center;
        left: 0px;
        right: 0px;
        background-color: #F7F7F7;
      }"))),
  titlePanel("Global Ease of Doing Business"),
  fluidRow(
    style = "border: 4px double #edf5ef; ",
    column(
      4,
      selectInput(
        inputId = 'country', label = 'Country', 
        choices = country_unique, multiple = F)
    ),
    column(
      4,
      uiOutput(
        outputId = "series_p"
      )
    ),
    column(
      4,
      uiOutput(
        outputId = "series_c"
      )
    )
  ),
  br(),
  fluidRow(
    plotOutput(
      outputId = 'plot_out', height = '600px')
  )
)

server <- function(input, output, session) {
  data_plotting <- reactive({
    Doing_Business_Dataset |>
      filter(country_code == input$country) |>
      pivot_longer(
        cols = x2010_yr2010:x2019_yr2019,
        names_to = "year",
        values_to = "value"
      ) |>
      mutate(
        year = lubridate::ymd(
          gsub(pattern = ".*yr", replacement = "", year) , truncated = 2L)
        
      ) |>
      separate(
        series_name2, into = c("series", "sub_series"), 
        sep = ": | -") |>
      mutate(
        sub_series = case_when(
          is.na(sub_series) ~ series,
          .default = sub_series
        )
      )
  })
  #
  output$series_p <- renderUI({
    req(data_plotting())
    selectInput(
      inputId = 'parent_series', label = 'Series', 
      choices = data_plotting()$series, multiple = F)
  })
  #
  child_series <- reactive({
    req(data_plotting())
    req(input$parent_series)
    data_plotting() |> 
      filter(series == input$parent_series) |>
      select(-country_code, -series)
  })
  #
  output$series_c <- renderUI({
    #req(child_series())
    req(input$parent_series)
    selectInput(
      inputId = 'child_series', label = 'Series C', 
      choices = child_series()$sub_series, multiple = F)
  })
  #
  output$plot_out <- renderPlot({
    req(input$child_series)
    plot_data = child_series() |> 
      filter(sub_series == input$child_series)
    country_name = unique(plot_data$country_name)
    plot_data |>
      ggplot() +
      # geom_point(data = child_series() , aes(x = year, y = value)) +
      # geom_point(
      #   aes(x = year, y = value)) +
      labs(
        #title = "Global Ease of Doing Business Dataset (2010-2019)",
        title = paste(
          "Business Landscape Across Global Economies", country_name, sep = ': '),
        caption = "Source: https://www.kaggle.com/datasets/saurabhbadole/global-ease-of-doing-business-dataset"
      ) +
      geom_line(
        aes(x = year, y = value, color = series_name3, group = series_code)) +
      theme_bw() +
      guides(
        colour = guide_legend(override.aes = list(linewidth = 3))
      ) +
      theme(
        plot.title = element_text(hjust = 0.5, face = 'bold', size = rel(2)), 
        plot.subtitle = element_text(hjust = 0.5, face = 'bold', size = rel(1.9)),
        axis.text = element_text(face = 'bold', size = 10),
        axis.title = element_blank(),
        legend.title = element_blank(),
        legend.background = element_blank(),
        legend.position = 'top',
        legend.text = element_text(size = rel(1.5))
      )
  })
  #
}

shinyApp(ui, server)