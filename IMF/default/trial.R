
library(shiny)
library(readr)
FM <- read_csv(
  file = "dataset_2024-11-30T14_49_34.831605686Z_DEFAULT_INTEGRATION_IMF.FAD_FM_2.0.0.csv", 
  name_repair = function(nms) {tolower(gsub("[.]", "_", nms))},
  show_col_types = FALSE)
# colnames(FM)
# View(dataset_2024_11_30T14_49_34_831605686Z_DEFAULT_INTEGRATION_IMF_FAD_FM_2_0_0)
library(tidyverse)
#
uniq_countries = unique(FM$country_name)
#
dataset_name = unique(FM$dataset)
#
ui <- fluidPage(
  titlePanel("Fiscal Monitor"),
  fluidRow(
    column(6, 
           selectInput(
             inputId = "country", label = "County", 
             choices = uniq_countries, 
             selected = uniq_countries[1])),
    br(),
    plotOutput('gg_plot_rain', width = "100%", height = 700)  
  )
)

server <- function(input, output, session) {
  plot_dataset <- reactive({
    FM |> 
      filter(country_name == input$country) |>
      select(
        -dataset, -series_code, -obs_measure, -frequency_name, 
        -scale_name, -country_name) |>
      pivot_longer(
        cols = !indicator_name,
        names_to = 'year', 
        values_to = 'value'
      ) |> 
      mutate(
        year = as.numeric(year)
      )
  })
  
  output$gg_plot_rain <- renderPlot({
    # print(plot_dataset())
    print(1)
    plot_dataset() |> 
      ggplot() + 
      geom_line(aes(x = year, y = value, color = indicator_name)) +
      scale_y_continuous(
        labels = scales::unit_format(unit = '%')
      ) +
      # theme_minimal() + 
      theme_bw() +
      labs(title = input$country) +
      scale_color_brewer(palette = 'Dark2') +
      
      guides(color = guide_legend(override.aes = list(linewidth = 7.5)))+  
      
      
      theme(
        panel.grid.minor.x = element_blank(),
        axis.line.y = element_blank(),
        panel.grid.minor = element_blank(),
        plot.title = element_text(face = "bold", hjust = 0.5),
        legend.text = element_text(face = "bold", size = rel(1.2)),
        # axis.title.y.right = element_text(angle = 90),
        axis.title = element_blank(),
        axis.text = element_text(face = "bold", size = rel(1)),
        axis.ticks = element_line(),
        legend.position = 'top',
        legend.title = element_blank()
      )
  })#, width = 1500, height = 900)
}

shinyApp(ui, server)
