#
library(tidyverse)
library(readr)
library(janitor)
library(ggthemes)    

access_to_electricity <- read_csv(
  file = "API_EG.ELC.ACCS.ZS_DS2_en_csv_v2_123/API_EG.ELC.ACCS.ZS_DS2_en_csv_v2_123.csv", 
  col_types = cols(...69 = col_skip()), 
  skip = 3) |> clean_names() 
#
metadata_country <- read_csv(
  file = "API_EG.ELC.ACCS.ZS_DS2_en_csv_v2_123/Metadata_Country_API_EG.ELC.ACCS.ZS_DS2_en_csv_v2_123.csv", 
  col_types = cols(...6 = col_skip())) |> clean_names()
# head(metadata_country)
all_data = merge(
  access_to_electricity, metadata_country, 
  by = "country_code", all.x = T) |>
  mutate(
    region = replace_na(region,"Not Specified"),
    income_group = replace_na(income_group, "Not Specified")
    )
# groupings of region or income level
region_lvl = unique(all_data$region)
income_lvl = unique(all_data$income_group)
indicator_name = unique(all_data$indicator_name)
unique_countries = unique(all_data$country_name)

library(shiny)

ui <- fluidPage(
  titlePanel(indicator_name),
  tags$head(
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
        justify-content: center;
        
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
    .row, .drop_down_cols {
      background-color: black;
      display: flex;
      text-align: center;
      
      align-items: center;
      justify-content: center;
      flex-direction: row;
    }
    .col-sm-4 {
      align-items: center;
      justify-content: center;
      position: relative;
    }
    .row, .individual_plots {
      background-color: black;
      display: flex;
      align-items: center;
      justify-content: center;
      flex-direction: row;
      
      width: 100%;
      padding-right: 15px;
      padding-left: 15px;
      margin-right: auto;
      margin-left: auto;
    }
    #access_out_plot {
      position: relative;
      
      transition: all 0.00004s ease-in-out;
      border-radius: 15px;
      box-shadow: 2px 1px 25px 5px rgba(166, 166, 166, 0.2);
      margin: 1rem 0;
      padding: 0px;
      color: black;
      border: 1px solid rgba(17, 72, 126, 0.1);
    }
    #reg_income_group, #reg_income_group_selected, #country {
      background-color: black;
      display: flex;
      align-items: center;
      justify-content: center;
      flex-direction: row;
      
      font-size: 2rem;
      font-weight: 700;
      background-color: black;
      border: 0.5px solid #fff;
      padding: 6px 12px;
      display: inline-block;
      width: 100%;
      position: relative;
      z-index: 1;
      box-sizing: border-box;
      box-shadow: none;
      border-radius: 1px;
      color: white;
    }
    .shiny-input-container:not(.shiny-input-container-inline) {
      width: 100% !important; /*Yeap Definitely wanted the drop downs to be centerd*/
    }
    "))),
  fluidRow(
    class = "drop_down_cols",
    #style = "border: 0; vertical-align: baseline; display: flex; flex-wrap:nowrap; gap: 1ch; width: calc(round(down, 100%,(1ch* var(--grid-cells)) -(1ch* var(--grid-cells) - 1))); margin-bottom: var(--line-height);",
    #style = "display: flex;margin-left: auto; margin-right: auto;",
    column(
      4, selectInput(
        inputId = "reg_income_group", label = "By Category", 
        choices = c("Region", "Income Group"), selectize = F),
      style="margin: 0 auto;display: block;"
      ),
    column(
      4, 
      uiOutput('reg_or_income_group'),
      style="margin: 0 auto;display: block;" # DOES NOT SEEM TO DO WHAT I WANT
    ),
    column(
      4, 
      uiOutput('out_country'),
      style="margin: 0 auto;display: block;"
    )
  ),
  fluidRow(
    class = "individual_plots",
    style="margin: 0 auto;display: block;",
    plotOutput(
      "access_out_plot", width = '100%',
      height = "550px"
    )
  )
)

server <- function(input, output, session) {
  # observe if the selected grouping is either 
  # region or income group
  # create an output of either regions or income group
  output[["reg_or_income_group"]] <- renderUI(
    {req(input$reg_income_group)
      #
      if (input$reg_income_group == "Region"){
        ri_choice_s = region_lvl
      } else {
        ri_choice_s = income_lvl
      }
      #
      selectInput(
        label = input$reg_income_group, selectize = F,
        inputId = 'reg_income_group_selected',
        choices = ri_choice_s
      )
      #
    })
  #
  region_data <- reactive({
    req(input$reg_income_group, input$reg_income_group)
    #
    all_data |> 
      filter(
        region == input$reg_income_group_selected | income_group == input$reg_income_group_selected
      )
    #
  })
  #
  output$out_country <- renderUI({
    req(input$reg_income_group, input$reg_income_group)
    unq_country_name = region_data() |>
      distinct(country_name) |> pull()
    selectInput(
      inputId = "country", label = "Country", 
      choices = unq_country_name, selectize = F)
  })
  #
  plot_data <- reactive({
    req(input$reg_income_group, input$reg_income_group, input$country)
    region_data() |> 
      filter(country_name == input$country) |>
      select(-country_name, -indicator_name, -indicator_code, 
             -region, -income_group, -special_notes, -table_name) |>
      pivot_longer(
        cols = -country_code,
        values_to = "values",
        names_to = "variables"
      ) |>
      mutate(
        variables = str_replace_all(
          variables, 
          c("x" = "")
        ) |> as.numeric()
      ) |> drop_na() 
  })
  output$access_out_plot <- renderPlot({
    plot_data() |> 
      ggplot(aes(x = variables, y = values)) +
      geom_line() +
      scale_y_continuous(
        labels = scales::unit_format(unit = "%")
      ) +
      labs(
        caption = 
          "World Bank: https://data.worldbank.org/indicator/EG.ELC.ACCS.ZS    |    https://github.com/akhapwoyaco"
      ) +
      theme_economist() + 
      theme(
        axis.title = element_blank()
      )
  })
}

shinyApp(ui, server)