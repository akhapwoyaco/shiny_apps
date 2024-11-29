library(shiny)
library(leaflet)
library(httr)
library(jsonlite)
library(tidyverse)
library(lubridate)
library(DT)
library(shinyWidgets)   
#
load('weather.RData')
load('geo_data.RData')
#
uniq_counties = c("ALL", unique(counties_data$county))
kenya_county <- kenya_county |> rename(uid = shapeID)
#
ui <- fluidPage(
  tags$head(
    # Note the wrapping of the string in HTML()
    tags$style(HTML("
      @import url('https://fonts.googleapis.com/css2?family=Yusei+Magic&display=swap');
      body {
        background-color: black;
        color: white;
        height: 100vh;
      }
      h2 {
        font-family: 'Yusei Magic', sans-serif;
        text-align: center;
        left: 0px;
        right: 0px;
        background-color: black;
        
      }
      .row {
        display: flex;
        
        flex: 0 0  100%;
        
        max-width: fit-content;
        
        margin-left: auto;
        margin-right: auto;
        
        flex-direction: row;
        flex-wrap: wrap;
        /*flex-wrap: wrap;*/
        
      }
      .leaflet {
        position: relative;
        width: 95%;
        }
      .datatables {
        font-family: 'Yusei Magic', sans-serif;
        background-color: white;
      }
      .shiny-html-output .shiny-bound-output {
          /*width: 300px;
          max-width: 100%;*/
          box-sizing: border-box;
          display: flex;
          flex: 1;
          
        }
      .input_data_row_w {
        display: flex; 
        flex-direction: column;
        justify-content: space-between;
      }")) #padding: 6px 12px; padding-top: 0.35em;
  ),
  titlePanel("Weather"),
  fluidRow(
    column(6, 
           selectInput(
             inputId = "county", label = "County", 
             choices = uniq_counties, 
             selected = uniq_counties[1])),
    column(6, 
           conditionalPanel(
             "input.county != 'ALL'",
             uiOutput(fill = "item",
                      "wards_selector_ui"
             ))
    ),
  ),
  br(),
  plotOutput('gg_plot'),
  absolutePanel(
    id = "leaf_map_panel", 
    class = "panel panel-default", fixed = TRUE,
    draggable = TRUE, top = 10, left = "auto", right = 100, bottom = "auto",
    width = 200, height = 150, style = "opacity: 0.5;",
    leafletOutput('map')
    #plotOutput('map')
  ),
  br(),
  DT::dataTableOutput("weather_table")
)

server <- function(input, output, session) {
  #
  county_layer <- reactive({
    #
    pattern_to_search = stringr::str_sub(input$county, 1,4)
    #
    if (grepl(pattern = "ALL" , x = input$county)){
      kenya_county
    } else {
      kenya_caw |> 
        filter(
          grepl(ignore.case = T,
                pattern = pattern_to_search, 
                x = county)
        )
    }
  })
  #
  ward_layer <- reactive({
    # print(county_layer())
    ward_name_ii = gsub("[[:punct:]]+",'',current_ward_name()) |> 
      trimws() |> tolower()
    county_layer() |> 
      mutate(
        ward_str = gsub("[[:punct:]]+",'',ward) |> 
          trimws() |> tolower()
      ) |>
      filter(
        ward_str == ward_name_ii
      ) |> select(-ward_str)
    # ward_name_i
  })
  #
  county_ward_data = reactive({
    county_subcounty_ward |>
      filter(
        county == input$county
      ) |> 
      select(ward, ward_code)
  })
  #
  ward_code = reactive({
    req(current_ward_name())
    #
    county_ward_data() |> 
      filter(ward == current_ward_name()) |> 
      (\(x) x$ward_code)()
  })
  #
  ward_data = reactive({
    req(ward_code())
    # print(ward_layer())
    forecast_collective_url = "https://kaop.co.ke/weather_api//forecast_collective"
    json_data = jsonlite::toJSON(
      list(
        "dataSrcId" = "4",
        "wardCode" = ward_code()
      ),
      pretty = T, auto_unbox = T
    )
    # print(json_data)
    resp = POST(
      forecast_collective_url,
      body =json_data
    )
    data_final = jsonlite::fromJSON(
      content(resp, 'text', encoding = 'UTF-8'))$data
    # print(data_final)
    data_final |> 
      select(date:humidity) |>
      mutate(
        # dataSrcId = "4",
        date = ymd(date)
      )
    #   
  })
  #
  # Reactive value to store the current ward index
  current_ward_i <- reactiveVal(1)
  
  ## Render the ward navigation UI conditionally
  output$wards_selector_ui <- renderUI({
    #
    fluidRow(
      class = "input_data_row_w",
      tags$label("Ward"),
      # br(),
      tags$div(
        column(
          2, offset = 0,
          actionButton("prev_ward", label = "", icon = icon("chevron-left")),
          style='width: fit-content; display : flex; align-items : center;'
        ),
        column(
          8, offset = 0,
          textOutput(outputId = "current_ward"),
          style='width: fit-content; max-width: 300px;  text-align: center; vertical-align:middle; line-height: 33.333px; box-sizing: border-box; display: flex; border-radius: 4px; background-color: #f2f2f2; color: black;'
          #style='box-sizing: border-box; border-radius: 4px; display: flex; background-color: #f2f2f2; color: black;'
        ),
        column(
          2, offset = 0,
          actionButton("next_ward", label = "", icon = icon("chevron-right")),
          style = "width: fit-content; display : flex; align-items : center;"
        ), style = 'padding: 0 !important; display: flex; justify-content: space-between;')
    )
  })
  #
  # Update the current ward display
  output$current_ward <- renderText({
    county_ward_data()$ward[current_ward_i()]
  })
  
  # Reactive value to store the current city name
  current_ward_name <- reactive({
    county_ward_data()$ward[current_ward_i()]
  })
  
  # Handle previous ward button click
  observeEvent(input$prev_ward, {
    if (current_ward_i() > 1) {
      current_ward_i(current_ward_i() - 1)
    }
  })
  
  # Handle next ward button click
  observeEvent(input$next_ward, {
    if (current_ward_i() < length( county_ward_data()$ward )) {
      current_ward_i(current_ward_i() + 1)
    }
  })
  #
  output$map <-  renderLeaflet({
    #
    leaflet(options = leafletOptions(zoomControl = FALSE)) |>
      addProviderTiles(
        providers$CartoDB.Positron,
        options = providerTileOptions(opacity = 0)
      ) |>
      addTiles() |>
      addPolygons(
        data = county_layer(),
        stroke = T,
        color = 'black', weight = 1, layerId = ~uid)
  })
  #
  output$gg_plot <- renderPlot({
    ward_data() |> 
      ggplot() +
      geom_col(
        aes(x= date, y = rainfall),
        position = position_dodge()
      ) + 
      geom_line(aes(x = date, y = humidity/1), color = 'blue') +
      geom_point(aes(x = date, y = humidity/1), shape = 8, color = 'blue') +
      geom_text(aes(x = date, y = rainfall,
                    label = rainfall),
                vjust = -1) +
      scale_y_continuous(
        name = "Rainfall (mm)",
        sec.axis = sec_axis(
          ~.*1,
          name = "Humidity (%)"
        )) +
      scale_x_date(
        date_breaks = "1 day",
        expand = c(0,0),
        date_labels = "%d-%m-%y") +
      theme_minimal(base_size = 18) +
      theme(
        panel.grid.minor.x = element_blank(),
        axis.line.y = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.y.right = element_text(angle = 90),
        axis.title.x = element_blank(),
        axis.text.x = element_text(face = "bold", size = rel(0.7)),
        axis.ticks = element_line()
      )
  })
  #
  observeEvent({
    input$county
    input$prev_ward
    input$next_ward}, {
    req(input$county != "ALL")
   #
    leafletProxy("map") |>
      removeShape(layerId = "selected") |>
      clearMarkers() |>
      addPolygons(
        stroke = TRUE, weight = 2, color = "red",
        fill = 'blue', opacity = 1,
        data = ward_layer(),
        layerId = "selected"
      )
    })
  #
  output$weather_table <- DT::renderDT({
    #
    ward_data() |>
      column_to_rownames(var = 'date') |>
      t() |>
      datatable(
        options = list(dom = 't', scrollX = TRUE),
        width = '100%', height = '100%' #table remains in container
      ) |>
      DT::formatRound(columns = 1:15,2) |>
      DT::formatStyle(0:25, backgroundColor = "white", opacity = 1)
  })
  
}

shinyApp(ui, server)


