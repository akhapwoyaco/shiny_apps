library(shiny)
library(readr)
library(tidyverse)
library(bslib)
library(ggrepel)
#
australian_weather_202412281334 <- read_csv("data/australian_weather_202412281334.csv")
colnames(australian_weather_202412281334) <- c(
  "date", "day", "minimum_temperature_c", "maximum_temperature_c", 
  "rainfall_mm", "evaporation_mm", "sunshine_hours", 
  "direction_of_maximum_wind_gust", "speed_of_maximum_wind_gust_km_h", 
  "time_of_maximum_wind_gust", "temperature_c_9am", 
  "relative_humidity_percent_9am" , "cloud_amount_oktas_9am", 
  "wind_direction_9am", "wind_speed_km_h_9am", "msl_pressure_h_pa_9am", 
  "temperature_c_3pm", "relative_humidity_percent_3pm", 
  "cloud_amount_oktas_3pm", "wind_direction_3pm", "wind_speed_km_h_3pm",
  "msl_pressure_h_pa_3pm", "territory", "location", "yyyymm"
)
unique_territory = unique(australian_weather_202412281334$territory)
# (australian_weather_202412281334)
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
        .navbar-brand {
          margin-right: 0;
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
          .container-fluid {
            width: auto; /*have header at center of page*/
            }
        "))),
  title = "Australian Weather: Daily Weather Observations (11/2023-12/2024)",
  sidebar = sidebar(
    bg = "white",
    accordion(
      accordion_panel(
        title = "Territory And Location",
        list(
          selectInput(
            label = "Territory", inputId =  'terri_tory', 
            choices = unique_territory)
          ,
          uiOutput("ui_location")
        )
      )
    )),
  accordion(
    
    open = NULL,
    accordion_panel(
      "Temperature",
      plotOutput('temperature_plot', height = '600px')
    ),
    accordion_panel(
      "Rainfall",
      plotOutput('rainfall_plot', height = '600px')
    ),
    accordion_panel(
      "Humidity",
      plotOutput('humidity_plot', height = '600px')
    ),
    accordion_panel(
      "Boxplot: Evaporation, Rain, Sunshine & Temperature",
      plotOutput('temp_rain_boxplot', height = '700px')
    ),
    accordion_panel(
      "Boxplot: Evaporation, Rain, Sunshine & Temperature (9am - 3pm)",
      plotOutput('temp_rain_boxplot_9am_3pm', height = '800px')
    )),
  div(
    class = "footer",
    style='height:50px;background:gray54;margin-top:auto;margin-bottom:auto;
                    text-align:center;',
    HTML(
      '<footer class="footer">
      Data Source: http://www.bom.gov.au/climate/dwo/index.shtml &nbsp; <br>
              Copyright &copy; 2025 &nbsp;
              Github Account: <a href="https://github.com/akhapwoyaco"
              target="_blank">akhapwoyaco</a>
              </footer>'
    )
  )
)
server <- function(input, output, session) {
  #' We have the county name, now get data on county
  territory_data <- reactive({
    req(input$terri_tory)
    australian_weather_202412281334 |>
      filter(
        territory %in% input$terri_tory) |>
      select(!(territory))
  })
  #' get unique groupings for grouping and update selector
  output$ui_location <- renderUI(
    {req(input$terri_tory)
      #
      location_unique <- territory_data() |>
        distinct(location) |> pull()
      selectInput(
        label = "Location", inputId =  'loca_tion',
        selectize = F, multiple = F, 
        choices = location_unique)
    })
  # plotting data
  australian_plot_data <- reactive({
    
    australian_weather_202412281334[,c(1,3:7,11:13, 16:19, 22, 24)] |>
      filter(location == input$loca_tion) #Subset numerical values and location
  })
  australian_plot_data_2 <- reactive({
    australian_plot_data() |> select(-location) |>
      pivot_longer(
        cols = !date, names_to = "variables",
        values_to = "Value" ) |>
      mutate(variables = str_replace_all(variables, c("_" = " ")))
  })
  # temperature
  australian_temperature <- reactive({
    australian_plot_data_2() |> 
      filter(grepl("temp", variables)) 
  })
  
  temp_plot <- reactive({
    req(input$terri_tory, input$loca_tion)
    # ensure df is non empty, else message
    validate(
      need(nrow(australian_temperature()) > 0, message = "NO DATA")
    )
    validate(
      need(nrow(australian_temperature()) != sum(is.na(australian_temperature()$Value)), message = "NO DATA")
    )
    #get maximum and minimum temperature data and the observation number
    data_geom_text = data.frame(
      date_min_temp = subset(
        australian_temperature(), 
        australian_temperature()$Value == min(australian_temperature()$Value,
                                              na.rm = T))$date[1]
      ,point_min_temp = subset(
        australian_temperature(), 
        australian_temperature()$Value == min(australian_temperature()$Value,
                                              na.rm = T))$Value[1]
      #
      ,date_max_temp = subset(
        australian_temperature(), 
        australian_temperature()$Value == max(australian_temperature()$Value,
                                              na.rm = T))$date[1]
      ,point_max_temp = subset(
        australian_temperature(), 
        australian_temperature()$Value == max(australian_temperature()$Value,
                                              na.rm = T))$Value[1]
    )
    # print(data_geom_text)
    #
    australian_temperature() |> 
      ggplot(aes(x = date, y = Value, color = variables)) +
      geom_point(aes(shape = variables), size = 0.5) +
      geom_line() + theme_bw() + 
      labs(x = "Day",
           caption = "https://github.com/akhapwoyaco") +
      geom_point(
        data = data_geom_text, aes(x = date_min_temp, y = point_min_temp), 
        color = "black") +
      geom_text(
        data = data_geom_text,
        aes(x = date_min_temp, y = point_min_temp,
            label = paste("Min Temp:", point_min_temp, "Day:", date_min_temp)), 
        color = "black"#, vjust = 0.5, hjust = -0.1
      ) +
      scale_x_date(date_breaks = "1 month", date_labels = "%b %Y") +
      geom_point(
        data = data_geom_text, 
        aes(x = date_max_temp, y = point_max_temp), color = "black") +
      geom_text(
        data = data_geom_text,
        aes(x = date_max_temp, y = point_max_temp,
            label = paste("Max Temp:", point_max_temp, "Day:", date_max_temp)), 
        color = "black", vjust = 0.5, hjust = -0.1, size = 3) + 
      scale_y_continuous(
        labels = scales::unit_format(unit = "°C")
      ) +
      guides(
        colour = guide_legend(override.aes = list(linewidth = 3))
      ) +
      theme(
        axis.title = element_blank(),
        legend.position = 'top',
        legend.text = element_text(face = 'bold', size = rel(1.2)),
        legend.background = element_blank(),
        legend.key = element_blank(),
        legend.title = element_blank()
      )
  })
  output$temperature_plot <- renderPlot({
    temp_plot()
  })
  
  
  # Humidity
  australian_humidity <- reactive({
    australian_plot_data_2() |> 
      filter(grepl("humidity", variables))
  })
  humid_plot <- reactive({
    validate(
      need(nrow(australian_humidity()) > 0, message = "NO DATA")
    )
    validate(
      need(nrow(australian_humidity()) != sum(is.na(australian_humidity()$Value)), message = "NO DATA")
    )
    #get maximum and minimum humidity values and the observation number
    data_geom_text = data.frame(
      date_min_humid = subset(
        australian_humidity(), 
        australian_humidity()$Value == min(australian_humidity()$Value,
                                           na.rm = T))$date[1]
      ,point_min_humid = subset(
        australian_humidity(), 
        australian_humidity()$Value == min(australian_humidity()$Value, 
                                           na.rm = T))$Value[1]
      #
      ,date_max_humid = subset(
        australian_humidity(), 
        australian_humidity()$Value == max(australian_humidity()$Value, 
                                           na.rm = T))$date[1]
      ,point_max_humid = subset(
        australian_humidity(), 
        australian_humidity()$Value == max(australian_humidity()$Value,
                                           na.rm = T))$Value[1]
    )
    
    #plot
    australian_humidity() |> 
      ggplot(aes(x = date, y = Value, color = variables)) +
      geom_point(aes(shape = variables), size = 0.5) +
      geom_line() + theme_bw() + 
      labs(x = "Day",
           caption = "https://github.com/akhapwoyaco") +
      geom_point(data=data.frame(x=date_min_humid,y=point_min_humid), 
                 aes(x = x, y=y), color = "black") +
      geom_text(
        data = data_geom_text,
        aes(x = date_min_humid, y = point_min_humid,
            label = paste("Min humid:", point_min_humid, "Day", date_min_humid)),
        color = "black", vjust = 0.5, hjust = 1
      ) +
      geom_point(x = date_max_humid, y = point_max_humid, color = "black") +
      scale_x_date(date_breaks = "1 month", date_labels = "%b %Y") +
      geom_text(
        data = data_geom_text,
        aes(x = date_max_humid, y = point_max_humid,
            label = paste("Max humid:", point_max_humid, "Day:", date_max_humid)
        ), 
        color = "black", vjust = 0.5, hjust = -0.1) + 
      scale_y_continuous(
        labels = scales::unit_format(unit = "%")
      ) +
      theme(
        axis.title = element_blank(),
        legend.position = 'top',
        legend.text = element_text(face = 'bold', size = rel(1.2)),
        legend.background = element_blank(),
        legend.key = element_blank(),
        legend.title = element_blank()
      )
  })
  output$humidity_plot <- renderPlot({
    humid_plot()
  })
  # Rainfall
  australian_Rainfall <- reactive({
    australian_plot_data_2() |> 
      filter(grepl("rain", variables))
  })
  rain_plot <- reactive({
    validate(
      need(nrow(australian_Rainfall()) > 0, message = F)
    )
    validate(
      need(nrow(australian_Rainfall()) != sum(is.na(australian_Rainfall()$Value)), message = "NO DATA")
    )
    #get maximum and minimu pressure and observation id
    data_geom_text = data.frame(
      date_min_Rainfall = subset(
        australian_Rainfall(), 
        australian_Rainfall()$Value == min(australian_Rainfall()$Value,
                                           na.rm = T))$date[1]
      ,point_min_rain = subset(
        australian_Rainfall(), 
        australian_Rainfall()$Value == min(australian_Rainfall()$Value, 
                                           na.rm = T))$Value[1]
      #
      ,date_max_rain = subset(
        australian_Rainfall(), 
        australian_Rainfall()$Value == max(australian_Rainfall()$Value, 
                                           na.rm = T))$date[1]
      ,point_max_rain = subset(
        australian_Rainfall(), 
        australian_Rainfall()$Value == max(australian_Rainfall()$Value, 
                                           na.rm = T))$Value[1]
      #
    )
    #
    # View(australian_Rainfall())
    #
    australian_Rainfall() |> 
      ggplot(aes(x = date, y = Value, color = variables)) +
      geom_point(aes(shape = variables), size = 0.5) +
      geom_line() + theme_bw() + 
      labs(x = "Day",
           caption = "https://github.com/akhapwoyaco") +
      geom_point(data=data.frame(x=date_min_Rainfall,y=point_min_rain), 
                 aes(x = x, y=y), color = "black") +
      geom_text(
        data = data_geom_text,
        aes(
          x = date_min_Rainfall, y = point_min_rain,
          label = paste("Min Ranfall:", point_min_rain, "Day", date_min_Rainfall)
        ), 
        color = "black", vjust = 0.5, hjust = -0.1) +
      geom_point(x = date_max_rain, y = point_max_rain, color = "black") +
      geom_text(
        data = data_geom_text,
        aes(x = date_max_rain, y = point_max_rain,
            label = paste("Max Rainfall:", point_max_rain, "Day:", date_max_rain)
        ), 
        color = "black", vjust = 0.5, hjust = -0.1)  +
      scale_x_date(date_breaks = "1 month", date_labels = "%b %Y") +
      scale_y_continuous(
        labels = scales::unit_format(unit = 'mm')
      ) +
      theme(
        axis.title = element_blank(),
        legend.position = 'none'#,
        # legend.text = element_text(face = 'bold', size = rel(1.2)),
        # legend.background = element_blank(),
        # legend.key = element_blank(),
        # legend.title = element_blank()
      )
  })
  #
  output$rainfall_plot <- renderPlot({
    rain_plot()
  })
  # temp rain monthly boxplot
  australian_monthly_data_agg_long <- reactive({
    australian_plot_data() |> 
      mutate(
        month = month(date, label = T, abbr = T)
      ) |>
      select(-date, -location) |>
      pivot_longer(
        cols = !month,
        names_to = "variables",
        values_to = "Value" ) |>
      drop_na() |>
      mutate(variables = str_replace_all(variables, c("_" = " ")))
  })
  #
  box_plots_temp_rain_plots <- reactive({
    am_pm = australian_monthly_data_agg_long() |>
      filter(!grepl("9am|3pm", variables))
    
    validate(
      need(nrow(am_pm) > 0, message = "NO DATA")
    )
    validate(
      need(nrow(am_pm) != sum(is.na(am_pm$Value)), message = "NO DATA")
    )
    am_pm |>
      group_by(variables) |>
      ggplot(aes(x = Value, y = month)) +
      geom_boxplot() + theme_bw() +
      theme(
        axis.title = element_blank(),
        strip.text = element_text(face = 'bold', size = rel(1.2)),
        strip.background = element_rect(
          fill = 'lightblue', color = 'black', size = 1)) +
      facet_wrap(~variables, scales = "free_x", dir = "h") +
      labs(caption = "https://github.com/akhapwoyaco")
  })
  #
  output$temp_rain_boxplot <- renderPlot({
    box_plots_temp_rain_plots()
  })
  #
  temp_rain_boxplot_9am_3pm_plots <- reactive({
    
    am_pm = australian_monthly_data_agg_long() |>
      filter(grepl("9am|3pm", variables))
    # print(am_pm)
    # validation
    validate(
      need(nrow(am_pm) > 0, message = "NO DATA")
    )
    validate(
      need(nrow(am_pm) != sum(is.na(am_pm$Value)), message = "NO DATA")
    )
    am_pm |> group_by(variables) |>
      ggplot(aes(x = Value, y = month)) +
      geom_boxplot() + theme_bw() +
      theme(
        axis.title = element_blank(),
        strip.text = element_text(face = 'bold', size = rel(1.2)),
        strip.background = element_rect(
          fill = 'lightblue', color = 'black', size = 1)) +
      facet_wrap(~variables, scales = "free_x", dir = "h") +
      labs(caption = "https://github.com/akhapwoyaco")
  })
  #
  output$temp_rain_boxplot_9am_3pm <- renderPlot({
    temp_rain_boxplot_9am_3pm_plots()
  })
}

shinyApp(ui, server)