library(shiny)
library(readr)
library(tidyverse)
library(bslib)
library(ggrepel)
library(psych)
library(reshape) # f
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
          uiOutput("ui_location"),
          br(),
          # source https://github.com/rstudio/shiny-examples/blob/main/016-knitr-pdf/
          radioButtons('format', 'Document format', c('PDF', 'HTML', 'Word'),
                       inline = TRUE),
          br(),
          downloadButton('downloadReport'),
          br(),
          br(),
          actionButton(inputId='fiver', label="Fiver R Shiny Services", 
                       icon = icon("th"), 
                       onclick ="window.open('https://www.fiverr.com/s/jj5dYam', '_blank')")
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
      "Wind",
      plotOutput('wind_kmh_plot', height = '600px')
    ),
    accordion_panel(
      "Time of Maximum Wind Gust",
      plotOutput('wind_time_plot', height = '600px')
    ),
    accordion_panel(
      "Direction of Maximum Wind Gust",
      plotOutput('wind_dir_max_gust_plot', height = '600px')
    ),
    accordion_panel(
      "Pressure",
      plotOutput('pressure_hpa_plot', height = '600px')
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
      select(!(territory)) |>
      mutate(
        speed_of_maximum_wind_gust_km_h = as.numeric(speed_of_maximum_wind_gust_km_h)
      )
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
    
    australian_weather_202412281334[,c(1,3:7,9,11:13, 16:19, 22, 24,  8, 10)] |> # 15,21 wind speed calm define calm
      filter(location == input$loca_tion) #Subset numerical values and location
  })
  #
  australian_plot_data_1 <- reactive({
    australian_plot_data()
  })
  #
  australian_plot_data_2 <- reactive({
    australian_plot_data_1()  |> 
      select(-location, -time_of_maximum_wind_gust, -direction_of_maximum_wind_gust) |>
      pivot_longer(
        cols = !date, names_to = "variables",
        values_to = "Value" ) |>
      mutate(variables = str_replace_all(variables, c("_" = " ")))
  })
  # temperature ---------------------------------------------------------
  australian_temperature <- reactive({
    australian_plot_data_2() |> 
      filter(grepl("temp", variables)) 
  })
  
  temp_plot <- reactive({
    req(input$terri_tory, input$loca_tion)
    # ensure df is non empty, else message
    validate(
      need(nrow(australian_temperature()) > 0, message = "NO TEMPERATURE DATA")
    )
    validate(
      need(
        nrow(australian_temperature()) != sum(
          is.na(australian_temperature()$Value)
        ), message = "NO TEMPERATURE DATA")
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
  
  
  # Humidity -------------------------------------------------------------
  australian_humidity <- reactive({
    australian_plot_data_2() |> 
      filter(grepl("humidity", variables))
  })
  humid_plot <- reactive({
    validate(
      need(nrow(australian_humidity()) > 0, message = "NO HUMIDITY DATA")
    )
    validate(
      need(
        nrow(australian_humidity()) != sum(is.na(australian_humidity()$Value)
        ), message = "NO HUMIDITY DATA")
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
      geom_point(
        data = data_geom_text,
        aes(x = date_min_humid, y = point_min_humid), color = "black") +
      geom_text(
        data = data_geom_text,
        aes(x = date_min_humid, y = point_min_humid,
            label = paste("Min humid:", point_min_humid, "Day", date_min_humid)),
        color = "black", vjust = 0.5, hjust = 1
      ) +
      geom_point(
        data = data_geom_text,
        aes(x = date_max_humid, y = point_max_humid), color = "black") +
      scale_x_date(date_breaks = "1 month", date_labels = "%b %Y") +
      geom_text(
        data = data_geom_text,
        aes(x = date_max_humid, y = point_max_humid,
            label = paste("Max humid:", point_max_humid, "Day:", date_max_humid)
        ), 
        color = "black", vjust = 0.5, hjust = -0.1) + 
      scale_y_continuous(
        labels = scales::unit_format(unit = "%")
      ) + guides(
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
  output$humidity_plot <- renderPlot({
    humid_plot()
  })
  # Rainfall ------------------------------------------------------------
  australian_Rainfall <- reactive({
    australian_plot_data_2() |> 
      filter(grepl("rain", variables))
  })
  rain_plot <- reactive({
    validate(
      need(nrow(australian_Rainfall()) > 0, message = "NO RAINFALL DATA")
    )
    validate(
      need(nrow(australian_Rainfall()) != sum(is.na(australian_Rainfall()$Value)
      ), message = "NO RAINFALL DATA")
    )
    #get maximum and minimu pressure and observation id
    data_geom_text = data.frame(
      date_min_rain = subset(
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
      geom_point(
        data = data_geom_text, 
        aes(x=date_min_rain, y=point_min_rain), color = "black") +
      geom_text(
        data = data_geom_text,
        aes(
          x = date_min_rain, y = point_min_rain,
          label = paste("Min Rainfall:", point_min_rain, "Day", date_min_rain)
        ), 
        color = "black", vjust = 0.5, hjust = -0.1) +
      geom_point(
        data = data_geom_text, 
        aes(x=date_max_rain, y=point_max_rain), color = "black") +
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
  #
  # wind ----------------------------------------------------------------
  australian_wind <- reactive({
    australian_plot_data_2() |> 
      filter(grepl("wind", variables))
  })
  #
  wind_plot <- reactive({
    validate(
      need(nrow(australian_wind()) > 0, message = "NO MAXIMUM WIND SPEED DATA")
    )
    validate(
      need(
        nrow(australian_wind()) != sum(is.na(australian_wind()$Value)
        ), message = "NO MAXIMUM WIND SPEED DATA")
    )
    #get maximum and minimu pressure and observation id
    data_geom_text = data.frame(
      date_min_wind = subset(
        australian_wind(), 
        australian_wind()$Value == min(australian_wind()$Value,
                                       na.rm = T))$date[1]
      ,point_min_wind = subset(
        australian_wind(), 
        australian_wind()$Value == min(australian_wind()$Value, 
                                       na.rm = T))$Value[1]
      #
      ,date_max_wind = subset(
        australian_wind(), 
        australian_wind()$Value == max(australian_wind()$Value, 
                                       na.rm = T))$date[1]
      ,point_max_wind = subset(
        australian_wind(), 
        australian_wind()$Value == max(australian_wind()$Value, 
                                       na.rm = T))$Value[1]
      #
    )
    #
    australian_wind() |> 
      ggplot(aes(x = date, y = Value, color = variables)) +
      geom_point(aes(shape = variables), size = 0.5) +
      geom_line() + theme_bw() + 
      labs(x = "Day",
           caption = "https://github.com/akhapwoyaco") +
      geom_point(
        data = data_geom_text, 
        aes(x=date_min_wind, y=point_min_wind), color = "black") +
      geom_text(
        data = data_geom_text,
        aes(
          x = date_min_wind, y = point_min_wind,
          label = paste("Min Wind:", point_min_wind, "Day", date_min_wind)
        ), 
        color = "black", vjust = 0.5, hjust = -0.1) +
      geom_point(
        data = data_geom_text, 
        aes(x=date_max_wind, y=point_max_wind), color = "black") +
      geom_text(
        data = data_geom_text,
        aes(x = date_max_wind, y = point_max_wind,
            label = paste("Max Wind:", point_max_wind, "Day:", date_max_wind)
        ), 
        color = "black", vjust = 0.5, hjust = -0.1)  +
      scale_x_date(date_breaks = "1 month", date_labels = "%b %Y") +
      scale_y_continuous(
        labels = scales::unit_format(unit = 'km/hr')
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
  output$wind_kmh_plot <- renderPlot({
    wind_plot()
  })
  
  # Time of Maximum Wind Gust
  #
  australian_wind_maxtime <- reactive({
    australian_plot_data_1() |> 
      select(time_of_maximum_wind_gust, date) |>
      mutate(
        hour = lubridate::hour(time_of_maximum_wind_gust), 
        month = month(date, label = T, abbr = T),
        weekday = lubridate::wday(date, label = T)) |> 
      # select(hour, month) |> 
      group_by(weekday, hour) |> 
      summarise(n = n()) |>
      mutate(
        hour_day = case_when(
          hour == 0 ~ '12 am',
          hour > 0 & hour < 12 ~ paste(hour, 'am', sep = ' '),
          hour == 12 ~ '12 pm',
          hour > 12 ~ paste(hour-12, 'pm', sep = ' ')
        )) |>
      mutate(
        hour_day = factor(
          hour_day, 
          levels =  c("12 am","1 am","2 am","3 am","4 am","5 am",
                      "6 am","7 am", "8 am", "9 am","10 am","11 am",
                      "12 pm", "1 pm","2 pm","3 pm","4 pm","5 pm",
                      "6 pm","7 pm", "8 pm", "9 pm", "10 pm","11 pm") 
        )
      ) |> drop_na() 
  })
  wind_maxtime_plot <- reactive({
    validate(
      need(nrow(australian_wind_maxtime()) > 0, 
           message = "NO WIND GUST MAXIMUM TIME DATA")
    )
    validate(
      need(
        nrow(australian_wind_maxtime()) != sum(is.na(australian_wind_maxtime()$n)
        ), message = "NO WIND GUST MAXIMUM TIME DATA")
    )
    australian_wind_maxtime() |> 
      ggplot(aes(x = weekday, y = hour_day, fill = n)) + 
      geom_tile(color = 'white', linewidth = 0.1) + 
      geom_text(aes(label = n), fontface = 'bold',colour = 'black') +
      scale_fill_gradient(low = 'yellow', high = 'red') + 
      hrbrthemes::theme_modern_rc() +
      theme(
        legend.title = element_blank(),
        legend.position = 'none', 
        axis.text = element_text(face = 'bold', size = rel(1.5)),
        axis.title = element_blank()
      ) 
  })
  #
  output$wind_time_plot <- renderPlot({
    wind_maxtime_plot()
  })
  #
  # direction on maximum wind gust
  #
  australian_wind_direction <- reactive({
    australian_plot_data_1() |> 
      select(direction_of_maximum_wind_gust, date) |>
      mutate(
        month = month(date, label = T, abbr = T) 
      ) |> 
      group_by(month, direction_of_maximum_wind_gust) |> 
      summarise(n = n()) |> 
      mutate(
        direction_of_maximum_wind_gust = factor(
          direction_of_maximum_wind_gust, 
          levels =  c(
            "N", "NNE", "NE", "ENE", 
            "E", "ESE", "SE", "SSE", 
            "S", "SSW", "SW", "WSW",
            "W", "WNW", "NW", "NNW"
          )
        )
      ) |>
      drop_na() 
  })
  #
  wind_direction_plot <- reactive({
    #
    validate(
      need(nrow(australian_wind_direction()) > 0,
           message = "NO MAXIMUM WIND DIRECTION DATA")
    )
    validate(
      need(
        nrow(australian_wind_direction()) != sum(
          is.na(australian_wind_direction()$n)
        ), message = "NO MAXIMUM WIND DIRECTION DATA")
    )
    australian_wind_direction() |> 
      ggplot(aes(x = month, y = direction_of_maximum_wind_gust, fill = n)) + 
      geom_tile(color = 'white', linewidth = 0.1) + 
      geom_text(aes(label = n), fontface = 'bold', colour = 'black') +
      scale_fill_gradient(low = 'yellow', high = 'red') + 
      hrbrthemes::theme_modern_rc() +
      theme(
        legend.title = element_blank(),
        legend.position = 'none', 
        axis.text = element_text(face = 'bold', size = rel(1.5)),
        axis.title = element_blank()
      )
  })
  #
  output$wind_dir_max_gust_plot <- renderPlot({
    wind_direction_plot()
  })
  #
  # PRESSURE ------------------------------------------------
  #
  australian_pressure <- reactive({
    australian_plot_data_2() |> 
      filter(grepl("pressure", variables))
  })
  #
  pressure_plot <- reactive({
    validate(
      need(nrow(australian_pressure()) > 0, message = "NO PRESSURE DATA")
    )
    validate(
      need(
        nrow(australian_pressure()) != sum(is.na(australian_pressure()$Value)
        ), message = "NO PRESSURE DATA")
    )
    #get maximum and minimu pressure and observation id
    data_geom_text = data.frame(
      date_min_pressure = subset(
        australian_pressure(), 
        australian_pressure()$Value == min(australian_pressure()$Value,
                                           na.rm = T))$date[1]
      ,point_min_pressure = subset(
        australian_pressure(), 
        australian_pressure()$Value == min(australian_pressure()$Value, 
                                           na.rm = T))$Value[1]
      #
      ,date_max_pressure = subset(
        australian_pressure(), 
        australian_pressure()$Value == max(australian_pressure()$Value, 
                                           na.rm = T))$date[1]
      ,point_max_pressure = subset(
        australian_pressure(), 
        australian_pressure()$Value == max(australian_pressure()$Value, 
                                           na.rm = T))$Value[1]
    )
    #
    australian_pressure() |> 
      ggplot(aes(x = date, y = Value, color = variables)) +
      geom_point(aes(shape = variables), size = 0.5) +
      geom_line() + theme_bw() + 
      labs(x = "Day",
           caption = "https://github.com/akhapwoyaco") +
      geom_point(
        data = data_geom_text, 
        aes(x=date_min_pressure, y=point_min_pressure), color = "black") +
      geom_text(
        data = data_geom_text,
        aes(
          x = date_min_pressure, y = point_min_pressure,
          label = paste("Min pressure:", point_min_pressure, "Day", date_min_pressure)
        ), 
        color = "black", vjust = 0.5, hjust = -0.1) +
      geom_point(
        data = data_geom_text, 
        aes(x=date_max_pressure, y=point_max_pressure), color = "black") +
      geom_text(
        data = data_geom_text,
        aes(x = date_max_pressure, y = point_max_pressure,
            label = paste("Max pressure:", point_max_pressure, "Day:", date_max_pressure)
        ), 
        color = "black", vjust = 0.5, hjust = -0.1)  +
      scale_x_date(date_breaks = "1 month", date_labels = "%b %Y") +
      scale_y_continuous(
        labels = scales::unit_format(unit = 'hPa')
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
  #
  output$pressure_hpa_plot <- renderPlot({
    pressure_plot()
  })
  #
  # BOXPLOTS ---------------------------------------------------------
  # temp rain monthly boxplot
  australian_monthly_data_agg_long <- reactive({
    australian_plot_data() |> 
      select(-time_of_maximum_wind_gust, -direction_of_maximum_wind_gust) |>
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
  #
  
  
  #
  # Download Report
  output$downloadReport <- downloadHandler(
    filename = function() {
      paste(
        paste('DWO', input$terri_tory, input$loca_tion, sep = "_"),
        sep = '.', switch(
          input$format, PDF = 'pdf', HTML = 'html', Word = 'docx'
        ))
    },
    
    content = function(file) {
      src <- normalizePath('report.Rmd')
      
      # temporarily switch to the temp dir, in case you do not have write
      # permission to the current working directory
      owd <- setwd(tempdir())
      on.exit(setwd(owd))
      file.copy(src, 'report.Rmd', overwrite = TRUE)
      
      library(rmarkdown)
      out <- render('report.Rmd', switch(
        input$format,
        PDF = pdf_document(toc = TRUE, toc_depth = 4), 
        HTML = html_document(toc = TRUE, toc_depth = 4), 
        Word = word_document(toc = TRUE, toc_depth = 4)
      )
      )
      file.rename(out, file)
    }
  )
  #
}

shinyApp(ui, server)