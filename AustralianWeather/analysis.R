#
library(readr)
australian_weather_202412281334 <- read_csv("data-raw/australian_weather_202412281334.csv")
(australian_weather_202412281334)
#
library(psych)
describe(australian_weather_202412281334)
#
# Correlation
australian_corr_data <- australian_weather_202412281334[,c(3:7,11:13, 16:19, 22)] #Subset the numerical values
# exclude mossing observations
australian_corr_data <- australian_corr_data[complete.cases(australian_corr_data),] 
#
library(ggplot2)
library(reshape) # for melting data to long format
corr_mat <- round(cor(australian_corr_data),2) # find correlation and round values
australian_corr_mat_melt <- melt(corr_mat) |>  #Data to long format
  mutate(
    X1 = str_replace_all(X1, c("x3" = '3', 'x9' = '9')),
    X2 = str_replace_all(X2, c("x3" = '3', 'x9' = '9'))
  )
corr_plot <- ggplot(australian_corr_mat_melt, aes(x = X1, y = X2, fill = value)) +
  geom_tile() + 
  geom_text(aes(x = X1, y = X2, label = value)) + 
  theme_bw() + 
  labs(caption = "https://github.com/akhapwoyaco") +
  theme(axis.text.x = element_text(size = 9,angle = 90,face = "bold"),
        axis.title = element_blank(),
        legend.position = "none",
        axis.text.y = element_text(size = 9,face = "bold"))
corr_plot
#
ggsave(
  corr_plot, height = 30, units = "cm",
  filename = "corr_plot.png", width = 30)
#
#
#
library(tidyverse)
library(magrittr)
australian_plot_data <- australian_weather_202412281334[,c(1,3:7,11:13, 16:19, 22, 24)] |>
  filter(location == "Canberra") #Subset the numerical values and location Canberra
australian_plot_data_2 <- australian_plot_data |> 
  select(-location) |>
  pivot_longer(
    cols = !date,
    names_to = "variables",
    values_to = "Value" ) |>
  mutate(
    variables = str_replace_all(variables, c("x3" = '3', 'x9' = '9')))

# temperatures
australian_temperature <- australian_plot_data_2 |> 
  filter(grepl("temp", variables)) 
#get maximum and minimum temperature data and the observation number
date_min_temp = subset(australian_temperature, 
                       australian_temperature$Value == min(australian_temperature$Value))$date[1]
point_min_temp = subset(australian_temperature, 
                        australian_temperature$Value == min(australian_temperature$Value))$Value[1]
#
date_max_temp = subset(australian_temperature, 
                       australian_temperature$Value == max(australian_temperature$Value))$date[1]
point_max_temp = subset(australian_temperature, 
                        australian_temperature$Value == max(australian_temperature$Value))$Value[1]
#
temperature_plot <- australian_temperature |> 
  ggplot(aes(x = date, y = Value, color = variables)) +
  geom_point(aes(shape = variables), size = 0.5) +
  geom_line() + theme_bw() + 
  labs(x = "Day",
                                  caption = "https://github.com/akhapwoyaco") +
  geom_point(x = date_min_temp, y = point_min_temp, color = "black") +
  geom_text(aes(x = date_min_temp, y = point_min_temp), color = "black",
            label = paste("Min Temp:", point_min_temp, "Day:", date_min_temp), 
            vjust = 0.5, hjust = -0.1) +
  scale_x_date(date_breaks = "1 month", date_labels = "%b %Y") +
  geom_point(x = date_max_temp, y = point_max_temp, color = "black") +
  geom_text(aes(x = date_max_temp, y = point_max_temp), color = "black",
            label = paste("Max Temp:", point_max_temp, "Day:", date_max_temp), 
            vjust = 0.5, hjust = -0.1, size = 3) + 
  theme(
    axis.title = element_blank(),
    legend.position = 'inside',
    legend.position.inside = c(0.2,0.2),
    legend.background = element_blank(),
    legend.key = element_blank(),
    legend.title = element_blank()
  )
temperature_plot


australian_humidity <- australian_plot_data_2 |> 
  filter(grepl("humidity", variables))
#get maximum and minimum humidity values and the observation number
date_min_humid = subset(australian_humidity, 
                        australian_humidity$Value == min(australian_humidity$Value, na.rm = T))$date[1]
point_min_humid = subset(australian_humidity, 
                         australian_humidity$Value == min(australian_humidity$Value, na.rm = T))$Value[1]
#
date_max_humid = subset(australian_humidity, 
                        australian_humidity$Value == max(australian_humidity$Value, na.rm = T))$date[1]
point_max_humid = subset(australian_humidity, 
                         australian_humidity$Value == max(australian_humidity$Value, na.rm = T))$Value[1]
#plot
humidity_plot <- australian_humidity |> 
  ggplot(aes(x = date, y = Value, color = variables)) +
  geom_point(aes(shape = variables), size = 0.5) +
  geom_line() + theme_bw() + 
  labs(x = "Day",
                                  caption = "https://github.com/akhapwoyaco") +
  geom_point(data=data.frame(x=date_min_humid,y=point_min_humid), 
             aes(x = x, y=y), color = "black") +
  geom_text(aes(x = date_min_humid[1], y = point_min_humid[1]), color = "black",
            label = paste("Min humid:", point_min_humid, "Day", date_min_humid), 
            vjust = 0.5, hjust = 1) +
  geom_point(x = date_max_humid, y = point_max_humid, color = "black") +
  scale_x_date(date_breaks = "1 month", date_labels = "%b %Y") +
  geom_text(aes(x = date_max_humid, y = point_max_humid), color = "black",
            label = paste("Max humid:", point_max_humid, "Day:", date_max_humid), 
            vjust = 0.5, hjust = -0.1) + 
  theme(
    axis.title = element_blank(),
    legend.position = c(0.1,0.1),
    legend.background = element_blank(),
    legend.key = element_blank(),
    legend.title = element_blank()
  )
humidity_plot


# Rainfall
australian_Rainfall <- australian_plot_data_2 |> 
  filter(grepl("rain", variables))
#get maximum and minimu pressure and observation id
date_min_Rainfall = subset(australian_Rainfall, 
                           australian_Rainfall$Value == min(australian_Rainfall$Value, na.rm = T))$date[1]
point_min_rain = subset(australian_Rainfall, 
                        australian_Rainfall$Value == min(australian_Rainfall$Value, na.rm = T))$Value[1]
#
date_max_rain = subset(australian_Rainfall, 
                       australian_Rainfall$Value == max(australian_Rainfall$Value, na.rm = T))$date[1]
point_max_rain = subset(australian_Rainfall, 
                        australian_Rainfall$Value == max(australian_Rainfall$Value, na.rm = T))$Value[1]
#
australian_Rainfall_plot <- australian_Rainfall |> 
  ggplot(aes(x = date, y = Value, color = variables)) +
  geom_point(aes(shape = variables), size = 0.5) +
  geom_line() + theme_bw() + 
  labs(x = "Day",
       caption = "https://github.com/akhapwoyaco") +
  geom_point(data=data.frame(x=date_min_Rainfall,y=point_min_rain), 
             aes(x = x, y=y), color = "black") +
  geom_text(aes(x = date_min_Rainfall[1], y = point_min_rain[1]), color = "black",
            label = paste("Min Ranfall:", point_min_rain, "Day", date_min_Rainfall), 
            vjust = 0.5, hjust = -0.1) +
  geom_point(x = date_max_rain, y = point_max_rain, color = "black") +
  geom_text(aes(x = date_max_rain, y = point_max_rain), color = "black",
            label = paste("Max Rainfall:", point_max_rain, "Day:", date_max_rain), 
            vjust = 0.5, hjust = -0.1)  +
  scale_x_date(date_breaks = "1 month", date_labels = "%b %Y") +
  theme(
    axis.title = element_blank(),
    legend.position = c(0.9,0.8),
    legend.background = element_blank(),
    legend.key = element_blank(),
    legend.title = element_blank()
  )
australian_Rainfall_plot


#
library(gridExtra)
library(cowplot)
box_australian_plot_data <- australian_plot_data_2 |> 
  filter(!grepl("pressure", variables)) |> 
  group_by(variables) |> 
  ggplot(aes(x = Value, y = variables)) +
  geom_boxplot() + theme_bw()  +
  labs(caption = "https://github.com/akhapwoyaco") + 
  theme(axis.title = element_blank())

box_australian_plot_data_press <- australian_plot_data_2 |> 
  filter(grepl("pressure", variables)) |> 
  group_by(variables) |> 
  ggplot(aes(x = Value, y = variables)) +
  geom_boxplot() + theme_bw() + 
  labs(caption = "https://github.com/akhapwoyaco") +
  theme(axis.title = element_blank())
annual_box_plots <- plot_grid(box_australian_plot_data,box_australian_plot_data_press,nrow=2,
                              align = "v", rel_heights = c(3/4,1/4))
annual_box_plots


#
# australian_monthly_data <- australian_weather_data
# australian_monthly_data$month <- months.POSIXt(australian_monthly_data$Date)
# australian_monthly_data$month <- factor(
#   australian_monthly_data$month, 
#   levels = c("January","February","March","April","May","June","July",
#              "August","September","October","November","December"), 
#   labels = c("Jan","Feb","Mar","Apr","May","Jun","Jul",
#              "Aug","Sep","Oct","Nov","Dec"),
#   ordered = T)
# #
# australian_monthly_data <- australian_monthly_data[c(
#   "MinTemp","MaxTemp","Rainfall","Evaporation","Sunshine","WindGustSpeed",
#   "WindSpeed9am","WindSpeed3pm","Humidity9am","Humidity3pm","Pressure9am",
#   "Pressure3pm","Cloud9am","Cloud3pm","Temp9am","Temp3pm","month"
# )]
#head(australian_monthly_data)
australian_monthly_data_agg_long <- australian_plot_data |> 
  mutate(
    month = month(date, label = T, abbr = T)
  ) |>
  select(-date) |>
  pivot_longer(
    cols = !month,
    names_to = "variables",
    values_to = "Value" )
australian_monthly_data_agg_long |> 
  ggplot(aes(x = Value)) + 
  geom_histogram() + theme_bw()  +
  labs(caption = "https://github.com/akhapwoyaco") + 
  theme(axis.title = element_blank()) +
  facet_wrap(variables~., scales = "free")


#
box_plots_3am9pm <- australian_monthly_data_agg_long |> 
  filter(grepl("9am|3pm", variables))
box_plots_3am9pm_plots <- box_plots_3am9pm |> 
  group_by(variables) |> 
  ggplot(aes(x = Value, y = month) ) +
  geom_boxplot() + theme_bw() + 
  theme(axis.title = element_blank())  +
  labs(caption = "https://github.com/akhapwoyaco") +
  facet_wrap(~variables, scales = "free_x", dir = "h")
box_plots_3am9pm_plots


#
box_plots_temp_rain <- australian_monthly_data_agg_long |> 
  filter(!grepl("9am|3pm", variables))
box_plots_temp_rain_plots <- box_plots_temp_rain |> 
  group_by(variables) |> 
  ggplot(aes(x = Value, y = month)) +
  geom_boxplot() + theme_bw() + 
  theme(axis.title = element_blank()) +
  facet_wrap(~variables, scales = "free_x", dir = "h") +
  labs(caption = "https://github.com/akhapwoyaco")
box_plots_temp_rain_plots
#



