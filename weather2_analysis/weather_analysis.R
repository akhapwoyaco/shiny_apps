#
url = "https://api.open-meteo.com/v1/forecast?latitude=37.59968&longitude=0.4002939&current=temperature_2m,wind_speed_10m&hourly=temperature_2m,relative_humidity_2m,wind_speed_10m"
#
mm = jsonlite::fromJSON(url) 
View(mm)
#
url = "https://archive-api.open-meteo.com/v1/era5?latitude=52.52&longitude=13.41&start_date=1990-01-01&end_date=2021-12-31&hourly=temperature_2m"
mm$hourly_units
mm$hourly_units$time
mm$hourly_units$temperature_2m
mm$hourly |> as.data.frame() |> View()
#

#
library(tidyverse)
library(hrbrthemes)
library(lubridate)
library(ggthemes)
#
mm$hourly |> as.data.frame() |>
  mutate(time = as.POSIXlt(time, format = "%Y-%m-%dT%H:%M")) |>
  mutate(
    week_day = wday(time, label = T),
    hour = lubridate::hour(time),
    month = lubridate::month(time, label = T, abbr = T)
  ) |> 
  mutate(
    hour_day = case_when(
      hour == 0 ~ '12 am',
      hour > 0 & hour < 12 ~ paste(hour, 'am', sep = ' '),
      hour == 12 ~ '12 pm',
      hour > 12 ~ paste(hour-12, 'pm', sep = ' ')
    )) |> mutate(
      hour_day = factor(
        hour_day,
        levels =  c("12 am","1 am","2 am","3 am","4 am","5 am",
                    "6 am","7 am", "8 am", "9 am","10 am","11 am",
                    "12 pm", "1 pm","2 pm","3 pm","4 pm","5 pm",
                    "6 pm","7 pm", "8 pm", "9 pm", "10 pm","11 pm")
      )) |>
  group_by(month, week_day, hour_day) |>
  summarise(average_rates = round(mean(temperature_2m), 1)) |>
  ggplot(
    aes(x = week_day, y = hour_day, fill = average_rates)
  ) +
  geom_tile(color = 'white', linewidth = 0.1) +
  geom_text(aes(label = average_rates), color = 'black', fontface ='bold') +
  scale_fill_gradient(low = 'yellow', high = 'red') +
  facet_wrap(.~month) +
  theme_excel_new() +#hrbrthemes::theme_modern_rc() +
  theme(
    strip.text = element_text(face = "bold", size = rel(1.5)),
    strip.background = element_rect(fill = 'lightblue'),
    legend.title = element_blank(),
    legend.position = 'none',
    axis.title = element_blank()
  )
#
mm$hourly |> as.data.frame() |>
  mutate(time = lubridate::date(time)) |>
  ggplot() +
  geom_line(
    aes(x = time, y = temperature_2m)
  )
# #
# ggsave(
#   hour_day_week_occupancy_plot, height = 15, units = "cm",
#   filename = "hour_day_week_occupancy_plot.jpeg", width = 25)
#
