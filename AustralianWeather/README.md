# WEBSCRAPPING: Australian Daily Weather

-   Scrapped from: <http://www.bom.gov.au/climate/dwo/index.shtml>
-   Scrapping Code: <https://github.com/akhapwoyaco/WEBSCRAPPING/tree/main/AustralianWeather>
-   Shiny App: <https://01940e82-7881-ac23-953c-68da09cea0f0.share.connect.posit.cloud/> 

### Data Details

| Heading |   | Meaning | Units |
|:-----------------|:------------------|:-----------------|------------------|
| Date |  | Day of the month |  |
| Day |  | Day of the week | first two letters |
| Temps | Min | Minimum temperature in the 24 hours to 9am. Sometimes only known to the nearest whole degree. | degrees Celsius |
|  | Max | Maximum temperature in the 24 hours from 9am. Sometimes only known to the nearest whole degree. | degrees Celsius |
| Rain |  | Precipitation (rainfall) in the 24 hours to 9am. Sometimes only known to the nearest whole millimetre. | millimetres |
| Evap |  | "Class A" pan evaporation in the 24 hours to 9am | millimetres |
| Sun |  | Bright sunshine in the 24 hours to midnight | hours |
| Max wind gust | Dirn | Direction of strongest gust in the 24 hours to midnight | 16 compass points |
|  | Spd | Speed of strongest wind gust in the 24 hours to midnight | kilometres per hour |
|  | Time | Time of strongest wind gust | local time hh:mm |
| 9 am | Temp | Temperature at 9 am degrees | Celsius |
|  | RH | Relative humidity at 9 am | percent |
|  | Cld | Fraction of sky obscured by cloud at 9 am | eighths |
|  | Dirn | Wind direction averaged over 10 minutes prior to 9 am | compass points |
|  | Spd | Wind speed averaged over 10 minutes prior to 9 am | kilometres per hour |
|  | MSLP | Atmospheric pressure reduced to mean sea level at 9 am | hectopascals |
| 3 pm | Temp | Temperature at 3 pm degrees | Celsius |
|  | RH | Relative humidity at 3 pm | percent |
|  | Cld | Fraction of sky obscured by cloud at 3 pm | eighths |
|  | Dirn | Wind direction averaged over 10 minutes prior to 3 pm compass | points |
|  | Spd | Wind speed averaged over 10 minutes prior to 3 pm kilometres per | hour |
|  | MSLP | Atmospheric pressure reduced to mean sea level at 3 pm | hectopascals |

### Shiny App

![Australian Weather](screen_app.png)
