#
library(rvest)
library(jsonlite)
library(tidyverse)
#
#
counties_data = jsonlite::fromJSON(
  '[{"county_code":"vvOK1BxTbet","county":"Baringo"}, 
  {"county_code":"HMNARUV2CW4","county":"Bomet"}, 
  {"county_code":"KGHhQ5GLd4k","county":"Bungoma"}, 
   {"county_code":"Tvf1zgVZ0K4","county":"Busia"},
   {"county_code":"MqnLxQBigG0","county":"Elgeyo-Marakwet"}, 
   {"county_code":"PFu8alU2KWG","county":"Embu"},
   {"county_code":"uyOrcHZBpW0","county":"Garissa"},
   {"county_code":"nK0A12Q7MvS","county":"Homa Bay"}, 
   {"county_code":"bzOfj0iwfDH","county":"Isiolo"}, 
   {"county_code":"Hsk1YV8kHkT","county":"Kajiado"}, 
   {"county_code":"BjC1xL40gHo","county":"Kakamega"}, 
   {"county_code":"ihZsJ8alvtb","county":"Kericho"}, 
   {"county_code":"qKzosKQPl6G","county":"Kiambu"}, 
   {"county_code":"nrI2khZx3d0","county":"Kilifi"}, 
   {"county_code":"Ulj33KBau7V","county":"Kirinyaga"}, 
   {"county_code":"sPkRcDvhGWA","county":"Kisii"},
   {"county_code":"tAbBVBbueqD","county":"Kisumu"}, 
   {"county_code":"j8o6iO4Njsi","county":"Kitui"}, 
   {"county_code":"N7YETT3A9r1","county":"Kwale"}, 
   {"county_code":"xuFdFy6t9AH","county":"Laikipia"}, 
   {"county_code":"NjWSbQTwys4","county":"Lamu"}, 
   {"county_code":"yhCUgGcCcOo","county":"Machakos"}, 
   {"county_code":"BoDytkJQ4Qi","county":"Makueni"}, 
   {"county_code":"R6f9znhg37c","county":"Mandera"}, 
   {"county_code":"Eey8fT4Im3y","county":"Marsabit"},
   {"county_code":"Y52XNJ50hYb","county":"Meru"}, 
   {"county_code":"fVra3Pwta0Q","county":"Migori"}, 
   {"county_code":"wsBsC6gjHvn","county":"Mombasa"}, 
   {"county_code":"ahwTMNAJvrL","county":"Muranga"},
   {"county_code":"jkG3zaihdSs","county":"Nairobi"}, 
   {"county_code":"ob6SxuRcqU4","county":"Nakuru"}, 
   {"county_code":"t0J75eHKxz5","county":"Nandi"}, 
   {"county_code":"kqJ83J2D72s","county":"Narok"}, 
   {"county_code":"uepLTG8wGWJ","county":"Nyamira"}, 
   {"county_code":"mYZacFNIB3h","county":"Nyandarua"}, 
   {"county_code":"ptWVfaCIdVx","county":"Nyeri"}, 
   {"county_code":"o36zCRjSd4G","county":"Samburu"}, 
   {"county_code":"u4t9H8XyU9P","county":"Siaya"}, 
   {"county_code":"QyGNX2DpR4h","county":"Taita Taveta"}, 
   {"county_code":"JsH2bnvNt2d","county":"Tana River"}, 
   {"county_code":"T4urHM47nlm","county":"Tharaka-Nithi"}, 
   {"county_code":"mThvosEflAU","county":"Trans Nzoia"}, 
   {"county_code":"kphDeKClFch","county":"Turkana"},
   {"county_code":"pZqQRRW7PHP","county":"Uasin Gishu"},
   {"county_code":"sANMZ3lpqGs","county":"Vihiga"}, 
   {"county_code":"CeLsrJOH0g9","county":"Wajir"}, 
  {"county_code":"XWALbfAPa6n","county":"West Pokot"}]',
  simplifyVector = !FALSE
)
#
counties_data
#
# SUB COUNTY CODES
sub_county_url = "https://kaop.co.ke/weather_api//subcounties?countyCode="
#
subcounties_data = data.frame()
len_counties = length(counties_data$county_code)
for (i in 41:len_counties){
  print(i)
  county_code = counties_data$county_code[i]
  print(county_code)
  url_subcounty_code = paste(
    sub_county_url, 
    county_code, sep = '')
  print(url_subcounty_code)
  #
  test_county = read_json(
    url_subcounty_code, 
    simplifyVector = TRUE
  )$data
  #
  # Concatenate the data DataFrame with the original DataFrame
  subcounties_data = rbind(
    subcounties_data, test_county)
}
#
subcounties_data
#
#  ward data
#
county_sub_county_ward_codes = subcounties_data |>
  mutate(
    county_sub_county_ward_url = paste(
      "?countyCode=", county_code,
      '&subCountyCode=', subcounty_code
      , sep = ''
    )
  )
#
head(county_sub_county_ward_codes)
#
ward_url = "https://kaop.co.ke/weather_api//wards"
wards_data = data.frame()
len_wards = length(county_sub_county_ward_codes$county_sub_county_ward_url)
#
for (i in 206:len_wards){
  print(i)
  c_s_code = county_sub_county_ward_codes$county_sub_county_ward_url[i]
  print(c_s_code)
  url_ward_code = paste(
    ward_url, 
    c_s_code, sep = '')
  print(url_ward_code)
  #
  test_ward = read_json(
    url_ward_code, 
    simplifyVector = TRUE
  )$data
  #
  # Concatenate the data DataFrame with the original DataFrame
  wards_data = rbind(
    wards_data, test_ward)
}
#
View(wards_data)
#
#
ls()
#
county_subcounty_ward = full_join(
  x = counties_data,
  y = subcounties_data
) |> 
  full_join(
    y = wards_data
  )
#
save(counties_data, subcounties_data, wards_data,
     county_subcounty_ward,
     file = "weather.RData")
#
#
library(httr)
library(jsonlite)
#
forecast_collective_url = "https://kaop.co.ke/weather_api//forecast_collective"
#
resp = POST(
  forecast_collective_url,
  body = '{"dataSrcId":"4","wardCode":"Kn2aiEXbw5B"}'
)
#
# stop_for_status(resp)
#
data_final = jsonlite::fromJSON(content(resp, 'text'))$data
data_final
#

