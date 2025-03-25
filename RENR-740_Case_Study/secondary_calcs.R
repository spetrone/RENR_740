library(readr)
library(cffdrs)
library(dplyr)
library(ggplot2)
library(lubridate)
library(tidyr)
library(readxl)

weather_df <- read_excel("LWF-161-2015-20250309T213257Z-001/LWF-161-2015/Weather/WX-LWF161-2015.xlsx")
View(weather_df)


latitude <- 55.476183
longitude <- -112.027267
elevation <- 500
slope <- 0
fuel_type <- "C2"
aspect <- 0


input_FBP_df <- weather_df %>%
  rename(WS = WIND_SPEED_KMH,
         WD = WIND_DIRECTION,
         FFMC = FINE_FUEL_MOISTURE_CODE,
         BUI = BUILD_UP_INDEX) %>%
  mutate(Dj =  yday(WEATHER_DATE),
         hr = 16,
         FuelType = fuel_type,
         GS = slope,
         ELV = elevation,
         LAT = latitude,
         LONG = longitude,
         Aspect = aspect) %>%
  select(FuelType, WEATHER_DATE, WS, WD, FFMC, BUI, Dj, hr, GS, ELV, LAT, LONG, Aspect)

input_FBP_df <- input_FBP_df[c(46,47,48),]

input_FBP_df[, (ncol(input_FBP_df)-10):ncol(input_FBP_df)] <- lapply(input_FBP_df[, (ncol(input_FBP_df)-10):ncol(input_FBP_df)], as.numeric)

fbp_res <- fbp(input_FBP_df, output = "All")

write_csv(fbp_res, "secondary_outputs_3_days.csv")

