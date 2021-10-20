
# Weather prediction based on analogs


library(rsoi)
library(lubridate)
library(dplyr)
library(tidyverse)
library(data.table)
library(readxl)
library(tidyr)
library(tidyverse)

rm(list = ls())

source("predictive_weather_FUN.R")

# weather forescast

weather_hist_data <- read.csv("Data/complete_weather.csv")

weather_hist_data$Date <- as.Date(weather_hist_data$Date,"%m/%d/%Y")

oni <- read.csv("ONI_Index.csv",row.names = 1)

# Enso forecast

enso_pred <- c(-0.87,-0.91,-0.84,-0.67,-0.46,-0.25)

# Months to be predicted

enso_months_fut <- c(10,11,12,1,2,3)

wp <-
  weather_predict(oni_length=6,k_nearest=7,predLeng=6,enso_pred=enso_pred,
                  enso_m = enso_months_fut,
                  Date_ini_stations="1982-01-01", 
                  completeWeather=weather_hist_data,train_oni = oni)


wp
