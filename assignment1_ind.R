# MMA860 Individual Assigment#1
#Bill Chau Jun 28, 2019

library(tidyverse)
library(dplyr)
library(readxl)
library(lubridate)

# Load data into dataframe
spec_df <- read_excel("/Users/localadmin/Downloads/MMA860_Assignment1_Data_vf.xlsx", sheet = 1)
loc_df <- read_excel("/Users/localadmin/Downloads/MMA860_Assignment1_Data_vf.xlsx", sheet = 2)
weath_df <- read_excel("/Users/localadmin/Downloads/MMA860_Assignment1_Data_vf.xlsx", sheet = 3)


#split and clean weather data
info <- weath_df[0:9, 0:2]

clean_df <- tail(weath_df, -16)
header <- c(t(weath_df[16,]))
n_header <- tolower(make.names(header, unique = TRUE))
names(clean_df) <- n_header

# a) Calculate air density

clean_df$stn.press..kpa. <- lapply(clean_df$stn.press..kpa., as.numeric)
clean_df$temp...c. <- lapply(clean_df$temp...c., as.numeric)

# define function to calculate 
cal_air_den <- function(press, temp){
	return((press*1000)/(287.05*(temp+273.15))
}

clean_df <- mutate(clean_df, air_den = cal_air_den(clean_df$stn.press..kpa., clean_df$temp...c.))

# b) Convert windspeed to m/s

# check for errors in windspeed and convert to numeric
colnames(clean_df)
sum(sapply(clean_df$wind.spd..km.h., is.na))
clean_df$wind.spd..km.h. <- as.numeric(clean_df$wind.spd..km.h.)

convert_ms <- function(kmh){
	return(kmh*0.277778)
}

clean_df <- mutate(clean_df, wind_spd_ms = convert_ms(clean_df$wind.spd..km.h.))