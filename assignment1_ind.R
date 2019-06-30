# MMA860 Individual Assigment#1
#Bill Chau Jun 28, 2019

library(tidyverse)
library(dplyr)
library(readxl)
library(lubridate)

# Load data into dataframe
loc_df <- read_excel("/Users/localadmin/Downloads/MMA860_Assignment1_Data_vf.xlsx", sheet = 1)
spec_df <- read_excel("/Users/localadmin/Downloads/MMA860_Assignment1_Data_vf.xlsx", sheet = 2)
weath_df <- read_excel("/Users/localadmin/Downloads/MMA860_Assignment1_Data_vf.xlsx", sheet = 3)


#split and clean weather data
info <- weath_df[0:9, 0:2]

clean_df <- tail(weath_df, -16)
header <- c(t(weath_df[16,]))
n_header <- tolower(make.names(header, unique = TRUE))
names(clean_df) <- n_header

# clean up datetime
clean_df$time <- as.numeric(clean_df$time)*24
clean_df$month <- as.numeric(clean_df$month)
clean_df$day <- as.numeric(clean_df$day)
clean_df <- unite(clean_df, datetime, month, day, time)
clean_df$datetime <- as.POSIXct(clean_df$datetime, format = "%m_%d_%H")


# a) Calculate air density

clean_df$stn.press..kpa. <- sapply(clean_df$stn.press..kpa., as.numeric)
clean_df$temp...c. <- sapply(clean_df$temp...c., as.numeric)

# define function to calculate 
cal_air_den <- function(press, temp){
	return((press*1000)/(287.05*(temp+273.15)))
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

# c) calculate  the power production at each turbine

# declare inputs
min <- as.numeric(spec_df[3,3])
max <- as.numeric(spec_df[4,3])
norm <- as.numeric(spec_df[7,3])*1000000
turbine_area <- as.numeric(spec_df[6, 3])
max_pow_co <- as.numeric(spec_df[5,3])

calc_power <- function(turbine_area, max_pow_co, air_den, windspeed){
	return(air_den*turbine_area*0.5*(windspeed)^3*max_pow_co)
}

clean_df <- mutate(clean_df, turb_output=if_else(wind_spd_ms<=min | wind_spd_ms>=max, 0, 
	if_else(calc_power(turbine_area, max_pow_co, air_den, wind_spd_ms)>norm, norm,calc_power(turbine_area, max_pow_co, air_den, wind_spd_ms)
	))
)

# do a quick true/false condition met column and count to match 0s in power output

# d) total amount of electricity produced for the entire windfarm in January in MW
round(sum(clean_df$turb_output)/1000000, 2)
paste("Total electricity produced in January:", round(sum(clean_df$turb_output)/1000000, 2))




