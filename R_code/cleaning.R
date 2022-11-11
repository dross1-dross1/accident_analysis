# Setup -------------------------------------------------------------------

#if (!is.null(dev.list()["RStudioGD"])) { dev.off(dev.list()["RStudioGD"]) }  # remove all plots
rm(list=ls())  # remove all variables
cat("\014")  # ctrl+L
set.seed(100)

library(dplyr)
library(ggplot2)
library(stringr)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

df = read.csv("../data/US_Accidents_Dec21_updated.csv")  # 3.64 GiB RAM

# Pruning -----------------------------------------------------------------

df = subset(df, select=c(Severity, Start_Time, Start_Lat, Start_Lng, Distance.mi., Side, City, County, State, Zipcode, Timezone, Temperature.F., Humidity..., Visibility.mi., Wind_Speed.mph., Precipitation.in., Weather_Condition, Amenity, Bump, Crossing, Give_Way, Junction, No_Exit, Railway, Roundabout, Station, Stop, Traffic_Calming, Traffic_Signal, Turning_Loop, Sunrise_Sunset))
names(df) = c("severity", "time", "lat", "long", "dist_mi", "is_right_side", "city", "county", "state", "zip", "timezone", "temp_f", "pct_humid", "visibility_mi", "spd_wind_mph", "precip_inch", "weather", "amenity", "bump", "crossing", "give_way", "junction", "no_exit", "railway", "tcircle", "station", "stop", "tcalming", "tsignal", "turning_loop", "is_day")

# df %>% filter(nchar(zip) > 6) %>% head

# Converting --------------------------------------------------------------

df$time = as.POSIXct(df$time, format="%Y-%m-%d %H:%M:%S")
df$is_right_side = setNames(c(1, 0), c("R", "L"))[df$is_right_side]

zip_code_cleaner = function(zip_string) {zip_string %>% str_match("\\d{5}") %>% as.numeric}
df$zip = df$zip %>% zip_code_cleaner
df = df %>% subset(!is.na(zip))

df$pct_humid = df$pct_humid / 100

df$amenity = setNames(c(1, 0), c("True", "False"))[df$amenity]
df$bump = setNames(c(1, 0), c("True", "False"))[df$bump]
df$crossing = setNames(c(1, 0), c("True", "False"))[df$crossing]
df$give_way = setNames(c(1, 0), c("True", "False"))[df$give_way]
df$junction = setNames(c(1, 0), c("True", "False"))[df$junction]
df$no_exit = setNames(c(1, 0), c("True", "False"))[df$no_exit]
df$railway = setNames(c(1, 0), c("True", "False"))[df$railway]
df$tcircle = setNames(c(1, 0), c("True", "False"))[df$tcircle]
df$station = setNames(c(1, 0), c("True", "False"))[df$station]
df$stop = setNames(c(1, 0), c("True", "False"))[df$stop]
df$tcalming = setNames(c(1, 0), c("True", "False"))[df$tcalming]
df$tsignal = setNames(c(1, 0), c("True", "False"))[df$tsignal]
df$turning_loop = setNames(c(1, 0), c("True", "False"))[df$turning_loop]
df$is_day = setNames(c(1, 0), c("Day", "Night"))[df$is_day]

# Cleaning ----------------------------------------------------------------

# number of accidents by california county
# filter(df, State == "TX") %>% group_by(City) %>% summarise(n()) %>% as.data.frame

# subset severe cases
# df.severe = filter(df, Severity == 4)

# convert datetime columns
# df$Weather_Timestamp = as.POSIXct(df$Weather_Timestamp, format="%Y-%m-%d %H:%M:%S")
# df$Start_Time = as.POSIXct(df$Start_Time, format="%Y-%m-%d %H:%M:%S")
# df$End_Time = as.POSIXct(df$End_Time, format="%Y-%m-%d %H:%M:%S")

# ggplot(df.severe) + geom_point(aes(x=Start_Lng, y=Start_Lat, color=as.factor(Severity)))

# ggplot(df.severe) + geom_histogram(aes(x=Start_Time), bins=100)

# Save Data ---------------------------------------------------------------

write.csv(df, "accident_data_pruned.csv", row.names=F)
