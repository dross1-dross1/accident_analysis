# Setup -----------------------------------------------------------------------------------------------------------

if (!is.null(dev.list()["RStudioGD"])) { dev.off(dev.list()["RStudioGD"]) }  # remove all plots
rm(list=ls())  # remove all variables
cat("\014")  # ctrl+L
set.seed(100)

# Imports
library(ggplot2)
library(plotly)
library(dplyr)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Data ------------------------------------------------------------------------------------------------------------

df = read.csv("../data/accident_data_pruned.csv")  # 1.3 GiB RAM
df$time = as.POSIXct(df$time, format="%Y-%m-%d %H:%M:%S")

# Functions -------------------------------------------------------------------------------------------------------

normalize = function(x) {(x - min(x)) / (max(x) - min(x))}

# Code ------------------------------------------------------------------------------------------------------------

# extract year from posix
df$year = df$time %>% format("%Y")
df$hour = df$time %>% format("%H")
df$day = df$time %>% format("%d") %>% as.numeric %% 7
df$month = df$time %>% format("%m")
df$day_abs = df$time %>% format("%d")

# remove na weather values
df %>% filter(weather == "") %>% nrow

# give all rows where weather has the keyword "snow"
df[grep("snow", df$weather, ignore.case=T), ] %>% head

# create df.severe where severity == 4
df.severe = df %>% filter(severity == 4)

# graph df.severe
ggplot(df.severe) + geom_point(aes(x=long, y=lat, color=severity))

# create df.austin where df is only austin
df.austin = df %>% filter(30 < lat, lat < 30.6, -98.2 < long, long < -97.5) %>% filter(city == "Austin")

# graph df.austin
ggplot(df.austin) + geom_point(aes(x=long, y=lat, color=as.factor(severity)), alpha=1)

# create bar graph of accidents based on year
ggplot(df.austin) + geom_bar(aes(x=year))

df$hour = df$time %>% format("%H")
ggplot(df.austin) + geom_bar(aes(x=hour, fill=as.factor(severity))) + facet_grid(rows=vars(severity))

# texas df missing years
df.tx = df %>% filter(state == "TX")
ggplot(df.tx) + geom_bar(aes(x=year))
ggplot(df.tx) + geom_point(aes(x=long, y=lat), alpha=.1)

# Compare Scales Hour Graph
graph_nca = function(df.in, day.in) {
  df.in = df.in %>% filter(day == day.in)

  df.us = df.in
  df.tx = df.in %>% filter(state == "TX")
  df.au = df.in %>% filter(30 < lat, lat < 30.6, -98.2 < long, long < -97.5) %>% filter(city == "Austin")

  dft.us = df.us %>% group_by(hour) %>% summarize(cnt=n()) %>% as.data.frame
  dft.us$cnt = dft.us$cnt %>% normalize

  dft.tx = df.tx %>% group_by(hour) %>% summarize(cnt=n()) %>% as.data.frame
  dft.tx$cnt = dft.tx$cnt %>% normalize

  dft.au = df.au %>% group_by(hour) %>% summarize(cnt=n()) %>% as.data.frame
  dft.au$cnt = dft.au$cnt %>% normalize

  print(
    ggplot(mapping=aes(x=as.numeric(hour), y=cnt)) +
      geom_line(data=dft.us, color="red", size=2,) +
      geom_line(data=dft.tx, color="green", size=2) +
      geom_line(data=dft.au, color="blue", size=2) +
      theme(text=element_text(size=28)) +
      ylab("Normalized Count of Accidents") +
      xlab("Hour of Day") +
      annotate("text", x=0, y=1, label="US", color="red", size=10) +
      annotate("text", x=0, y=0.9, label="TX", color="green", size=10) +
      annotate("text", x=0, y=0.8, label="AU", color="blue", size=10) +
      ggtitle(paste0("Accidents for Day: ", day.in))
  )
}

for (day in 0:6) {graph_nca(df, day)}

# dec 2021 bar graph
ggplot(df.tx %>% filter(month == 12, year == 2021)) + geom_bar(aes(x=day_abs))