library(ggplot2)
library(ggpubr)
library(dplyr)
library(tidyr)
library(tibble)
library(zoo)
library(changepoint)
library(r2r)
library(lubridate)

setwd("./")
func <- function(lake_name, balance_component, df) {
  filename = paste(
    "./l2s_posterior/",
    lake_name,
    balance_component,
    "_GLWBData.csv",
    sep = ""
  )
  sup_precip <-
    read.csv(filename)
  str(sup_precip)
  sup_precip$yearmon <-
    as.yearmon(paste(sup_precip$Year, sup_precip$Month), "%Y %m")
  sup_precip$formated_date <-
    format(as.Date(sup_precip$yearmon), "%m/%Y")
  
  annual_mean <- aggregate(Median ~ Year , data = sup_precip , mean)
  
  mean_1950_2019 <- mean(annual_mean$Median)
  
  mean_1950_2014 <- mean(annual_mean[annual_mean$Year <= 2014,]$Median )
  
  mean_2015_2019 <- mean(annual_mean[annual_mean$Year >= 2015,]$Median)
  
  mean_2015 <- mean(annual_mean[annual_mean$Year == 2015,]$Median)
  
  mean_2016 <- mean(annual_mean[annual_mean$Year == 2016,]$Median)
  
  mean_2017 <- mean(annual_mean[annual_mean$Year == 2017,]$Median)
  
  mean_2018 <- mean(annual_mean[annual_mean$Year == 2018,]$Median)
  
  mean_2019 <- mean(annual_mean[annual_mean$Year == 2019,]$Median)
  
  df[nrow(df) + 1,] <- c(paste(lake_name, balance_component,sep='_'),mean_1950_2019,mean_1950_2014 , mean_2015_2019,mean_2015,mean_2016,mean_2017,mean_2018,mean_2019)
  df
}

lake_names = c("superior", "miHuron", "erie")
balance_components = c("Precip", "Evap", "Runoff")

df <- data.frame(matrix(ncol = 9, nrow = 0))
x <- c("","1950-2019", "1950-2014", "2015-2019","2015","2016","2017","2018","2019")
colnames(df) <- x


for (lake_name in lake_names) {
  for (balance_component in balance_components) {
    df = func(lake_name, balance_component, df)
  }
}
df
