library(ggplot2)
library(ggpubr)
library(dplyr)
library(tidyr)
library(tibble)
library(zoo)
library(changepoint)
library(r2r)
library(lubridate)
library(segmented)
library(SiZer)
library(plotrix)
library(strucchange)
library(cpm)
library(EnvCpt)
library(reshape2)

theme_set(theme_bw())
setwd("./")

lake_name = c("Superior", "MichiganHuron", "Erie", "Ontario")
balance_component = c("Precipitation", "Evaporation", "Runoff", "Outflow")

get_labs = function(lake_name, balance_component){
  label = if (lake_name == "Superior")
    labs(y = balance_component, x = NULL)
  else
    labs(y = NULL, x = NULL)
  return(label)
}
get_title = function(lake_name, balance_component){
  title = if (balance_component == "Precipitation")
    ggtitle(lake_name)
  else
    NULL
  return(title)
}

func_1979 <- function(lake_name, balance_component) {
  
  month <- seq(as.Date("2020-01-01"), 
               as.Date("2020-12-01"), 
               by = "1 month")
  month_numeric <- 1:12
  month_label <- format(month, format = "%b")
  filename = paste("./l2s_posterior/",
                   lake_name,
                   balance_component,
                   "_2019.csv",
                   sep = "")
  sup_precip <-
    read.csv(filename)
  str(sup_precip)
  sup_precip$yearmon <-
    as.yearmon(paste(sup_precip$Year, sup_precip$Month), "%Y %m")
  sup_precip$formated_date <-
    format(as.Date(sup_precip$yearmon), "%m/%Y")
  
  labels = get_labs(lake_name, balance_component)
  title = get_title(lake_name, balance_component)
  
  sup_precip_before_1979 <- sup_precip[which(sup_precip$Year < 1979), ]
  sup_precip_after_1979 <- sup_precip[which(sup_precip$Year >= 1979), ]
  month_mean_before_1979 <- aggregate(Median ~ Month , data = sup_precip_before_1979 , mean)
  month_mean_after_1979 <- aggregate(Median ~ Month , data = sup_precip_after_1979 , mean)
  
  df <- data.frame(Month=1:12,
                   MonthlyMean1950_1979=month_mean_before_1979$Median,
                   MonthlyMean1980_2019=month_mean_after_1979$Median)
  df <- melt(df,  id.vars ='Month',variable.name = 'TimePeriod')
  
  # labels = if (balance_component == "Precipitation")
  #   labs(y = lake_name, x = NULL)
  # else
  #   labs(y = NULL, x = NULL)
  # title = if (lake_name == "Superior")
  #   ggtitle(balance_component)
  # else
  #   NULL
  
  ggplot(df, aes(Month, value)) +
    geom_line(aes(colour = TimePeriod)) +
    scale_x_continuous(breaks = month_numeric, labels = month_label) +
    labels + title + theme(plot.title = element_text(hjust = 0.5)) 
}
func_2000 <- function(lake_name, balance_component) {
  
  month <- seq(as.Date("2020-01-01"), 
               as.Date("2020-12-01"), 
               by = "1 month")
  month_numeric <- 1:12
  month_label <- format(month, format = "%b")
  filename = paste("./l2s_posterior/",
                   lake_name,
                   balance_component,
                   "_2019.csv",
                   sep = "")
  sup_precip <-
    read.csv(filename)
  str(sup_precip)
  sup_precip$yearmon <-
    as.yearmon(paste(sup_precip$Year, sup_precip$Month), "%Y %m")
  sup_precip$formated_date <-
    format(as.Date(sup_precip$yearmon), "%m/%Y")
  
  labels = get_labs(lake_name, balance_component)
  title = get_title(lake_name, balance_component)
  
  sup_precip_before_2000 <- sup_precip[which(sup_precip$Year < 2000), ]
  sup_precip_after_2000 <- sup_precip[which(sup_precip$Year >= 2000), ]
  month_mean_before_2000 <- aggregate(Median ~ Month , data = sup_precip_before_2000 , mean)
  month_mean_after_2000 <- aggregate(Median ~ Month , data = sup_precip_after_2000 , mean)
  
  df <- data.frame(Month=1:12,
                   MonthlyMean1950_2000=month_mean_before_2000$Median,
                   MonthlyMean2000_2019=month_mean_after_2000$Median)
  df <- melt(df,  id.vars ='Month',variable.name = 'TimePeriod')
  
  # labels = if (balance_component == "Precipitation")
  #   labs(y = lake_name, x = NULL)
  # else
  #   labs(y = NULL, x = NULL)
  # title = if (lake_name == "Superior")
  #   ggtitle(balance_component)
  # else
  #   NULL
  
  ggplot(df, aes(Month, value)) +
    geom_line(aes(colour = TimePeriod)) +
    scale_x_continuous(breaks = month_numeric, labels = month_label) +
    labels + title + theme(plot.title = element_text(hjust = 0.5)) 
}

# func = func_1979
# func = func_2000
# 
# func("Superior", "Precipitation")

ggarrange(
  func("Superior", "Precipitation"),
  func("MichiganHuron", "Precipitation"),
  func("Erie", "Precipitation"),
  func("Ontario", "Precipitation"),
  func("Superior", "Evaporation"),
  func("MichiganHuron", "Evaporation"),
  func("Ontario", "Evaporation"),
  func("Erie", "Evaporation"),
  func("Superior", "Runoff"),
  func("MichiganHuron", "Runoff"),
  func("Erie", "Runoff"),
  func("Ontario", "Runoff"),
  func("Superior", "Outflow"),
  func("MichiganHuron", "Outflow"),
  func("Erie", "Outflow"),
  func("Ontario", "Outflow"),
  ncol = 4,
  nrow = 4,
  common.legend = TRUE, 
  legend="bottom"
)

