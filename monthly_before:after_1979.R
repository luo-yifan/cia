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

theme_set(theme_grey())
setwd("./")

lake_name = c("Superior", "MichiganHuron", "Erie", "Ontario")
balance_component = c("Precipitation", "Evaporation", "Runoff", "Outflow")

func <- function(lake_name, balance_component) {
  
  month <- seq(as.Date("2020-01-01"), 
               as.Date("2020-12-01"), 
               by = "1 month")
  month_numeric <- 1:12
  month_label <- format(month, format = "%b")
  filename = paste("./l2s_posterior/",
                   lake_name,
                   balance_component,
                   "_GLWBData.csv",
                   sep = "")
  sup_precip <-
    read.csv(filename)
  str(sup_precip)
  sup_precip$yearmon <-
    as.yearmon(paste(sup_precip$Year, sup_precip$Month), "%Y %m")
  sup_precip$formated_date <-
    format(as.Date(sup_precip$yearmon), "%m/%Y")
  
  sup_precip_before_1979 <- sup_precip[which(sup_precip$Year <= 1979), ]
  sup_precip_after_1979 <- sup_precip[which(sup_precip$Year > 1979), ]
  month_mean_before_1979 <- aggregate(Median ~ Month , data = sup_precip_before_1979 , mean)
  month_mean_after_1979 <- aggregate(Median ~ Month , data = sup_precip_after_1979 , mean)
  
  df <- data.frame(Month=1:12,
                   Y1950_1979=month_mean_before_1979$Median,
                   Y1980_2021=month_mean_after_1979$Median)
  df <- melt(df,  id.vars ='Month',variable.name = 'years')
  
  labels = if (balance_component == "Precipitation")
    labs(y = lake_name, x = NULL)
  else
    labs(y = NULL, x = NULL)
  title = if (lake_name == "Superior")
    ggtitle(balance_component)
  else
    NULL
  
  ggplot(df, aes(Month, value)) +
    geom_line(aes(colour = years)) +
    scale_x_continuous(breaks = month_numeric, labels = month_label) +
    labels + title + theme(plot.title = element_text(hjust = 0.5)) 
}

ggarrange(
  func("Superior", "Precipitation"),
  func("Superior", "Evaporation"),
  func("Superior", "Runoff"),
  func("Superior", "Outflow"),
  func("MichiganHuron", "Precipitation"),
  func("MichiganHuron", "Evaporation"),
  func("MichiganHuron", "Runoff"),
  func("MichiganHuron", "Outflow"),
  func("Erie", "Precipitation"),
  func("Erie", "Evaporation"),
  func("Erie", "Runoff"),
  func("Erie", "Outflow"),
  func("Ontario", "Precipitation"),
  func("Ontario", "Evaporation"),
  func("Ontario", "Runoff"),
  func("Ontario", "Outflow"),
  ncol = 4,
  nrow = 4,
  common.legend = TRUE, 
  legend="bottom"
)

