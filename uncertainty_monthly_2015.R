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

lake_name = c("Superior", "MichiganHuron", "Erie", "Ontario")
balance_component = c("Precipitation", "Evaporation", "Runoff", "Outflow")

uncertainty_compare_func_precip <- function(lake_name, balance_component) {
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
  
  labels = if (balance_component == "Precipitation")  labs(y = lake_name, x = NULL) else labs(y = NULL, x = NULL)
  title = if (lake_name == "Superior") ggtitle(balance_component) else NULL
  
  plot_sup_precip_l2serror_usgshigh <-
    ggplot(data = sup_precip,
           aes(x = yearmon,
               y = Median)) +
    xlim (2015, 2021) +
    # ylim (0, 120) +
    geom_ribbon(aes(ymin = Median * (1 - 0.45),
                    ymax = Median * (1 + 0.45)), fill = "blue", alpha=0.3) +
    geom_ribbon(aes(
      ymin = X2.5.Percentile,
      ymax = X97.5.Percentile
    ),
    fill = "yellow", alpha=0.3) +
    labs(
      # title = "Uncertainty of Lake Superior Percipitation L2SWBM Model (95% Confidence Interval)",
      y = "mm",
      x = "Year") +
    geom_point(colour = "black", size = 0.5) +
    geom_line(color = "red", size = 0.5)+
    labels + title  + theme(plot.title = element_text(hjust = 0.5))
  plot_sup_precip_l2serror_usgshigh
}

uncertainty_compare_func_evap <- function(lake_name, balance_component) {
  filename = paste(
    "./l2s_posterior/",
    lake_name,
    balance_component,
    "_GLWBData.csv",
    sep = ""
  )
  sup_evap <-
    read.csv(filename)
  str(sup_evap)
  sup_evap$yearmon <-
    as.yearmon(paste(sup_evap$Year, sup_evap$Month), "%Y %m")
  sup_evap$formated_date <-
    format(as.Date(sup_evap$yearmon), "%m/%Y")

  labels = if (balance_component == "Precipitation")  labs(y = lake_name, x = NULL) else labs(y = NULL, x = NULL)
  title = if (lake_name == "Superior") ggtitle(balance_component) else NULL
  
  plot_sup_evap_l2serror_usgshigh <-
    ggplot(data = sup_evap,
           aes(x = yearmon,
               y = Median)) +
    xlim (2015, 2021) +
    # ylim (0, 120) +
    geom_ribbon(aes(ymin = Median * (1 - 0.35),
                    ymax = Median * (1 + 0.35)), fill = "blue", alpha=0.3) +
    geom_ribbon(aes(
      ymin = X2.5.Percentile,
      ymax = X97.5.Percentile
    ),
    fill = "yellow", alpha=0.3) +
    labs(
      # title = "Uncertainty of Lake Superior Percipitation L2SWBM Model (95% Confidence Interval)",
      y = "mm",
      x = "Year") +
    geom_point(colour = "black", size = 0.5) +
    geom_line(color = "red", size = 0.5)+
    labels + title  + theme(plot.title = element_text(hjust = 0.5))
  plot_sup_evap_l2serror_usgshigh
}

uncertainty_compare_func_runoff <- function(lake_name, balance_component) {
  filename = paste(
    "./l2s_posterior/",
    lake_name,
    balance_component,
    "_GLWBData.csv",
    sep = ""
  )
  sup_runoff <-
    read.csv(filename)
  str(sup_runoff)
  sup_runoff$yearmon <-
    as.yearmon(paste(sup_runoff$Year, sup_runoff$Month), "%Y %m")
  sup_runoff$formated_date <-
    format(as.Date(sup_runoff$yearmon), "%m/%Y")
  
  labels = if (balance_component == "Precipitation")  labs(y = lake_name, x = NULL) else labs(y = NULL, x = NULL)
  title = if (lake_name == "Superior") ggtitle(balance_component) else NULL
  
  plot_sup_runoff_l2serror_usgshigh <-
    ggplot(data = sup_runoff,
           aes(x = yearmon,
               y = Median)) +
    xlim (2015, 2021) +
    # ylim (0, 120) +
    geom_ribbon(aes(ymin = Median * (1 - 0.35),
                    ymax = Median * (1 + 0.35)), fill = "blue", alpha=0.3) +
    geom_ribbon(aes(
      ymin = X2.5.Percentile,
      ymax = X97.5.Percentile
    ),
    fill = "yellow", alpha=0.3) +
    labs(
      # title = "Uncertainty of Lake Superior Percipitation L2SWBM Model (95% Confidence Interval)",
      y = "mm",
      x = "Year") +
    geom_point(colour = "black", size = 0.5) +
    geom_line(color = "red", size = 0.5)+
    labels + title  + theme(plot.title = element_text(hjust = 0.5))
  plot_sup_runoff_l2serror_usgshigh
}  

uncertainty_compare_func_outflow <- function(lake_name, balance_component) {
  filename = paste(
    "./l2s_posterior/",
    lake_name,
    balance_component,
    "_GLWBData.csv",
    sep = ""
  )
  sup_outflow <-
    read.csv(filename)
  str(sup_outflow)
  sup_outflow$yearmon <-
    as.yearmon(paste(sup_outflow$Year, sup_outflow$Month), "%Y %m")
  sup_outflow$formated_date <-
    format(as.Date(sup_outflow$yearmon), "%m/%Y")
  
  labels = if (balance_component == "Precipitation")  labs(y = lake_name, x = NULL) else labs(y = NULL, x = NULL)
  title = if (lake_name == "Superior") ggtitle(balance_component) else NULL
  
  plot_sup_outflow_l2serror_usgshigh <-
    ggplot(data = sup_outflow,
           aes(x = yearmon,
               y = Median)) +
    xlim (2015, 2021) +
    # ylim (0, 120) +
    geom_ribbon(aes(ymin = Median * (1 - 0.15),
                    ymax = Median * (1 + 0.15)), fill = "blue", alpha=0.3) +
    geom_ribbon(aes(
      ymin = X2.5.Percentile,
      ymax = X97.5.Percentile
    ),
    fill = "yellow", alpha=0.3) +
    labs(
      # title = "Uncertainty of Lake Superior Percipitation L2SWBM Model (95% Confidence Interval)",
      y = "mm",
      x = "Year") +
    geom_point(colour = "black", size = 0.5) +
    geom_line(color = "red", size = 0.5)+
    labels + title  + theme(plot.title = element_text(hjust = 0.5))
  plot_sup_outflow_l2serror_usgshigh
}  

ggarrange(
  uncertainty_compare_func_precip("Superior", "Precipitation"),
  uncertainty_compare_func_evap("Superior", "Evaporation"),
  uncertainty_compare_func_runoff("Superior", "Runoff"),
  uncertainty_compare_func_outflow("Superior", "Outflow"),
  
  uncertainty_compare_func_precip("MichiganHuron", "Precipitation"),
  uncertainty_compare_func_evap("MichiganHuron", "Evaporation"),
  uncertainty_compare_func_runoff("MichiganHuron", "Runoff"),
  uncertainty_compare_func_outflow("MichiganHuron", "Outflow"),
  
  uncertainty_compare_func_precip("Erie", "Precipitation"),
  uncertainty_compare_func_evap("Erie", "Evaporation"),
  uncertainty_compare_func_runoff("Erie", "Runoff"),
  uncertainty_compare_func_outflow("Erie", "Outflow"),
  
  uncertainty_compare_func_precip("Ontario", "Precipitation"),
  uncertainty_compare_func_evap("Ontario", "Evaporation"),
  uncertainty_compare_func_runoff("Ontario", "Runoff"),
  uncertainty_compare_func_outflow("Ontario", "Outflow"),
  
  ncol = 4,
  nrow = 4
)
