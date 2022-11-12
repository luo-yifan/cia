library(ggplot2)
library(ggpubr)
library(dplyr)
library(tidyr)
library(tibble)
library(zoo)
library(changepoint)
library(r2r)
library(lubridate)

theme_set(theme_bw())
setwd("./")

lake_name = c("superior", "miHuron", "erie", "ontario")
balance_component = c("Precip", "Evap", "Runoff", "Outflow")


get_labs = function(lake_name, balance_component){
  label = if (lake_name == "superior")
    labs(y = balance_component, x = NULL)
  else
    labs(y = NULL, x = NULL)
  return(label)
}
get_title = function(lake_name, balance_component){
  title = if (balance_component == "Precip")
    ggtitle(lake_name)
  else
    NULL
  return(title)
}

uncertainty_compare_func_precip <- function(lake_name, balance_component) {
  filename = paste(
    "./l2s_posterior/",
    lake_name,
    balance_component,
    "_MonthlyRun.csv",
    sep = ""
  )
  sup_Precip <-
    read.csv(filename)
  str(sup_Precip)
  sup_Precip$yearmon <-
    as.yearmon(paste(sup_Precip$Year, sup_Precip$Month), "%Y %m")
  sup_Precip$formated_date <-
    format(as.Date(sup_Precip$yearmon), "%m/%Y")
  
  labels = get_labs(lake_name, balance_component)
  title = get_title(lake_name, balance_component)
  
  plot_sup_Precip_l2serror_usgshigh <-
    ggplot(data = sup_Precip,
           aes(x = yearmon,
               y = Median)) +
    xlim (2016, 2021) +
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
  plot_sup_Precip_l2serror_usgshigh
}

uncertainty_compare_func_evap <- function(lake_name, balance_component) {
  filename = paste(
    "./l2s_posterior/",
    lake_name,
    balance_component,
    "_MonthlyRun.csv",
    sep = ""
  )
  sup_evap <-
    read.csv(filename)
  str(sup_evap)
  sup_evap$yearmon <-
    as.yearmon(paste(sup_evap$Year, sup_evap$Month), "%Y %m")
  sup_evap$formated_date <-
    format(as.Date(sup_evap$yearmon), "%m/%Y")
  
  labels = get_labs(lake_name, balance_component)
  title = get_title(lake_name, balance_component)
  
  plot_sup_evap_l2serror_usgshigh <-
    ggplot(data = sup_evap,
           aes(x = yearmon,
               y = Median)) +
    xlim (2016, 2021) +
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
"_MonthlyRun.csv",
    sep = ""
  )
  sup_runoff <-
    read.csv(filename)
  str(sup_runoff)
  sup_runoff$yearmon <-
    as.yearmon(paste(sup_runoff$Year, sup_runoff$Month), "%Y %m")
  sup_runoff$formated_date <-
    format(as.Date(sup_runoff$yearmon), "%m/%Y")
  
  labels = get_labs(lake_name, balance_component)
  title = get_title(lake_name, balance_component)
  
  plot_sup_runoff_l2serror_usgshigh <-
    ggplot(data = sup_runoff,
           aes(x = yearmon,
               y = Median)) +
    xlim (2016, 2021) +
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
"_MonthlyRun.csv",
    sep = ""
  )
  sup_outflow <-
    read.csv(filename)
  str(sup_outflow)
  sup_outflow$yearmon <-
    as.yearmon(paste(sup_outflow$Year, sup_outflow$Month), "%Y %m")
  sup_outflow$formated_date <-
    format(as.Date(sup_outflow$yearmon), "%m/%Y")
  
  labels = get_labs(lake_name, balance_component)
  title = get_title(lake_name, balance_component)
  
  plot_sup_outflow_l2serror_usgshigh <-
    ggplot(data = sup_outflow,
           aes(x = yearmon,
               y = Median)) +
    xlim (2016, 2021) +
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

# uncertainty_compare_func_Precip("Superior", "Precipitation")

ggarrange(
  uncertainty_compare_func_precip("superior", "Precip"),
  uncertainty_compare_func_precip("miHuron", "Precip"),
  uncertainty_compare_func_precip("erie", "Precip"),
  uncertainty_compare_func_precip("ontario", "Precip"),
  
  uncertainty_compare_func_evap("superior", "Evap"),
  uncertainty_compare_func_evap("miHuron", "Evap"),
  uncertainty_compare_func_evap("erie", "Evap"),
  uncertainty_compare_func_evap("ontario", "Evap"),
  
  uncertainty_compare_func_runoff("superior", "Runoff"),
  uncertainty_compare_func_runoff("miHuron", "Runoff"),
  uncertainty_compare_func_runoff("erie", "Runoff"),
  uncertainty_compare_func_runoff("ontario", "Runoff"),
  
  uncertainty_compare_func_outflow("superior", "Outflow"),
  uncertainty_compare_func_outflow("miHuron", "Outflow"),
  uncertainty_compare_func_outflow("erie", "Outflow"),
  uncertainty_compare_func_outflow("ontario", "Outflow"),
  
  ncol = 4,
  nrow = 4
)


ggarrange(
  uncertainty_compare_func_precip("superior", "Precip"),
  uncertainty_compare_func_precip("miHuron", "Precip"),
  uncertainty_compare_func_precip("erie", "Precip"),
  uncertainty_compare_func_precip("ontario", "Precip"),
  
  uncertainty_compare_func_evap("superior", "Evap"),
  uncertainty_compare_func_evap("miHuron", "Evap"),
  uncertainty_compare_func_evap("erie", "Evap"),
  uncertainty_compare_func_evap("ontario", "Evap"),
  
  uncertainty_compare_func_runoff("superior", "Runoff"),
  uncertainty_compare_func_runoff("miHuron", "Runoff"),
  uncertainty_compare_func_runoff("erie", "Runoff"),
  uncertainty_compare_func_runoff("ontario", "Runoff"),
  
  uncertainty_compare_func_outflow("superior", "Outflow"),
  uncertainty_compare_func_outflow("miHuron", "Outflow"),
  uncertainty_compare_func_outflow("erie", "Outflow"),
  uncertainty_compare_func_outflow("ontario", "Outflow"),
  
  ncol = 4,
  nrow = 4
)


# uncertainty_compare_func_Precip("superior", "Precip(mm)")
# 
#  ggarrange(
#   uncertainty_compare_func_Precip("superior", "Precip(mm)"),
#   uncertainty_compare_func_Precip("miHuron", "Precip(mm)"),
#   uncertainty_compare_func_Precip("erie", "Precip(mm)"),
#   uncertainty_compare_func_Precip("ontario", "Precip(mm)"),
# 
#   uncertainty_compare_func_evap("superior", "Evap(mm)"),
#   uncertainty_compare_func_evap("miHuron", "Evap(mm)"),
#   uncertainty_compare_func_evap("ontario", "Evap(mm)"),
#   uncertainty_compare_func_evap("erie", "Evap(mm)"),
# 
#   uncertainty_compare_func_runoff("superior", "Runoff(mm)"),
#   uncertainty_compare_func_runoff("miHuron", "Runoff(mm)"),
#   uncertainty_compare_func_runoff("erie", "Runoff(mm)"),
#   uncertainty_compare_func_runoff("ontario", "Runoff(mm)"),
# 
#   uncertainty_compare_func_outflow("superior", "Outflow(cms)"),
#   uncertainty_compare_func_outflow("miHuron", "Outflow(cms)"),
#   uncertainty_compare_func_outflow("erie", "Outflow(cms)"),
#   uncertainty_compare_func_outflow("ontario", "Outflow(cms)"),
# 
#   ncol = 4,
#   nrow = 4
# )

# ggarrange(
#   uncertainty_compare_func_Precip("Superior", "Precipitation"),
#   uncertainty_compare_func_evap("Superior", "Evap"),
#   uncertainty_compare_func_runoff("Superior", "Runoff"),
#   uncertainty_compare_func_outflow("Superior", "Outflow"),
# 
#   uncertainty_compare_func_Precip("miHuron", "Precipitation"),
#   uncertainty_compare_func_evap("MichiganHuron", "Evap"),
#   uncertainty_compare_func_runoff("MichiganHuron", "Runoff"),
#   uncertainty_compare_func_outflow("MichiganHuron", "Outflow"),
# 
#   uncertainty_compare_func_Precip("erie", "Precipitation"),
#   uncertainty_compare_func_evap("erie", "Evap"),
#   uncertainty_compare_func_runoff("erie", "Runoff"),
#   uncertainty_compare_func_outflow("erie", "Outflow"),
# 
#   uncertainty_compare_func_Precip("ontario", "Precipitation"),
#   uncertainty_compare_func_evap("ontario", "Evap"),
#   uncertainty_compare_func_runoff("ontario", "Runoff"),
#   uncertainty_compare_func_outflow("ontario", "Outflow"),
# 
#   ncol = 4,
#   nrow = 4
# )
