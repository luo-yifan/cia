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
func <- function(lake_name, balance_component) {
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
  annual_2.5 <-
    aggregate(X2.5.Percentile ~ Year , data = sup_precip , mean)
  annual_97.5 <-
    aggregate(X97.5.Percentile ~ Year , data = sup_precip , mean)
  
  reference_mean <- mean(annual_mean$Median[annual_mean$Year < 1979])
  reference_sd <- sd(annual_mean$Median[annual_mean$Year < 1979])
  recent_mean <- mean(annual_mean$Median[annual_mean$Year >= 1979])
  recent_sd <- sd(annual_mean$Median[annual_mean$Year >= 1979])
  
  plot_sup_precip_mean <-
    ggplot(data = annual_mean, aes(x = Year, y = Median)) +
    geom_line(color = "red") +
    labs(
      # title = "Lake Superior Annual Percipitation Comparison: 1950-1978 vs 1979-2019",
         y = "(mm)",
         x = "Year") +
    geom_segment(aes(
      x = 1979,
      xend = 2019,
      y = recent_mean,
      yend = recent_mean
    ),
    data = annual_mean)  +
    # geom_ribbon(aes(ymin = recent_mean - recent_sd,
    #                 ymax = recent_mean + recent_sd),
    #             alpha = 0.1) + 
    geom_segment(aes(
      x = 1950,
      xend = 1979,
      y = reference_mean,
      yend = reference_mean
    ),
    data = annual_mean)
  # + geom_ribbon(aes(ymin = reference_mean -  reference_sd,
  #                   ymax = reference_mean +  reference_sd),
  #               alpha = 0.1)
  plot_sup_precip_mean
  ggsave(paste('./output/',lake_name, balance_component, "mean.png", sep = "_"))
  
  sup_precip.ts <-
    ts(annual_mean$Median,
       start = c(1950),
       end = c(2019))
  year_index = cpts(cpt.mean(sup_precip.ts))
  split_year = annual_mean[year_index, ]$Year
  #plot(cpt.mean(sup_precip.ts))
  
  reference_mean_cpt <-
    mean(annual_mean$Median[annual_mean$Year < split_year])
  reference_sd_cpt <-
    sd(annual_mean$Median[annual_mean$Year < split_year])
  recent_mean_cpt <-
    mean(annual_mean$Median[annual_mean$Year >= split_year])
  recent_sd_cpt <-
    sd(annual_mean$Median[annual_mean$Year >= split_year])
  
  plot_sup_precip_mean_cpt <-
    ggplot(data = annual_mean, aes(x = Year, y = Median)) +
    geom_line(color = "red") +
    labs(
      # title = "Lake Superior Annual Percipitation Comparison: 1950-2011 vs 2012-2019",
         y = "(mm)",
         x = "Year") +
    geom_segment(aes(
      x = split_year,
      xend = 2019,
      y = recent_mean_cpt,
      yend = recent_mean_cpt
    ),
    data = annual_mean) +
    geom_segment(
      aes(
        x = 1950,
        xend = split_year,
        y = reference_mean_cpt,
        yend = reference_mean_cpt
      ),
      data = annual_mean
    )
  plot_sup_precip_mean_cpt
  ggsave(paste('./output/',lake_name, balance_component, "mean_cpt.png", sep = "_"))
  
  plot_sup_precip_smoothmean <-
    ggplot(data = annual_mean, aes(x = Year, y = Median)) +
    geom_line(color = "red") +
    labs(y = "(mm)",
         x = "Year") +
    geom_smooth(color = "black", size = 0.5)
  plot_sup_precip_smoothmean
  ggsave(paste('./output/',lake_name, balance_component, "smoothmean.png", sep = "_"))
  
  plot_sup_precip_usgserror_low <-
    ggplot(data = annual_mean,
           aes(x = Year,
               y = Median)) +
    xlim (1980, 2000) +
    ylim (0, 120) +
    geom_ribbon(aes(ymin = Median * (1 - 0.15),
                    ymax = Median * (1 + 0.15)), fill = "grey70") +
    labs(title = "Uncertainty of Lake Superior Percipitation from Neff and Nicholas Paper (15%)",
         y = "Annual Percipitation (mm)",
         x = "Year") +
    geom_line(color = "red")
  plot_sup_precip_usgserror_low
  
  plot_sup_precip_usgserror_high <-
    ggplot(data = annual_mean,
           aes(x = Year,
               y = Median)) +
    xlim (1980, 2000) +
    ylim (0, 120) +
    geom_ribbon(aes(ymin = Median * (1 - 0.45),
                    ymax = Median * (1 + 0.45)), fill = "grey70") +
    labs(title = "Uncertainty of Lake Superior Percipitation from Neff and Nicholas Paper (45%)",
         y = "Annual Percipitation (mm)",
         x = "Year") +
    geom_line(color = "red")
  plot_sup_precip_usgserror_high
  
  plot_sup_precip_l2serror <-
    ggplot(data = annual_mean,
           aes(x = Year,
               y = Median)) +
    xlim (1980, 2000) +
    ylim (0, 120) +
    geom_ribbon(aes(ymin = Median * (1 - 0.45),
                    ymax = Median * (1 + 0.45)), fill = "grey70") +
    geom_ribbon(aes(
      ymin = annual_2.5$X2.5.Percentile,
      ymax = annual_97.5$X97.5.Percentile
    ),
    fill = "grey60") +
    labs(title = "Uncertainty of Lake Superior Percipitation L2SWBS Model (95% Confidence Interval)",
         y = "Annual Percipitation (mm)",
         x = "Year") +
    geom_line(color = "red")
  plot_sup_precip_l2serror
 
}

lake_names = c("superior", "miHuron", "erie")
balance_components = c("Precip", "Evap", "Runoff")

for (lake_name in lake_names) {
  for (balance_component in balance_components) {
    func(lake_name, balance_component)
  }
}

