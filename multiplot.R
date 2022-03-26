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

setwd("./")

mean_func <- function(lake_name, balance_component) {
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
  
  annual_sum <- aggregate(Median ~ Year , data = sup_precip , sum)
  annual_2.5 <-
    aggregate(X2.5.Percentile ~ Year , data = sup_precip , sum)
  annual_97.5 <-
    aggregate(X97.5.Percentile ~ Year , data = sup_precip , sum)
  
  reference_mean <-
    mean(annual_sum$Median[annual_sum$Year < 1979])
  reference_sd <- sd(annual_sum$Median[annual_sum$Year < 1979])
  recent_sum <- mean(annual_sum$Median[annual_sum$Year >= 1979])
  recent_sd <- sd(annual_sum$Median[annual_sum$Year >= 1979])
  
  plot_sup_precip_mean <-
    ggplot(data = annual_sum, aes(x = Year, y = Median)) +
    # ylim(40,100) +
    geom_line() +
    geom_point(colour = "black", size = 0.5) +
    theme(axis.title.x = element_blank(), axis.title.y = element_blank()) +
    # labs(# title = "Lake Superior Annual Percipitation Comparison: 1950-1978 vs 1979-2019",
    #   y = "mm",
    #   x = "Year") +
    geom_segment(aes(
      x = 1979,
      xend = 2019,
      y = recent_mean,
      yend = recent_mean
    ),
    data = annual_sum) +
    geom_segment(aes(
      x = 1950,
      xend = 1979,
      y = reference_mean,
      yend = reference_mean
    ),
    data = annual_sum)
  return(plot_sup_precip_mean)
}

mean_ci_func <- function(lake_name, balance_component) {
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
  
  annual_sum <- aggregate(Median ~ Year , data = sup_precip , sum)
  annual_2.5 <-
    aggregate(X2.5.Percentile ~ Year , data = sup_precip , sum)
  annual_97.5 <-
    aggregate(X97.5.Percentile ~ Year , data = sup_precip , sum)
  
  reference_mean <-
    mean(annual_sum$Median[annual_sum$Year < 1979])
  reference_sd <- sd(annual_sum$Median[annual_sum$Year < 1979])
  recent_mean <- mean(annual_sum$Median[annual_sum$Year >= 1979])
  recent_sd <- sd(annual_sum$Median[annual_sum$Year >= 1979])
  
  annual_sum = annual_sum %>%
    mutate(low =
             if_else(
               Year < 1979,
               reference_mean - reference_sd,
               recent_mean - recent_sd
             ))
  
  
  annual_sum = annual_sum %>%
    mutate(high =
             if_else(
               Year  < 1979,
               reference_mean + reference_sd,
               recent_mean + recent_sd
             ))
  
  plot_sup_precip_mean <-
    ggplot(data = annual_sum, aes(x = Year, y = Median)) +
    # ylim(40,100) +
    geom_line() +
    geom_point(colour = "black", size = 0.5) +
    geom_ribbon(
      aes(ymin = low, ymax = high),
      alpha = 0.1,
      linetype = "dashed",
      color = "grey"
    ) +
    theme(axis.title.x = element_blank(), axis.title.y = element_blank()) +
    # labs(# title = "Lake Superior Annual Precipitation Comparison: 1950-1978 vs 1979-2019",
    #   y = "mm",
    #   x = "Year") +
    geom_segment(aes(
      x = 1979,
      xend = 2019,
      y = recent_mean,
      yend = recent_mean
    ),
    data = annual_sum) +
    geom_segment(aes(
      x = 1950,
      xend = 1979,
      y = reference_mean,
      yend = reference_mean
    ),
    data = annual_sum)
  return(plot_sup_precip_mean)
}

cpt_func <- function(lake_name, balance_component) {
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
  
  annual_sum <- aggregate(Median ~ Year , data = sup_precip , sum)
  annual_2.5 <-
    aggregate(X2.5.Percentile ~ Year , data = sup_precip , sum)
  annual_97.5 <-
    aggregate(X97.5.Percentile ~ Year , data = sup_precip , sum)
  
  reference_mean <-
    mean(annual_sum$Median[annual_sum$Year < 1979])
  reference_sd <- sd(annual_sum$Median[annual_sum$Year < 1979])
  recent_mean <- mean(annual_sum$Median[annual_sum$Year >= 1979])
  recent_sd <- sd(annual_sum$Median[annual_sum$Year >= 1979])
  
  sup_precip.ts <-
    ts(annual_sum$Median,
       start = c(1950),
       end = c(2019))
  year_index = cpts(cpt.mean(sup_precip.ts))
  split_year = annual_sum[year_index,]$Year
  
  reference_mean_cpt <-
    mean(annual_sum$Median[annual_sum$Year < split_year])
  reference_sd_cpt <-
    sd(annual_sum$Median[annual_sum$Year < split_year])
  recent_mean_cpt <-
    mean(annual_sum$Median[annual_sum$Year >= split_year])
  recent_sd_cpt <-
    sd(annual_sum$Median[annual_sum$Year >= split_year])
  
  plot_sup_precip_mean_cpt <-
    ggplot(data = annual_sum, aes(x = Year, y = Median)) +
    # ylim(40,100) +
    geom_line() +
    geom_point(colour = "black", size = 0.5) +
    theme(axis.title.x = element_blank(), axis.title.y = element_blank()) +
    # labs(# title = "Lake Superior Annual Percipitation Comparison: 1950-2011 vs 2012-2019",
    #   y = "mm",
    #   x = "Year") +
    geom_segment(aes(
      x = split_year,
      xend = 2019,
      y = recent_mean_cpt,
      yend = recent_mean_cpt
    ),
    data = annual_sum) +
    geom_segment(
      aes(
        x = 1950,
        xend = split_year,
        y = reference_mean_cpt,
        yend = reference_mean_cpt
      ),
      data = annual_sum
    )
  
  return(plot_sup_precip_mean_cpt)
}

cpt_ci_func <- function(lake_name, balance_component) {
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
  
  sup_precip.ts <-
    ts(annual_sum$Median,
       start = c(1950),
       end = c(2019))
  year_index = cpts(cpt.mean(sup_precip.ts))
  split_year = annual_sum[year_index,]$Year
  #plot(cpt.mean(sup_precip.ts))
  
  annual_sum <- aggregate(Median ~ Year , data = sup_precip , sum)
  annual_2.5 <-
    aggregate(X2.5.Percentile ~ Year , data = sup_precip , sum)
  annual_97.5 <-
    aggregate(X97.5.Percentile ~ Year , data = sup_precip , sum)
  
  reference_mean_cpt <-
    mean(annual_sum$Median[annual_sum$Year < split_year])
  reference_sd_cpt <-
    sd(annual_sum$Median[annual_sum$Year < split_year])
  recent_mean_cpt <-
    mean(annual_sum$Median[annual_sum$Year >= split_year])
  recent_sd_cpt <-
    sd(annual_sum$Median[annual_sum$Year >= split_year])
  
  annual_sum = annual_sum %>%
    mutate(
      low =
        if_else(
          Year < split_year,
          reference_mean_cpt - reference_sd_cpt,
          recent_mean_cpt - recent_sd_cpt
        )
    )
  
  
  annual_sum = annual_sum %>%
    mutate(
      high =
        if_else(
          Year < split_year,
          reference_mean_cpt + reference_sd_cpt,
          recent_mean_cpt + recent_sd_cpt
        )
    )
  
  plot_sup_precip_mean_cpt <-
    ggplot(data = annual_sum, aes(x = Year, y = Median)) +
    # ylim(40,100) +
    geom_line() +
    geom_point(colour = "black", size = 0.5) +
    # geom_bar(stat="identity", position = "identity", fill="red") +
    geom_ribbon(
      aes(ymin = low, ymax = high),
      alpha = 0.1,
      linetype = "dashed",
      color = "grey"
    ) +
    theme(axis.title.x = element_blank(), axis.title.y = element_blank()) +
    # labs(# title = "Lake Superior Annual Percipitation Comparison: 1950-2011 vs 2012-2019",
    #   y = "mm",
    #   x = "Year") +
    geom_segment(aes(
      x = split_year,
      xend = 2019,
      y = recent_mean_cpt,
      yend = recent_mean_cpt
    ),
    data = annual_sum) +
    geom_segment(
      aes(
        x = 1950,
        xend = split_year,
        y = reference_mean_cpt,
        yend = reference_mean_cpt
      ),
      data = annual_sum
    )
  
  return(plot_sup_precip_mean_cpt)
}

smooth_func <- function(lake_name, balance_component) {
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
  
  annual_sum <- aggregate(Median ~ Year , data = sup_precip , sum)
  annual_2.5 <-
    aggregate(X2.5.Percentile ~ Year , data = sup_precip , sum)
  annual_97.5 <-
    aggregate(X97.5.Percentile ~ Year , data = sup_precip , sum)
  
  reference_mean <-
    mean(annual_sum$Median[annual_sum$Year < 1979])
  reference_sd <- sd(annual_sum$Median[annual_sum$Year < 1979])
  recent_mean <- mean(annual_sum$Median[annual_sum$Year >= 1979])
  recent_sd <- sd(annual_sum$Median[annual_sum$Year >= 1979])
  
  plot_sup_precip_smoothmean <-
    ggplot(data = annual_sum, aes(x = Year, y = Median)) +
    # ylim(40,100) +
    geom_line() +
    geom_point(colour = "black", size = 0.5) +
    theme(axis.title.x = element_blank(), axis.title.y = element_blank()) +
    # labs(y = "mm",
    #      x = "Year") +
    geom_smooth(color = "black", size = 0.5)
  plot_sup_precip_smoothmean
  
  return(plot_sup_precip_smoothmean)
}

hockeystick_func <- function(lake_name, balance_component) {
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
  
  annual_sum <- aggregate(Median ~ Year , data = sup_precip , sum)
  annual_2.5 <-
    aggregate(X2.5.Percentile ~ Year , data = sup_precip , sum)
  annual_97.5 <-
    aggregate(X97.5.Percentile ~ Year , data = sup_precip , sum)
  
  reference_mean <-
    mean(annual_sum$Median[annual_sum$Year < 1979])
  reference_sd <- sd(annual_sum$Median[annual_sum$Year < 1979])
  recent_mean <- mean(annual_sum$Median[annual_sum$Year >= 1979])
  recent_sd <- sd(annual_sum$Median[annual_sum$Year >= 1979])
  
  set.seed(12)
  xx <- annual_sum$Year
  yy <- annual_sum$Median
  dati <- data.frame(x = xx, y = yy)
  out.lm <- lm(y ~ x, data = dati)
  o <- segmented(out.lm,
                 seg.Z = ~ x,
                 control = seg.control(display = FALSE))
  summary(o)
  slope(o)
  dat2 <- data.frame(x = xx, y = broken.line(o)$fit)
  
  ggplot(dati, aes(x = x, y = y)) +
    ylim(40, 110) +
    geom_line() +
    geom_line(data = dat2, color = 'red') +
    theme(axis.title.x = element_blank(), axis.title.y = element_blank()) +
    geom_point(colour = "black", size = 0.5)
  # + labs(y = "mm",
  #      x = "Year")
}

 
# func = cpt_func
# func = smooth_func
func = mean_ci_func
# func = cpt_ci_func
# func = hockeystick_func

ggarrange(
  func("superior", "Precip"),
  func("superior", "Evap"),
  func("superior", "Runoff"),
  func("miHuron", "Precip"),
  func("miHuron", "Evap"),
  func("miHuron", "Runoff"),
  func("erie", "Precip"),
  func("erie", "Evap"),
  func("erie", "Runoff"),
  ncol = 3,
  nrow = 3
) 
