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

theme_set(theme_grey())
setwd("./")

lake_name = c("Superior", "MichiganHuron", "Erie", "Ontario")
balance_component = c("Precipitation", "Evaporation", "Runoff", " Outflow")

uncertainty_percent_func <- function(lake_name, balance_component) {
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
  
  sup_precip <- sup_precip %>%
    mutate(uncertainty_percent = (abs(X97.5.Percentile - Median)) / abs(Median))
  
  labels = if (balance_component == "Precipitation")
    labs(y = lake_name, x = NULL)
  else
    labs(y = NULL, x = NULL)
  title = if (lake_name == "Superior")
    ggtitle(balance_component)
  else
    NULL
  
  plot_sup_precip_uncertainty_percent <-
    ggplot(data = sup_precip, aes(x = abs(Median), y = uncertainty_percent)) +
    # geom_line() +
    geom_smooth(method = 'loess',
                color = "red",
                size = 0.5) +
    geom_point(colour = "black", size = 0.5) +
    labels +
    title +
    theme(plot.title = element_text(hjust = 0.5))
  
  return(plot_sup_precip_uncertainty_percent)
}

uncertainty_mm_func <- function(lake_name, balance_component) {
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
  
  sup_precip <- sup_precip %>%
    mutate(uncertainty_mm = X97.5.Percentile - Median)
  
  labels = if (balance_component == "Precipitation")
    labs(y = lake_name, x = NULL)
  else
    labs(y = NULL, x = NULL)
  title = if (lake_name == "Superior")
    ggtitle(balance_component)
  else
    NULL
  
  plot_sup_precip_uncertainty_mm <-
    ggplot(data = sup_precip, aes(x = Median, y = uncertainty_mm)) +
    # geom_line() +
    geom_point(colour = "black", size = 0.5) +
    geom_smooth(method = 'loess',
                color = "red",
                size = 0.5) +
    labels +
    title +
    theme(plot.title = element_text(hjust = 0.5))
  
  return(plot_sup_precip_uncertainty_mm)
}

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
  recent_mean <- mean(annual_sum$Median[annual_sum$Year >= 1979])
  recent_sd <- sd(annual_sum$Median[annual_sum$Year >= 1979])
  
  labels = if (balance_component == "Precipitation")
    labs(y = lake_name, x = NULL)
  else
    labs(y = NULL, x = NULL)
  title = if (lake_name == "Superior")
    ggtitle(balance_component)
  else
    NULL
  
  plot_sup_precip_mean <-
    ggplot(data = annual_sum, aes(x = Year, y = Median)) +
    geom_line() +
    geom_point(colour = "black", size = 0.5) +
    labels +
    title +
    theme(plot.title = element_text(hjust = 0.5)) +
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
  
  labels = if (balance_component == "Precipitation")
    labs(y = lake_name, x = NULL)
  else
    labs(y = NULL, x = NULL)
  title = if (lake_name == "Superior")
    ggtitle(balance_component)
  else
    NULL
  
  plot_sup_precip_mean <-
    ggplot(data = annual_sum, aes(x = Year, y = Median)) +
    geom_line() +
    geom_point(colour = "black", size = 0.5) +
    labels + title + theme(plot.title = element_text(hjust = 0.5)) +
    geom_ribbon(
      aes(ymin = low, ymax = high),
      alpha = 0.1,
      linetype = "dashed",
      color = "grey"
    ) +
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

mean_stderror_func <- function(lake_name, balance_component) {
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
  reference_stderror <-
    std.error(annual_sum$Median[annual_sum$Year < 1979])
  recent_mean <- mean(annual_sum$Median[annual_sum$Year >= 1979])
  recent_stderror <-
    std.error(annual_sum$Median[annual_sum$Year >= 1979])
  
  annual_sum = annual_sum %>%
    mutate(
      low =
        if_else(
          Year < 1979,
          reference_mean - reference_stderror,
          recent_mean - recent_stderror
        )
    )
  
  
  annual_sum = annual_sum %>%
    mutate(
      high =
        if_else(
          Year  < 1979,
          reference_mean + reference_stderror,
          recent_mean + recent_stderror
        )
    )
  
  labels = if (balance_component == "Precipitation")
    labs(y = lake_name, x = NULL)
  else
    labs(y = NULL, x = NULL)
  title = if (lake_name == "Superior")
    ggtitle(balance_component)
  else
    NULL
  
  plot_sup_precip_mean <-
    ggplot(data = annual_sum, aes(x = Year, y = Median)) +
    geom_line() +
    geom_point(colour = "black", size = 0.5) +
    labels + title + theme(plot.title = element_text(hjust = 0.5)) +
    geom_ribbon(
      aes(ymin = low, ymax = high),
      alpha = 0.1,
      linetype = "dashed",
      color = "grey"
    ) +
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
  split_year = annual_sum[year_index, ]$Year
  
  reference_mean_cpt <-
    mean(annual_sum$Median[annual_sum$Year < split_year])
  reference_sd_cpt <-
    sd(annual_sum$Median[annual_sum$Year < split_year])
  recent_mean_cpt <-
    mean(annual_sum$Median[annual_sum$Year >= split_year])
  recent_sd_cpt <-
    sd(annual_sum$Median[annual_sum$Year >= split_year])
  
  labels = if (balance_component == "Precipitation")
    labs(y = lake_name, x = NULL)
  else
    labs(y = NULL, x = NULL)
  title = if (lake_name == "Superior")
    ggtitle(balance_component)
  else
    NULL
  
  plot_sup_precip_mean_cpt <-
    ggplot(data = annual_sum, aes(x = Year, y = Median)) +
    geom_line() +
    geom_point(colour = "black", size = 0.5) +
    labels + title + theme(plot.title = element_text(hjust = 0.5)) +
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
  
  annual_sum <- aggregate(Median ~ Year , data = sup_precip , sum)
  annual_2.5 <-
    aggregate(X2.5.Percentile ~ Year , data = sup_precip , sum)
  annual_97.5 <-
    aggregate(X97.5.Percentile ~ Year , data = sup_precip , sum)
  
  sup_precip.ts <-
    ts(annual_sum$Median,
       start = c(1950),
       end = c(2019))
  year_index = cpts(cpt.mean(sup_precip.ts))
  split_year = annual_sum[year_index, ]$Year
  
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
  
  labels = if (balance_component == "Precipitation")
    labs(y = lake_name, x = NULL)
  else
    labs(y = NULL, x = NULL)
  title = if (lake_name == "Superior")
    ggtitle(balance_component)
  else
    NULL
  
  plot_sup_precip_mean_cpt <-
    ggplot(data = annual_sum, aes(x = Year, y = Median)) +
    geom_line() +
    geom_point(colour = "black", size = 0.5) +
    labels + title + theme(plot.title = element_text(hjust = 0.5)) +
    geom_ribbon(
      aes(ymin = low, ymax = high),
      alpha = 0.1,
      linetype = "dashed",
      color = "grey"
    ) +
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

cpt_stderror_func <- function(lake_name, balance_component) {
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
  
  sup_precip.ts <-
    ts(annual_sum$Median,
       start = c(1950),
       end = c(2019))
  year_index = cpts(cpt.mean(sup_precip.ts))
  split_year = annual_sum[year_index, ]$Year
  
  reference_mean_cpt <-
    mean(annual_sum$Median[annual_sum$Year < split_year])
  reference_stderror_cpt <-
    std.error(annual_sum$Median[annual_sum$Year < split_year])
  recent_mean_cpt <-
    mean(annual_sum$Median[annual_sum$Year >= split_year])
  recent_stderror_cpt <-
    std.error(annual_sum$Median[annual_sum$Year >= split_year])
  
  annual_sum = annual_sum %>%
    mutate(
      low =
        if_else(
          Year < split_year,
          reference_mean_cpt - reference_stderror_cpt,
          recent_mean_cpt - recent_stderror_cpt
        )
    )
  
  
  annual_sum = annual_sum %>%
    mutate(
      high =
        if_else(
          Year < split_year,
          reference_mean_cpt + reference_stderror_cpt,
          recent_mean_cpt + recent_stderror_cpt
        )
    )
  
  labels = if (balance_component == "Precipitation")
    labs(y = lake_name, x = NULL)
  else
    labs(y = NULL, x = NULL)
  title = if (lake_name == "Superior")
    ggtitle(balance_component)
  else
    NULL
  
  plot_sup_precip_mean_cpt <-
    ggplot(data = annual_sum, aes(x = Year, y = Median)) +
    geom_line() +
    geom_point(colour = "black", size = 0.5) +
    labels + title + theme(plot.title = element_text(hjust = 0.5)) +
    geom_ribbon(
      aes(ymin = low, ymax = high),
      alpha = 0.1,
      linetype = "dashed",
      color = "grey"
    ) +
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
  
  labels = if (balance_component == "Precipitation")
    labs(y = lake_name, x = NULL)
  else
    labs(y = NULL, x = NULL)
  title = if (lake_name == "Superior")
    ggtitle(balance_component)
  else
    NULL
  
  plot_sup_precip_smoothmean <-
    ggplot(data = annual_sum, aes(x = Year, y = Median)) +
    geom_line() +
    geom_point(colour = "black", size = 0.5) +
    labels + title + theme(plot.title = element_text(hjust = 0.5)) +
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
  
  labels = if (balance_component == "Precipitation")
    labs(y = lake_name, x = NULL)
  else
    labs(y = NULL, x = NULL)
  title = if (lake_name == "Superior")
    ggtitle(balance_component)
  else
    NULL
  
  ggplot(dati, aes(x = x, y = y)) +
    geom_line() +
    geom_line(data = dat2, color = 'red') +
    geom_point(colour = "black", size = 0.5) +
    labels + title  + theme(plot.title = element_text(hjust = 0.5))
}

autodetect_func <- function(lake_name, balance_component) {
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
  model <- piecewise.linear(dati$x, dati$y, CI = FALSE)
  
  dati$grp = factor(ifelse(dati$x > model$change.point, 1, 0))
  
  labels = if (balance_component == "Precipitation")
    labs(y = lake_name, x = NULL)
  else
    labs(y = NULL, x = NULL)
  title = if (lake_name == "Superior")
    ggtitle(balance_component)
  else
    NULL
  
  ggplot(dati, aes(x = x, y = y, group = grp)) +
    geom_line() +
    geom_point(size = 0.5) + geom_smooth(method = "lm",
                                         formula = y ~ x,
                                         col = "red") +
    labels + title  + theme(plot.title = element_text(hjust = 0.5))
}

set1979_func <- function(lake_name, balance_component) {
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
  model <- piecewise.linear(dati$x, dati$y, CI = FALSE)
  
  dati$grp = factor(ifelse(dati$x >= 1979, 1, 0))
  
  labels = if (balance_component == "Precipitation")
    labs(y = lake_name, x = NULL)
  else
    labs(y = NULL, x = NULL)
  title = if (lake_name == "Superior")
    ggtitle(balance_component)
  else
    NULL
  
  ggplot(dati, aes(x = x, y = y, group = grp)) +
    geom_line() +
    geom_point(size = 0.5) + geom_smooth(method = "lm",
                                         formula = y ~ x,
                                         col = "red") +
    labels + title  + theme(plot.title = element_text(hjust = 0.5))
}

rollmean_func <- function(lake_name, balance_component) {
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
  
  labels = if (balance_component == "Precipitation")
    labs(y = lake_name, x = NULL)
  else
    labs(y = NULL, x = NULL)
  title = if (lake_name == "Superior")
    ggtitle(balance_component)
  else
    NULL
  
  
  
  dur = 20
  
  dati %>%
    mutate(ten_avg = rollmean(y, dur,
                              align = "right",
                              fill = NA)) %>%
    mutate(ten_sd = rollapply(y, dur, sd,
                              align = "right",
                              fill = NA)) %>%
    ggplot(aes(x = x,
               y = y)) +
    geom_line() +
    geom_point(size = 0.5) +
    geom_line(aes(y = ten_avg),
              color = "red",
              size = .75) +
    geom_ribbon(
      aes(ymin = ten_avg + ten_sd, ymax = ten_avg - ten_sd),
      alpha = 0.1,
      linetype = "dashed",
      color = "grey"
    ) +
    labels + title  + theme(plot.title = element_text(hjust = 0.5))
}

# func = uncertainty_percent_func
# func = uncertainty_mm_func
# func = mean_func
# func = cpt_func
# func = smooth_func
# func = mean_ci_func
# func = cpt_ci_func
# func = hockeystick_func
# func = autodetect_func
# func = set1979_func
# func = rollmean_func
# func = mean_stderror_func
# func = cpt_stderror_func

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
  nrow = 4
)
