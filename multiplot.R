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
  
  labels = get_labs(lake_name, balance_component)
  title = get_title(lake_name, balance_component)
  
  plot_sup_precip_uncertainty_percent <-
    ggplot(data = sup_precip, aes(x = abs(Median), y = uncertainty_percent)) +
    # geom_line() +
    geom_smooth(method = 'loess',
                colour = "red",
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
  
  labels = get_labs(lake_name, balance_component)
  title = get_title(lake_name, balance_component)
  
  plot_sup_precip_uncertainty_mm <-
    ggplot(data = sup_precip, aes(x = Median, y = uncertainty_mm)) +
    # geom_line() +
    geom_point(colour = "black", size = 0.5) +
    geom_smooth(method = 'loess',
                colour = "red",
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
  
  labels = get_labs(lake_name, balance_component)
  title = get_title(lake_name, balance_component)
  
  plot_sup_precip_mean <-
    ggplot(data = annual_sum, aes(x = Year, y = Median)) +
    geom_line() +
    geom_point(colour = "black", size = 0.5) +
    labels +
    title +
    theme(plot.title = element_text(hjust = 0.5)) +
    geom_segment(aes(
      x = 1979,
      xend = 2021,
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
  
  labels = get_labs(lake_name, balance_component)
  title = get_title(lake_name, balance_component)
  
  plot_sup_precip_mean <-
    ggplot(data = annual_sum, aes(x = Year, y = Median)) +
    geom_line() +
    geom_point(colour = "black", size = 0.5) +
    labels + title + theme(plot.title = element_text(hjust = 0.5)) +
    geom_ribbon(
      aes(ymin = low, ymax = high),
      alpha = 0.1,
      linetype = "dashed",
      colour = "grey"
    ) +
    geom_segment(aes(
      x = 1979,
      xend = 2021,
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
  
  labels = get_labs(lake_name, balance_component)
  title = get_title(lake_name, balance_component)
  
  plot_sup_precip_mean <-
    ggplot(data = annual_sum, aes(x = Year, y = Median)) +
    geom_line() +
    geom_point(colour = "black", size = 0.5) +
    labels + title + theme(plot.title = element_text(hjust = 0.5)) +
    geom_ribbon(
      aes(ymin = low, ymax = high),
      alpha = 0.1,
      linetype = "dashed",
      colour = "grey"
    ) +
    geom_segment(aes(
      x = 1979,
      xend = 2021,
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
       end = c(2021))
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
  
  labels = get_labs(lake_name, balance_component)
  title = get_title(lake_name, balance_component)
  
  plot_sup_precip_mean_cpt <-
    ggplot(data = annual_sum, aes(x = Year, y = Median)) +
    geom_line() +
    geom_point(colour = "black", size = 0.5) +
    labels + title + theme(plot.title = element_text(hjust = 0.5)) +
    geom_segment(aes(
      x = split_year,
      xend = 2021,
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
       end = c(2021))
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
  
  labels = get_labs(lake_name, balance_component)
  title = get_title(lake_name, balance_component)
  
  plot_sup_precip_mean_cpt <-
    ggplot(data = annual_sum, aes(x = Year, y = Median)) +
    geom_line() +
    geom_point(colour = "black", size = 0.5) +
    labels + title + theme(plot.title = element_text(hjust = 0.5)) +
    geom_ribbon(
      aes(ymin = low, ymax = high),
      alpha = 0.1,
      linetype = "dashed",
      colour = "grey"
    ) +
    geom_segment(aes(
      x = split_year,
      xend = 2021,
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
  
  # fit_bp = breakpoints(Median ~ 1, data = annual_sum, breaks = 2)
  
  sup_precip.ts <-
    ts(annual_sum$Median,
       start = c(1950),
       end = c(2021))
  year_index = cpts(cpt.mean(sup_precip.ts))

  # library(cpm)
  # fit_cpm = processStream(annual_sum$Median, cpmType = "Mann-Whitney")  # Multiple change points
  # fit_cpm$changePoints
  # split_year = fit_cpm$changePoints + 1950
  
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
  
  labels = get_labs(lake_name, balance_component)
  title = get_title(lake_name, balance_component)
  
  plot_sup_precip_mean_cpt <-
    ggplot(data = annual_sum, aes(x = Year, y = Median)) +
    geom_line() +
    geom_point(colour = "black", size = 0.5) +
    labels + title + theme(plot.title = element_text(hjust = 0.5)) +
    geom_ribbon(
      aes(ymin = low, ymax = high),
      alpha = 0.1,
      linetype = "dashed",
      colour = "grey"
    ) +
    geom_segment(aes(
      x = split_year,
      xend = 2021,
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
  
  labels = get_labs(lake_name, balance_component)
  title = get_title(lake_name, balance_component)
  
  plot_sup_precip_smoothmean <-
    ggplot(data = annual_sum, aes(x = Year, y = Median)) +
    geom_line() +
    geom_point(colour = "black", size = 0.5) +
    labels + title + theme(plot.title = element_text(hjust = 0.5)) +
    geom_smooth(colour = "red", size = 0.5)
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
  
  labels = get_labs(lake_name, balance_component)
  title = get_title(lake_name, balance_component)
  
  ggplot(dati, aes(x = x, y = y)) +
    geom_line() +
    geom_line(data = dat2, colour = 'red') +
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
  
  labels = get_labs(lake_name, balance_component)
  title = get_title(lake_name, balance_component)
  
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
  
  labels = get_labs(lake_name, balance_component)
  title = get_title(lake_name, balance_component)
  
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
  
  labels = get_labs(lake_name, balance_component)
  title = get_title(lake_name, balance_component)
  
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
              colour = "red",
              size = .75) +
    geom_ribbon(
      aes(ymin = ten_avg + ten_sd, ymax = ten_avg - ten_sd),
      alpha = 0.1,
      linetype = "dashed",
      colour = "grey"
    ) +
    labels + title  + theme(plot.title = element_text(hjust = 0.5))
}

cpt_multiple_func <- function(lake_name, balance_component) {
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
  
  labels = get_labs(lake_name, balance_component)
  title = get_title(lake_name, balance_component)
  
  library(segmented)
  fit_lm = lm(Median ~ 1 + Year, data = annual_sum)  # intercept-only model
  fit_segmented = segmented(fit_lm, seg.Z = ~Year, npsi = 2)  # Two change points along x
  
  if(!is.null(fit_segmented$psi)){
    fit <- numeric(length(annual_sum$Year)) * NA
    fit[complete.cases(rowSums(cbind(annual_sum$Median, annual_sum$Year)))] <- broken.line(fit_segmented)$fit
    data1 <- data.frame(Year = annual_sum$Year, Median = annual_sum$Median, fit = fit)
    
    res = ggplot(data1, aes(x = Year, y = Median)) + 
      geom_line() +
      geom_point(colour = "black", size = 0.5) +
      geom_line(aes(x = Year, y = fit), colour = 'red') +
      labels + title + theme(plot.title = element_text(hjust = 0.5)) 
    return(res)
  }
  else{
    res = ggplot(annual_sum, aes(x = Year, y = Median)) + 
      geom_line() +
      geom_point(colour = "black", size = 0.5) +
      labels + title + theme(plot.title = element_text(hjust = 0.5)) 
    return(res)
  }
}

# func = uncertainty_percent_func
# func = uncertainty_mm_func
# func = mean_func
# func = cpt_func
# func = mean_ci_func
# func = cpt_ci_func
func = smooth_func
# func = hockeystick_func
# func = autodetect_func
# func = set1979_func
# func = rollmean_func
# func = mean_stderror_func
# func = cpt_stderror_func
# func = cpt_multiple_func 

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
  nrow = 4
)



