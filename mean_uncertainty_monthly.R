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


lake_names = c("Superior", "MichiganHuron", "Erie", "Ontario")
balance_components = c("Precipitation", "Evaporation", "Runoff","Outflow")
months = c("1", "2", "3", "4", "5" , "6", "7", "8", "9", "10", "11", "12")

func <- function(lake_name, balance_component, month_str) {
  filename = paste("./l2s_posterior/",
                   lake_name,
                   balance_component,
                   "_GLWBData.csv",
                   sep = "")
  
  sup_precip <- read.csv(filename)
  sup_precip <- filter(sup_precip, Month == month_str)
  
  str(sup_precip)
  sup_precip$yearmon <-
    as.yearmon(paste(sup_precip$Year, sup_precip$Month), "%Y %m")
  sup_precip$formated_date <-
    format(as.Date(sup_precip$yearmon), "%m/%Y")
  
  sup_precip$order<-1:nrow(sup_precip)
  sup_precip
  
  reference_mean <- mean(sup_precip$Median[sup_precip$Year < 1979])
  reference_sd <- sd(sup_precip$Median[sup_precip$Year < 1979])
  recent_mean <- mean(sup_precip$Median[sup_precip$Year >= 1979])
  recent_sd <- sd(sup_precip$Median[sup_precip$Year >= 1979])
  
  plot_sup_precip_linear <-
    ggplot(data = sup_precip, aes(x = order, y = Median)) +
    geom_point() +
    labs(title = "",
         y = "mm",
         x = "Date") +
    geom_smooth(method="lm", col="black") +
    stat_regline_equation()
  # geom_segment(aes(
  #   x = as.yearmon(paste("1979 ", month_str, sep = ''), "%Y %m"),
  #   xend = as.yearmon(paste("2019 ", month_str, sep = ''), "%Y %m"),
  #   y = recent_mean,
  #   yend = recent_mean
  # ),
  # data = sup_precip) +
  # geom_segment(
  #   aes(
  #     x = as.yearmon(paste("1950 ", month_str, sep = ''), "%Y %m"),
  #     xend = as.yearmon(paste("1978 ", month_str, sep = ''), "%Y %m"),
  #     y = reference_mean,
  #     yend = reference_mean
  #   ),
  #   data = sup_precip
  # )
  plot_sup_precip_linear
  ggsave(
    paste(
      './output_monthly/',
      lake_name,
      balance_component,
      month_str,
      "linear.png",
      sep = "_"
    )
  )

  plot_sup_precip_mean <-
    ggplot(data = sup_precip, aes(x = yearmon, y = Median)) +
    geom_line(color = "red") +
    labs(title = "",
         y = "mm",
         x = "Date") +
    geom_segment(aes(
      x = as.yearmon(paste("1979 ", month_str, sep = ''), "%Y %m"),
      xend = as.yearmon(paste("2019 ", month_str, sep = ''), "%Y %m"),
      y = recent_mean,
      yend = recent_mean
    ),
    data = sup_precip) +
    geom_segment(
      aes(
        x = as.yearmon(paste("1950 ", month_str, sep = ''), "%Y %m"),
        xend = as.yearmon(paste("1978 ", month_str, sep = ''), "%Y %m"),
        y = reference_mean,
        yend = reference_mean
      ),
      data = sup_precip
    )
  plot_sup_precip_mean
  ggsave(
    paste(
      './output_monthly/',
      lake_name,
      balance_component,
      month_str,
      "mean.png",
      sep = "_"
    )
  )

  sup_precip.ts <-
    ts(sup_precip$Median,
       start = c(1950),
       end = c(2019))
  year_index = cpts(cpt.mean(sup_precip.ts))
  split_year = sup_precip[year_index, ]$Year

  reference_mean_cpt <-
    mean(sup_precip$Median[sup_precip$Year < split_year])
  reference_sd_cpt <-
    sd(sup_precip$Median[sup_precip$Year < split_year])
  recent_mean_cpt <-
    mean(sup_precip$Median[sup_precip$Year >= split_year])
  recent_sd_cpt <-
    sd(sup_precip$Median[sup_precip$Year >= split_year])

  plot_sup_precip_mean_cpt <-
    ggplot(data = sup_precip, aes(x = yearmon, y = Median)) +
    geom_line(color = "red") +
    labs(title = "",
         y = "mm",
         x = "Date") +
    geom_segment(
      aes(
        x = as.yearmon(paste(
          as.character(split_year), " ", month_str, sep = ''
        ), "%Y %m"),
        xend = as.yearmon(paste("2019 ", month_str, sep = ''), "%Y %m"),
        y = recent_mean_cpt,
        yend = recent_mean_cpt
      ),
      data = sup_precip
    ) +
    geom_segment(
      aes(
        x = as.yearmon(paste("1950 ", month_str, sep = ''), "%Y %m"),
        xend = as.yearmon(paste(
          as.character(split_year), " ", month_str, sep = ''
        ), "%Y %m"),
        y = reference_mean_cpt,
        yend = reference_mean_cpt
      ),
      data = sup_precip
    )
  plot_sup_precip_mean_cpt

  ggsave(
    paste(
      './output_monthly/',
      lake_name,
      balance_component,
      month_str,
      "mean_cpt.png",
      sep = "_"
    )
  )


  plot_sup_precip_smoothmean <-
    ggplot(data = sup_precip, aes(x = yearmon, y = Median)) +
    geom_line(color = "red") +
    labs(y = "mm",
         x = "Date") +
    geom_smooth(color = "black", size = 0.5)
  plot_sup_precip_smoothmean
  ggsave(
    paste(
      './output_monthly/',
      lake_name,
      balance_component,
      month_str,
      "smoothmean.png",
      sep = "_"
    )
  )


  plot_sup_precip_usgserror_low <-
    ggplot(data = sup_precip,
           aes(x = yearmon,
               y = Median)) +
    # xlim (1980, 1985) +
    # ylim (0, 200) +
    geom_ribbon(aes(ymin = Median * (1 - 0.15),
                    ymax = Median * (1 + 0.15)), fill = "grey70") +
    # labs(title = "Uncertainty from Neff and Nicholas Paper (low:15%)",
    #      y = "mm",
    #      x = "Year") +
    geom_line(color = "red")
  # stat_summary(
  #   geom = "ribbon",
  #   fun.min = Median * (1 - 0.15),
  #   fun.max = Median * (1 + 0.15),
  #   aes(fill = type),
  #   alpha = 0.3
  # ) +
  # theme_bw() +
  # geom_errorbar(aes(
  #   ymin = Median * (1 - 0.15),
  #   ymax = Median * (1 + 0.15),
  #   size = .2)
  plot_sup_precip_usgserror_low

  plot_sup_precip_usgserror_high <-
    ggplot(data = sup_precip, aes(x = yearmon, y = Median)) +
    # xlim (1980, 1985) +
    # ylim (0, 200) +
    geom_ribbon(aes(ymin = Median * (1 - 0.45),
                    ymax = Median * (1 + 0.45)), fill = "grey70") +
    # labs(title = "Uncertainty from Neff and Nicholas Paper (high:45%)",
    #      y = "mm",
    #      x = "Year") +
    geom_line(color = "red")
  plot_sup_precip_usgserror_high

  plot_sup_precip_l2serror <-
    ggplot(data = sup_precip, aes(x = yearmon, y = Median)) +
    # xlim (1979, 2019) +
    # ylim (0, 200) +
    geom_ribbon(aes(ymin = X2.5.Percentile,
                    ymax = X97.5.Percentile),
                fill = "grey60") +
    # labs(title = "Uncertainty from L2SWBM Model (95% Confidence Interval)",
    #      y = "mm",
    #      x = "Year") +
    geom_line(color = "red")
  plot_sup_precip_l2serror
}

# func("erie", "Evap", "1")

for (month in months) {
  for (lake_name in lake_names) {
    for (balance_component in balance_components) {
      func(lake_name, balance_component, month)
    }
  }
}
