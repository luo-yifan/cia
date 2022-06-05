library(ggplot2)
library(ggpubr)
library(dplyr)
library(tidyr)
library(tibble)
library(zoo)
library(changepoint)
library(r2r)
library(lubridate)

setwd("~/WorkSpace/cia")

lake_names = c("Superior", "MichiganHuron", "Erie", "Ontario")
balance_components = c("Precipitation", "Evaporation", "Runoff", "Outflow")
months = c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12")

long_19502020_func <- function(lake_name, balance_component, month_str) {
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
  
  sup_precip_1979 <- sup_precip[which(sup_precip$Year >= 1979), ]
  
  sup_precip_1979$order <- 1:nrow(sup_precip_1979)
  sup_precip_1979
  
  reference_mean <- mean(sup_precip$Median[sup_precip$Year < 1979])
  reference_sd <- sd(sup_precip$Median[sup_precip$Year < 1979])
  recent_mean <- mean(sup_precip$Median[sup_precip$Year >= 1979])
  recent_sd <- sd(sup_precip$Median[sup_precip$Year >= 1979])
  
  labels = if (month_str == "1")
    labs(y = lake_name, x = NULL)
  else
    labs(y = NULL, x = NULL)
  title = if (lake_name == "Superior")
    ggtitle(month_str)
  else
    NULL
  
  plot_sup_precip_linear <-
    ggplot(data = sup_precip, aes(x = Year, y = Median)) +
    geom_bar(stat="identity", width=1) +
    labels + title + theme(plot.title = element_text(hjust = 0.5)) +
    geom_smooth(method = "lm", col = "black") +
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
  # ggsave(
  #   paste(
  #     './output_monthly_19502020/',
  #     lake_name,
  #     balance_component,
  #     month_str,
  #     "linear_bar.png",
  #     sep = "_"
  #   )
  # )
  
 
  # plot_sup_precip_mean <-
  #   ggplot(data = sup_precip, aes(x = yearmon, y = Median)) +
  #   geom_line(color = "red") +
  #   labs(title = "",
  #        y = "mm",
  #        x = "Date") +
  #   geom_segment(aes(
  #     x = as.yearmon(paste("1979 ", month_str, sep = ''), "%Y %m"),
  #     xend = as.yearmon(paste("2019 ", month_str, sep = ''), "%Y %m"),
  #     y = recent_mean,
  #     yend = recent_mean
  #   ),
  #   data = sup_precip) +
  #   geom_segment(
  #     aes(
  #       x = as.yearmon(paste("1950 ", month_str, sep = ''), "%Y %m"),
  #       xend = as.yearmon(paste("1978 ", month_str, sep = ''), "%Y %m"),
  #       y = reference_mean,
  #       yend = reference_mean
  #     ),
  #     data = sup_precip
  #   )
  # plot_sup_precip_mean
  # ggsave(
  #   paste(
  #     './output_monthly/',
  #     lake_name,
  #     balance_component,
  #     month_str,
  #     "mean.png",
  #     sep = "_"
  #   )
  # )
  # 
  # sup_precip.ts <-
  #   ts(sup_precip$Median,
  #      start = c(1950),
  #      end = c(2019))
  # year_index = cpts(cpt.mean(sup_precip.ts))
  # split_year = sup_precip[year_index,]$Year
  # 
  # reference_mean_cpt <-
  #   mean(sup_precip$Median[sup_precip$Year < split_year])
  # reference_sd_cpt <-
  #   sd(sup_precip$Median[sup_precip$Year < split_year])
  # recent_mean_cpt <-
  #   mean(sup_precip$Median[sup_precip$Year >= split_year])
  # recent_sd_cpt <-
  #   sd(sup_precip$Median[sup_precip$Year >= split_year])
  # 
  # plot_sup_precip_mean_cpt <-
  #   ggplot(data = sup_precip, aes(x = yearmon, y = Median)) +
  #   geom_line(color = "red") +
  #   labs(title = "",
  #        y = "mm",
  #        x = "Date") +
  #   geom_segment(
  #     aes(
  #       x = as.yearmon(paste(
  #         as.character(split_year), " ", month_str, sep = ''
  #       ), "%Y %m"),
  #       xend = as.yearmon(paste("2019 ", month_str, sep = ''), "%Y %m"),
  #       y = recent_mean_cpt,
  #       yend = recent_mean_cpt
  #     ),
  #     data = sup_precip
  #   ) +
  #   geom_segment(
  #     aes(
  #       x = as.yearmon(paste("1950 ", month_str, sep = ''), "%Y %m"),
  #       xend = as.yearmon(paste(
  #         as.character(split_year), " ", month_str, sep = ''
  #       ), "%Y %m"),
  #       y = reference_mean_cpt,
  #       yend = reference_mean_cpt
  #     ),
  #     data = sup_precip
  #   )
  # plot_sup_precip_mean_cpt
  # 
  # ggsave(
  #   paste(
  #     './output_monthly/',
  #     lake_name,
  #     balance_component,
  #     month_str,
  #     "mean_cpt.png",
  #     sep = "_"
  #   )
  # )
  # 
  # 
  # plot_sup_precip_smoothmean <-
  #   ggplot(data = sup_precip, aes(x = yearmon, y = Median)) +
  #   geom_line(color = "red") +
  #   labs(y = "mm",
  #        x = "Date") +
  #   geom_smooth(color = "black", size = 0.5)
  # plot_sup_precip_smoothmean
  # ggsave(
  #   paste(
  #     './output_monthly/',
  #     lake_name,
  #     balance_component,
  #     month_str,
  #     "smoothmean.png",
  #     sep = "_"
  #   )
  # )
  # 
  # 
  # plot_sup_precip_usgserror_low <-
  #   ggplot(data = sup_precip,
  #          aes(x = yearmon,
  #              y = Median)) +
  #   # xlim (1980, 1985) +
  #   # ylim (0, 200) +
  #   geom_ribbon(aes(ymin = Median * (1 - 0.15),
  #                   ymax = Median * (1 + 0.15)), fill = "grey70") +
  #   # labs(title = "Uncertainty from Neff and Nicholas Paper (low:15%)",
  #   #      y = "mm",
  #   #      x = "Year") +
  #   geom_line(color = "red")
  # # stat_summary(
  # #   geom = "ribbon",
  # #   fun.min = Median * (1 - 0.15),
  # #   fun.max = Median * (1 + 0.15),
  # #   aes(fill = type),
  # #   alpha = 0.3
  # # ) +
  # # theme_bw() +
  # # geom_errorbar(aes(
  # #   ymin = Median * (1 - 0.15),
  # #   ymax = Median * (1 + 0.15),
  # #   size = .2)
  # plot_sup_precip_usgserror_low
  # 
  # plot_sup_precip_usgserror_high <-
  #   ggplot(data = sup_precip, aes(x = yearmon, y = Median)) +
  #   # xlim (1980, 1985) +
  #   # ylim (0, 200) +
  #   geom_ribbon(aes(ymin = Median * (1 - 0.45),
  #                   ymax = Median * (1 + 0.45)), fill = "grey70") +
  #   # labs(title = "Uncertainty from Neff and Nicholas Paper (high:45%)",
  #   #      y = "mm",
  #   #      x = "Year") +
  #   geom_line(color = "red")
  # plot_sup_precip_usgserror_high
  # 
  # plot_sup_precip_l2serror <-
  #   ggplot(data = sup_precip, aes(x = yearmon, y = Median)) +
  #   # xlim (1979, 2019) +
  #   # ylim (0, 200) +
  #   geom_ribbon(aes(ymin = X2.5.Percentile,
  #                   ymax = X97.5.Percentile),
  #               fill = "grey60") +
  #   # labs(title = "Uncertainty from L2SWBM Model (95% Confidence Interval)",
  #   #      y = "mm",
  #   #      x = "Year") +
  #   geom_line(color = "red")
  # plot_sup_precip_l2serror
}

recent_19802020_func <- function(lake_name, balance_component, month_str) {
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
  
  sup_precip_1979 <- sup_precip[which(sup_precip$Year >= 1979), ]
  
  sup_precip_1979$order <- 1:nrow(sup_precip_1979)
  sup_precip_1979
  
  labels = if (month_str == "1")
    labs(y = lake_name, x = NULL)
  else
    labs(y = NULL, x = NULL)
  title = if (lake_name == "Superior")
    ggtitle(month_str)
  else
    NULL
  
  plot_sup_precip_linear_1979 <-
    ggplot(data = sup_precip_1979, aes(x = Year, y = Median)) +
    geom_bar(stat="identity", width=1) +
    labels + title + theme(plot.title = element_text(hjust = 0.5)) +
    geom_smooth(method = "lm", col = "black") +
    stat_regline_equation()

  # # geom_segment(aes(
  # #   x = as.yearmon(paste("1979 ", month_str, sep = ''), "%Y %m"),
  # #   xend = as.yearmon(paste("2019 ", month_str, sep = ''), "%Y %m"),
  # #   y = recent_mean,
  # #   yend = recent_mean
  # # ),
  # # data = sup_precip) +
  # # geom_segment(
  # #   aes(
  # #     x = as.yearmon(paste("1950 ", month_str, sep = ''), "%Y %m"),
  # #     xend = as.yearmon(paste("1978 ", month_str, sep = ''), "%Y %m"),
  # #     y = reference_mean,
  # #     yend = reference_mean
  # #   ),
  # #   data = sup_precip
  # # )
  plot_sup_precip_linear_1979
  # ggsave(
  #   paste(
  #     './output_monthly_19792020/',
  #     lake_name,
  #     balance_component,
  #     month_str,
  #     "linear_bar.png",
  #     sep = "_"
  #   )
  # )
  # 
  # plot_sup_precip_mean <-
  #   ggplot(data = sup_precip, aes(x = yearmon, y = Median)) +
  #   geom_line(color = "red") +
  #   labs(title = "",
  #        y = "mm",
  #        x = "Date") +
  #   geom_segment(aes(
  #     x = as.yearmon(paste("1979 ", month_str, sep = ''), "%Y %m"),
  #     xend = as.yearmon(paste("2019 ", month_str, sep = ''), "%Y %m"),
  #     y = recent_mean,
  #     yend = recent_mean
  #   ),
  #   data = sup_precip) +
  #   geom_segment(
  #     aes(
  #       x = as.yearmon(paste("1950 ", month_str, sep = ''), "%Y %m"),
  #       xend = as.yearmon(paste("1978 ", month_str, sep = ''), "%Y %m"),
  #       y = reference_mean,
  #       yend = reference_mean
  #     ),
  #     data = sup_precip
  #   )
  # plot_sup_precip_mean
  # ggsave(
  #   paste(
  #     './output_monthly/',
  #     lake_name,
  #     balance_component,
  #     month_str,
  #     "mean.png",
  #     sep = "_"
  #   )
  # )
  # 
  # sup_precip.ts <-
  #   ts(sup_precip$Median,
  #      start = c(1950),
  #      end = c(2019))
  # year_index = cpts(cpt.mean(sup_precip.ts))
  # split_year = sup_precip[year_index,]$Year
  # 
  # reference_mean_cpt <-
  #   mean(sup_precip$Median[sup_precip$Year < split_year])
  # reference_sd_cpt <-
  #   sd(sup_precip$Median[sup_precip$Year < split_year])
  # recent_mean_cpt <-
  #   mean(sup_precip$Median[sup_precip$Year >= split_year])
  # recent_sd_cpt <-
  #   sd(sup_precip$Median[sup_precip$Year >= split_year])
  # 
  # plot_sup_precip_mean_cpt <-
  #   ggplot(data = sup_precip, aes(x = yearmon, y = Median)) +
  #   geom_line(color = "red") +
  #   labs(title = "",
  #        y = "mm",
  #        x = "Date") +
  #   geom_segment(
  #     aes(
  #       x = as.yearmon(paste(
  #         as.character(split_year), " ", month_str, sep = ''
  #       ), "%Y %m"),
  #       xend = as.yearmon(paste("2019 ", month_str, sep = ''), "%Y %m"),
  #       y = recent_mean_cpt,
  #       yend = recent_mean_cpt
  #     ),
  #     data = sup_precip
  #   ) +
  #   geom_segment(
  #     aes(
  #       x = as.yearmon(paste("1950 ", month_str, sep = ''), "%Y %m"),
  #       xend = as.yearmon(paste(
  #         as.character(split_year), " ", month_str, sep = ''
  #       ), "%Y %m"),
  #       y = reference_mean_cpt,
  #       yend = reference_mean_cpt
  #     ),
  #     data = sup_precip
  #   )
  # plot_sup_precip_mean_cpt
  # 
  # ggsave(
  #   paste(
  #     './output_monthly/',
  #     lake_name,
  #     balance_component,
  #     month_str,
  #     "mean_cpt.png",
  #     sep = "_"
  #   )
  # )
  # 
  # 
  # plot_sup_precip_smoothmean <-
  #   ggplot(data = sup_precip, aes(x = yearmon, y = Median)) +
  #   geom_line(color = "red") +
  #   labs(y = "mm",
  #        x = "Date") +
  #   geom_smooth(color = "black", size = 0.5)
  # plot_sup_precip_smoothmean
  # ggsave(
  #   paste(
  #     './output_monthly/',
  #     lake_name,
  #     balance_component,
  #     month_str,
  #     "smoothmean.png",
  #     sep = "_"
  #   )
  # )
  # 
  # 
  # plot_sup_precip_usgserror_low <-
  #   ggplot(data = sup_precip,
  #          aes(x = yearmon,
  #              y = Median)) +
  #   # xlim (1980, 1985) +
  #   # ylim (0, 200) +
  #   geom_ribbon(aes(ymin = Median * (1 - 0.15),
  #                   ymax = Median * (1 + 0.15)), fill = "grey70") +
  #   # labs(title = "Uncertainty from Neff and Nicholas Paper (low:15%)",
  #   #      y = "mm",
  #   #      x = "Year") +
  #   geom_line(color = "red")
  # # stat_summary(
  # #   geom = "ribbon",
  # #   fun.min = Median * (1 - 0.15),
  # #   fun.max = Median * (1 + 0.15),
  # #   aes(fill = type),
  # #   alpha = 0.3
  # # ) +
  # # theme_bw() +
  # # geom_errorbar(aes(
  # #   ymin = Median * (1 - 0.15),
  # #   ymax = Median * (1 + 0.15),
  # #   size = .2)
  # plot_sup_precip_usgserror_low
  # 
  # plot_sup_precip_usgserror_high <-
  #   ggplot(data = sup_precip, aes(x = yearmon, y = Median)) +
  #   # xlim (1980, 1985) +
  #   # ylim (0, 200) +
  #   geom_ribbon(aes(ymin = Median * (1 - 0.45),
  #                   ymax = Median * (1 + 0.45)), fill = "grey70") +
  #   # labs(title = "Uncertainty from Neff and Nicholas Paper (high:45%)",
  #   #      y = "mm",
  #   #      x = "Year") +
  #   geom_line(color = "red")
  # plot_sup_precip_usgserror_high
  # 
  # plot_sup_precip_l2serror <-
  #   ggplot(data = sup_precip, aes(x = yearmon, y = Median)) +
  #   # xlim (1979, 2019) +
  #   # ylim (0, 200) +
  #   geom_ribbon(aes(ymin = X2.5.Percentile,
  #                   ymax = X97.5.Percentile),
  #               fill = "grey60") +
  #   # labs(title = "Uncertainty from L2SWBM Model (95% Confidence Interval)",
  #   #      y = "mm",
  #   #      x = "Year") +
  #   geom_line(color = "red")
  # plot_sup_precip_l2serror
}

# for (month in months) {
#   for (lake_name in lake_names) {
#     for (balance_component in balance_components) {
#       func(lake_name, balance_component, month)
#     }
#   }
# }

func = long_19502020_func
# func = recent_19802020_func

ggarrange(
    func("Superior", "Precipitation","1"),
    func("Superior", "Precipitation","2"),
    func("Superior", "Precipitation","3"),
    func("Superior", "Precipitation","4"),
    func("Superior", "Precipitation","5"),
    func("Superior", "Precipitation","6"),
    func("Superior", "Precipitation","7"),
    func("Superior", "Precipitation","8"),
    func("Superior", "Precipitation","9"),
    func("Superior", "Precipitation","10"),
    func("Superior", "Precipitation","11"),
    func("Superior", "Precipitation","12"),
    func("MichiganHuron", "Precipitation","1"),
    func("MichiganHuron", "Precipitation","2"),
    func("MichiganHuron", "Precipitation","3"),
    func("MichiganHuron", "Precipitation","4"),
    func("MichiganHuron", "Precipitation","5"),
    func("MichiganHuron", "Precipitation","6"),
    func("MichiganHuron", "Precipitation","7"),
    func("MichiganHuron", "Precipitation","8"),
    func("MichiganHuron", "Precipitation","9"),
    func("MichiganHuron", "Precipitation","10"),
    func("MichiganHuron", "Precipitation","11"),
    func("MichiganHuron", "Precipitation","12"),
    func("Erie", "Precipitation","1"),
    func("Erie", "Precipitation","2"),
    func("Erie", "Precipitation","3"),
    func("Erie", "Precipitation","4"),
    func("Erie", "Precipitation","5"),
    func("Erie", "Precipitation","6"),
    func("Erie", "Precipitation","7"),
    func("Erie", "Precipitation","8"),
    func("Erie", "Precipitation","9"),
    func("Erie", "Precipitation","10"),
    func("Erie", "Precipitation","11"),
    func("Erie", "Precipitation","12"),
    func("Ontario", "Precipitation","1"),
    func("Ontario", "Precipitation","2"),
    func("Ontario", "Precipitation","3"),
    func("Ontario", "Precipitation","4"),
    func("Ontario", "Precipitation","5"),
    func("Ontario", "Precipitation","6"),
    func("Ontario", "Precipitation","7"),
    func("Ontario", "Precipitation","8"),
    func("Ontario", "Precipitation","9"),
    func("Ontario", "Precipitation","10"),
    func("Ontario", "Precipitation","11"),
    func("Ontario", "Precipitation","12"),
  ncol = 12,
  nrow = 4
) %>%
  ggexport(filename = "precip_monthly_summarize_19502020.png", width = 3200, height = 900, dpi = 600)


ggarrange(
    func("Superior", "Evaporation","1"),
    func("Superior", "Evaporation","2"),
    func("Superior", "Evaporation","3"),
    func("Superior", "Evaporation","4"),
    func("Superior", "Evaporation","5"),
    func("Superior", "Evaporation","6"),
    func("Superior", "Evaporation","7"),
    func("Superior", "Evaporation","8"),
    func("Superior", "Evaporation","9"),
    func("Superior", "Evaporation","10"),
    func("Superior", "Evaporation","11"),
    func("Superior", "Evaporation","12"),
    func("MichiganHuron", "Evaporation","1"),
    func("MichiganHuron", "Evaporation","2"),
    func("MichiganHuron", "Evaporation","3"),
    func("MichiganHuron", "Evaporation","4"),
    func("MichiganHuron", "Evaporation","5"),
    func("MichiganHuron", "Evaporation","6"),
    func("MichiganHuron", "Evaporation","7"),
    func("MichiganHuron", "Evaporation","8"),
    func("MichiganHuron", "Evaporation","9"),
    func("MichiganHuron", "Evaporation","10"),
    func("MichiganHuron", "Evaporation","11"),
    func("MichiganHuron", "Evaporation","12"),
    func("Erie", "Evaporation","1"),
    func("Erie", "Evaporation","2"),
    func("Erie", "Evaporation","3"),
    func("Erie", "Evaporation","4"),
    func("Erie", "Evaporation","5"),
    func("Erie", "Evaporation","6"),
    func("Erie", "Evaporation","7"),
    func("Erie", "Evaporation","8"),
    func("Erie", "Evaporation","9"),
    func("Erie", "Evaporation","10"),
    func("Erie", "Evaporation","11"),
    func("Erie", "Evaporation","12"),
    func("Ontario", "Evaporation","1"),
    func("Ontario", "Evaporation","2"),
    func("Ontario", "Evaporation","3"),
    func("Ontario", "Evaporation","4"),
    func("Ontario", "Evaporation","5"),
    func("Ontario", "Evaporation","6"),
    func("Ontario", "Evaporation","7"),
    func("Ontario", "Evaporation","8"),
    func("Ontario", "Evaporation","9"),
    func("Ontario", "Evaporation","10"),
    func("Ontario", "Evaporation","11"),
    func("Ontario", "Evaporation","12"),
  ncol = 12,
  nrow = 4
) %>%
  ggexport(filename = "evap_monthly_summarize_19502020.png", width = 3200, height = 900, dpi = 600)

ggarrange(
    func("Superior", "Runoff","1"),
    func("Superior", "Runoff","2"),
    func("Superior", "Runoff","3"),
    func("Superior", "Runoff","4"),
    func("Superior", "Runoff","5"),
    func("Superior", "Runoff","6"),
    func("Superior", "Runoff","7"),
    func("Superior", "Runoff","8"),
    func("Superior", "Runoff","9"),
    func("Superior", "Runoff","10"),
    func("Superior", "Runoff","11"),
    func("Superior", "Runoff","12"),
    func("MichiganHuron", "Runoff","1"),
    func("MichiganHuron", "Runoff","2"),
    func("MichiganHuron", "Runoff","3"),
    func("MichiganHuron", "Runoff","4"),
    func("MichiganHuron", "Runoff","5"),
    func("MichiganHuron", "Runoff","6"),
    func("MichiganHuron", "Runoff","7"),
    func("MichiganHuron", "Runoff","8"),
    func("MichiganHuron", "Runoff","9"),
    func("MichiganHuron", "Runoff","10"),
    func("MichiganHuron", "Runoff","11"),
    func("MichiganHuron", "Runoff","12"),
    func("Erie", "Runoff","1"),
    func("Erie", "Runoff","2"),
    func("Erie", "Runoff","3"),
    func("Erie", "Runoff","4"),
    func("Erie", "Runoff","5"),
    func("Erie", "Runoff","6"),
    func("Erie", "Runoff","7"),
    func("Erie", "Runoff","8"),
    func("Erie", "Runoff","9"),
    func("Erie", "Runoff","10"),
    func("Erie", "Runoff","11"),
    func("Erie", "Runoff","12"),
    func("Ontario", "Runoff","1"),
    func("Ontario", "Runoff","2"),
    func("Ontario", "Runoff","3"),
    func("Ontario", "Runoff","4"),
    func("Ontario", "Runoff","5"),
    func("Ontario", "Runoff","6"),
    func("Ontario", "Runoff","7"),
    func("Ontario", "Runoff","8"),
    func("Ontario", "Runoff","9"),
    func("Ontario", "Runoff","10"),
    func("Ontario", "Runoff","11"),
    func("Ontario", "Runoff","12"),
  ncol = 12,
  nrow = 4
) %>%
  ggexport(filename = "runoff_monthly_summarize_19502020.png", width = 3200, height = 900, dpi = 600)

ggarrange(
    func("Superior", "Outflow","1"),
    func("Superior", "Outflow","2"),
    func("Superior", "Outflow","3"),
    func("Superior", "Outflow","4"),
    func("Superior", "Outflow","5"),
    func("Superior", "Outflow","6"),
    func("Superior", "Outflow","7"),
    func("Superior", "Outflow","8"),
    func("Superior", "Outflow","9"),
    func("Superior", "Outflow","10"),
    func("Superior", "Outflow","11"),
    func("Superior", "Outflow","12"),
    func("MichiganHuron", "Outflow","1"),
    func("MichiganHuron", "Outflow","2"),
    func("MichiganHuron", "Outflow","3"),
    func("MichiganHuron", "Outflow","4"),
    func("MichiganHuron", "Outflow","5"),
    func("MichiganHuron", "Outflow","6"),
    func("MichiganHuron", "Outflow","7"),
    func("MichiganHuron", "Outflow","8"),
    func("MichiganHuron", "Outflow","9"),
    func("MichiganHuron", "Outflow","10"),
    func("MichiganHuron", "Outflow","11"),
    func("MichiganHuron", "Outflow","12"),
    func("Erie", "Outflow","1"),
    func("Erie", "Outflow","2"),
    func("Erie", "Outflow","3"),
    func("Erie", "Outflow","4"),
    func("Erie", "Outflow","5"),
    func("Erie", "Outflow","6"),
    func("Erie", "Outflow","7"),
    func("Erie", "Outflow","8"),
    func("Erie", "Outflow","9"),
    func("Erie", "Outflow","10"),
    func("Erie", "Outflow","11"),
    func("Erie", "Outflow","12"),
    func("Ontario", "Outflow","1"),
    func("Ontario", "Outflow","2"),
    func("Ontario", "Outflow","3"),
    func("Ontario", "Outflow","4"),
    func("Ontario", "Outflow","5"),
    func("Ontario", "Outflow","6"),
    func("Ontario", "Outflow","7"),
    func("Ontario", "Outflow","8"),
    func("Ontario", "Outflow","9"),
    func("Ontario", "Outflow","10"),
    func("Ontario", "Outflow","11"),
    func("Ontario", "Outflow","12"),
  ncol = 12,
  nrow = 4
) %>%
  ggexport(filename = "outflow_monthly_summarize_19502020.png", width = 3200, height = 900, dpi = 600)

