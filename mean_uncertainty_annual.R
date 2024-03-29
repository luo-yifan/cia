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
balance_component = c("Precip(mm)", "Evap(mm)", "Runoff(mm)", "Outflow(cms)")

get_labs = function(lake_name, balance_component){
  label = if (lake_name == "superior")
    labs(y = balance_component, x = NULL)
  else
    labs(y = NULL, x = NULL)
  return(label)
}
get_title = function(lake_name, balance_component){
  title = if (balance_component == "Precip(mm)")
    ggtitle(lake_name)
  else
    NULL
  return(title)
}

uncertainty_compare_func_precip <- function(lake_name, balance_component, radio, rname) {
  filename = paste("./l2s_posterior/",
                   lake_name,
                   balance_component,
                   "_analysis19502022_prior19001969_1m.csv",
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
  
  # reference_mean <-
  #   mean(annual_sum$Median[annual_sum$Year < 1979])
  # reference_sd <- sd(annual_sum$Median[annual_sum$Year < 1979])
  # recent_mean <- mean(annual_sum$Median[annual_sum$Year >= 1979])
  # recent_sd <- sd(annual_sum$Median[annual_sum$Year >= 1979])
  
  # plot_sup_precip_mean <-
  #   ggplot(data = annual_sum, aes(x = Year, y = Median)) +
  #   geom_line(color = "red", size = 0.5) +
  #   labs(
  #     # title = "Lake Superior Annual Percipitation Comparison: 1950-1978 vs 1979-2019",
  #        y = "(mm)",
  #        x = "Year") +
  #   geom_segment(aes(
  #     x = 1979,
  #     xend = 2019,
  #     y = recent_mean,
  #     yend = recent_mean
  #   ),
  #   data = annual_sum)  +
  #   # geom_ribbon(aes(ymin = recent_mean - recent_sd,
  #   #                 ymax = recent_mean + recent_sd),
  #   #             alpha = 0.1) + 
  #   geom_segment(aes(
  #     x = 1950,
  #     xend = 1979,
  #     y = reference_mean,
  #     yend = reference_mean
  #   ),
  #   data = annual_sum)
  # # + geom_ribbon(aes(ymin = reference_mean -  reference_sd,
  # #                   ymax = reference_mean +  reference_sd),
  # #               alpha = 0.1)
  # plot_sup_precip_mean
  # ggsave(paste('./output/',lake_name, balance_component, "mean.png", sep = "_"))
  # 
  # sup_precip.ts <-
  #   ts(annual_sum$Median,
  #      start = c(1950),
  #      end = c(2019))
  # year_index = cpts(cpt.mean(sup_precip.ts))
  # split_year = annual_sum[year_index, ]$Year
  # #plot(cpt.mean(sup_precip.ts))
  
  # reference_mean_cpt <-
  #   mean(annual_sum$Median[annual_sum$Year < split_year])
  # reference_sd_cpt <-
  #   sd(annual_sum$Median[annual_sum$Year < split_year])
  # recent_mean_cpt <-
  #   mean(annual_sum$Median[annual_sum$Year >= split_year])
  # recent_sd_cpt <-
  #   sd(annual_sum$Median[annual_sum$Year >= split_year])
  # 
  # plot_sup_precip_mean_cpt <-
  #   ggplot(data = annual_sum, aes(x = Year, y = Median)) +
  #   geom_line(color = "red", size = 0.5) +
  #   labs(
  #     # title = "Lake Superior Annual Percipitation Comparison: 1950-2011 vs 2012-2019",
  #        y = "mm",
  #        x = "Year") +
  #   geom_segment(aes(
  #     x = split_year,
  #     xend = 2019,
  #     y = recent_mean_cpt,
  #     yend = recent_mean_cpt
  #   ),
  #   data = annual_sum) +
  #   geom_segment(
  #     aes(
  #       x = 1950,
  #       xend = split_year,
  #       y = reference_mean_cpt,
  #       yend = reference_mean_cpt
  #     ),
  #     data = annual_sum
  #   )
  # plot_sup_precip_mean_cpt
  # ggsave(paste('./output/',lake_name, balance_component, "mean_cpt.png", sep = "_"))
  
  # plot_sup_precip_smoothmean <-
  #   ggplot(data = annual_sum, aes(x = Year, y = Median)) +
  #   geom_line(color = "red", size = 0.5) +
  #   labs(y = "mm",
  #        x = "Year") +
  #   geom_smooth(color = "black", size = 0.5)
  # plot_sup_precip_smoothmean
  # ggsave(paste('./output/',lake_name, balance_component, "smoothmean.png", sep = "_"))
  
  # plot_sup_precip_usgserror_low <-
  #   ggplot(data = annual_sum,
  #          aes(x = Year,
  #              y = Median)) +
  #   # xlim (1980, 2000) +
  #   # ylim (0, 120) +
  #   geom_ribbon(aes(ymin = Median * (1 - 0.15),
  #                   ymax = Median * (1 + 0.15)), fill = "grey70") +
  #   labs(title = "Uncertainty of Lake Superior Percipitation from Neff and Nicholas Paper (15%)",
  #        y = "mm",
  #        x = "Year") +
  #   geom_line(color = "red", size = 0.5)
  # plot_sup_precip_usgserror_low
  # 
  # plot_sup_precip_usgserror_high <-
  #   ggplot(data = annual_sum,
  #          aes(x = Year,
  #              y = Median)) +
  #   # xlim (1980, 2000) +
  #   # ylim (0, 120) +
  #   geom_ribbon(aes(ymin = Median * (1 - 0.45),
  #                   ymax = Median * (1 + 0.45)), fill = "grey70") +
  #   labs(title = "Uncertainty of Lake Superior Percipitation from Neff and Nicholas Paper (45%)",
  #        y = "mm",
  #        x = "Year") +
  #   geom_line(color = "red", size = 0.5)
  # plot_sup_precip_usgserror_high
  
  labels = get_labs(lake_name, balance_component)
  title = get_title(lake_name, balance_component)

  
  plot_sup_precip_l2serror_usgshigh <-
    ggplot(data = annual_sum,
           aes(x = Year,
               y = Median)) +
    # xlim (1980, 2000) +
    # ylim (0, 120) +
    geom_ribbon(aes(ymin = Median * (1 - 0.45),
                    ymax = Median * (1 + 0.45)), fill = "blue", alpha=0.3) +
    geom_ribbon(aes(
      ymin = annual_2.5$X2.5.Percentile,
      ymax = annual_97.5$X97.5.Percentile
    ),
    fill = "yellow", alpha=0.3) +
    # labs(
    #   # title = "Uncertainty of Lake Superior Percipitation L2SWBM Model (95% Confidence Interval)",
    #   y = "mm",
    #   x = "Year") +
    geom_line(color = "red", size = 0.5)+
    labels + title  + theme(plot.title = element_text(hjust = 0.5)) + 
    # Custom the Y scales:
    scale_y_continuous(
      
      # Features of the first axis
      name = balance_component,
      
      
      # Add a second axis and specify its features
      sec.axis = sec_axis( trans=~.*radio, name=rname,labels = function(x) format(x, scientific = TRUE))
    )
  plot_sup_precip_l2serror_usgshigh
 
  # plot_sup_precip_l2serror_usgslow <-
  #   ggplot(data = annual_sum,
  #          aes(x = Year,
  #              y = Median)) +
  #   # xlim (1980, 2000) +
  #   # ylim (0, 120) +
  #   geom_ribbon(aes(ymin = Median * (1 - 0.15),
  #                   ymax = Median * (1 + 0.15)), fill = "blue", alpha=0.3) +
  #   geom_ribbon(aes(
  #     ymin = annual_2.5$X2.5.Percentile,
  #     ymax = annual_97.5$X97.5.Percentile
  #   ),
  #   fill = "yellow", alpha=0.3) +
  #   labs(
  #     # title = "Uncertainty of Lake Superior Percipitation L2SWBM Model (95% Confidence Interval)",
  #     y = "mm",
  #     x = "Year") +
  #   geom_line(color = "red", size = 0.5)
  # plot_sup_precip_l2serror_usgslow
}

uncertainty_compare_func_evap <- function(lake_name, balance_component, radio, rname) {
  filename = paste("./l2s_posterior/",
                   lake_name,
                   balance_component,
                   "_analysis19502022_prior19001969_1m.csv",
                   sep = "")
  sup_evap <-
    read.csv(filename)
  str(sup_evap)
  sup_evap$yearmon <-
    as.yearmon(paste(sup_evap$Year, sup_evap$Month), "%Y %m")
  sup_evap$formated_date <-
    format(as.Date(sup_evap$yearmon), "%m/%Y")
  
  annual_sum <- aggregate(Median ~ Year , data = sup_evap , sum)
  annual_2.5 <-
    aggregate(X2.5.Percentile ~ Year , data = sup_evap , sum)
  annual_97.5 <-
    aggregate(X97.5.Percentile ~ Year , data = sup_evap , sum)
  
  labels = get_labs(lake_name, balance_component)
  title = get_title(lake_name, balance_component)
  
  plot_sup_evap_l2serror_usgshigh <-
    ggplot(data = annual_sum,
           aes(x = Year,
               y = Median)) +
    # xlim (1980, 2000) +
    # ylim (0, 120) +
    geom_ribbon(aes(ymin = Median * (1 - 0.35),
                    ymax = Median * (1 + 0.35)), fill = "blue", alpha=0.3) +
    geom_ribbon(aes(
      ymin = annual_2.5$X2.5.Percentile,
      ymax = annual_97.5$X97.5.Percentile
    ),
    fill = "yellow", alpha=0.3) +
    geom_line(color = "red", size = 0.5)+
    labels + title  + theme(plot.title = element_text(hjust = 0.5))+
    # Custom the Y scales:
    scale_y_continuous(
      
      # Features of the first axis
      name = balance_component,
      
      
      # Add a second axis and specify its features
      sec.axis = sec_axis( trans=~.*radio, name=rname,labels = function(x) format(x, scientific = TRUE))
    )
  plot_sup_evap_l2serror_usgshigh

  # plot_sup_evap_l2serror_usgslow <-
  #   ggplot(data = annual_sum,
  #          aes(x = Year,
  #              y = Median)) +
  #   # xlim (1980, 2000) +
  #   # ylim (0, 120) +
  #   geom_ribbon(aes(ymin = Median * (1 - 0.15),
  #                   ymax = Median * (1 + 0.15)), fill = "blue", alpha=0.3) +
  #   geom_ribbon(aes(
  #     ymin = annual_2.5$X2.5.Percentile,
  #     ymax = annual_97.5$X97.5.Percentile
  #   ),
  #   fill = "yellow", alpha=0.3) +
  #   labs(
  #     # title = "Uncertainty of Lake Superior Percipitation L2SWBM Model (95% Confidence Interval)",
  #     y = "mm",
  #     x = "Year") +
  #   geom_line(color = "red", size = 0.5)
  # plot_sup_evap_l2serror_usgslow
}

uncertainty_compare_func_runoff <- function(lake_name, balance_component, radio, rname) {
  filename = paste("./l2s_posterior/",
                   lake_name,
                   balance_component,
                   "_analysis19502022_prior19001969_1m.csv",
                   sep = "")
  sup_runoff <-
    read.csv(filename)
  str(sup_runoff)
  sup_runoff$yearmon <-
    as.yearmon(paste(sup_runoff$Year, sup_runoff$Month), "%Y %m")
  sup_runoff$formated_date <-
    format(as.Date(sup_runoff$yearmon), "%m/%Y")
  
  annual_sum <- aggregate(Median ~ Year , data = sup_runoff , sum)
  annual_2.5 <-
    aggregate(X2.5.Percentile ~ Year , data = sup_runoff , sum)
  annual_97.5 <-
    aggregate(X97.5.Percentile ~ Year , data = sup_runoff , sum)
  
  labels = get_labs(lake_name, balance_component)
  title = get_title(lake_name, balance_component)
  
  plot_sup_runoff_l2serror_usgshigh <-
    ggplot(data = annual_sum,
           aes(x = Year,
               y = Median)) +
    # xlim (1980, 2000) +
    # ylim (0, 120) +
    geom_ribbon(aes(ymin = Median * (1 - 0.35),
                    ymax = Median * (1 + 0.35)), fill = "blue", alpha=0.3) +
    geom_ribbon(aes(
      ymin = annual_2.5$X2.5.Percentile,
      ymax = annual_97.5$X97.5.Percentile
    ),
    fill = "yellow", alpha=0.3) +
    geom_line(color = "red", size = 0.5)+
    labels + title  + theme(plot.title = element_text(hjust = 0.5))+
    # Custom the Y scales:
    scale_y_continuous(
      
      # Features of the first axis
      name = balance_component,
      
      
      # Add a second axis and specify its features
      sec.axis = sec_axis( trans=~.*radio, name=rname,labels = function(x) format(x, scientific = TRUE))
    )
  plot_sup_runoff_l2serror_usgshigh
  
  # plot_sup_runoff_l2serror_usgslow <-
  #   ggplot(data = annual_sum,
  #          aes(x = Year,
  #              y = Median)) +
  #   # xlim (1980, 2000) +
  #   # ylim (0, 120) +
  #   geom_ribbon(aes(ymin = Median * (1 - 0.10),
  #                   ymax = Median * (1 + 0.10)), fill = "blue", alpha=0.3) +
  #   geom_ribbon(aes(
  #     ymin = annual_2.5$X2.5.Percentile,
  #     ymax = annual_97.5$X97.5.Percentile
  #   ),
  #   fill = "yellow", alpha=0.3) +
  #   labs(
  #     # title = "Uncertainty of Lake Superior Percipitation L2SWBM Model (95% Confidence Interval)",
  #     y = "mm",
  #     x = "Year") +
  #   geom_line(color = "red", size = 0.5)
  # plot_sup_runoff_l2serror_usgslow
}  
  
uncertainty_compare_func_outflow <- function(lake_name, balance_component, radio, rname) {
  filename = paste("./l2s_posterior/",
                   lake_name,
                   balance_component,
                   "_analysis19502022_prior19001969_1m.csv",
                   sep = "")
  sup_outflow <-
    read.csv(filename)
  str(sup_outflow)
  sup_outflow$yearmon <-
    as.yearmon(paste(sup_outflow$Year, sup_outflow$Month), "%Y %m")
  sup_outflow$formated_date <-
    format(as.Date(sup_outflow$yearmon), "%m/%Y")
  
  sup_outflow$Median = sup_outflow$Median / 12.0
  sup_outflow$X2.5.Percentile = sup_outflow$X2.5.Percentile / 12.0
  sup_outflow$X97.5.Percentile = sup_outflow$X97.5.Percentile / 12.0
  
  annual_sum <- aggregate(Median ~ Year , data = sup_outflow , sum)
  annual_2.5 <-
    aggregate(X2.5.Percentile ~ Year , data = sup_outflow , sum)
  annual_97.5 <-
    aggregate(X97.5.Percentile ~ Year , data = sup_outflow , sum)
  
  labels = get_labs(lake_name, balance_component)
  title = get_title(lake_name, balance_component)
  
  plot_sup_outflow_l2serror_usgshigh <-
    ggplot(data = annual_sum,
           aes(x = Year,
               y = Median)) +
    # xlim (1980, 2000) +
    # ylim (0, 120) +
    geom_ribbon(aes(ymin = Median * (1 - 0.15),
                    ymax = Median * (1 + 0.15)), fill = "blue", alpha=0.3) +
    geom_ribbon(aes(
      ymin = annual_2.5$X2.5.Percentile,
      ymax = annual_97.5$X97.5.Percentile
    ),
    fill = "yellow", alpha=0.3) +
    geom_line(color = "red", size = 0.5)+
    labels + title  + theme(plot.title = element_text(hjust = 0.5)) + 
    # Custom the Y scales:
    scale_y_continuous(
      
      # Features of the first axis
      name = balance_component,
      
      # Add a second axis and specify its features
      sec.axis = sec_axis( trans=~.*radio, name=rname,labels = function(x) format(x, scientific = TRUE))
    )
  plot_sup_outflow_l2serror_usgshigh
  
  # plot_sup_outflow_l2serror_usgslow <-
  #   ggplot(data = annual_sum,
  #          aes(x = Year,
  #              y = Median)) +
  #   # xlim (1980, 2000) +
  #   # ylim (0, 120) +
  #   geom_ribbon(aes(ymin = Median * (1 - 0.05),
  #                   ymax = Median * (1 + 0.05)), fill = "blue", alpha=0.3) +
  #   geom_ribbon(aes(
  #     ymin = annual_2.5$X2.5.Percentile,
  #     ymax = annual_97.5$X97.5.Percentile
  #   ),
  #   fill = "yellow", alpha=0.3) +
  #   labs(
  #     # title = "Uncertainty of Lake Superior Percipitation L2SWBM Model (95% Confidence Interval)",
  #     y = "mm",
  #     x = "Year") +
  #   geom_line(color = "red", size = 0.5)
  # plot_sup_outflow_l2serror_usgslow
}  

# lake_names = c("Superior", "MichiganHuron", "Erie")
# balance_components = c("Precipitation", "Evaporation", "Runoff")

# uncertainty_compare_func_precip("Superior", "Precipitation")

ggarrange(
  uncertainty_compare_func_precip("superior", "Precip(mm)",1.0,"Precip(mm)"),
  uncertainty_compare_func_precip("miHuron", "Precip(mm)",1.0,"Precip(mm)"),
  uncertainty_compare_func_precip("erie", "Precip(mm)",1.0,"Precip(mm)"),
  uncertainty_compare_func_precip("ontario", "Precip(mm)",1.0,"Precip(mm)"),
  
  uncertainty_compare_func_evap("superior", "Evap(mm)",1.0,"Evap(mm)"),
  uncertainty_compare_func_evap("miHuron", "Evap(mm)",1.0,"Evap(mm)"),
  uncertainty_compare_func_evap("ontario", "Evap(mm)",1.0,"Evap(mm)"),
  uncertainty_compare_func_evap("erie", "Evap(mm)",1.0,"Evap(mm)"),
  
  uncertainty_compare_func_runoff("superior", "Runoff(mm)",1.0,"Runoff(mm)"),
  uncertainty_compare_func_runoff("miHuron", "Runoff(mm)",1.0,"Runoff(mm)"),
  uncertainty_compare_func_runoff("erie", "Runoff(mm)",1.0,"Runoff(mm)"),
  uncertainty_compare_func_runoff("ontario", "Runoff(mm)",1.0,"Runoff(mm)"),
  
  uncertainty_compare_func_outflow("superior", "Outflow(cms)",1.0,"Runoff(mm)"),
  uncertainty_compare_func_outflow("miHuron", "Outflow(cms)",1.0,"Runoff(mm)"),
  uncertainty_compare_func_outflow("erie", "Outflow(cms)",1.0,"Runoff(mm)"),
  uncertainty_compare_func_outflow("ontario", "Outflow(cms)",1.0,"Runoff(mm)"),
  
  ncol = 4,
  nrow = 4
)


# ggarrange(
#   uncertainty_compare_func_precip("superior", "Precip"),
#   uncertainty_compare_func_precip("miHuron", "Precip"),
#   uncertainty_compare_func_precip("erie", "Precip"),
#   uncertainty_compare_func_precip("ontario", "Precip"),
# 
#   uncertainty_compare_func_evap("superior", "Evap"),
#   uncertainty_compare_func_evap("miHuron", "Evap"),
#   uncertainty_compare_func_evap("erie", "Evap"),
#   uncertainty_compare_func_evap("ontario", "Evap"),
# 
#   uncertainty_compare_func_runoff("superior", "Runoff"),
#   uncertainty_compare_func_runoff("miHuron", "Runoff"),
#   uncertainty_compare_func_runoff("erie", "Runoff"),
#   uncertainty_compare_func_runoff("ontario", "Runoff"),
# 
#   uncertainty_compare_func_outflow("superior", "Outflow"),
#   uncertainty_compare_func_outflow("miHuron", "Outflow"),
#   uncertainty_compare_func_outflow("erie", "Outflow"),
#   uncertainty_compare_func_outflow("ontario", "Outflow"),
# 
#   ncol = 4,
#   nrow = 4
# )


# func = uncertainty_compare_func_evap
# ggarrange(
#   func("Superior", "Evaporation"),
#   func("miHuron", "Evaporation"),
#   func("Erie", "Evaporation"),
#   func("Ontario", "Evaporation"),
#   ncol = 1,
#   nrow = 4
# )
# 
# func = uncertainty_compare_func_runoff
# ggarrange(
#   func("Superior", "Runoff"),
#   func("MichiganHuron", "Runoff"),
#   func("Erie", "Runoff"),
#   func("Ontario", "Runoff"),
#   ncol = 1,
#   nrow = 4
# )

# for (lake_name in lake_names) {
#   for (balance_component in balance_components) {
#     func(lake_name, balance_component)
#   }
# }



