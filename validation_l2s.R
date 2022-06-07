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
balance_component = c("Precipitation", "Evaporation", "Runoff", "Outflow")

cpt_stderror_func <- function(lake_name, balance_component) {
  filename = paste("./l2s_posterior/",
                   lake_name,
                   balance_component,
                   "_2019.csv",
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
func = cpt_stderror_func

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
