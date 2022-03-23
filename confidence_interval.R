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
lake_name = "superior"
balance_component = "Precip"
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

annual_mean <- aggregate(Median ~ Year , data = sup_precip , mean)
annual_2.5 <-
  aggregate(X2.5.Percentile ~ Year , data = sup_precip , mean)
annual_97.5 <-
  aggregate(X97.5.Percentile ~ Year , data = sup_precip , mean)

reference_mean <-
  mean(annual_mean$Median[annual_mean$Year < 1979])
reference_sd <- sd(annual_mean$Median[annual_mean$Year < 1979])
recent_mean <- mean(annual_mean$Median[annual_mean$Year >= 1979])
recent_sd <- sd(annual_mean$Median[annual_mean$Year >= 1979])

annual_mean = annual_mean %>%
  mutate(low = 
           if_else(Year <= 1978, reference_mean - reference_sd, recent_mean - recent_sd)
  )


annual_mean = annual_mean %>%
  mutate(high = 
           if_else(Year <= 1978, reference_mean + reference_sd, recent_mean + recent_sd)
  )

plot_sup_precip_mean <-
  ggplot(data = annual_mean, aes(x = Year, y = Median)) +
  geom_line(color = "red") +
  geom_point() + 
  geom_ribbon(aes(ymin = low, ymax = high), 
              alpha=0.1, 
              linetype="dashed",
              color="grey")+
  labs(# title = "Lake Superior Annual Percipitation Comparison: 1950-1978 vs 1979-2019",
    y = "(mm)",
    x = "Year")+
  geom_segment(aes(
    x = 1979,
    xend = 2019,
    y = recent_mean,
    yend = recent_mean
  ),data = annual_mean) +
  geom_segment(aes(
    x = 1950,
    xend = 1978,
    y = reference_mean,
    yend = reference_mean
  ),
  data = annual_mean)
plot_sup_precip_mean
