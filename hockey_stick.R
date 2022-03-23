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


library(segmented)
set.seed(12)
xx <- annual_mean$Year
yy <- annual_mean$Median
dati <- data.frame(x = xx, y = yy)
out.lm <- lm(y ~ x, data = dati)
o <- segmented(
  out.lm,
  seg.Z = ~ x,
  control = seg.control(display = FALSE)
)
slope(o)
dat2 <- data.frame(x = xx, y = broken.line(o)$fit)

ggplot(dati, aes(x = x, y = y)) +
  geom_point() +
  geom_line(data = dat2, color = 'blue')
