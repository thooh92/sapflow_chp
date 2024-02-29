## Author: Thomas Ohnemus
## Date: 28/02/2024
## Process weather data to find nights with no evaporative demand

# Initialize Script
rm(list = ls())

library(data.table)
library(tidyverse)
library(suncalc)

# Load Functions
source("C:/Docs/MIRO/sapflow_CHP/sapflow_chp/VPD_functions.R")

# Load Data
setwd("C:/Docs/MIRO/sapflow_CHP/data_raw/")
files <- list.files(pattern = ".csv")

  # Load coordinates
parameters        <- read.csv2("C:/Docs/MIRO/Baumparameter.csv")
parameters        <- parameters[1,]

  # Data Wrangling
dat     <- left_join(read.csv(files[1]), read.csv(files[3]), "dateLocale")
dat$UTC <- as.POSIXct(dat$dateUTC.x, format = "%Y-%m-%dT%H:%M:%OSZ")
dat     <- dat[,c(8,3,4,5,7)]

dat$Windgeschwindigkeit <- dat$Windgeschwindigkeit/3.6  # transform from km/h to m/s

  # save first level data
setwd("C:/Docs/MIRO/sapflow_CHP/")
write.csv(dat, "data_1st_lvl/LA_weather.csv")

# Assign sunset and sunrise times
    # To Do: automatize lat & lon
sunriset        <- getSunlightTimes(date = as.Date(dat$UTC), lat = parameters$Lat, 
                                    lon = parameters$Lon, keep = c("sunrise", "sunset"), 
                                    tz = "UTC")[4:5]
dat             <- cbind(dat, sunriset)
rm(sunriset)

# Assign night and daytime
dat$time        <- ifelse(dat$UTC < dat$sunrise | dat$UTC > dat$sunset, "night", "day")
dat$Date        <- as.Date(dat$UTC)

# Subset to night times & assign one date to each night
dat             <- dat[dat$time == "night",]
dat$night       <- as.Date(ifelse(dat$UTC < dat$sunrise, dat$Date-1, dat$Date))

# Group values per night
dat_nightly     <- dat %>% group_by(night) %>% summarize(mean_w = mean(Windgeschwindigkeit, na.rm = T),
                                                         mean_t = mean(Temperatur, na.rm = T),
                                                         min_t = min(Temperatur, na.rm = T),
                                                         max_t = max(Temperatur, na.rm = T),
                                                         mean_rh = mean(Relative.Luftfeuchtigkeit, na.rm = T),
                                                         min_rh = min(Relative.Luftfeuchtigkeit, na.rm = T),
                                                         max_rh = max(Relative.Luftfeuchtigkeit, na.rm = T)) 

# Calculate VPD & save data
dat_nightly <- dat_nightly %>% mutate(vpd_allen = allen.vpd(tmax = max_t, tmin = min_t, rhmin = min_rh, rhmax = max_rh),
                                      vpd_maes = maes.vpd(t = mean_t, RH = mean_rh))
write.csv(dat_nightly, "data_1st_lvl/LA_weather_nightly.csv")

# Plot VPD & Wind Speed per night
dat_nightly <- pivot_longer(dat_nightly, cols = 9:10, names_to = "VPD", values_to = "value")
ggplot(dat_nightly, aes(x = night, y = value, colour = VPD)) +
  geom_point(alpha = 0.5) + theme_bw() + geom_hline(yintercept = 0, linetype = "dashed") +
  labs(y = "Vapour Pressure Deficit [kPa]\nMean Wind Speed [m/s]", x = "Night beginning ...") +
  geom_point(aes(y = mean_w), shape = "diamond", color = "black")
ggsave("C:/Docs/MIRO/sapflow_CHP/plots/Nocturnal_VPD.png", units = "cm", dpi = 300, width = 14, height = 10)

