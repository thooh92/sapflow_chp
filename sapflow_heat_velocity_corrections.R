## Author: Thomas Ohnemus
## Date: 29/02/2024
## Correction of heat velocity data

rm(list = ls())

library(data.table)
library(tidyverse)
library(suncalc)

setwd("C:/Docs/MIRO/sapflow_CHP/")
# Read measured variables
parameters        <- read.csv2("Baumparameter.csv")
parameters$sheet  <- paste0(parameters$Tree, ".csv")
p                 <- parameters[parameters$Tree == unique(parameters$Tree)[1],]

# Get Sapflow data
dat               <- read.csv(paste0("data_raw/",p$sheet))
dat$Time          <- as.POSIXct(dat$Time, "%Y-%m-%d %H:%M:%S", tz = "UTC")

# Background Calculations
source("sapflow_chp/Sapflow_functions.R")

# Get weather data
weather           <- read.csv("data_1st_lvl/LA_weather.csv")
weather$UTC       <- as.POSIXct(weather$UTC, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")

    # To Do: Find Diagnostics to identify flaws in data
dat_l <- pivot_longer(dat, cols = 2:22, names_to = "variable", values_to = "value")
dat_l$value[dat_l$value > 100] <- NA
ggplot(dat_l[dat_l$variable %like% "Tds" | dat_l$variable %like% "SWC",], 
       aes(x = Time, y = value)) + geom_line() +
  theme_bw() + facet_wrap(~variable, scales = "free", ncol = 1) +
  geom_vline(xintercept = as.POSIXct("2024-02-24 01:43:44", tz = "UTC"), color = "steelblue") +
  geom_vline(xintercept = as.POSIXct("2024-02-24 05:43:42", tz = "UTC"), color = "steelblue") +
  geom_vline(xintercept = as.POSIXct("2024-02-25 22:13:48", tz = "UTC"), color = "steelblue") +
  geom_vline(xintercept = as.POSIXct("2024-02-25 20:00:00", tz = "UTC"), color = "red") +
  geom_line(data = weather, aes(x = UTC, y = Temperatur), colour = "coral")
  
#ggsave("plots/Frost_100_ambT.png", dpi = 300, width = 14, height = 15, units = "cm")

  # 1st Correction: Remove data where ambient temperature is below 0
neg_T <- weather$UTC[weather$Temperatur < 0]
neg_T <- neg_T[!is.na(neg_T)]

rmv_indices <- c()
for(i in 1:length(neg_T)){
  inds <- which(abs(difftime(dat_l$Time, neg_T[i], units = "mins")) < 30)
  rmv_indices <- c(rmv_indices, inds)
}

dat_cleaned  <- dat_l
dat_cleaned$value[unique(rmv_indices)] <- NA

ggplot(dat_cleaned, aes(x = Time, y = value)) + geom_line() +
  theme_bw() + facet_wrap(~variable, scales = "free")


# Plot DMA Heat Velocity to identify periods with odd data
dat_cleaned_w <- pivot_wider(dat_cleaned, 
                             names_from = "variable",
                             values_from = "value")
dat           <- dat_cleaned_w

source("sapflow_chp/Sapflow_functions.R")
ggsave("plots/DMA_HV_cleaned.png", units = "cm", dpi = 300,
       width = 14, height = 10)

    # To Do: Data gap 23rd of February early morning? (SWC)



#####################
# Correct Probe Misalignment
# Identify and correct probe misalignment
  # Assign sunset and sunrise times
sunriset        <- getSunlightTimes(date = as.Date(mis_df$time), lat = p$Lat, lon = p$Lon, 
                                    keep = c("sunrise", "sunset"), tz = "UTC")[4:5]
    # To Do: Timezone sapflow?
mis_df             <- cbind(mis_df, sunriset)
rm(sunriset)

# Assign night and daytime
mis_df$period      <- ifelse(mis_df$time < mis_df$sunrise | mis_df$time > mis_df$sunset, "night", "day")
mis_df$Date        <- as.Date(mis_df$time)

# Subset to night times & assign one date to each night
mis_df             <- mis_df[mis_df$period == "night",]
mis_df$night       <- as.Date(ifelse(mis_df$time < mis_df$sunrise, mis_df$Date-1, mis_df$Date))

# Group by night
df_nightly         <- mis_df %>% group_by(night, position) %>%
  summarize(hv_mean = mean(value, na.rm = T))

ggplot(df_nightly, aes(x = night, y = hv_mean, color = position)) + geom_point() +
  theme_bw() + geom_hline(yintercept = 0, linetype = "dashed") +
  labs(x = "Night beginning ...", y = "Mean Heat Velocity [cm/hr]")

