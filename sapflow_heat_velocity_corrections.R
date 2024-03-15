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
p                 <- parameters[parameters$Tree == unique(parameters$Tree)[2],]

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
  geom_line(data = weather, aes(x = UTC, y = Temperatur), colour = "coral")
  
#ggsave("plots/Frost_100_ambT.png", dpi = 300, width = 14, height = 15, units = "cm")

  # 1st Correction: Remove data where ambient temperature is below 0
neg_T <- weather$UTC[weather$Temperatur < 0]
neg_T <- neg_T[!is.na(neg_T)]

rmv_indices <- c()
for(i in 1:length(neg_T)){
  inds <- which(abs(difftime(dat_l$Time, neg_T[i], units = "mins")) < 60)
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
ggsave(paste0("plots/",p$Tree,"_DMA_HV_peclet_cleaned.png"), units = "cm", dpi = 300,
       width = 14, height = 10)


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
df_nightly         <- mis_df[!is.na(mis_df$value),] %>% group_by(night, position) %>%
  summarize(hv_mean = mean(value, na.rm = T),
            hv_sd = sd(value, na.rm = T),
            count = n())

ggplot(df_nightly[df_nightly$count >= 12,], aes(x = night, y = hv_mean, color = position)) + geom_point() +
  theme_bw() + geom_hline(yintercept = 0, linetype = "dashed") +
  labs(x = "Night beginning ...", y = "Mean Heat Velocity [cm/hr]") +
  geom_errorbar(aes(ymin = hv_mean - hv_sd, 
                ymax = hv_mean + hv_sd), width = 0.1) 
ggsave(paste0("plots/",p$Tree,"_Nocturnal_HV_6h.png"), dpi = 300, units = "cm",
       width = 14, height = 10)


# Load nocturnal VPD data
weather_n    <- read.csv("data_1st_lvl/LA_weather_nightly.csv")
weather_n$night <- as.Date(weather_n$night)

VPD_sapflow  <- full_join(weather_n, df_nightly[df_nightly$count >= 12,])
VPD_sapflow  <- pivot_longer(VPD_sapflow, cols = c(3,10,11),
                             values_to = "value", names_to = "factors")

ggplot(VPD_sapflow, aes(x = value, y = hv_mean, color = position)) +
  theme_bw() + geom_point() + facet_wrap(~factors, scales = "free") +
  labs(x = "Nocturnal VPD [kPa]\nMean Nocturnal Wind Speed [m/s]", y = "Mean Heat Velocity [cm/hr]") +
  geom_smooth(method = "lm", se = F, linewidth = 0.1)
ggsave(paste0("plots/",p$Tree,"_Nocturnal_VPD_Sapflow.png"), units = "cm", dpi = 300,
       width = 20, height = 10)

# Plot Sap Flow from DMA Heat Velocity
# Sap Flux Density
heartwood_area         <- ifelse(p$sapwood_depth < 1.51, pi*((p$stem_diameter/2)-p$bark_depth-p$sapwood_depth)^2,
                                 pi*((p$stem_diameter/2)-p$bark_depth-2)^2)  #cm²
sapwood_area_inner     <- ifelse(p$sapwood_depth < 1.51, 0,
                                 pi*((p$stem_diameter/2)-p$bark_depth-1)^2-heartwood_area)  #cm²
sapwood_area_outer     <- pi*((p$stem_diameter/2)-p$bark_depth)^2-heartwood_area-sapwood_area_inner  #cm²

sf_dense_outer <- (as.numeric(DMA_outer[[1]])*sapwood_dry_density*(heat_cap_wood+(sapwood_g_watercontent*heat_cap_sap)))/
  (density_water*heat_cap_sap)
sf_dense_inner <- (as.numeric(DMA_inner[[1]])*sapwood_dry_density*(heat_cap_wood+(sapwood_g_watercontent*heat_cap_sap)))/
  (density_water*heat_cap_sap)

# Sap Flow Total
dat$sf_tot  <- ((sf_dense_outer*sapwood_area_outer)+(sf_dense_inner*sapwood_area_inner))/1000


ggplot(dat, aes(x = Time, y = sf_tot)) + geom_line(alpha = 0.5) +
  theme_bw() + labs(x = "", y = "Total Sap Flow [l/h]", title = parameters$Tree[1]) +
  geom_hline(yintercept = 0, linetype = "dashed")
ggsave(paste0("plots/",p$Tree,"_Sapflow.png"), units = "cm", dpi = 300,
       width = 10, height = 10)



