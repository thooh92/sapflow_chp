## Author: Thomas Ohnemus
## Date: 14/02/2024
## Background Calculations and Fixed Parameters for Sapflow

# Fixed Variables
n_dist_d_outer       <- 0.8  #[cm]
n_dist_d_inner       <- 0.8  #[cm]
n_dist_u_outer       <- 0.8  #[cm]
n_dist_u_inner       <- 0.8  #[cm]
heat_cap_wood        <- 1200 #[J/kg/°C] at 20 °C
heat_cap_sap         <- 4182 #[J/kg/°C] at 20 °C
density_water        <- 1000 #[kg/m³]
density_cellwater    <- 1530 #[kg/m³]
thermal_con_water    <- 0.5984 #[J/m/s] at 20 °C; Lide 1992

# Derived Variables
wound_correction     <- 6.8166*p$wound_diameter+0.5308             # dimensionless
sapwood_dry_density  <- p$sapwood_dry_weight/p$sapwood_fresh_volume  # [kg/m³]
sapwood_fresh_density<- p$sapwood_fresh_weight/p$sapwood_fresh_volume  # [kg/m³]

sapwood_g_watercontent <- (p$sapwood_fresh_weight-p$sapwood_dry_weight)/p$sapwood_dry_weight  #[kg/kg]
fibre_sat_watercontent <- 0.2*(sapwood_dry_density*(1/density_water))^-0.5 # dimensionless

void_fraction_fibre_sat<- 1-(sapwood_dry_density/density_water)*((density_water/density_cellwater)+fibre_sat_watercontent)
heat_cap_spec_wood     <- ((p$sapwood_dry_weight*heat_cap_wood)+heat_cap_sap*
                             (p$sapwood_fresh_weight-p$sapwood_dry_weight))/p$sapwood_fresh_weight #[J/kg/°C] at 20 °C; Edwards & Warwick 1984
heat_cap_wood_vol      <- sapwood_fresh_density*heat_cap_spec_wood  #[J/m³/°C]
therm_cond             <- thermal_con_water*(sapwood_g_watercontent-fibre_sat_watercontent)*(sapwood_dry_density/density_water)+
  0.04186*(21-20*void_fraction_fibre_sat) #[J/m/s/°C at 20 °C]; Vandegehuchte & Steppe 2012
therm_diffusivity      <- therm_cond/heat_cap_wood_vol*10000  #[cm²/s], Vandegehuchte & Steppe 2012

  # Calcuating Peclet Number
Tmax_outer             <- (n_dist_d_outer^2)/(4*therm_diffusivity)
Tmax_inner             <- (n_dist_d_inner^2)/(4*therm_diffusivity)
peclet_outer           <- 0.9696*Tmax_outer-3.2363
peclet_inner           <- 0.9696*Tmax_inner-3.2363


# Heat Velocity Calculations
HRM_outer <- (((2*therm_diffusivity)/(n_dist_d_outer+n_dist_u_outer))*dat$SF.01.AlphaOuter...+
                ((n_dist_d_outer-n_dist_u_outer)/(2*58.5)))*wound_correction*3600
HRM_inner <- (((2*therm_diffusivity)/(n_dist_d_inner+n_dist_u_inner))*dat$SF.01.AlphaInner...+
                ((n_dist_d_inner-n_dist_u_inner)/(2*58.5)))*wound_correction*3600 
TMx_outer <- ((sqrt(((4*therm_diffusivity)/3)*(log(1-(3/dat$SF.01.tMaxTouter..s.)))+
                      ((n_dist_d_outer^2)/(dat$SF.01.tMaxTouter..s.*(dat$SF.01.tMaxTouter..s.-3)))))*3600)*wound_correction
TMx_inner <- ((sqrt(((4*therm_diffusivity)/3)*(log(1-(3/dat$SF.01.tMaxTinner..s.)))+
                      ((n_dist_d_inner^2)/(dat$SF.01.tMaxTinner..s.*(dat$SF.01.tMaxTinner..s.-3)))))*3600)*wound_correction
DMA_outer <- ifelse(dat$SF.01.tMaxTouter..s. < 0.9*peclet_outer, dat$SF.01.tMaxTouter..s., HRM_outer)
DMA_inner <- ifelse(dat$SF.01.tMaxTinner..s. < 0.9*peclet_inner, dat$SF.01.tMaxTinner..s., HRM_inner)

# Check probe misalignment
mis_df <- data.frame(
  time = dat$Time,
  DMA_outer = DMA_outer,
  DMA_inner = DMA_inner
)
mis_df <- pivot_longer(mis_df, cols = 2:3, names_to = "position", values_to = "value")

print(ggplot(mis_df, aes(x = time, y = value, color = position)) +
  geom_line() + theme_bw() +
  labs(x = "", y = "Heat Velocity [cm/hr]", colour = "") + 
  geom_hline(yintercept = 0, linetype = "dashed"))
