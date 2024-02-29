## Author: Thomas Ohnemus
## Date: 04/01/2024
## Producing VPD-functions


  # Allen et al. (FAO); equals Hartmann (1994)
allen.vpd  <- function(tmax, tmin, rhmin, rhmax){ # RH in %, T in C
  # Mean es for a day/week calculate from min and max Tair
  e0max <- 0.6108 * exp(17.27 * tmax/(tmax + 237.3))
  e0min <- 0.6108 * exp(17.27 * tmin/(tmin + 237.3))
  es <- (e0max + e0min)/2
  
  
  ea <- (e0min * (rhmax/100) + e0max * (rhmin/100))/2
  vpd <- es - ea
  return(vpd)  # kPa
}

  # Meas & Steppe 2012
maes.vpd  <- function(t, RH){ # RH in %, T in C
  a <- 613.75
  b <- 17.502
  c <- 240.97
  vpd <- ((1-(RH/100)) * a * exp((b*t)/(c+t)))/1000
  return(vpd)  # kPa
}
