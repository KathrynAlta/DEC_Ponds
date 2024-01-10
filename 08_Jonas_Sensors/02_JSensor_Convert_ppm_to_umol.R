#######################################
# Jonas Sensors: Convert ppmV to umoles  
#######################################

# Package on Jonas's Git Hub: https://github.com/JonasStage/FluxSeparator/tree/main 
# Convertion of \eqn{ppmV} to \eqn{µmol} \eqn{m^{-2}} \eqn{h^{-1}} using the ideal gas law

#__________________________________________
# 0. Set Up R Environment 

    # Set Working Directory 
    setwd("~/DEC_Ponds/08_Jonas_Sensors")  # Desktop 
    
    # Install Jonas GitHUb
    remotes::install_github('JonasStage/FluxSeparator', force = TRUE)
    
    # Install example data from Jonas 
    load(file='JSensor_Input_Data/DIY_sensor_data.rda')
    
    # Packages 
    
    library(lubridate)
    library(tidyverse)
    library(ggplot2)
    library(tibble)
    library(purrr)
    library(dplyr)
    library(stringr)
    library(TTR)
    library(ggpubr)
    
#__________________________________________
# 1. Write Function to convert from ppmV to umoles (From Jonas Code on Github )
    
    ppm_to_umol <- function(pressure, concentration, volume, temperature_C, area) {
      ((pressure*concentration*volume)/(8.314*(temperature_C+ 273.15)))/area
    }
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    