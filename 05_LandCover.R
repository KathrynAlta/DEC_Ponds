############################################################
# LAND COVER AROUND PONDS
############################################################
# Holgerson Lab, Katie Gannon


# 0. Set up R environment and Load Data 

#Set working directory: 
setwd("~/Sediment_Mapping_DEC") # Desktop 
getwd()

# Load packages: 
# Standard 
library(dplyr)
library(raster)
library(ape) 
library(mosaic)
library(readxl)
library(tidyverse)
library(devtools)
library(lme4)
library(car)
library(effects)
library(mosaic)
library(writexl)
library(purrr)
library(EnvStats)

library(sf)
library(ggspatial)

# install.packages("gstat")
library(gstat)

install.packages("mgcv")
library(mgcv)
#install.packages("mgcv")
# library(mgcv)


# Load Data 

  # Load key for land cover grid ID 
  landcover_gridID <- read_xlsx("230321_LandCover_Values.xlsx")

  # Load dissolved shape files for each pond and each buffer size 
  applegate_dissolved_1km <- read_sf("Spatial_Data_LandCover/dissolved_applegate_1km.shp")  
  aquadro_dissolved_1km <- read_sf("Spatial_Data_LandCover/dissolved_1km_aquadro.shp")
  aquadro_dissolved_500m <- read_sf("Spatial_Data_LandCover/dissolved_500m_aquadro.shp")

# Join dissolved shape files with the key of land cover grid 
  applegate_dissolved_1km_named <- left_join(applegate_dissolved_1km, landcover_gridID)
  aquadro_plc_1km <- left_join(aquadro_dissolved_1km, landcover_gridID)   #Aquadro percent land cover for 1 km buffer
  aquadro_plc_500m <- left_join(aquadro_dissolved_500m, landcover_gridID)   #Aquadro percent land cover for 500 m buffer
  
# Conver to actual percent 
  aquadro_plc_1km$Percent_Co <- aquadro_plc_1km$Percent_Co * 100 
  aquadro_plc_500m$Percent_Co <- aquadro_plc_500m$Percent_Co * 100 



























