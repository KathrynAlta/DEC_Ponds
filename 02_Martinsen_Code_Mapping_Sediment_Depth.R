############################################################
# MAPPING SEDIMENT DEPTHS 
############################################################
# Holgerson Lab, Katie Gannon
# Code modified from https://www.datainwater.com/post/lake_bathymetry/


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
      
      # Spatial packages 
      library(sf)   # classes and functions for vector data
      library(ggspatial)
      library(terra)   # classes and functions for raster data
      library(spData) # load geographic data
      library(spDataLarge)  # load larger geographic data
      
      # Heathcote Packages 
      library(shapefiles)
      library(maptools)
      library(sp)
      library(rgdal)
      library(geoR)
      library(gstat)
      library(maps)
      
      install.packages("interp")
      # library(fields)
      library(interp)
      # library(leaflet)


# Read in Shape Files for DEC sediment Mapping (made by KG in GIS)
    setwd("~/Sediment_Mapping_DEC")
    getinfo.shape("Spatial_Data/Ellens_Pond_020123/Ellens_Pond_Depths_with_Edge_020123_1407.shp")
    getinfo.shape("Spatial_Data/Ellens_Pond_020123/Ellens_Pond_poly_020123_1134.shp")
    
    lake.points.hol <- readShapePoints("Spatial_Data/Ellens_Pond_020123/Ellens_Pond_Depths_with_Edge_020123_1407.shp")
    lake.poly.hol <- readShapePoly("Spatial_Data/Ellens_Pond_020123/Ellens_Pond_poly_020123_1134.shp")
