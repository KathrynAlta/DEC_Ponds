############################
# BATHYMETRY & LAKE VOLUME ESTIMATION USING R
############################

# RESOURCES:  
# ______________________________________
# Tutorial : https://dewey.dunnington.ca/post/2019/bathymetry-lake-volume-estimation-using-r/ 
# GIS Course: https://mgimond.github.io/Spatial/spatial-interpolation.html 
# Once you are set up with GIS through cornell you should have access to the online Esri trainings: https://www.esri.com/training/unlimited-esri-training/ 
      # Free Esri Intro course: https://www.esri.com/training/catalog/5c9a7395190cf23eac62a998/getting-started-with-data-management/ 
# Mapping in R : https://geocompr.robinlovelace.net/adv-map.html 
# Git https://github.com/geocompx/geocompr 
# Book about Geospatial: https://geocompr.robinlovelace.net/intro.html #I am liking working through this one -KG 010923


# NOTES : 
# ______________________________________
# Steps in calculating volume of sediment:  
#     1. Set the boundary for the pond as a polygons [pond_poly]
#           --> need to make in ArcGIS 
#     2. Turn the depth data (collected by sediment mapping in the field) into a shape file [sed_depths]
#           --> should be able to just open the csv of this (with lat long) in ArcGIS and then save it as a shapefile
#     3. Put together the pond_poly and sed_depths 
#     4. use the data that you have to interpolate more data points (depth measurements) in the pond (fine grain bathymetry)
#           --> ause some sort of interpolation math (ex: TIN, IDW, TPRS etc) 
#     5. use all of those points (measured + interpolated) to estimate the volume of sediment 
#         (mean sediment depth) * (area of the pond) = volume of sediment 




#__________________________________________________________________________________________________
# 0. Set up R Environment 
setwd("~/Sediment_Mapping_DEC") # Desktop 
getwd()

library(dplyr)
library(sf)
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
library(ggspatial)


#__________________________________________________________________________________________________
# 1.  Input data

comp_data <- read_xlsx("OutputFiles/compiled_data_1222222.xlsx")
head(comp_data)