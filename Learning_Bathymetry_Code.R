############################
# BATHYMETRY & LAKE VOLUME ESTIMATION USING R
############################
# Tutorial : https://dewey.dunnington.ca/post/2019/bathymetry-lake-volume-estimation-using-r/ 
# GIS Course: https://mgimond.github.io/Spatial/spatial-interpolation.html 
# Once you are set up with GIS through cornell you should have access to the online Esri trainings: https://www.esri.com/training/unlimited-esri-training/ 
      # Free Esri Intro course: https://www.esri.com/training/catalog/5c9a7395190cf23eac62a998/getting-started-with-data-management/ 

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