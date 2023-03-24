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
      install.packages("fields")
      library(fields)
      library(interp)
      library(leaflet)


# 1. Read in Shape Files for DEC sediment Mapping (made by KG in GIS)
    setwd("~/Sediment_Mapping_DEC")
    getinfo.shape("Spatial_Data/Ellens_Pond_020123/Ellens_Pond_Depths_with_Edge_020123_1407.shp")
    getinfo.shape("Spatial_Data/Ellens_Pond_020123/Ellens_Pond_poly_020123_1134.shp")
    
    lake.points.hol <- readShapePoints("Spatial_Data/Ellens_Pond_020123/Ellens_Pond_Depths_with_Edge_020123_1407.shp")
    lake.poly.hol <- readShapePoly("Spatial_Data/Ellens_Pond_020123/Ellens_Pond_poly_020123_1134.shp")
    
    lake.points.hol$y <- as.numeric(lake.points.hol$lat)
    lake.points.hol$x <- as.numeric(lake.points.hol$long)
    lake.points.hol$z <- as.numeric(lake.points.hol$Sediment_T)
    head(lake.points.hol@data)
    str(lake.points.hol@data)
   
# 2. Basic interpolation and visualization using the interp function and the basic help function in R 
    
    # Visualize using basic R help function 
    linear_interp_base <- interp(xk, yk, zk, nx = 80, ny = 80, method = "linear")
    ## prepare breaks and colors that match for image and contour:
    breaks <- pretty(seq(min(linear_interp_base$z,na.rm=TRUE),max(linear_interp_base$z,na.rm=TRUE),length=11))
    db <- breaks[2]-breaks[1]
    nb <- length(breaks)
    breaks <- c(breaks[1]-db,breaks,breaks[nb]+db)
    colors <- topo.colors(length(breaks)-1)
    
    # Visualize 
    image(linear_interp_base,breaks=breaks,col=colors,main="Ellens Pond Sediment Depth -- Linear Interpolation",
          sub=paste("linear interpolation, ", ni,"points"))
    contour(linear_interp_base,add=TRUE,levels=breaks)
    points(xk,yk)
    
#2. Now try core from Martinsen 
# We try two interpolation methods: piece-wise interpolation in the interp package and thin-plate spline in fields package
    
    #First create an empty raster to interpolate over, adjust resolution depending on lake and computer power
    tmp_raster <- raster(lake.points.hol, res = c(5, 5), crs = st_crs(lake.points.hol)$proj4string)
    
    #First the simpler linear interpolation 
    #Input data and dimensions of the output grid are provided
  
    # Can't get this to work but got around it by just pulling all of the peices that I need individually 
    linear <- interp(lake.points.hol,
                     z = "z",
                     ny=ncol(tmp_raster),
                     nx=nrow(tmp_raster),
                     duplicate = "mean")
    
   
  # Figure out interp function in R 
    ?interp() 
    xk <- as.numeric(lake.points.hol$lat)
    yk <- as.numeric(lake.points.hol$long)
    zk <- as.numeric(lake.points.hol$z)
    
    #Linear Interpolation 
    linear_interp <- interp(x = as.numeric(lake.points.hol$lat),
                            y = as.numeric(lake.points.hol$long), 
                            z = as.numeric(lake.points.hol$z),
                            ny=ncol(tmp_raster),
                            nx=nrow(tmp_raster),
                            method = "linear")
    
  
    #Convert to raster object and resample to resolution in tmp_raster
    step1 <- raster(linear_interp)
    linear_warp <- resample(raster(linear_interp), tmp_raster)
    
    
    #Next, the more flexible smooth thin-plate spline smoothing method
    #Fit function
    tps_fun <- Tps(st_coordinates(lake.points.hol), lake.points.hol$z)
    