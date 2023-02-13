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
    #########################################################
    # Martinsen Code 
    # https://www.datainwater.com/post/lake_bathymetry/ 
    
    
    
    #We try two interpolation methods: piece-wise interpolation in the interp package and thin-plate spline in fields package
    #First create an empty raster to interpolate over, adjust resolution depending on lake and computer power
    #tmp_raster <- raster(lake_xyz, res = c(5, 5), crs = st_crs(lake_xyz)$proj4string)
    tmp_raster <- raster(lake.points.hol, res = c(5, 5), crs = st_crs(lake.points.hol)$proj4string)
    
    #First the simpler linear interpolation 
    #Input data and dimensions of the output grid are provided
    lake.points.hol$lat
    names(lake.points.hol)
    names(lake.points.hol)[names(lake.points.hol) == "lat"] <- "y"
    names(lake.points.hol)[names(lake.points.hol) == "long"] <- "x"
    str(lake.points.hol)
    
    linear <- interp(as(lake.points.hol, "Spatial"),
                     z = "z" ,
                     ny=ncol(tmp_raster),
                     nx=nrow(tmp_raster),
                     duplicate = "mean")
    
    linear <- interp(lake.points.hol,
                     z = "z",
                     ny=ncol(tmp_raster),
                     nx=nrow(tmp_raster),
                     duplicate = "mean")
    
    whatisspatial <- as(lake.points.hol, "Spatial")
    
    ?interp() # Figure out interp 
    
    oldseed <- set.seed(42)
    ni <- 64
    xi <- runif(ni,0,1)
    yi <- runif(ni,0,1)
    xyi <- cbind(xi,yi)
    data(franke)
    fi <- franke.fn(xi,yi,1)
    fi
    xi
    yi
    fi
    
    xk <- lake.points.hol$lat
    yk <- lake.points.hol$long
    zk <- lake.points.hol$z
    
    KG <- interp(xk, yk, zk, nx = 80, ny = 80, method = "linear")
    IL <- interp(xi, yi, fi, nx = 80, ny = 80, method = "linear")
    
    ## prepare breaks and colors that match for image and contour:
    
    # Example Code 
    breaks <- pretty(seq(min(IL$z,na.rm=TRUE),max(IL$z,na.rm=TRUE),length=11))
    db <- breaks[2]-breaks[1]
    nb <- length(breaks)
    breaks <- c(breaks[1]-db,breaks,breaks[nb]+db)
    colors <- terrain.colors(length(breaks)-1)
    image(IL,breaks=breaks,col=colors,main="Franke function 1",
          sub=paste("linear interpolation, ", ni,"points"))
    contour(IL,add=TRUE,levels=breaks)
    points(xi,yi)
    
    # katie Code 
    breaks <- pretty(seq(min(KG$z,na.rm=TRUE),max(KG$z,na.rm=TRUE),length=11))
    db <- breaks[2]-breaks[1]
    nb <- length(breaks)
    breaks <- c(breaks[1]-db,breaks,breaks[nb]+db)
    colors <- topo.colors(length(breaks)-1)
    colors <- cols
    image(KG,breaks=breaks,col=colors,main="Katie function 1",
          sub=paste("linear interpolation, ", ni,"points"))
    contour(KG,add=TRUE,levels=breaks)
    points(xk,yk)
    
    
    
    # ALLLL EXAMPLE CODE 
    ### Use all datasets from Franke, 1979:
    data(franke)
    
    ## x-y irregular grid points:
    oldseed <- set.seed(42)
    ni <- 64
    xi <- runif(ni,0,1)
    yi <- runif(ni,0,1)
    xyi <- cbind(xi,yi)
    
    ## linear interpolation
    fi <- franke.fn(xi,yi,1)
    IL <- interp(xi,yi,fi,nx=80,ny=80,method="linear")
    
    ## prepare breaks and colors that match for image and contour:
    breaks <- pretty(seq(min(IL$z,na.rm=TRUE),max(IL$z,na.rm=TRUE),length=11))
    db <- breaks[2]-breaks[1]
    nb <- length(breaks)
    breaks <- c(breaks[1]-db,breaks,breaks[nb]+db)
    colors <- terrain.colors(length(breaks)-1)
    image(IL,breaks=breaks,col=colors,main="Franke function 1",
          sub=paste("linear interpolation, ", ni,"points"))
    contour(IL,add=TRUE,levels=breaks)
    points(xi,yi)
    
    
    ## spline interpolation
    fi <- franke.fn(xi,yi,1)
    IS <- interp(xi,yi,fi,method="akima",
                 kernel="gaussian",solver="QR")
    
    ## prepare breaks and colors that match for image and contour:
    breaks <- pretty(seq(min(IS$z,na.rm=TRUE),max(IS$z,na.rm=TRUE),length=11))
    db <- breaks[2]-breaks[1]
    nb <- length(breaks)
    breaks <- c(breaks[1]-db,breaks,breaks[nb]+db)
    colors <- terrain.colors(length(breaks)-1)
    image(IS,breaks=breaks,col=colors,main="Franke function 1",
          sub=paste("spline interpolation, ", ni,"points"))
    contour(IS,add=TRUE,levels=breaks)
    points(xi,yi)