# Geocomputation with R 
# Chapter 2 Notes 

 #__________________________________________________________________________________________________
# 0. Set up R Environment 

    # Set working directory 
      setwd("~/Sediment_Mapping_DEC") # Desktop 
      getwd()
    
    # Load standard packages 
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
    
    
    # Install software packages 
      install.packages("terra")
      install.packages("spData")
      install.packages("spDataLarge", repos = "https://nowosad.r-universe.dev")
      remotes::install_github("geocompr/geocompkg")
      remotes::install_github("geocompr/geocompkg", dependencies = TRUE)
      
      
# Updating R 
      # installing/loading the package:
      if(!require(installr)) {
        install.packages("installr"); 
        require(installr)
      } #load / install+load installr
      library(installr)
      # using the package:
      updateR()

#__________________________________________________________________________________________________
# 2.2.1 An Introduction to Simple Features 
    vignette(package = "sf") # see which vignettes are available
    vignette("sf1")          # an introduction to the package
    
    # Exploring the world 
    class(world)
    head(world)
    names(world)
    world$geom
    world_df <- as.data.frame(world)
    plot(world)
    summary(world["lifeExp"])
    world_mini <- world[1:3, 1:2]
    head(world_mini)
    world_mini
    
# 2.2.2 Why Simple Features?    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    