############################################################
# MAPPING SEDIMENT DEPTHS 
############################################################
# Holgerson Lab, Katie Gannon
# Code modified from https://dewey.dunnington.ca/post/2019/bathymetry-lake-volume-estimation-using-r/ 


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
    
# 1. Measured Depths 
    
    # Example Data 
    measured_depths <- read_sf(
      system.file(
        "longlake/LongLakeDepthSurvey.shp", 
        package = "ggspatial"
      )  # this is loading data using normal read_sf from within a package in r 
    ) %>%
      transmute(source = "measured", depth = DEPTH_M) %>%   # this is subsetting down to only the columns needed 
      st_transform(26920)
    
    
  # Holgerson Lab Data
   pond_depths <- read_sf("Spatial_Data/Ellens_Pond_020123/Ellens_Pond_Depths_1129.shp")   %>%   # Pull in shape file
     transmute(source = "measured", pond = Pond_Name, water_depth = Depth_top_ , sed_depth = Sediment_T) %>% # Subset to only columns that you need 
     st_transform(26920) # Transform or convert coordinates of simple feature 

   pond_depths
    
# 2. Boundary / Polygon 
   
   # Example Data 
   boundary <- read_sf(
     system.file(
       "longlake/LongLakeMarshWaterPoly.shp", 
       package = "ggspatial"
     )
   ) %>%
     filter(label == "Long Lake") %>%
     transmute(source = "boundary", depth = 0) %>%
     st_transform(26920) %>%
     st_zm()
   
   boundary
   
   #Holgerson Lab Data
   pond_boundary <- read_sf("Spatial_Data/Ellens_Pond_020123/Ellens_Pond_poly_020123_1134.shp") %>%  # read in polygon of pond 
     transmute(source = "boundary", depth = 0) %>%  #Saying that the depth at the edge of the pond is zero 
     st_transform(26920) %>%
     st_zm()
   
   pond_boundary
   
# 3. Take a look at the measurements 
   
   # Example Data 
   ggplot() +
     geom_sf(data = boundary) +
     geom_sf_text(aes(label = depth), data = measured_depths, size = 2.5) +
     annotation_scale(location = "br")
   
   # Example Data 
   ggplot() +
     geom_sf(data = pond_boundary) +
     geom_sf_text(aes(label = sed_depth), data = pond_depths, size = 2.5) +
     annotation_scale(location = "br")
    
# 4. Add the coordinates as columns 
   
   # Example Data 
   boundary_points <- st_cast(boundary, "POINT")
   depths <- rbind(boundary_points, measured_depths) %>%
     cbind(., st_coordinates(.))
   
   depths
    
   # Holgerson Lab Data 
   pond_boundary
   pond_boundary_points <- st_cast(pond_boundary, "POINT")
   # Format pond boundary points 
     pond_name <- pond_depths[[1, "pond"]]
     formatted_pond_boundary_points <- pond_boundary_points %>% transmute(source = "boundary", pond = pond_name, water_depth = 0, sed_depth = 0)
     pond_boundary_points
     formatted_pond_boundary_points
     
   # Bind together boundary and depth 
   full_pond_depths <- rbind(formatted_pond_boundary_points, pond_depths) %>%
     cbind(., st_coordinates(.))
   full_pond_depths
   
# 5. Create a grid to hold the raster output 
   
   # --- > Start here next time, trouble with the grid
   
   #Example Data 
   depths
   grid <- st_make_grid(depths, cellsize = c(10, 10), what = "centers") %>%
     st_as_sf() %>% 
     filter(st_contains(boundary, ., sparse = FALSE)) %>%
     cbind(., st_coordinates(.))
   
   # Holgerson Data 
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
