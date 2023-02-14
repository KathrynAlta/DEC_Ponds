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
    install.packages("Rtools")
    install.packages("interpp")
    install.packages("gstat")
    library(interpp)
    
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
   
   # Holgerson Data - Sediment Depth 
   ggplot() +
     geom_sf(data = pond_boundary) +
     geom_sf_text(aes(label = sed_depth), data = pond_depths, size = 2.5) +
     annotation_scale(location = "br")
    
   # Example Data 
   ggplot() +
     geom_sf(data = pond_boundary) +
     geom_sf_text(aes(label = water_depth), data = pond_depths, size = 2.5) +
     annotation_scale(location = "br")
   
# 4. Add the coordinates of the boundary as columns and set the depth at the boundary to zero 
   
   # Example Data 
   boundary_points <- st_cast(boundary, "POINT")
   depths <- rbind(boundary_points, measured_depths) %>%
     cbind(., st_coordinates(.))
   
   depths
    
   # Holgerson Lab Data 
   pond_boundary
   pond_boundary_points <- st_cast(pond_boundary, "POINT")
   pond_boundary_points
   # Format pond boundary points 
     pond_name <- pond_depths[[1, "pond"]]
     formatted_pond_boundary_points <- pond_boundary_points %>% transmute(source = "boundary", pond = pond_name, water_depth = 0, sed_depth = 0)
     pond_boundary_points
     formatted_pond_boundary_points
     
   # Bind together boundary and depth 
     formatted_pond_boundary_points
     pond_depths
   full_pond_depths <- rbind(formatted_pond_boundary_points, pond_depths) %>%
     cbind(., st_coordinates(.))
   full_pond_depths
   
# 5. Create a grid to hold the raster output 
 
   
   #Example Code but I don't like the pipes here 
       # grid <- st_make_grid(depths, cellsize = c(10, 10), what = "centers") %>%
        # st_as_sf() %>% 
        # filter(st_contains(boundary, ., sparse = FALSE)) %>%  # This step is trying to cut off the grid so that it is only in the boundary of the polygon of the waterbody 
        # cbind(., st_coordinates(.))
   
   # Do step by step rather than in pipes 
   grid_step1 <- st_make_grid(depths, cellsize = c(10, 10), what = "centers")
   grid_step2 <- st_as_sf(grid_step1)
   grid_step3 <- st_contains(boundary, grid_step2, sparse = FALSE)
   grid_step4 <- grid_step2[grid_step3 == "TRUE" , ]
   grid_step5 <- cbind(grid_step4, st_coordinates(grid_step4))
   example_grid <- grid_step5

   
   # Holgerson Data 
   # Do step by step rather than in pipes 
   pond_grid_step1 <- st_make_grid(full_pond_depths, cellsize = c(5, 5), what = "centers")   #I made a bigger grid here (well smaller squares, more points)
   pond_grid_step2 <- st_as_sf(pond_grid_step1)
   pond_grid_step3 <- st_contains(pond_boundary, pond_grid_step2, sparse = FALSE)
   pond_grid_step4 <- pond_grid_step2[pond_grid_step3 == "TRUE" , ]
   pond_grid_step5 <- cbind(pond_grid_step4, st_coordinates(pond_grid_step4))
   pond_grid <- pond_grid_step5
    
# 6. TIN --> required packages not availale for this version of R 
   # triangular irregular network surface (TIN) connects points using a Delaunay triangulation (a network of triangles as round as
   # possible) and approximates each triangle as a plane. It results in contours that are pointier than you may be hoping for, but 
   # doesn’t predict any values higher or lower than you measured and is a good reality check on any other method as its main 
   # assumption is that you bothered to take a depth measurement anywhere that it mattered.
   
   # Exampl Data 
   
   fit_TIN <- interp::interpp(
     x = depths$X,
     y = depths$Y,
     z = depths$depth,
     xo = grid$X,
     yo = grid$Y,
     duplicate = "strip"
   )
   
   grid$TIN <- fit_TIN$z
    
# 7. [inverse distance weighted interpolation]    
   fit_gstat <- gstat::gstat(
     formula = depth ~ 1,
     data = as(depths, "Spatial"),
     nmax = 10, nmin = 3,
     set = list(idp = 0.5)
   )
   
   grid$IDW <- predict(fit_gstat, newdata = as(grid, "Spatial")) %>%
     st_as_sf() %>%
     pull(1)
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
