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
    
    # install.packages("gstat")
    library(gstat)
    
    # install.packages("mgcv")
    library(mgcv)
    
    # Not compatible wiht this version of R 
          # install.packages("Rtools")
          # install.packages("interpp")
          # library(interpp)
    
    
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
     transmute(source = "measured", pond = Pond_Name, water_depth_cm = Depth_top_ , sed_depth_cm = Sediment_T) %>% # Subset to only columns that you need 
     st_transform(26920) # Transform or convert coordinates of simple feature 
   pond_depths$water_depth <- pond_depths$water_depth_cm / 100
   pond_depths$sed_depth <- pond_depths$sed_depth_cm / 100

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
     pond_depths <- subset(pond_depths, select = c("source", "pond", "water_depth", "sed_depth", "geometry"))
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
   pond_grid_step1 <- st_make_grid(full_pond_depths, cellsize = c(1, 1), what = "centers")   #I made a bigger grid here (well smaller squares, more points)
   pond_grid_step2 <- st_as_sf(pond_grid_step1)
   pond_grid_step3 <- st_contains(pond_boundary, pond_grid_step2, sparse = FALSE)
   pond_grid_step4 <- pond_grid_step2[pond_grid_step3 == "TRUE" , ]
   pond_grid_step5 <- cbind(pond_grid_step4, st_coordinates(pond_grid_step4))
   pond_grid <- pond_grid_step5
    
# 6. TIN --> required packages not availale for this version of R 
  
    
# 7. Inverse Distance Weighting (IDW) 
# _____________________________________________________________________________  
   #Example Data 
       fit_gstat <- gstat::gstat(
         formula = depth ~ 1,
         data = as(depths, "Spatial"),
         nmax = 10, nmin = 3,
         set = list(idp = 0.5)
       )
       
       example_grid
       
       example_grid$IDW <- predict(fit_gstat, newdata = as(example_grid, "Spatial")) %>%
         st_as_sf() %>%
         pull(1)
   
   # Holgerson Lab Data 
   
   # build model for water depth 
   fit_gstat_ponds_water_depth <- gstat::gstat(
     formula = water_depth ~ 1,
     data = as(full_pond_depths, "Spatial"),
     nmax = 10, nmin = 3,
     set = list(idp = 0.5)
   )
   
   #build model for sediment depth 
   fit_gstat_ponds_sed_depth <- gstat::gstat(
     formula = sed_depth ~ 1,
     data = as(full_pond_depths, "Spatial"),
     nmax = 10, nmin = 3,
     set = list(idp = 0.5)
   )
  
   # use model to predict water depth 
   pond_grid$IDW_water_depth <- predict(fit_gstat_ponds_water_depth, newdata = as(pond_grid, "Spatial")) %>%
     st_as_sf() %>%
     pull(1)
   
   # Use model to predict sediment depth 
   pond_grid$IDW_sed_depth <- predict(fit_gstat_ponds_sed_depth, newdata = as(pond_grid, "Spatial")) %>%
     st_as_sf() %>%
     pull(1)
    
# 8 Thin Plate Regression Spline (TPRS) 
# ______________________________________________________________________________
    
   # Example Data 
  
   depths
   fit_gam_reml <- mgcv::gam(depth ~ s(X, Y, k = 60), data = depths, method = "REML")
   fit_gam_reml
   example_grid
   
   library(terra)
   example_grid$TPRS <- predict(fit_gam_reml, newdata = example_grid, type = "response")  
   
   # Holgerson Data 
   
   # Water Depth 
   fit_gam_reml_water_depth <- mgcv::gam(water_depth ~ s(X, Y, k = 60), data = full_pond_depths, method = "REML")
   pond_grid$TPRS_water_depth <- predict(fit_gam_reml_water_depth, newdata = pond_grid, type = "response")  
 
   # Sedimnet Depth 
   fit_gam_reml_sed_depth <- mgcv::gam(sed_depth ~ s(X, Y, k = 60), data = full_pond_depths, method = "REML")
   pond_grid$TPRS_sed_depth <- predict(fit_gam_reml_sed_depth, newdata = pond_grid, type = "response")  

# 9. Soap Film Smooth (SFS)   
# ______________________________________________________________________________ 
   
   # ---> 2/14/23 KG can't get this one to work just yet 
   
   library(sf)
   # Example Data 
   boundary_coords <- st_coordinates(boundary)
   
   gam_bound <- list(
     list(
       X = boundary_coords[-1, "X"], 
       Y = boundary_coords[-1, "Y"], 
       f = rep(0, nrow(boundary_coords))
     )
   )
   
   knot_points <- st_make_grid(
     boundary,
     n = c(10, 10),
     what = "centers"
   ) %>%
     st_as_sf() %>%
     filter(st_contains(boundary, x, sparse = FALSE)) %>%
     filter(
       !st_intersects(
         boundary %>% st_cast("LINESTRING") %>% st_buffer(10), 
         x, 
         sparse = FALSE
       )
     ) %>%
     cbind(., st_coordinates(.))
   
   fit_gam_soap <- gam(
     depth ~ s(X, Y, bs = "so", xt = list(bnd = gam_bound)),
     data = depths %>% 
       filter(source == "measured") %>% 
       filter(st_contains(boundary, geometry, sparse = FALSE)), 
     method = "REML", 
     knots = knot_points
   )
   
   grid$GAM_Soap <- predict(fit_gam_soap, newdata = grid, type = "response")
    
    
# 10. Compute Volume of water and volume of sediment 
    
  #  Example Data 
   boundary_area <- st_area(boundary) %>% 
     as.numeric()
   
   example_grid %>% 
     st_set_geometry(NULL) %>% 
     summarise(
       mean_depth = mean(IDW),
       volume = mean(IDW) * boundary_area
     )
   
   # Holgerson Data TPRS IDW
   pond_boundary_area <- st_area(pond_boundary) %>% 
     as.numeric()
   
   pond_grid %>% 
     st_set_geometry(NULL) %>% 
     summarise(
       mean_depth = mean(TPRS_sed_depth),
       volume = mean(TPRS_sed_depth) * pond_boundary_area
     )
    
# 11. Contouring 
   
   # Katie can't figure out piping 
   # depth_raster <- example_grid %>% 
   #   st_set_geometry(NULL) %>% 
   #   select(X, Y, IDW) %>% 
   #   raster::rasterFromXYZ(crs = raster::crs("+init=epsg:26920"))
   
   # Example Data 
   depth_raster <- example_grid %>% 
     st_set_geometry(NULL) 
   depth_raster <- subset(depth_raster, select = c(X, Y, IDW)) %>% 
     raster::rasterFromXYZ(crs = raster::crs("+init=epsg:26920"))
   
   depth_contours <- depth_raster %>% 
     raster::rasterToContour(levels = c(0.5, 1, 1.5)) %>% 
     st_as_sf()
   
   # Holgerson data 
   pond_depth_raster <- pond_grid %>% 
     st_set_geometry(NULL) 
   
   pond_depth_raster <- subset(pond_depth_raster, select = c(X, Y, IDW_sed_depth, IDW_water_depth)) %>% 
     raster::rasterFromXYZ(crs = raster::crs("+init=epsg:26920"))
   
   # Can't get contours to work right now 2/14/23 KG 
   pond_depth_contours <- pond_depth_raster %>% 
     raster::rasterToContour(levels = c(0.5, 1, 1.5)) %>% 
     st_as_sf()
   
   
# 12. Plotting 
   
   # Example Data 
   ggplot(example_grid) +
     geom_sf(data = boundary) +
     geom_raster(aes(X, Y, fill = TPRS)) +
     geom_sf(data = depth_contours) +
     scale_fill_viridis_c() +
     annotation_scale(location = "br") +
     labs(x = NULL, y = NULL, fill = "Depth (m)")
   
  # IDW MODEL: _______________________________________________________________
   
   # HOlgerson Data - Water Depth 
  ellens_water_depth_IDW <- ggplot(pond_grid) +
     geom_sf(data = pond_boundary) +
     geom_raster(aes(X, Y, fill = IDW_water_depth)) +
     scale_fill_viridis_c() +
     annotation_scale(location = "br") +
     labs(title= "Ellens Pond Water Depth (m) -- IDW", x = NULL, y = NULL, fill = "Water Depth (m)")
  ellens_water_depth_IDW
   
   # HOlgerson Data - Sed Depth 
  ellens_sed_depth_IDW <- ggplot(pond_grid) +
     geom_sf(data = pond_boundary) +
     geom_raster(aes(X, Y, fill = IDW_sed_depth)) +
     scale_fill_viridis_c() +
     annotation_scale(location = "br") +
     labs(title= "Ellens Pond Sediment Depth (m) -- IDW", x = NULL, y = NULL, fill = "Sediment Depth (m)")
  ellens_sed_depth_IDW
   
   # TPRS MODEL: _____________________________________________________________
   
   # HOlgerson Data - Water Depth 
  ellens_water_depth_TPRS <- ggplot(pond_grid) +
     geom_sf(data = pond_boundary) +
     geom_raster(aes(X, Y, fill = TPRS_water_depth)) +
     scale_fill_viridis_c() +
     annotation_scale(location = "br") +
     labs(title= "Ellens Pond Water Depth (m) -- TPRS", x = NULL, y = NULL, fill = "Water Depth (m)")
  ellens_water_depth_TPRS
   
   # HOlgerson Data - Sed Depth 
  ellens_sed_depth_TPRS <- ggplot(pond_grid) +
     geom_sf(data = pond_boundary) +
     geom_raster(aes(X, Y, fill = TPRS_sed_depth)) +
     scale_fill_viridis_c() +
     annotation_scale(location = "br") +
     labs(title= "Ellens Pond Sediment Depth (m) -- TPRS", x = NULL, y = NULL, fill = "Sediment Depth (m)")
  ellens_sed_depth_TPRS
    
# Save Output figures 
    getwd()
    ggsave("Output_Figures/ellens_water_depth_IDW_0214.png", ellens_water_depth_IDW)
    ggsave("Output_Figures/ellens_sed_depth_IDW_0214.png", ellens_sed_depth_IDW)
    ggsave("Output_Figures/ellens_water_depth_TPRS_0214.png", ellens_water_depth_TPRS)
    ggsave("Output_Figures/ellens_sed_depth_TPRS_0214.png", ellens_sed_depth_TPRS)
    
# Averages 
    mean(pond_depths$sed_depth)
    
