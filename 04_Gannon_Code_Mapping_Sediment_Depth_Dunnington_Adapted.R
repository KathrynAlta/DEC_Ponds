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
    
# 1. Input all data
    
  # Points data 
    harrison_pond_points <- read_sf("Spatial_Data/Harrison_Pond_030123/Harrison_Pond_Points_030123.shp")   %>%   # Pull in shape file
      transmute(source = "measured", pond = "Harrison_Pond") %>% # Subset to only columns that you need 
      st_transform(26920) # Transform or convert coordinates of simple feature 
    
    applegate_pond_points <- read_sf("Spatial_Data/Applegate_Pond_022723/Applegate_Pond_Points_030123_1559.shp")   %>%   # Pull in shape file
      transmute(source = "measured", pond = "Applegate_Pond") %>% # Subset to only columns that you need 
      st_transform(26920) # Transform or convert coordinates of simple feature 
    
    aquadro_pond_points <- read_sf("Spatial_Data/Aquadro_Pond_022723/Aquadro_Points_030123_1629.shp")   %>%   # Pull in shape file
      transmute(source = "measured", pond = "Aquadro_Pond") %>% # Subset to only columns that you need 
      st_transform(26920) # Transform or convert coordinates of simple feature 
    
  # Polygons 
    harrison_pond_boundary <- read_sf("Spatial_Data/Harrison_Pond_030123/Harrison_Pond_Poly_030123.shp") %>%  # read in polygon of pond 
      transmute(source = "boundary", depth = 0) %>%  #Saying that the depth at the edge of the pond is zero 
      st_transform(26920) %>%
      st_zm()
    
    applegate_pond_boundary <- read_sf("Spatial_Data/Applegate_Pond_022723/Applegate_Pond_Poly.shp") %>%  # read in polygon of pond 
      transmute(source = "boundary", depth = 0) %>%  #Saying that the depth at the edge of the pond is zero 
      st_transform(26920) %>%
      st_zm()
    
    aquadro_pond_boundary <- read_sf("Spatial_Data/Aquadro_Pond_022723/Aquadro_Pond_Poly_030123.shp") %>%  # read in polygon of pond 
      transmute(source = "boundary", depth = 0) %>%  #Saying that the depth at the edge of the pond is zero 
      st_transform(26920) %>%
      st_zm()
    
    
  # Depth Measurements
    harrison_pond_depth_meas <- read_xlsx("Depth_Measurements/Harrison_Pond_Depth_Measurements.xlsx")
    applegate_pond_depth_meas <- read_xlsx("Depth_Measurements/Applegate_Pond_Depth_Measurements.xlsx")
    aquadro_pond_depth_meas <- read_xlsx("Depth_Measurements/Aquadro_Pond_Depth_Measurements.xlsx")
    
    
  # Example Data from Dunnington Code 
      # Example Boundary 
        example_boundary <- read_sf(
          system.file(
            "longlake/LongLakeMarshWaterPoly.shp", 
            package = "ggspatial"
          )
        ) %>%
          filter(label == "Long Lake") %>%
          transmute(source = "boundary", depth = 0) %>%
          st_transform(26920) %>%
          st_zm()
        
        example_boundary
        
        # Example Depths 
        example_depths <- read_sf(
          system.file(
            "longlake/LongLakeDepthSurvey.shp", 
            package = "ggspatial"
          )  # this is loading data using normal read_sf from within a package in r 
        ) %>%
          transmute(source = "measured", depth = DEPTH_M) %>%   # this is subsetting down to only the columns needed 
          st_transform(26920)
    

# 2. Connect the lat long from the pond depths shape file to the measuremed depths from seperate df 
   
    # Write a function to connect the measured depths to the lat long in the shape file 
        pond_points <- applegate_pond_points
        measured_depths <- applegate_pond_depth_meas
        
    Connect_Depth_LatLong_FUNC <- function(pond_points, measured_depths){
      
      # Make a Measurement number in the points shape file that you can use to connect to 
      pond_points$Measurement_Number <- seq(1:nrow(pond_points)) 
      pond_points$Measurement_Number <- as.numeric(pond_points$Measurement_Number) #Change to a numeric so that it plays nicely with the Measurement Number from the other data frame 
      
      # Join the measured depths and the shape file of 
      pond_depths <- full_join(pond_points, measured_depths)
      
      #Columns 
      pond_depths$source <- "measured"
      pond_depths$pond <- pond_depths$Pond_Name
      
      # Calculate sediment depth in meters
      pond_depths$Sed_Thickness_m <- (pond_depths$Depth_to_btm_of_sediment_cm - pond_depths$Depth_to_top_of_Sediment_cm)/100
      
      # Convert water depth to meters 
      pond_depths$Water_Depth_m <- pond_depths$Depth_to_top_of_Sediment_cm / 100
      
      # Output 
      output <- pond_depths
    }
        
   # Apply function over the points shape file and measured depths for each pond 
    harrison_pond_depths <- Connect_Depth_LatLong_FUNC(harrison_pond_points, harrison_pond_depth_meas)
    aquadro_pond_depths <- Connect_Depth_LatLong_FUNC(aquadro_pond_points, aquadro_pond_depth_meas)
    applegate_pond_depths <- Connect_Depth_LatLong_FUNC(applegate_pond_points, applegate_pond_depth_meas)

   
# 3. Plot measured depths and thickness on a basic map  
   
   # Write a function to plot measured water depths 
     Plot_Water_Depths_FUNC <- function(pond_boundary, pond_depths){
       ggplot() +
         geom_sf(data = pond_boundary) +
         ggtitle("Measured Water Depths (m)") +
         geom_sf_text(aes(label = Water_Depth_m), data = pond_depths, size = 2.5) +
         annotation_scale(location = "br")
     }
   
   # Write a function to plot measured sediment thickness 
     Plot_Sed_Thick_FUNC <- function(pond_boundary, pond_depths){
       ggplot() +
         geom_sf(data = pond_boundary) +
         ggtitle("Measured Sediment Thickness (m)") +
         geom_sf_text(aes(label = Sed_Thickness_m), data = pond_depths, size = 2.5) +
         annotation_scale(location = "br")
     }
   
   # Plot each pond 
     Plot_Water_Depths_FUNC(aquadro_pond_boundary, aquadro_pond_depths)
     Plot_Sed_Thick_FUNC(aquadro_pond_boundary, aquadro_pond_depths)
     
     Plot_Water_Depths_FUNC(applegate_pond_boundary, applegate_pond_depths)
     Plot_Sed_Thick_FUNC(applegate_pond_boundary, applegate_pond_depths)
     
     Plot_Water_Depths_FUNC(harrison_pond_boundary, harrison_pond_depths)
     Plot_Sed_Thick_FUNC(harrison_pond_boundary, harrison_pond_depths)
   
   
# 4. Add the coordinates of the boundary as columns and set the depth at the boundary to zero 
   
  # Write Function 
     pond_boundary <- aquadro_pond_boundary
     pond_depths <- aquadro_pond_depths
     
   Coord_Bound_FUNC <- function(pond_boundary, pond_depths){
     
     # cast geometry to another type, change from polygon to points 
     pond_boundary
     pond_boundary_points <- st_cast(pond_boundary, "POINT") 
     pond_boundary_points
     
     # Format pond boundary points 
     pond_name <- pond_depths[[1, "Pond_Name"]]
     formatted_pond_boundary_points <- pond_boundary_points %>% transmute(source = "boundary", Pond_Name = pond_name, Water_Depth_m = 0, Sed_Thickness_m = 0)
     
     # Bind together boundary and depth 
     pond_depths <- subset(pond_depths, select = c("source", "Pond_Name", "Water_Depth_m", "Sed_Thickness_m", "geometry"))
     pond_depths
     formatted_pond_boundary_points
     full_pond_depths <- rbind(formatted_pond_boundary_points, pond_depths) %>%
       cbind(., st_coordinates(.))
     output <- full_pond_depths
   }
   
    names(applegate_pond_depths)
   applegate_pond_full <- Coord_Bound_FUNC(applegate_pond_boundary, applegate_pond_depths)
   
   
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
   
  #*******************************
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
   
   # HOlgerson Data - Sed Depth  - (option = "plasma") or (option = "magma")
  ellens_sed_depth_TPRS <- ggplot(pond_grid) +
     geom_sf(data = pond_boundary) +
     geom_raster(aes(X, Y, fill = TPRS_sed_depth)) +
     scale_fill_viridis_c(option = "plasma") +
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
    
