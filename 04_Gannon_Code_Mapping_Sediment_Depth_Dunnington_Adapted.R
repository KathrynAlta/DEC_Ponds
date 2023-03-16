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
    
    install.packages("mgcv")
    library(mgcv)
    #install.packages("mgcv")
    # library(mgcv)
    
    # Not compatible wiht this version of R 
          # install.packages("installr")
          # install.packages("Rtools")
          # install.packages("interpp")
          # library(interpp)
    
# 1. Input all data
    
  # Points data 
    harrison_pond_points <- read_sf("Spatial_Data/Harrison_Pond_030123/Harrison_Pond_Points_030123_1734.shp")   %>%   # Pull in shape file
      transmute(source = "measured", pond = "Harrison_Pond") %>% # Subset to only columns that you need 
      st_transform(26920) # Transform or convert coordinates of simple feature 
    
    applegate_pond_points <- read_sf("Spatial_Data/Applegate_Pond_022723/Applegate_Pond_Points_030123_noz.shp")   %>%   # Pull in shape file
      transmute(source = "measured", pond = "Applegate_Pond") %>% # Subset to only columns that you need 
      st_transform(26920) # Transform or convert coordinates of simple feature 
    
    aquadro_pond_points <- read_sf("Spatial_Data/Aquadro_Pond_022723/Aquadro_Pond_Points_030123_noz.shp")   %>%   # Pull in shape file
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
     
     
   
  # 4.1 ) For all ponds cast geometry to another type, change from polygon to points 
     # This has to be done individually because it throws a warning 
     aquadro_pond_boundary_points <- st_cast(aquadro_pond_boundary, "POINT") 
     applegate_pond_boundary_points <- st_cast(applegate_pond_boundary, "POINT")
     harrison_pond_boundary_points <- st_cast(harrison_pond_boundary, "POINT")
     
  # 4.2) Write Function 
     
         # dfs to test function 
           pond_boundary_points <- aquadro_pond_boundary_points  # this is just a long list of points on the boundary with depth set to zero 
           pond_depths <- aquadro_pond_depths  # this is all of the points where we have measured water and sediment depths 
           
     Coord_Bound_FUNC <- function(pond_boundary_points, pond_depths){
       
         # Format pond boundary points 
         pond_name <- pond_depths[[1, "Pond_Name"]] #grabbing the pond name from the pond depths data frame 
         formatted_pond_boundary_points <- pond_boundary_points %>% transmute(source = "boundary", Pond_Name = pond_name, Water_Depth_m = 0, Sed_Thickness_m = 0)
         
         # Bind together boundary and depth 
         pond_depths <- subset(pond_depths, select = c("source", "Pond_Name", "Water_Depth_m", "Sed_Thickness_m", "geometry"))
         pond_depths
         formatted_pond_boundary_points
         full_pond_depths <- rbind(formatted_pond_boundary_points, pond_depths) %>%
           cbind(., st_coordinates(.))
         output <- full_pond_depths
       }
    
  # 4.3) Run function across all ponds 
   aquadro_pond_full <- Coord_Bound_FUNC(aquadro_pond_boundary_points, aquadro_pond_depths)
   applegate_pond_full <- Coord_Bound_FUNC(applegate_pond_boundary_points, applegate_pond_depths)
   harrison_pond_full <- Coord_Bound_FUNC(harrison_pond_boundary_points, harrison_pond_depths)
   
   
# 5. Create a grid to hold the raster output
   thing1 <- as.character(aquadro_pond_full[1, "Pond_Name"])
   thing1[1]

   # Write a function to create a grid 
   GridCreate_FUNC <- function(name_pond_full, name_pond_boundary){
     pond_name <- as.character(name_pond_full[1,"Pond_Name"])
     pond_name_form <- pond_name[1]
     pond_grid_step1 <- st_make_grid(name_pond_full, cellsize = c(1, 1), what = "centers")   #I made a bigger grid here (well smaller squares, more points)
     pond_grid_step2 <- st_as_sf(pond_grid_step1)
     pond_grid_step3 <- st_contains(name_pond_boundary, pond_grid_step2, sparse = FALSE)
     pond_grid_step4 <- pond_grid_step2[pond_grid_step3 == "TRUE" , ]
     pond_grid_step5 <- cbind(pond_grid_step4, st_coordinates(pond_grid_step4))
     pond_grid_step5$Pond_Name <- pond_name_form
     pond_grid <- pond_grid_step5
   }
   
   # Run the Function for each pond 
   aquadro_grid <- GridCreate_FUNC(aquadro_pond_full, aquadro_pond_boundary)
   applegate_grid <- GridCreate_FUNC(applegate_pond_full, applegate_pond_boundary)
   harrison_grid <- GridCreate_FUNC(harrison_pond_full, harrison_pond_boundary)
    
# 6. TIN --> required packages not availale for this version of R 
  
    
# 7. Inverse Distance Weighting (IDW) 
# _____________________________________________________________________________  
   
   # 7.1) BATHYMETRY 
   
     # Write a function to build a model for water depth using IDW
         IDW_bathym_FUNC <- function(name_pond_full){
           output <- gstat::gstat(
             formula = Water_Depth_m ~ 1,
             data = as(name_pond_full, "Spatial"),
             nmax = 10, nmin = 3,
             set = list(idp = 0.5)
           )
         }
       
      # Apply the model function for each pond that you are interested in and save the output model as FIT  
           IDW_bathym_harrison_FIT <- IDW_bathym_FUNC(harrison_pond_full) # Model based on inverse distance weighted (IDW) predicting the bathymetry (water depth) of harrison pond (farm and res pond named by last name of land owner)
           IDW_bathym_applegate_FIT <- IDW_bathym_FUNC(applegate_pond_full)
           IDW_bathym_aquadro_FIT <- IDW_bathym_FUNC(aquadro_pond_full)
           
      # Write a function that uses the spatial model to predict the water depth & adds predicted values to the grid 
        IDW_predict_FUNC <- function(model_FIT, name_grid){
          predicted_formal_class <- predict(model_FIT, newdata = as(name_grid, "Spatial"))
          predicted_sf <- st_as_sf(predicted_formal_class)
          output <- predicted_sf$var1.pred
        }
        
      # Apply the predict function to each pond to get predictions 
        harrison_grid$IDW_water_depth <- IDW_predict_FUNC(IDW_bathym_harrison_FIT, harrison_grid)
        aquadro_grid$IDW_water_depth <- IDW_predict_FUNC(IDW_bathym_aquadro_FIT, aquadro_grid)
        applegate_grid$IDW_water_depth <- IDW_predict_FUNC(IDW_bathym_applegate_FIT, applegate_grid)
        
        
  # 7.2) SED THICKNESS 
        # Write a function to build a model for water depth using IDW
        IDW_sed_FUNC <- function(name_pond_full){
          output <- gstat::gstat(
            formula = Sed_Thickness_m ~ 1,
            data = as(name_pond_full, "Spatial"),
            nmax = 10, nmin = 3,
            set = list(idp = 0.5)
          )
        }
        
        # Apply the Model function for each pond that you are interested in and save the output model as FIT  
        IDW_sed_harrison_FIT <- IDW_sed_FUNC(harrison_pond_full) # Model based on inverse distance weighted (IDW) predicting the bathymetry (water depth) of harrison pond (farm and res pond named by last name of land owner)
        IDW_sed_applegate_FIT <- IDW_sed_FUNC(applegate_pond_full)
        IDW_sed_aquadro_FIT <- IDW_sed_FUNC(aquadro_pond_full)
        
        # Apply the predict function to each pond to get predictions 
        harrison_grid$IDW_sed_depth <- IDW_predict_FUNC(IDW_sed_harrison_FIT, harrison_grid)
        aquadro_grid$IDW_sed_depth <- IDW_predict_FUNC(IDW_sed_aquadro_FIT, aquadro_grid)
        applegate_grid$IDW_sed_depth <- IDW_predict_FUNC(IDW_sed_applegate_FIT, applegate_grid)
        
   
# 8 Thin Plate Regression Spline (TPRS) 
# ______________________________________________________________________________
   
  # 8.1) BATHYMETRY 

    # create a model for water depth using TPRS
      TPRS_bathym_harrison_FIT <- mgcv::gam(Water_Depth_m ~ s(X, Y, k = 60), data = harrison_pond_full, method = "REML")
      TPRS_bathym_aquadro_FIT <- mgcv::gam(Water_Depth_m ~ s(X, Y, k = 60), data = aquadro_pond_full, method = "REML")
      TPRS_bathym_applegate_FIT <- mgcv::gam(Water_Depth_m ~ s(X, Y, k = 60), data = applegate_pond_full, method = "REML")
      
      
      # Use that TPRS model to get predictions and add them to the grid  
      # NOTE --> If this spits an error reinstall mgcv package and try again (no idea why that works but it does)
       harrison_grid$TPRS_water_depth <- predict(TPRS_bathym_harrison_FIT, newdata = harrison_grid, type = "response")  
       aquadro_grid$TPRS_water_depth <- predict(TPRS_bathym_aquadro_FIT, newdata = aquadro_grid, type = "response")  
       applegate_grid$TPRS_water_depth <- predict(TPRS_bathym_applegate_FIT, newdata = applegate_grid, type = "response")  
     
  # 8.1) BATHYMETRY 
       
    # create a model for water depth using TPRS
       TPRS_sedmap_harrison_FIT <- mgcv::gam(Sed_Thickness_m ~ s(X, Y, k = 60), data = harrison_pond_full, method = "REML")
       TPRS_sedmap_aquadro_FIT <- mgcv::gam(Sed_Thickness_m ~ s(X, Y, k = 60), data = aquadro_pond_full, method = "REML")
       TPRS_sedmap_applegate_FIT <- mgcv::gam(Sed_Thickness_m ~ s(X, Y, k = 60), data = applegate_pond_full, method = "REML")
       
       
    # Use that TPRS model to get predictions and add them to the grid  
       harrison_grid$TPRS_sed_depth <- predict(TPRS_sedmap_harrison_FIT, newdata = harrison_grid, type = "response")  
       aquadro_grid$TPRS_sed_depth <- predict(TPRS_sedmap_aquadro_FIT, newdata = aquadro_grid, type = "response")  
       applegate_grid$TPRS_sed_depth <- predict(TPRS_sedmap_applegate_FIT, newdata = applegate_grid, type = "response")  
       
      
# 9. Soap Film Smooth (SFS)   
# ______________________________________________________________________________ 
   
   # ---> 2/14/23 KG can't get this one to work just yet 
   # 3/16/23 KG working on it 
   library(sf)
 
  #OG Code 
   boundary_coords <- st_coordinates(boundary) 
   
   # Gam_bound 
       gam_bound <- list(
         list(
           X = boundary_coords[-1, "X"], 
           Y = boundary_coords[-1, "Y"], 
           f = rep(0, nrow(boundary_coords))
         )
       )
   # Knot POints 
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
   
  # FIT gam Soap 
       fit_gam_soap <- gam(
         depth ~ s(X, Y, bs = "so", xt = list(bnd = gam_bound)),
         data = depths %>% 
           filter(source == "measured") %>% 
           filter(st_contains(boundary, geometry, sparse = FALSE)), 
         method = "REML", 
         knots = knot_points
       )
       
   # Add to Grid 
   grid$GAM_Soap <- predict(fit_gam_soap, newdata = grid, type = "response")
   
   
#**********************************************   
  # FAFIO Code
   boundary_coords <- st_coordinates( harrison_pond_boundary) 
   
   # Gam Bound --> Runs 
   gam_bound <- list(
     list(
       X = boundary_coords[-1, "X"], 
       Y = boundary_coords[-1, "Y"], 
       f = rep(0, nrow(boundary_coords))
     )
   )
   
   # KNot Points - make a 10 by ten grid inside of the pond boundary and save the center points of each grid square into a simple feature collection of spatial points 
   # st_cast does not like being fed into a vector 
   
   # knot_points_ Step 1  
      # make a 10 by ten grid inside of the pond boundary and save the center points of each grid square
      # convert that list of center points to an sf 
      # subset that list of center points to only the points that fall within the the pond boundary 
   knot_points_1 <- st_make_grid(
     harrison_pond_boundary,      
     n = c(10, 10),
     what = "centers"
   ) %>%
     st_as_sf() %>%
     filter(as.vector(st_contains(harrison_pond_boundary, x, sparse = FALSE))) 
   
   # Knot Points Step 2 
      
      # Step by step 
     thing_to_intersect <- harrison_pond_boundary %>% st_cast("LINESTRING") %>% st_buffer(10)
     thing1 <- harrison_pond_boundary %>% st_cast("LINESTRING")
     thing2 <- thing1 %>% st_buffer(10)
     thing3 <- as.vector(thing2)
   
   # knot_points_2 
   knot_points_2 <- knot_points_1 %>%  # this next section is where we hit out issue 
     filter(
       !st_intersects(
         harrison_pond_boundary %>% st_cast("LINESTRING") %>% st_buffer(10), 
         x, 
         sparse = FALSE
       )
     ) %>%
     cbind(., st_coordinates(.))
   
   # Messed with 
            knot_points <- st_make_grid(
                 harrison_pond_boundary,
                 n = c(10, 10),
                 what = "centers"
               ) 
           knot_points_sf <- st_as_sf(knot_points)  # convert to an sf 
           
           knot_points_contained <- st_contains(harrison_pond_boundary, knot_points_2, sparse = FALSE)
           knot_points_contained <- as.vector(knot_points_contained)
           
           
           knot_points <- knot_points_sf %>% filter(as.vector(st_contains(harrison_pond_boundary, knot_points_sf, sparse = FALSE))) 
           
           
           knot_points <- knot_points %>% filter(st_contains(harrison_pond_boundary, x, sparse = FALSE)) 
             
             %>%
             filter(
               !st_intersects(
                 harrison_pond_boundary %>% st_cast("LINESTRING") %>% st_buffer(10), 
                 x, 
                 sparse = FALSE
               )
             ) %>%
             cbind(., st_coordinates(.))
   
   
   # Fit gam soap 
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
   
##################################################################################################################  
# 12. Plotting 
   
   # Write function so that you can select which model to plot 
   
   # Write a Function to plot water depth 
   Plot_bathym_FUNC <- function(name_grid, name_boundary){
     pond_name <- as.character(name_grid[1, "Pond_Name"])  # save pond name to use in the title of the plot 
     pond_name_form <- pond_name[1]  # format pond name 
     output_plot <- ggplot(name_grid) +
       geom_sf(data = name_boundary) +
       geom_raster(aes(X, Y, fill = IDW_water_depth)) +
       scale_fill_viridis_c() +
       annotation_scale(location = "br") +
       labs(title= paste(pond_name, "Water Depth (m) -- IDW", sep = " "), x = NULL, y = NULL, fill = "Water Depth (m)")
   }
   
   # Write a Function to plot Sediment depth 
   Plot_sedmap_FUNC <- function(name_grid, name_boundary, IDW_sed_depth){
     pond_name <- as.character(name_grid[1, "Pond_Name"])  # save pond name to use in the title of the plot 
     pond_name_form <- pond_name[1] # format pond name 
     output_plot <- ggplot(name_grid) +
       geom_sf(data = name_boundary) +
       geom_raster(aes(X, Y, fill = IDW_sed_depth)) +
       scale_fill_viridis_c(option = "plasma") +
       annotation_scale(location = "br") +
       labs(title= paste(pond_name, "Sediment Depth (m) -- IDW", sep = " "), x = NULL, y = NULL, fill = "Sediment Depth (m)")
   }
   
  # Plot and Save 
   
   # Plot
     plot_bathym_IDW_applegate <- Plot_bathym_FUNC(applegate_grid, applegate_pond_boundary)
     plot_sedmap_IDW_applegate <- Plot_sedmap_FUNC(applegate_grid, applegate_pond_boundary)
     
     plot_bathym_IDW_aquadro <- Plot_bathym_FUNC(aquadro_grid, aquadro_pond_boundary)
     plot_sedmap_IDW_aquadro <- Plot_sedmap_FUNC(aquadro_grid, aquadro_pond_boundary)
     
     plot_bathym_IDW_harrison <- Plot_bathym_FUNC(harrison_grid, harrison_pond_boundary)
     plot_sedmap_IDW_harrison <- Plot_sedmap_FUNC(harrison_grid, harrison_pond_boundary)
     
    # Save 
     ggsave("Output_Figures/plot_bathym_IDW_applegate_031323.png", plot_bathym_IDW_applegate)
     ggsave("Output_Figures/plot_sedmap_IDW_applegate_031323.png", plot_sedmap_IDW_applegate)
     
     ggsave("Output_Figures/plot_bathym_IDW_harrison_031323.png", plot_bathym_IDW_harrison)
     ggsave("Output_Figures/plot_sedmap_IDW_harrison_031323.png", plot_sedmap_IDW_harrison)
     
     ggsave("Output_Figures/plot_bathym_IDW_aquadro_031323.png", plot_bathym_IDW_aquadro)
     ggsave("Output_Figures/plot_sedmap_IDW_aquadro_031323.png", plot_sedmap_IDW_aquadro)
   
   
  
  # IDW MODEL: _______________________________________________________________
   
   # HOlgerson Data - Water Depth 
   harrison_grid
  applegate_water_depth_IDW <- ggplot(applegate_grid) +
     geom_sf(data = applegate_pond_boundary) +
     geom_raster(aes(X, Y, fill = IDW_water_depth)) +
     scale_fill_viridis_c() +
     annotation_scale(location = "br") +
     labs(title= "Applegate Pond Water Depth (m) -- IDW", x = NULL, y = NULL, fill = "Water Depth (m)")
  applegate_water_depth_IDW
   
   # HOlgerson Data - Sed Depth 
  harrison_sed_depth_IDW <- ggplot(harrison_grid) +
     geom_sf(data = harrison_pond_boundary) +
     geom_raster(aes(X, Y, fill = IDW_sed_depth)) +
      scale_fill_viridis_c(option = "plasma") +
     annotation_scale(location = "br") +
     labs(title= "Harrison Pond Sediment Depth (m) -- IDW", x = NULL, y = NULL, fill = "Sediment Depth (m)")
  harrison_sed_depth_IDW
   
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
    
