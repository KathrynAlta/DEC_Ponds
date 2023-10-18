############################################################
# MAPPING SEDIMENT DEPTHS 
############################################################
# Holgerson Lab, Katie Gannon
# Code modified from https://dewey.dunnington.ca/post/2019/bathymetry-lake-volume-estimation-using-r/ 


# 0. Set up R environment and Load Data 

    #Set working directory: 
    # setwd("~/DEC_Ponds") 
    setwd("~/OneDrive/Holgerson_Lab/DEC_Ponds") # Mac 
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
    
    #install.packages("gstat")
    library(gstat)
    
    # install.packages("mgcv")
    library(mgcv)
    
    #install.packages("interp")
    library(interp)  # could not install properly on Mac? "compilation failed" 
    
    # Not compatible wiht this version of R 
          # install.packages("installr")
          # install.packages("Rtools")
          # install.packages("interpp")
          # library(interpp)
    
# 1. Input all data
    
    # Load Depths data 
    sediment_depths <- read_xlsx("~/OneDrive/Holgerson_Lab/DEC_Ponds/Input_Files/DEC_Ponds_Sediment_Mapping_Depths.xlsx")
      # Formatt 
      names(sediment_depths)[names(sediment_depths) == "Pond Name"] <- "Pond"
      names(sediment_depths)[names(sediment_depths) == "Site Number"] <- "Measurement_Number"
      names(sediment_depths)[names(sediment_depths) == "Depth of top of sediments (cm)"] <- "Depth_to_top_of_Sediment_cm"
      names(sediment_depths)[names(sediment_depths) == "Depth of bottom of sediments (cm)"] <- "Depth_to_btm_of_sediment_cm"
      sediment_depths$Measurement_Number <- as.numeric(sediment_depths$Measurement_Number)
      sediment_depths$Depth_to_top_of_Sediment_cm <- as.numeric(sediment_depths$Depth_to_top_of_Sediment_cm)
      sediment_depths$Depth_to_btm_of_sediment_cm <- as.numeric(sediment_depths$Depth_to_btm_of_sediment_cm)
      sediment_depths <- subset(sediment_depths, select = c( "Pond", "Measurement_Number", "Depth_to_top_of_Sediment_cm", "Depth_to_btm_of_sediment_cm"))
      
      
      
    # Input Spatial Files on Desktop 
      #####    
  # Points data 
    # Intensive Desktop 
    boyce_points <- read_sf("/Users/kag326/Documents/ArcGIS/Projects/DEC_Farm_Residential_Ponds/Output_Shape_Files/Boyce_Points.shp")   %>%   # Pull in shape file
      transmute(source = "measured", pond = "Boyce") %>% # Subset to only columns that you need 
      st_transform(26920) # Transform or convert coordinates of simple feature 
    
    white_points <- read_sf("/Users/kag326/Documents/ArcGIS/Projects/DEC_Farm_Residential_Ponds/Output_Shape_Files/White_Points.shp")   %>%   # Pull in shape file
      transmute(source = "measured", pond = "White") %>% # Subset to only columns that you need 
      st_transform(26920) # Transform or convert coordinates of simple feature 
    
    howarth_points <- read_sf("/Users/kag326/Documents/ArcGIS/Projects/DEC_Farm_Residential_Ponds/Output_Shape_Files/Howarth_Points.shp")   %>%   # Pull in shape file
      transmute(source = "measured", pond = "Howarth") %>% # Subset to only columns that you need 
      st_transform(26920) # Transform or convert coordinates of simple feature 
    
    edwards_points <- read_sf("/Users/kag326/Documents/ArcGIS/Projects/DEC_Farm_Residential_Ponds/Output_Shape_Files/Edwards_Points.shp")   %>%   # Pull in shape file
      transmute(source = "measured", pond = "Edwards") %>% # Subset to only columns that you need 
      st_transform(26920) # Transform or convert coordinates of simple feature 
    
    shelterbelt_points <- read_sf("/Users/kag326/Documents/ArcGIS/Projects/DEC_Farm_Residential_Ponds/Output_Shape_Files/Shelterbelt_Points.shp")   %>%   # Pull in shape file
      transmute(source = "measured", pond = "Shelterbelt") %>% # Subset to only columns that you need 
      st_transform(26920) # Transform or convert coordinates of simple feature 
    
    mtpleas_se_points <- read_sf("/Users/kag326/Documents/ArcGIS/Projects/DEC_Farm_Residential_Ponds/Output_Shape_Files/Mt_Pleasant_SE_Points.shp")   %>%   # Pull in shape file
      transmute(source = "measured", pond = "MtPleasantSE") %>% # Subset to only columns that you need 
      st_transform(26920) # Transform or convert coordinates of simple feature 
    
    levine_points <- read_sf("/Users/kag326/Documents/ArcGIS/Projects/DEC_Farm_Residential_Ponds/Output_Shape_Files/Levine_Points.shp")   %>%   # Pull in shape file
      transmute(source = "measured", pond = "Levine") %>% # Subset to only columns that you need 
      st_transform(26920) # Transform or convert coordinates of simple feature 
    
    harrison_pond_points <- read_sf("Spatial_Data_SedimentMapping/Harrison_Pond_030123/Harrison_Pond_Points_030123_1734.shp")   %>%   # Pull in shape file
      transmute(source = "measured", pond = "Harrison") %>% # Subset to only columns that you need 
      st_transform(26920) # Transform or convert coordinates of simple feature 
    
    # Extensive 
    applegate_pond_points <- read_sf("Spatial_Data_SedimentMapping/Applegate_Pond_022723/Applegate_Pond_Points_030123_noz.shp")   %>%   # Pull in shape file
      transmute(source = "measured", pond = "Applegate") %>% # Subset to only columns that you need 
      st_transform(26920) # Transform or convert coordinates of simple feature 
    
    aquadro_pond_points <- read_sf("Spatial_Data_SedimentMapping/Aquadro_Pond_022723/Aquadro_Pond_Points_030123_noz.shp")   %>%   # Pull in shape file
      transmute(source = "measured", pond = "Aquadro") %>% # Subset to only columns that you need 
      st_transform(26920) # Transform or convert coordinates of simple feature 
    
   
    
  # Polygons 
    harrison_pond_boundary <- read_sf("Spatial_Data_SedimentMapping/Harrison_Pond_030123/Harrison_Pond_Poly_030123.shp") %>%  # read in polygon of pond 
      transmute(source = "boundary", depth = 0) %>%  #Saying that the depth at the edge of the pond is zero 
      st_transform(26920) %>%
      st_zm()
    
    applegate_pond_boundary <- read_sf("Spatial_Data_SedimentMapping/Applegate_Pond_022723/Applegate_Pond_Poly.shp") %>%  # read in polygon of pond 
      transmute(source = "boundary", depth = 0) %>%  #Saying that the depth at the edge of the pond is zero 
      st_transform(26920) %>%
      st_zm()
    
    aquadro_pond_boundary <- read_sf("Spatial_Data_SedimentMapping/Aquadro_Pond_022723/Aquadro_Pond_Poly_030123.shp") %>%  # read in polygon of pond 
      transmute(source = "boundary", depth = 0) %>%  #Saying that the depth at the edge of the pond is zero 
      st_transform(26920) %>%
      st_zm()
    
    
  # Depth Measurements
    harrison_pond_depth_meas <- read_xlsx("Depth_Measurements/Harrison_Pond_Depth_Measurements.xlsx")
    applegate_pond_depth_meas <- read_xlsx("Depth_Measurements/Applegate_Pond_Depth_Measurements.xlsx")
    aquadro_pond_depth_meas <- read_xlsx("Depth_Measurements/Aquadro_Pond_Depth_Measurements.xlsx")
      #####    

    # Input Spatial Files Mac 
      #####
    # Points 
    boyce_points <- read_sf("~/OneDrive/Holgerson_Lab/DEC_Ponds_Sediment_Data/Boyce_Points/Boyce_Points.shp")   %>%   # Pull in shape file
      transmute(source = "measured", pond = "Boyce") %>% # Subset to only columns that you need 
      st_transform(26920) # Transform or convert coordinates of simple feature 
    
    white_points <- read_sf("~/OneDrive/Holgerson_Lab/DEC_Ponds_Sediment_Data/White_Points/White_Points.shp")   %>%   # Pull in shape file
      transmute(source = "measured", pond = "White") %>% # Subset to only columns that you need 
      st_transform(26920) # Transform or convert coordinates of simple feature 
    
    howarth_points <- read_sf("~/OneDrive/Holgerson_Lab/DEC_Ponds_Sediment_Data/Howarth_Points/Howarth_Points.shp")   %>%   # Pull in shape file
      transmute(source = "measured", pond = "Howarth") %>% # Subset to only columns that you need 
      st_transform(26920) # Transform or convert coordinates of simple feature 
    
    edwards_points <- read_sf("~/OneDrive/Holgerson_Lab/DEC_Ponds_Sediment_Data/Edwards_Points/Edwards_Points.shp")   %>%   # Pull in shape file
      transmute(source = "measured", pond = "Edwards") %>% # Subset to only columns that you need 
      st_transform(26920) # Transform or convert coordinates of simple feature 
    
    shelterbelt_points <- read_sf("~/OneDrive/Holgerson_Lab/DEC_Ponds_Sediment_Data/Shelterbelt_Points/Shelterbelt_Points.shp")   %>%   # Pull in shape file
      transmute(source = "measured", pond = "Shelterbelt") %>% # Subset to only columns that you need 
      st_transform(26920) # Transform or convert coordinates of simple feature 
    
    mtpleasantse_points <- read_sf("~/OneDrive/Holgerson_Lab/DEC_Ponds_Sediment_Data/Mt_Pleasant_SE_Points/Mt_Pleasant_SE_Points.shp")   %>%   # Pull in shape file
      transmute(source = "measured", pond = "Mt_Pleasant_SE") %>% # Subset to only columns that you need 
      st_transform(26920) # Transform or convert coordinates of simple feature 
    
    harrison_points <- read_sf("~/OneDrive/Holgerson_Lab/DEC_Ponds_Sediment_Data/Harrison_Points_030123_noz/Harrison_Pond_Points_030123_1734.shp")   %>%   # Pull in shape file
      transmute(source = "measured", pond = "Harrison") %>% # Subset to only columns that you need 
      st_transform(26920) # Transform or convert coordinates of simple feature 
    
    levine_points <- read_sf("~/OneDrive/Holgerson_Lab/DEC_Ponds_Sediment_Data/Levine_Points/Levine_Points.shp")   %>%   # Pull in shape file
      transmute(source = "measured", pond = "Levine") %>% # Subset to only columns that you need 
      st_transform(26920) # Transform or convert coordinates of simple feature 
    
    aquadro_points <- read_sf("~/OneDrive/Holgerson_Lab/DEC_Ponds_Sediment_Data/Aquadro_Points_030123_noz/Aquadro_Pond_Points_030123_noz.shp")   %>%   # Pull in shape file
      transmute(source = "measured", pond = "Aquadro") %>% # Subset to only columns that you need 
      st_transform(26920) # Transform or convert coordinates of simple feature 
    
    applegate_points <- read_sf("~/OneDrive/Holgerson_Lab/DEC_Ponds_Sediment_Data/Applegate_Points_030123_noz/Applegate_Pond_Points_030123_noz.shp")   %>%   # Pull in shape file
      transmute(source = "measured", pond = "Applegate") %>% # Subset to only columns that you need 
      st_transform(26920) # Transform or convert coordinates of simple feature 
    
    # Polygons
    boyce_polygon <- read_sf("~/OneDrive/Holgerson_Lab/DEC_Ponds_Sediment_Data/Boyce_Polygon/Boyce_Polygon.shp") %>%  # read in polygon of pond 
      transmute(source = "boundary", depth = 0) %>%  #Saying that the depth at the edge of the pond is zero 
      st_transform(26920) %>%
      st_zm()
    
    white_polygon <- read_sf("~/OneDrive/Holgerson_Lab/DEC_Ponds_Sediment_Data/White_Polygon/White_Polygon.shp") %>%  # read in polygon of pond 
      transmute(source = "boundary", depth = 0) %>%  #Saying that the depth at the edge of the pond is zero 
      st_transform(26920) %>%
      st_zm()
    
    howarth_polygon <- read_sf("~/OneDrive/Holgerson_Lab/DEC_Ponds_Sediment_Data/Howarth_Polygon/Howarth_Polygon.shp") %>%  # read in polygon of pond 
      transmute(source = "boundary", depth = 0) %>%  #Saying that the depth at the edge of the pond is zero 
      st_transform(26920) %>%
      st_zm()
    
    edwards_polygon <- read_sf("~/OneDrive/Holgerson_Lab/DEC_Ponds_Sediment_Data/Edwards_Polygon/Edwards_Polygon.shp") %>%  # read in polygon of pond 
      transmute(source = "boundary", depth = 0) %>%  #Saying that the depth at the edge of the pond is zero 
      st_transform(26920) %>%
      st_zm()
    
    shelterbelt_polygon <- read_sf("~/OneDrive/Holgerson_Lab/DEC_Ponds_Sediment_Data/Shelterbelt_Polygon/Shelterbelt_Polygon.shp") %>%  # read in polygon of pond 
      transmute(source = "boundary", depth = 0) %>%  #Saying that the depth at the edge of the pond is zero 
      st_transform(26920) %>%
      st_zm()
    
    mtpleasantse_polygon <- read_sf("~/OneDrive/Holgerson_Lab/DEC_Ponds_Sediment_Data/Mt_Pleasant_SE_Polygon/Mt_Pleasant_SE_Polygon.shp") %>%  # read in polygon of pond 
      transmute(source = "boundary", depth = 0) %>%  #Saying that the depth at the edge of the pond is zero 
      st_transform(26920) %>%
      st_zm()
    
    harrison_polygon <- read_sf("~/OneDrive/Holgerson_Lab/DEC_Ponds_Sediment_Data/Harrison_Polygon/Harrison_Pond_Poly_030123.shp") %>%  # read in polygon of pond 
      transmute(source = "boundary", depth = 0) %>%  #Saying that the depth at the edge of the pond is zero 
      st_transform(26920) %>%
      st_zm()
    
    levine_polygon <- read_sf("~/OneDrive/Holgerson_Lab/DEC_Ponds_Sediment_Data/Levine_Polygon/Levine_Polygon.shp") %>%  # read in polygon of pond 
      transmute(source = "boundary", depth = 0) %>%  #Saying that the depth at the edge of the pond is zero 
      st_transform(26920) %>%
      st_zm()
    
    aquadro_polygon <- read_sf("~/OneDrive/Holgerson_Lab/DEC_Ponds_Sediment_Data/Aquadro_Polygon/Aquardo_Pond_Polygon.shp") %>%  # read in polygon of pond 
      transmute(source = "boundary", depth = 0) %>%  #Saying that the depth at the edge of the pond is zero 
      st_transform(26920) %>%
      st_zm()
    
    applegate_polygon <- read_sf("~/OneDrive/Holgerson_Lab/DEC_Ponds_Sediment_Data/Applegate_Polygon/Applegate_Pond_Poly.shp") %>%  # read in polygon of pond 
      transmute(source = "boundary", depth = 0) %>%  #Saying that the depth at the edge of the pond is zero 
      st_transform(26920) %>%
      st_zm()
      #####

#####    
# 2. Connect the lat long from the pond depths shape file to the measuremed depths from seperate df --> make spatial
#_______________________________________________________________________________    
    # Seperate Sediment depth data out by pond 
    meas_depths_list <-split(sediment_depths, sediment_depths$Pond)  
      # this seperates the one big df of all the measured depths into a list with a seperate df for each pond 
    
    # Intensive by hand 
        boyce_meas_depths <- subset(sediment_depths, sediment_depths$Pond == "Boyce")
        white_meas_depths <- subset(sediment_depths, sediment_depths$Pond == "White")
        howarth_meas_depths <- subset(sediment_depths, sediment_depths$Pond == "Howarth")
        edwards_meas_depths <- subset(sediment_depths, sediment_depths$Pond == "Edwards")
        shelterbelt_meas_depths <- subset(sediment_depths, sediment_depths$Pond == "Shelterbelt")
        mtpleasantse_meas_depths <- subset(sediment_depths, sediment_depths$Pond == "Mt_Pleasant_SE")
        harrison_meas_depths <- subset(sediment_depths, sediment_depths$Pond == "Harrison")
        levine_meas_depths <- subset(sediment_depths, sediment_depths$Pond == "Levine")
        applegate_meas_depths <- subset(sediment_depths, sediment_depths$Pond == "Applegate")
        aquadro_meas_depths <- subset(sediment_depths, sediment_depths$Pond == "Aquadro")
        
    # Write a function to connect the measured depths to the lat long in the shape file 
        
        # Dummy data to write function 
        pond_points <- aquadro_points
        measured_depths <- aquadro_meas_depths
        
    Connect_Depth_LatLong_FUNC <- function(pond_points, measured_depths){
      
      # Make a Measurement number in the points shape file that you can use to connect to 
      pond_points$Measurement_Number <- seq(1:nrow(pond_points)) 
      pond_points$Measurement_Number <- as.numeric(pond_points$Measurement_Number) #Change to a numeric so that it plays nicely with the Measurement Number from the other data frame 
      
      # Join the measured depths and the shape file of 
      pond_depths <- full_join(pond_points, measured_depths)
      
      #Columns 
      pond_depths$source <- "measured"
      pond_depths$pond <- measured_depths$Pond
      
      # Calculate sediment depth in meters
      pond_depths$Sed_Thickness_m <- (pond_depths$Depth_to_btm_of_sediment_cm - pond_depths$Depth_to_top_of_Sediment_cm)/100
      
      # Convert water depth to meters 
      pond_depths$Water_Depth_m <- pond_depths$Depth_to_top_of_Sediment_cm / 100
      
      # Output 
      output <- as.data.frame(pond_depths)
    }
        
   # Apply function over the points shape file and measured depths for each pond 
    
    # Applying function individually for each pond 
      boyce_depths <- Connect_Depth_LatLong_FUNC(boyce_points, boyce_meas_depths) #Doesn't work because incorrectly exported shape file 
      
      applegate_depths_latlong <- Connect_Depth_LatLong_FUNC(applegate_points, applegate_meas_depths)
      aquadro_depths_latlong <- Connect_Depth_LatLong_FUNC(aquadro_points, aquadro_meas_depths)
      harrison_depths_latlong <- Connect_Depth_LatLong_FUNC(harrison_points, harrison_meas_depths)
      
    # Applying function over all ponds using mapply()
    
      # Make a list of points dfs and a list of depths dfs 
      points_shp_list <- list(applegate_points, aquadro_points, harrison_points)
      meas_depths_list <- list(applegate_meas_depths, aquadro_meas_depths, harrison_meas_depths)
      
      # Run function over the two lists 
      meas_depths_latlong_list <- mapply(Connect_Depth_LatLong_FUNC, points_shp_list, meas_depths_list, USE.NAMES = TRUE, SIMPLIFY = FALSE)
      # Output is a list of dataframes with a df for each pond (I am a genius)
      
#********* see if this works feeding into next steps 
    
# 3. Plot measured depths and thickness on a basic map  
#_______________________________________________________________________________   
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
     Plot_Water_Depths_FUNC(aquadro_polygon, aquadro_depths_latlong)
     Plot_Sed_Thick_FUNC(aquadro_polygon, aquadro_depths_latlong)
     
     Plot_Water_Depths_FUNC(applegate_polygon, applegate_depths_latlong)
     Plot_Sed_Thick_FUNC(applegate_polygon, applegate_depths_latlong)
     
     Plot_Water_Depths_FUNC(harrison_polygon, harrison_depths_latlong)
     Plot_Sed_Thick_FUNC(harrison_polygon, harrison_depths_latlong)
   
   
# 4. Add the coordinates of the boundary as columns and set the depth at the boundary to zero 
#_______________________________________________________________________________    
  # 4.1 ) For all ponds cast geometry to another type, change from polygon to points 
     # This has to be done individually because it throws a warning 
     applegate_polygon_cast <- st_cast(applegate_polygon, "POINT")
     aquadro_polygon_cast <- st_cast(aquadro_polygon, "POINT") 
     harrison_polygon_cast <- st_cast(harrison_polygon, "POINT")
     
     # Listing out for each pond 
     
       # Intensive 
         boyce_polygon_cast <- st_cast(boyce_polygon, "POINT")
         white_polygon_cast <- st_cast(white_polygon, "POINT")
         howarth_polygon_cast <- st_cast(howarth_polygon, "POINT")
         edwards_polygon_cast <- st_cast(edwards_polygon, "POINT")
         shelterbelt_polygon_cast <- st_cast(shelterbelt_polygon, "POINT")
         mtpleasantse_polygon_cast <- st_cast(mtpleasantse_polygon, "POINT")
         harrison_polygon_cast <- st_cast(harrison_polygon, "POINT")
         levine_polygon_cast <- st_cast(levine_polygon, "POINT")
     
      #Extensive 
         aquadro_polygon_cast <- st_cast(aquadro_polygon, "POINT")
         longhouse_polygon_cast <- st_cast(longhouse_polygon, "POINT")
         ecovillage_polygon_cast <- st_cast(ecovillage_polygon, "POINT")
         dybowski_polygon_cast <- st_cast(dybowski_polygon, "POINT")
         applegate_polygon_cast <- st_cast(applegate_polygon, "POINT")
         mtpleasantne_polygon_cast <- st_cast(mtpleasantne_polygon, "POINT")
         barber_polygon_cast <- st_cast(barber_polygon, "POINT")
         stickandstone_polygon_cast <- st_cast(stickandstone_polygon, "POINT")
         englishshallow_polygon_cast <- st_cast(englishshallow_polygon, "POINT")
         engst_polygon_cast <- st_cast(engst_polygon, "POINT")
         rogers_polygon_cast <- st_cast(rogers_polygon, "POINT")
         carpenter_polygon_cast <- st_cast(carpenter_polygon, "POINT")
         walnutridge_polygon_cast <- st_cast(walnutridge_polygon, "POINT")
         lucas_polygon_cast <- st_cast(lucas_polygon, "POINT")
         collmer_polygon_cast <- st_cast(collmer_polygon, "POINT")
         vesa_polygon_cast <- st_cast(vesa_polygon, "POINT")
         conley_polygon_cast <- st_cast(conley_polygon, "POINT")
         hahn_polygon_cast <- st_cast(hahn_polygon, "POINT")
         marks_polygon_cast <- st_cast(marks_polygon, "POINT")
         englishdeep_polygon_cast <- st_cast(englishdeep_polygon, "POINT")
         
      # Put all of the cast boundaries together into a list 
         cast_boundaries_list <- list(boyce_polygon_cast, white_polygon_cast, howarth_polygon_cast, edwards_polygon_cast,
                                      shelterbelt_polygon_cast, mtpleasantse_polygon_cast, harrison_polygon_cast, levine_polygon_cast, 
                                      aquadro_polygon_cast, longhouse_polygon_cast, ecovillage_polygon_cast, dybowski_polygon_cast, 
                                      applegate_polygon_cast, mtpleasantne_polygon_cast, barber_polygon_cast, stickandstone_polygon_cast,
                                      englishshallow_polygon_cast, engst_polygon_cast, rogers_polygon_cast, carpenter_polygon_cast, 
                                      walnutridge_polygon_cast, lucas_polygon_cast, collmer_polygon_cast, vesa_polygon_cast, 
                                      conley_polygon_cast, hahn_polygon_cast, marks_polygon_cast, englishdeep_polygon_cast)
     
  # 4.2) Write Function 
     
         # Dummy data to write function 
           pond_boundary_points <- harrison_polygon_cast  # this is just a long list of points on the boundary with depth set to zero 
           pond_depths <- harrison_depths  # this is all of the points where we have measured water and sediment depths 
           
     Coord_Bound_FUNC <- function(pond_boundary_points, pond_depths){
       
         # Format pond boundary points 
         pond_name <- pond_depths[[1, "pond"]] #grabbing the pond name from the pond depths data frame 
         formatted_pond_boundary_points <- pond_boundary_points %>% transmute(source = "boundary", Pond_Name = pond_name, Water_Depth_m = 0, Sed_Thickness_m = 0)
         
         # Bind together boundary and depth 
         pond_depths <- subset(pond_depths, select = c("source", "pond", "Water_Depth_m", "Sed_Thickness_m", "geometry"))
         names(pond_depths)[names(pond_depths) == "pond"] <- "Pond_Name"
         formatted_pond_boundary_points
         pond_depths
         full_pond_depths <- rbind(formatted_pond_boundary_points, pond_depths) %>%
           cbind(., st_coordinates(.))
         output <- full_pond_depths
       }
    
  # 4.3) Run function across all ponds 
     
     # Run the function individually for each pond 
     applegate_full <- Coord_Bound_FUNC(applegate_polygon_cast, applegate_depths)
     aquadro_full <- Coord_Bound_FUNC(aquadro_polygon_cast, aquadro_depths)
     harrison_full <- Coord_Bound_FUNC(harrison_polygon_cast, harrison_depths)
     
     # Try Running the function across multiple ponds using mapply 
     cast_boundaries_list <- list(applegate_polygon_cast, aquadro_polygon_cast, harrison_polygon_cast)
     meas_depths_latlong_list <- list(applegate_depths_latlong, aquadro_depths_latlong, harrison_depths_latlong)
   
     # Run function over the two lists 
     depths_full_list <- mapply(Coord_Bound_FUNC, cast_boundaries_list, meas_depths_latlong_list, USE.NAMES = TRUE, SIMPLIFY = FALSE)
     ## This throws an error something about the geometry
     
# 5. Create a grid to hold the raster output
#_______________________________________________________________________________  
   # Dummy data to write function 
       thing1 <- as.character(aquadro_full[1, "Pond_Name"])
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
   aquadro_grid <- GridCreate_FUNC(aquadro_full, aquadro_polygon)
   applegate_grid <- GridCreate_FUNC(applegate_full, applegate_polygon)
   harrison_grid <- GridCreate_FUNC(harrison_full, harrison_polygon)

   
   # Up to here is all set up, getting the grid made (place to put output from the model) and processing the input 
   #   data (make spatial, calc depth, add zero depth around boundary) getting ready to make a feed the model 

   # Now there are multiple models that you can build that use different methods (math) to interpolate the water
   #   and sediment depths at each point on the grid. You are going to build and run multiple models and look at the
   #   output, then make a decision about what model to use for your final estimates 
      
    
# 6. Triangular Irregular Network Surface (TIN) 
# _____________________________________________________________________________ 
   # Need Interp function in R and can't get that to load on Mac right now 
   
   # Katie Mess with 
   TIN_sed_harrison_FIT <- interp::interpp(
     x = harrison_pond_full$X,
     y = harrison_pond_full$Y,
     z = harrison_pond_full$Sed_Thickness_m,
     xo = harrison_grid$X,
     yo = harrison_grid$Y,
     duplicate = "strip"
   )
   
   harrison_grid$TIN_sed_depth <- TIN_sed_harrison_FIT$z
   
   # Write a function to fit a TIN model to sediment depths 
    TIN_sed_FUNC <- function(pond_full, pond_grid){
      TIN_model <- interp::interpp(
        x = pond_full$X,
        y = pond_full$Y,
        z = pond_full$Sed_Thickness_m,
        xo = pond_grid$X,
        yo = pond_grid$Y,
        duplicate = "strip"
      )
      output <- TIN_model$z
    }
    
    # Write a function to fit a TIN model to water depths 
    TIN_bathym_FUNC <- function(pond_full, pond_grid){
      TIN_model <- interp::interpp(
        x = pond_full$X,
        y = pond_full$Y,
        z = pond_full$Water_Depth_m,
        xo = pond_grid$X,
        yo = pond_grid$Y,
        duplicate = "strip"
      )
      output <- TIN_model$z
    }
    
   # Apply function to build model for each pond for sediment thickness 
    harrison_grid$TIN_sed_depth <- TIN_sed_FUNC(harrison_full, harrison_grid)
    aquadro_grid$TIN_sed_depth <- TIN_sed_FUNC(aquadro_full, aquadro_grid)
    applegate_grid$TIN_sed_depth <- TIN_sed_FUNC(applegate_full, applegate_grid)
    
    # Apply function to build model for each pond for water depth 
    harrison_grid$TIN_water_depth <- TIN_bathym_FUNC(harrison_pond_full, harrison_grid)
    aquadro_grid$TIN_water_depth <- TIN_bathym_FUNC(aquadro_pond_full, aquadro_grid)
    applegate_grid$TIN_water_depth <- TIN_bathym_FUNC(applegate_pond_full, applegate_grid)
  
    
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
       
      # Apply the model function for each pond that you are interested in and save the model output as FIT  
           IDW_bathym_harrison_FIT <- IDW_bathym_FUNC(harrison_full) # Model based on inverse distance weighted (IDW) predicting the bathymetry (water depth) of harrison pond (farm and res pond named by last name of land owner)
           IDW_bathym_applegate_FIT <- IDW_bathym_FUNC(applegate_full)
           IDW_bathym_aquadro_FIT <- IDW_bathym_FUNC(aquadro_full)
           
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
        # Write a function to build a model for sed depth using IDW
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
          #install.packages("mgcv")
          library(mgcv)
      
       harrison_grid$TPRS_water_depth <- predict(TPRS_bathym_harrison_FIT, newdata = harrison_grid, type = "response")  
       aquadro_grid$TPRS_water_depth <- predict(TPRS_bathym_aquadro_FIT, newdata = aquadro_grid, type = "response")  
       applegate_grid$TPRS_water_depth <- predict(TPRS_bathym_applegate_FIT, newdata = applegate_grid, type = "response")  
     
  # 8.1) SEDIMENT DEPTH  
       
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
   
   
# Writing code to scale 9/29/23 
   
   # 9.1. Make Linestring for each pond (throws an air but works)
   harrison_linestring <- harrison_pond_boundary %>% st_cast("LINESTRING") %>% st_buffer(10)  # Need to make line string one at a time up here, it makes it but will throw an error 
   aquadro_linestring <- aquadro_pond_boundary %>% st_cast("LINESTRING") %>% st_buffer(10)  # Need to make line string one at a time up here, it makes it but will throw an error 
   applegate_linestring <- applegate_pond_boundary %>% st_cast("LINESTRING") %>% st_buffer(10)  # Need to make line string one at a time up here, it makes it but will throw an error 

  # 9.2 Create Gam bound for each pond
      # 9.2.a write a function to create gam bound 
      SOAP_gam_bound_FUNC <- function(pond_boundary){ 
        
        boundary_coords <- st_coordinates(pond_boundary)
        
        gam_bound <- list(
          list(
            X = boundary_coords[-1, "X"], 
            Y = boundary_coords[-1, "Y"], 
            f = rep(0, nrow(pond_boundary))
          )
        )
        }
   
      # 9.2.b apply function to create a gam boutnd for each pond  
      harrison_gam_bound <- SOAP_gam_bound_FUNC(harrison_pond_boundary)
      applegate_gam_bound <- SOAP_gam_bound_FUNC(applegate_pond_boundary)
      aquadro_gam_bound <- SOAP_gam_bound_FUNC(aquadro_pond_boundary)
   
   
  # 9.3. Create knot points for each pond 
      
      # 9.3.a Write a function to create knot points from line string and boundary 
   
         SOAP_knot_points_FUNC <- function(pond_boundary, pond_linestring){
           
               
               # FUNC Step 1. Make knot points 
               knot_points_1 <- st_make_grid(
                 pond_boundary,      
                 n = c(10, 10),
                 what = "centers"
               ) %>%
                 st_as_sf() %>%
                 filter(as.vector(st_contains(pond_boundary, x, sparse = FALSE)))
               
               # FUNC Step 2. Make Intersection  
               intersection <- !st_intersects(
                 pond_linestring, 
                 knot_points_1, 
                 sparse = FALSE
               )
           
               # FUNC Step 3. Filter knot points by intersection    
               knot_points <- knot_points_1 %>%   
                 filter(as.vector(intersection)) %>%
                 cbind(., st_coordinates(.))
         }
   
       # 9.3.b Apply Knot Points Function to Each Pond 
       harrison_knot_points <- SOAP_knot_points_FUNC(harrison_pond_boundary, harrison_linestring )
       aquadro_knot_points <- SOAP_knot_points_FUNC(aquadro_pond_boundary, aquadro_linestring )
       applegate_knot_points <- SOAP_knot_points_FUNC(applegate_pond_boundary, applegate_linestring )
   
   # 9.5. Bathymetry GAM model 
       
       # 9.5.a Write a function to fit a GAM model for bathymetry 
           SOAP_bathym_model_FUNC <- function(pond_full, pond_boundary, pond_knot_points, pond_gam_bound){
             
             SOAP_bathym_model <- gam(
               Water_Depth_m ~ s(X, Y, bs = "so", xt = list(bnd = pond_gam_bound)),
               data = pond_full %>% 
                 filter(source == "measured") %>% 
                 filter(as.vector(st_contains(pond_boundary, geometry, sparse = FALSE))), 
               method = "REML", 
               knots = pond_knot_points
             )
           }
           
      # 9.5.b Use bathym model function to make a bathymetry GAM model for each lake 
           SOAP_bathym_harrison_FIT <- SOAP_bathym_model_FUNC(harrison_pond_full, harrison_pond_boundary, harrison_knot_points, harrison_gam_bound)
           SOAP_bathym_aquadro_FIT <- SOAP_bathym_model_FUNC(aquadro_pond_full, aquadro_pond_boundary, aquadro_knot_points, aquadro_gam_bound)
           SOAP_bathym_applegate_FIT <- SOAP_bathym_model_FUNC(applegate_pond_full, applegate_pond_boundary, applegate_knot_points, applegate_gam_bound)
           
          
   # 9.6. Sediment GAM Model 
      #9.6.a  Write a function to fit a GAM model for sediment thickness 
         SOAP_seddepth_model_FUNC <- function(pond_full, pond_boundary, pond_knot_points, pond_gam_bound){
           SOAP_bathym_model <- gam(
             Sed_Thickness_m ~ s(X, Y, bs = "so", xt = list(bnd = pond_gam_bound)),
             data = pond_full %>% 
               filter(source == "measured") %>% 
               filter(as.vector(st_contains(pond_boundary, geometry, sparse = FALSE))), 
             method = "REML", 
             knots = pond_knot_points
           )
         }
         
      #9.6.b Use sediment model function to make a sediment GAM model for each lake
        SOAP_seddepth_harrison_FIT <- SOAP_seddepth_model_FUNC(harrison_pond_full, harrison_pond_boundary, harrison_knot_points, harrison_gam_bound)
        SOAP_seddepth_aquadro_FIT <- SOAP_seddepth_model_FUNC(aquadro_pond_full, aquadro_pond_boundary, aquadro_knot_points, aquadro_gam_bound)
        SOAP_seddepth_applegate_FIT <- SOAP_seddepth_model_FUNC(applegate_pond_full, applegate_pond_boundary, applegate_knot_points, applegate_gam_bound)
        
   # 9.7 Use models to predict sediment and water depth 
        # 9.7.a  Use bathymetry GAM model to predict water depth for each pond 
        harrison_grid$SOAP_water_depth <- predict.gam(SOAP_bathym_harrison_FIT, newdata = harrison_grid, type = "response")
        aquadro_grid$SOAP_water_depth <- predict.gam(SOAP_bathym_aquadro_FIT, newdata = aquadro_grid, type = "response")
        applegate_grid$SOAP_water_depth <- predict.gam(SOAP_bathym_applegate_FIT, newdata = applegate_grid, type = "response")
        
        # 9.7.b USe sediment thickness GAM Model to predict sedimemt depth for each pond
        harrison_grid$SOAP_sed_depth <- predict.gam(SOAP_seddepth_harrison_FIT, newdata = harrison_grid, type = "response")
        aquadro_grid$SOAP_sed_depth <- predict.gam(SOAP_seddepth_aquadro_FIT, newdata = aquadro_grid, type = "response")
        applegate_grid$SOAP_sed_depth <- predict.gam(SOAP_seddepth_applegate_FIT, newdata = applegate_grid, type = "response")
        
    
   
#_______________________________________________________________________________
# 10. Compute Volume of water and volume of sediment 
   
   # 10.1 Compute Sediment Volume 
       # Write a function to calculate sediment volume for each pond 
       sed_vol_calc_FUNC <- function(pond_boundary, pond_grid){
         
         pond_name <- as.character(pond_grid[1, "Pond_Name"])  # save pond name to use in the title of the plot 
         pond_name_form <- pond_name[1] # format pond name 
         
         pond_boundary_area <- as.numeric(st_area(pond_boundary)) 
         
         output <- pond_grid %>% 
           st_set_geometry(NULL) %>% 
           summarise(
             Pond_Name = pond_name_form,
             TIN_mean_sed_depth = mean(TIN_sed_depth),
             TIN_sed_volume = mean(TIN_sed_depth) * pond_boundary_area,
             IDW_mean_sed_depth = mean(IDW_sed_depth),
             IDW_sed_volume = mean(IDW_sed_depth) * pond_boundary_area,
             TPRS_mean_sed_depth = mean(TPRS_sed_depth),
             TPRS_sed_volume = mean(TPRS_sed_depth) * pond_boundary_area, 
             SOAP_mean_sed_depth = mean(SOAP_sed_depth),
             SOAP_sed_volume = mean(SOAP_sed_depth) * pond_boundary_area, 
           )
       }
       
       # Apply Function to calcualte sediment volume for each pond 
       harrison_sed_vol <- sed_vol_calc_FUNC(harrison_pond_boundary, harrison_grid)
       applegate_sed_vol <- sed_vol_calc_FUNC(applegate_pond_boundary, applegate_grid)
       aquadro_sed_vol <- sed_vol_calc_FUNC(aquadro_pond_boundary, aquadro_grid)
       harrison_sed_vol 
       aquadro_sed_vol
       applegate_sed_vol
       
       # Create dataframe with the estimated sediment volume for all ponds for all models 
       all_sed_vols <- rbind(harrison_sed_vol, aquadro_sed_vol, applegate_sed_vol)
       
  # 10.2 Water Volume 
       # Write a function to calculate sediment volume for each pond 
       water_vol_calc_FUNC <- function(pond_boundary, pond_grid){
         
         pond_name <- as.character(pond_grid[1, "Pond_Name"])  # save pond name to use in the title of the plot 
         pond_name_form <- pond_name[1] # format pond name 
         
         pond_boundary_area <- as.numeric(st_area(pond_boundary)) 
         
         output <- pond_grid %>% 
           st_set_geometry(NULL) %>% 
           summarise(
             Pond_Name = pond_name_form,
             TIN_mean_water_depth = mean(TIN_water_depth),
             TIN_water_volume = mean(TIN_water_depth) * pond_boundary_area,
             IDW_mean_water_depth = mean(IDW_water_depth),
             IDW_water_volume = mean(IDW_water_depth) * pond_boundary_area,
             TPRS_mean_water_depth = mean(TPRS_water_depth),
             TPRS_water_volume = mean(TPRS_water_depth) * pond_boundary_area,
             SOAP_mean_water_depth = mean(SOAP_water_depth),
             SOAP_water_volume = mean(SOAP_water_depth) * pond_boundary_area,
           )
       }
       
       # Apply Function to calcualte sediment volume for each pond 
       harrison_water_vol <- water_vol_calc_FUNC(harrison_pond_boundary, harrison_grid)
       harrison_water_vol

#_______________________________________________________________________________       
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
   
#_______________________________________________________________________________ 
# 12. Plotting 
   
   # 12.1 Write a Function to plot Sediment depth -- TIN 
       Plot_sedmap_TIN_FUNC <- function(name_grid, name_boundary, pond_depths){
         pond_name <- as.character(name_grid[1, "Pond_Name"])  # save pond name to use in the title of the plot 
         pond_name_form <- pond_name[1] # format pond name 
         output_plot <- ggplot(name_grid) +
           geom_sf(data = name_boundary) +
           geom_raster(aes(X, Y, fill = TIN_sed_depth)) +
           scale_fill_viridis_c() +
           geom_sf_text(aes(label = Sed_Thickness_m), data = pond_depths, size = 3) + 
           annotation_scale(location = "br") +
           labs(title= paste(pond_name, "Sediment Depth (m) -- TIN", sep = " "), x = NULL, y = NULL, fill = "Sediment Depth (m)")
       }
   
   # 12.2 Write a Function to plot Sediment depth -- IDW 
       Plot_sedmap_IDW_FUNC <- function(name_grid, name_boundary, pond_depths){
         pond_name <- as.character(name_grid[1, "Pond_Name"])  # save pond name to use in the title of the plot 
         pond_name_form <- pond_name[1] # format pond name 
         output_plot <- ggplot(name_grid) +
           geom_sf(data = name_boundary) +
           geom_raster(aes(X, Y, fill = IDW_sed_depth)) +
           scale_fill_viridis_c() +
           geom_sf_text(aes(label = Sed_Thickness_m), data = pond_depths, size = 3) + 
           annotation_scale(location = "br") +
           labs(title= paste(pond_name, "Sediment Depth (m) -- IDW", sep = " "), x = NULL, y = NULL, fill = "Sediment Depth (m)")
       }
   
   # 12.3 Write a Function to plot Sediment depth -- TPRS 
       Plot_sedmap_TPRS_FUNC <- function(name_grid, name_boundary, pond_depths){
         pond_name <- as.character(name_grid[1, "Pond_Name"])  # save pond name to use in the title of the plot 
         pond_name_form <- pond_name[1] # format pond name 
         output_plot <- ggplot(name_grid) +
           geom_sf(data = name_boundary) +
           geom_raster(aes(X, Y, fill = TPRS_sed_depth)) +
           scale_fill_viridis_c(option = "D") +
           geom_sf_text(aes(label = Sed_Thickness_m), data = pond_depths, size = 3) + 
           annotation_scale(location = "br") +
           labs(title= paste(pond_name, "Sediment Depth (m) -- TPRS", sep = " "), x = NULL, y = NULL, fill = "Sediment Depth (m)")
       }
   
   # 12.4 Write a Function to plot Sediment depth -- SOAP 
       Plot_sedmap_SOAP_FUNC <- function(name_grid, name_boundary, pond_depths){
         pond_name <- as.character(name_grid[1, "Pond_Name"])  # save pond name to use in the title of the plot 
         pond_name_form <- pond_name[1] # format pond name 
         output_plot <- ggplot(name_grid) +
           geom_sf(data = name_boundary) +
           geom_raster(aes(X, Y, fill = SOAP_sed_depth)) +
           scale_fill_viridis_c(option = "D") +
           geom_sf_text(aes(label = Sed_Thickness_m), data = pond_depths, size = 3) + 
           annotation_scale(location = "br") +
           labs(title= paste(pond_name, "Sediment Depth (m) -- SOAP", sep = " "), x = NULL, y = NULL, fill = "Sediment Depth (m)")
       }
       
           
   
    # 12.5 Plot each model for each pond 
       
       # Applegate 
        plot_sedmap_TIN_applegate <- Plot_sedmap_TIN_FUNC(applegate_grid, applegate_pond_boundary, applegate_pond_depths)
        plot_sedmap_IDW_applegate <- Plot_sedmap_IDW_FUNC(applegate_grid, applegate_pond_boundary, applegate_pond_depths)
        plot_sedmap_TPRS_applegate <- Plot_sedmap_TPRS_FUNC(applegate_grid, applegate_pond_boundary, applegate_pond_depths)
        plot_sedmap_SOAP_applegate <- Plot_sedmap_SOAP_FUNC(applegate_grid, applegate_pond_boundary, applegate_pond_depths)
            plot_sedmap_TIN_applegate 
            plot_sedmap_IDW_applegate
            plot_sedmap_TPRS_applegate
            plot_sedmap_SOAP_applegate
     
        # Aquadro 
        plot_sedmap_TIN_aquadro <- Plot_sedmap_TIN_FUNC(aquadro_grid, aquadro_pond_boundary, aquadro_pond_depths) 
        plot_sedmap_IDW_aquadro <- Plot_sedmap_IDW_FUNC(aquadro_grid, aquadro_pond_boundary, aquadro_pond_depths)
        plot_sedmap_TPRS_aquadro <- Plot_sedmap_TPRS_FUNC(aquadro_grid, aquadro_pond_boundary, aquadro_pond_depths)
        plot_sedmap_SOAP_aquadro <- Plot_sedmap_SOAP_FUNC(aquadro_grid, aquadro_pond_boundary, aquadro_pond_depths)
             plot_sedmap_TIN_aquadro
             plot_sedmap_IDW_aquadro
             plot_sedmap_TPRS_aquadro
             plot_sedmap_SOAP_aquadro
     
             
        # Harrison      
        plot_sedmap_TIN_harrison <- Plot_sedmap_TIN_FUNC(harrison_grid, harrison_pond_boundary, harrison_pond_depths)
        plot_sedmap_IDW_harrison <- Plot_sedmap_IDW_FUNC(harrison_grid, harrison_pond_boundary, harrison_pond_depths)
        plot_sedmap_TPRS_harrison <- Plot_sedmap_TPRS_FUNC(harrison_grid, harrison_pond_boundary, harrison_pond_depths)
        plot_sedmap_SOAP_harrison <- Plot_sedmap_SOAP_FUNC(harrison_grid, harrison_pond_boundary, harrison_pond_depths)
              plot_sedmap_TIN_harrison
              plot_sedmap_IDW_harrison
              plot_sedmap_TPRS_harrison
              plot_sedmap_SOAP_harrison
     
    # 12. Save maps for each pond 
      # Applegate 
         ggsave("Output_Figures/plot_sedmap_TIN_applegate_230830.png", plot_sedmap_TIN_applegate)
         ggsave("Output_Figures/plot_sedmap_IDW_applegate_230828.png", plot_sedmap_IDW_applegate)
         ggsave("Output_Figures/plot_sedmap_TRPS_applegate_230828.png", plot_sedmap_TPRS_applegate)
        
      # Aquadro 
         ggsave("Output_Figures/plot_sedmap_TIN_aquadro_230830.png", plot_sedmap_TIN_aquadro)
         ggsave("Output_Figures/plot_sedmap_IDW_aquadro_230828.png", plot_sedmap_IDW_aquadro)
         ggsave("Output_Figures/plot_sedmap_TRPS_aquadro_230828.png", plot_sedmap_TPRS_aquadro)
         
      # Harrison 
         ggsave("Output_Figures/plot_sedmap_TIN_harrison_230830.png", plot_sedmap_TIN_harrison)
         ggsave("Output_Figures/plot_sedmap_IDW_harrison_230828.png", plot_sedmap_IDW_harrison)
         ggsave("Output_Figures/plot_sedmap_TRPS_harrison_230828.png", plot_sedmap_TPRS_harrison)
         
     
     
   
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
  ########################################################################################################### 
  # OLD CODE 9/29/23
  ###########################################################################################################
     
         
         
         
         
         
  # ___________________________________________________________________       
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
  # SOAP 9/29/23 
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
     # Working SOAP Code
     boundary_coords <- st_coordinates( harrison_pond_boundary) 
     boundary_linestring <- harrison_pond_boundary %>% st_cast("LINESTRING") %>% st_buffer(10)  # Need to make line string one at a time up here, it makes it but will throw an error 
     
     # make gam_bound 
     gam_bound <- list(
       list(
         X = boundary_coords[-1, "X"], 
         Y = boundary_coords[-1, "Y"], 
         f = rep(0, nrow(boundary_coords))
       )
     )
     
     # Make knot points  
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
     
     # Make Intersection  
     intersection <- !st_intersects(
       boundary_linestring, 
       knot_points_1, 
       sparse = FALSE
     )
     
     #Filter knot points by intersection    
     knot_points <- knot_points_1 %>%   
       filter(as.vector(intersection)) %>%
       cbind(., st_coordinates(.))
     
     
     # Fit gam soap 
     SOAP_bathym_harrison_FIT <- gam(
       Water_Depth_m ~ s(X, Y, bs = "so", xt = list(bnd = gam_bound)),
       data = harrison_pond_full %>% 
         filter(source == "measured") %>% 
         filter(as.vector(st_contains(harrison_pond_boundary, geometry, sparse = FALSE))), 
       method = "REML", 
       knots = knot_points
     )
     
     SOAP_sedmap_harrison_FIT <- gam(
       Sed_Thickness_m ~ s(X, Y, bs = "so", xt = list(bnd = gam_bound)),
       data = harrison_pond_full %>% 
         filter(source == "measured") %>% 
         filter(as.vector(st_contains(harrison_pond_boundary, geometry, sparse = FALSE))), 
       method = "REML", 
       knots = knot_points
     )
     
     
     harrison_grid$SOAP_water_depth <- predict.gam(SOAP_bathym_harrison_FIT, newdata = harrison_grid, type = "response")
     harrison_grid$SOAP_sed_depth <- predict.gam(SOAP_sedmap_harrison_FIT, newdata = harrison_grid, type = "response")
     
     
     
     
     
     
     
     
     
     
     
     
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
     labs(title= "Ellens Pond Sediment Depth (m) -- TPRS", x = NULL, y = NULL, fill = "Sediment Depth (m)") ellens_sed_depth_TPRS
    
# Save Output figures 
    getwd()
    ggsave("Output_Figures/ellens_water_depth_IDW_0214.png", ellens_water_depth_IDW)
    ggsave("Output_Figures/ellens_sed_depth_IDW_0214.png", ellens_sed_depth_IDW)
    ggsave("Output_Figures/ellens_water_depth_TPRS_0214.png", ellens_water_depth_TPRS)
    ggsave("Output_Figures/ellens_sed_depth_TPRS_0214.png", ellens_sed_depth_TPRS)
    
# Averages 
    mean(pond_depths$sed_depth)
    
