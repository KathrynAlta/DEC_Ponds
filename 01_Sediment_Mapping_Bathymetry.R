############################################################
# MAPPING SEDIMENT DEPTHS 
############################################################
# Holgerson Lab, Katie Gannon
# Code modified from https://dewey.dunnington.ca/post/2019/bathymetry-lake-volume-estimation-using-r/ 

# BAB

# 0. Set up R environment and Load Data 

    #Set working directory: 
   setwd("~/DEC_Ponds") 
    #setwd("~/OneDrive/Holgerson_Lab/DEC_Ponds") # Mac 
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
    library(ggplot2)
    
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
    # sediment_depths <- read_xlsx("~/OneDrive/Holgerson_Lab/DEC_Ponds/Input_Files/DEC_Ponds_Sediment_Mapping_Depths.xlsx")
    sediment_depths <- read_xlsx("Input_Files/DEC_Ponds_Sediment_Mapping_Depths.xlsx")
    # Formatt 
      names(sediment_depths)[names(sediment_depths) == "Pond Name"] <- "Pond"
      names(sediment_depths)[names(sediment_depths) == "Site Number"] <- "Measurement_Number"
      names(sediment_depths)[names(sediment_depths) == "Depth of top of sediments (cm)"] <- "Depth_to_top_of_Sediment_cm"
      names(sediment_depths)[names(sediment_depths) == "Depth of bottom of sediments (cm)"] <- "Depth_to_btm_of_sediment_cm"
      sediment_depths$Measurement_Number <- as.numeric(sediment_depths$Measurement_Number)
      sediment_depths$Depth_to_top_of_Sediment_cm <- as.numeric(sediment_depths$Depth_to_top_of_Sediment_cm)
      sediment_depths$Depth_to_btm_of_sediment_cm <- as.numeric(sediment_depths$Depth_to_btm_of_sediment_cm)
      sediment_depths <- subset(sediment_depths, select = c( "Pond", "Measurement_Number", "Depth_to_top_of_Sediment_cm", "Depth_to_btm_of_sediment_cm"))
      
      # Individual depth files 
         # harrison_meas_depths <- read_xlsx("Depth_Measurements/Harrison_Pond_Depth_Measurements.xlsx")
         # applegate_meas_depths <- read_xlsx("Depth_Measurements/Applegate_Pond_Depth_Measurements.xlsx")
         # aquadro_meas_depths <- read_xlsx("Depth_Measurements/Aquadro_Pond_Depth_Measurements.xlsx")
      
    # Input Spatial Files on Desktop 
      #####    
  # Points data 
    # Intensive Desktop 
    boyce_points <- read_sf("/Users/kag326/Documents/ArcGIS/Projects/DEC_Farm_Residential_Ponds/Output_Shape_Files_Peri/Boyce_Points.shp")   %>%   # Pull in shape file
      transmute(source = "measured", pond = "Boyce") %>% # Subset to only columns that you need 
      st_transform(26920) # Transform or convert coordinates of simple feature 
    
    white_points <- read_sf("/Users/kag326/Documents/ArcGIS/Projects/DEC_Farm_Residential_Ponds/Output_Shape_Files_Peri/White_Points.shp")   %>%   # Pull in shape file
      transmute(source = "measured", pond = "White") %>% # Subset to only columns that you need 
      st_transform(26920) # Transform or convert coordinates of simple feature 
    
    howarth_points <- read_sf("/Users/kag326/Documents/ArcGIS/Projects/DEC_Farm_Residential_Ponds/Output_Shape_Files_Peri/Howarth_Points.shp")   %>%   # Pull in shape file
      transmute(source = "measured", pond = "Howarth") %>% # Subset to only columns that you need 
      st_transform(26920) # Transform or convert coordinates of simple feature 
    
    edwards_points <- read_sf("/Users/kag326/Documents/ArcGIS/Projects/DEC_Farm_Residential_Ponds/Output_Shape_Files_Peri/Edwards_Points.shp")   %>%   # Pull in shape file
      transmute(source = "measured", pond = "Edwards") %>% # Subset to only columns that you need 
      st_transform(26920) # Transform or convert coordinates of simple feature 
    
    shelterbelt_points <- read_sf("/Users/kag326/Documents/ArcGIS/Projects/DEC_Farm_Residential_Ponds/Output_Shape_Files_Peri/Shelterbelt_Points.shp")   %>%   # Pull in shape file
      transmute(source = "measured", pond = "Shelterbelt") %>% # Subset to only columns that you need 
      st_transform(26920) # Transform or convert coordinates of simple feature 
    
    mtpleasantse_points <- read_sf("/Users/kag326/Documents/ArcGIS/Projects/DEC_Farm_Residential_Ponds/Output_Shape_Files_Peri/Mt_Pleasant_SE_Points.shp")   %>%   # Pull in shape file
      transmute(source = "measured", pond = "MtPleasantSE") %>% # Subset to only columns that you need 
      st_transform(26920) # Transform or convert coordinates of simple feature 
    
    levine_points <- read_sf("/Users/kag326/Documents/ArcGIS/Projects/DEC_Farm_Residential_Ponds/Output_Shape_Files_Peri/Levine_Points.shp")   %>%   # Pull in shape file
      transmute(source = "measured", pond = "Levine") %>% # Subset to only columns that you need 
      st_transform(26920) # Transform or convert coordinates of simple feature 
    
    harrison_points <- read_sf("Spatial_Data_SedimentMapping/Harrison_Pond_030123/Harrison_Pond_Points_030123_1734.shp")   %>%   # Pull in shape file
      transmute(source = "measured", pond = "Harrison") %>% # Subset to only columns that you need 
      st_transform(26920) # Transform or convert coordinates of simple feature 
    
    # Extensive 
    applegate_points <- read_sf("Spatial_Data_SedimentMapping/Applegate_Pond_022723/Applegate_Pond_Points_030123_noz.shp")   %>%   # Pull in shape file
      transmute(source = "measured", pond = "Applegate") %>% # Subset to only columns that you need 
      st_transform(26920) # Transform or convert coordinates of simple feature 
    
    aquadro_points <- read_sf("Spatial_Data_SedimentMapping/Aquadro_Pond_022723/Aquadro_Pond_Points_030123_noz.shp")   %>%   # Pull in shape file
      transmute(source = "measured", pond = "Aquadro") %>% # Subset to only columns that you need 
      st_transform(26920) # Transform or convert coordinates of simple feature 
    
   
    
  # Polygons
    
    #Intensive 
    boyce_polygon <- read_sf("/Users/kag326/Documents/ArcGIS/Projects/DEC_Farm_Residential_Ponds/Output_Shape_Files/Boyce_Polygon.shp") %>%  # read in polygon of pond 
      transmute(source = "boundary", depth = 0) %>%  #Saying that the depth at the edge of the pond is zero 
      st_transform(26920) %>%
      st_zm()
    
    white_polygon <- read_sf("/Users/kag326/Documents/ArcGIS/Projects/DEC_Farm_Residential_Ponds/Output_Shape_Files/White_Polygon.shp") %>%  # read in polygon of pond 
      transmute(source = "boundary", depth = 0) %>%  #Saying that the depth at the edge of the pond is zero 
      st_transform(26920) %>%
      st_zm()
    
    howarth_polygon <- read_sf("/Users/kag326/Documents/ArcGIS/Projects/DEC_Farm_Residential_Ponds/Output_Shape_Files/Howarth_Polygon.shp") %>%  # read in polygon of pond 
      transmute(source = "boundary", depth = 0) %>%  #Saying that the depth at the edge of the pond is zero 
      st_transform(26920) %>%
      st_zm()
    
    edwards_polygon <- read_sf("/Users/kag326/Documents/ArcGIS/Projects/DEC_Farm_Residential_Ponds/Output_Shape_Files/Edwards_Polygon.shp") %>%  # read in polygon of pond 
      transmute(source = "boundary", depth = 0) %>%  #Saying that the depth at the edge of the pond is zero 
      st_transform(26920) %>%
      st_zm()
    
    shelterbelt_polygon <- read_sf("/Users/kag326/Documents/ArcGIS/Projects/DEC_Farm_Residential_Ponds/Output_Shape_Files/Shelterbelt_Polygon.shp") %>%  # read in polygon of pond 
      transmute(source = "boundary", depth = 0) %>%  #Saying that the depth at the edge of the pond is zero 
      st_transform(26920) %>%
      st_zm()
    
    mtpleasantse_polygon <- read_sf("/Users/kag326/Documents/ArcGIS/Projects/DEC_Farm_Residential_Ponds/Output_Shape_Files/Mt_Pleasant_SE_Polygon.shp") %>%  # read in polygon of pond 
      transmute(source = "boundary", depth = 0) %>%  #Saying that the depth at the edge of the pond is zero 
      st_transform(26920) %>%
      st_zm()
    
    harrison_polygon <- read_sf("Spatial_Data_SedimentMapping/Harrison_Pond_030123/Harrison_Pond_Poly_030123.shp") %>%  # read in polygon of pond 
      transmute(source = "boundary", depth = 0) %>%  #Saying that the depth at the edge of the pond is zero 
      st_transform(26920) %>%
      st_zm()
    
    levine_polygon <- read_sf("/Users/kag326/Documents/ArcGIS/Projects/DEC_Farm_Residential_Ponds/Output_Shape_Files/Levine_Polygon.shp") %>%  # read in polygon of pond 
      transmute(source = "boundary", depth = 0) %>%  #Saying that the depth at the edge of the pond is zero 
      st_transform(26920) %>%
      st_zm()
    
    # Extensive 
    applegate_polygon <- read_sf("Spatial_Data_SedimentMapping/Applegate_Pond_022723/Applegate_Pond_Poly.shp") %>%  # read in polygon of pond 
      transmute(source = "boundary", depth = 0) %>%  #Saying that the depth at the edge of the pond is zero 
      st_transform(26920) %>%
      st_zm()
    
    aquadro_polygon <- read_sf("Spatial_Data_SedimentMapping/Aquadro_Pond_022723/Aquadro_Pond_Poly_030123.shp") %>%  # read in polygon of pond 
      transmute(source = "boundary", depth = 0) %>%  #Saying that the depth at the edge of the pond is zero 
      st_transform(26920) %>%
      st_zm()
    
    
  
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
   
    
    ## Put all of the points shape df into a list 
    # pond_points_list <- list(boyce_points, white_points, howarth_points, edwards_points,
    #                        shelterbelt_points, mtpleasantse_points, harrison_points, levine_points, 
    #                         aquadro_points, longhouse_points, ecovillage_points, dybowski_points, 
    #                         applegate_points, mtpleasantne_points, barber_points, stickandstone_points,
    #                        englishshallow_points, engst_points, rogers_points, carpenter_points, 
    #                         walnutridge_points, lucas_points, collmer_points, vesa_points, 
    #                         conley_points, hahn_points, marks_points, englishdeep_points)
    
    
    
    # names(pond_points_list ) <- c("Boyce", "White", "Howarth", "Edwards",
    #                             "Shelterbelt", "Mt_Pleasant_SE", "Harrison", "Levine", 
    #                              "Aquadro", "Longhouse", "Ecovillage", "Dybowski", 
    #                              "Applegate", "Mt_Pleasant_NE", "Barber", "Stick_and_Stone",
    #                              "English_Shallow", "Engst", "Rodgers", "Carpenter", 
    #                              "Walnut_Ridge", "Lucas", "Collmer", "Vesa", 
    #                              "Conley", "Hahn", "Marks", "English_Deep")
    
    # Remove the Z component 
   #  pond_points_list <- mapply(st_zm, pond_points_list, drop = TRUE, what = "ZM", USE.NAMES = TRUE, SIMPLIFY = FALSE)
    
    
    # Intensive Only 
        pond_points_list <- list(boyce_points, white_points, howarth_points, edwards_points,
                                 shelterbelt_points, mtpleasantse_points, harrison_points, levine_points, 
                                 aquadro_points, applegate_points)
        
        names(pond_points_list ) <- c("Boyce", "White", "Howarth", "Edwards",
                                      "Shelterbelt", "Mt_Pleasant_SE", "Harrison", "Levine", 
                                      "Aquadro", "Applegate")
        # Remove the Z component 
        pond_points_list <- mapply(st_zm, pond_points_list, drop = TRUE, what = "ZM", USE.NAMES = TRUE, SIMPLIFY = FALSE)
    
    # Put all of the polygon shape df into a list 
    
        # Practice Subset 
         #  pond_polygon_list <- list(applegate_polygon, aquadro_polygon, harrison_polygon)
         #  names(pond_polygon_list) <- c("Applegate", "Aquadro", "Harrison")
    
          
   #  pond_polygon_list <- list(boyce_polygon, white_polygon, howarth_polygon, edwards_polygon,
    #                             shelterbelt_polygon, mtpleasantse_polygon, harrison_polygon, levine_polygon, 
    #                            aquadro_polygon, longhouse_polygon, ecovillage_polygon, dybowski_polygon, 
    #                            applegate_polygon, mtpleasantne_polygon, barber_polygon, stickandstone_polygon,
    #                            englishshallow_polygon, engst_polygon, rogers_polygon, carpenter_polygon, 
    #                            walnutridge_polygon, lucas_polygon, collmer_polygon, vesa_polygon, 
    #                            conley_polygon, hahn_polygon, marks_polygon, englishdeep_polygon)
    
    # names(pond_polygon_list ) <- c("Boyce", "White", "Howarth", "Edwards",
    #                            "Shelterbelt", "Mt_Pleasant_SE", "Harrison", "Levine", 
    #                             "Aquadro", "Longhouse", "Ecovillage", "Dybowski", 
    #                            "Applegate", "Mt_Pleasant_NE", "Barber", "Stick_and_Stone",
    #                            "English_Shallow", "Engst", "Rodgers", "Carpenter", 
    #                             "Walnut_Ridge", "Lucas", "Collmer", "Vesa", 
    #                             "Conley", "Hahn", "Marks", "English_Deep")
    
        # Intensive Only 
        pond_polygon_list <- list(boyce_polygon, white_polygon, howarth_polygon, edwards_polygon,
                                  shelterbelt_polygon, mtpleasantse_polygon, harrison_polygon, levine_polygon, 
                                  aquadro_polygon, applegate_polygon)
        
        names(pond_polygon_list ) <- c("Boyce", "White", "Howarth", "Edwards",
                                       "Shelterbelt", "Mt_Pleasant_SE", "Harrison", "Levine", 
                                       "Aquadro", "Applegate")
      
    
   
# 2. Connect the lat long from the pond depths shape file to the measuremed depths from seperate df --> make spatial
#_______________________________________________________________________________    
    
    # 2.1 Formatt the Sediment Depth Data 
      
        # Remove ponds that you have sediment data for but that we did not include in study 
        sediment_depths <- sediment_depths[!sediment_depths$Pond %in% c("Artibee", "Bensons", "Thru_the_Woods", "Whitmore" ), ]
        
        # Subset to only intensive 
        sediment_depths <- sediment_depths[sediment_depths$Pond %in% c("Boyce", "White", "Howarth", "Edwards", "Shelterbelt", "Mt_Pleasant_SE", "Harrison", "Levine", "Aquadro", "Applegate") , ]
        sediment_depths$Pond <- as.character(sediment_depths$Pond)
        
        # Seperate Sediment depth data out by pond 
        meas_depths_list <-split(sediment_depths, sediment_depths$Pond)  
          # this seperates the one big df of all the measured depths into a list with a seperate df for each pond 
        
        # Order the dfs in the meas deths list so that the ponds are in the same order as in the other lists 
        # meas_depths_list <- meas_depths_list[c("Boyce", "White", "Howarth", "Edwards",
          #                                             "Shelterbelt", "Mt_Pleasant_SE", "Harrison", "Levine", 
          #                                             "Aquadro", "Longhouse", "Ecovillage", "Dybowski", 
          #                                             "Applegate", "Mt_Pleasant_NE", "Barber", "Stick_and_Stone",
          #                                            "English_Shallow", "Engst", "Rodgers", "Carpenter", 
          #                                             "Walnut_Ridge", "Lucas", "Collmer", "Vesa", 
          #                                             "Conley", "Hahn", "Marks", "English_Deep")]
        
        # Intensive Only 
        meas_depths_list <- meas_depths_list[c("Boyce", "White", "Howarth", "Edwards",
                                                       "Shelterbelt", "Mt_Pleasant_SE", "Harrison", "Levine", 
                                                       "Aquadro","Applegate")]
    
      # Practice subset 
        # applegate_meas_depths <- subset(sediment_depths, sediment_depths$Pond == "Applegate")
        # aquadro_meas_depths <- subset(sediment_depths, sediment_depths$Pond == "Aquadro")
        # harrison_meas_depths <- subset(sediment_depths, sediment_depths$Pond == "Harrison")
        
        # meas_depths_list <- list(applegate_meas_depths, aquadro_meas_depths, harrison_meas_depths)
    
        # names(meas_depths_list) <- c("Applegate", "Aquadro", "Harrison")
        
    # Intensive by hand 
        # boyce_meas_depths <- subset(sediment_depths, sediment_depths$Pond == "Boyce")
        # white_meas_depths <- subset(sediment_depths, sediment_depths$Pond == "White")
        # howarth_meas_depths <- subset(sediment_depths, sediment_depths$Pond == "Howarth")
        # edwards_meas_depths <- subset(sediment_depths, sediment_depths$Pond == "Edwards")
        # shelterbelt_meas_depths <- subset(sediment_depths, sediment_depths$Pond == "Shelterbelt")
        # mtpleasantse_meas_depths <- subset(sediment_depths, sediment_depths$Pond == "Mt_Pleasant_SE")
        # harrison_meas_depths <- subset(sediment_depths, sediment_depths$Pond == "Harrison")
        # levine_meas_depths <- subset(sediment_depths, sediment_depths$Pond == "Levine")
        # applegate_meas_depths <- subset(sediment_depths, sediment_depths$Pond == "Applegate")
        # aquadro_meas_depths <- subset(sediment_depths, sediment_depths$Pond == "Aquadro")
        
  # 2.2) Write a function to connect the measured depths to the lat long in the shape file 
        
            # Dummy data to write function 
            # pond_points <- aquadro_points
            # measured_depths <- aquadro_meas_depths
            
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
          output <- pond_depths
        }
        
   # 2.3) Apply function over the points shape file and measured depths for each pond 
    
    # Applying function individually for each pond 
      # applegate_depths_latlong <- Connect_Depth_LatLong_FUNC(applegate_points, applegate_meas_depths)
      # aquadro_depths_latlong <- Connect_Depth_LatLong_FUNC(aquadro_points, aquadro_meas_depths)
      # harrison_depths_latlong <- Connect_Depth_LatLong_FUNC(harrison_points, harrison_meas_depths)
      
    # Applying function over all ponds using mapply()
    
      # Make a list of points dfs and a list of depths dfs for the subset of ponds you are workign with 
      # points_shp_list <- list(applegate_points, aquadro_points, harrison_points)
      
      # Make a list of points dfs and a list of depths dfs for the subset of ponds you are workign with 
      # pond_polygon_list <- list(applegate_polygon, aquadro_polygon, harrison_polygon)
      
      # Run function over the two lists 
      meas_depths_latlong_list <- mapply(Connect_Depth_LatLong_FUNC, pond_points_list, meas_depths_list, USE.NAMES = TRUE, SIMPLIFY = FALSE)
      # Output is a list of dataframes with a df for each pond (I am a genius)
      
    ## * see if this works feeding into next steps 
    
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
     # Plot_Water_Depths_FUNC(aquadro_polygon, aquadro_depths_latlong)
     # Plot_Sed_Thick_FUNC(aquadro_polygon, aquadro_depths_latlong)
     
    # Apply the plotting function over lists so that you don't have to write it out every time 
     par(ask = TRUE)  #Setting par$ask equal to TRUE allows you to flip through all of the plots one at a a time 
     mapply(Plot_Sed_Thick_FUNC, pond_polygon_list, meas_depths_latlong_list, USE.NAMES = TRUE, SIMPLIFY = FALSE)
     par(ask = FALSE)
   
# 4. Add the coordinates of the boundary as columns and set the depth at the boundary to zero 
#_______________________________________________________________________________    
  # 4.1 ) For all ponds cast geometry to another type, change from polygon to points 
    
         # Example list 
         # applegate_polygon_cast <- st_cast(applegate_polygon, "POINT")
         # aquadro_polygon_cast <- st_cast(aquadro_polygon, "POINT") 
         # harrison_polygon_cast <- st_cast(harrison_polygon, "POINT")
         
         # cast_boundaries_list <- list(applegate_polygon_cast, aquadro_polygon_cast, harrison_polygon_cast)
         
     # Write a Function to cast all of the polygons 
     CastPolygon_FUNC <- function(pond_polygon){
       output <- st_cast(pond_polygon, "POINT")
     }
     
     # Apply cast function across all ponds (this works, it will just print a warning for each pond)
     cast_boundaries_list <- mapply(CastPolygon_FUNC, pond_polygon_list, USE.NAMES = TRUE ,SIMPLIFY = FALSE)
         
  # 4.2) Write Function to add boundary coordinates to the points 
     
         # Dummy data to write function 
           # pond_boundary_points <- applegate_polygon_cast  # this is just a long list of points on the boundary with depth set to zero 
           # pond_depths <- applegate_depths_latlong  # this is all of the points where we have measured water and sediment depths 
           
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
       # applegate_full <- Coord_Bound_FUNC(applegate_polygon_cast, applegate_depths_latlong)
      #  aquadro_full <- Coord_Bound_FUNC(aquadro_polygon_cast, aquadro_depths_latlong)
       # harrison_full <- Coord_Bound_FUNC(harrison_polygon_cast, harrison_depths_latlong)
       
     # Running the function across multiple ponds using mapply 
    #  cast_boundaries_list <- list(applegate_polygon_cast, aquadro_polygon_cast, harrison_polygon_cast)
     # meas_depths_latlong_list <- list(applegate_depths_latlong, aquadro_depths_latlong, harrison_depths_latlong)
   
     # Run function over the two lists 
     pond_full_list <- mapply(Coord_Bound_FUNC, cast_boundaries_list, meas_depths_latlong_list, USE.NAMES = TRUE, SIMPLIFY = FALSE)
   
     
# 5. Create a grid to hold the raster output
#_______________________________________________________________________________  
   # Dummy data to write function 
       # thing1 <- as.character(aquadro_full[1, "Pond_Name"])
       # thing1[1]
       
       # name_pond_full <- harrison_full
       # name_pond_boundary <- harrison_polygon
       
       # harrison_full$Pond_Name

   # Write a function to create a grid 
   GridCreate_FUNC <- function(name_pond_full, name_pond_boundary){
     pond_name <- as.character(name_pond_full[1,"Pond_Name"])
     pond_name_form <- pond_name[1]
     pond_grid_step1 <- st_make_grid(name_pond_full, cellsize = c(2, 2), what = "centers")   # as of 10/24 this was set to a cellsixe of c(2,2) I changed that from code from onine to make more points 
     pond_grid_step2 <- st_as_sf(pond_grid_step1)
     pond_grid_step3 <- st_contains(name_pond_boundary, pond_grid_step2, sparse = FALSE)
     pond_grid_step4 <- pond_grid_step2[pond_grid_step3 == "TRUE" , ]
     pond_grid_step5 <- cbind(pond_grid_step4, st_coordinates(pond_grid_step4))
     pond_grid_step5$Pond_Name <- pond_name_form
     pond_grid <- pond_grid_step5
   }
   
       # Run the Function for each pond to check 
           # str(applegate_full)
           # applegate_full$Pond_Name <- as.character(applegate_full$Pond_Name)
           # aquadro_full$Pond_Name <- as.character(aquadro_full$Pond_Name)
           # harrison_full$Pond_Name <- as.character(harrison_full$Pond_Name)
       
           # applegate_grid <- GridCreate_FUNC(applegate_full, applegate_polygon)
           # aquadro_grid <- GridCreate_FUNC(aquadro_full, aquadro_polygon)
           # harrison_grid <- GridCreate_FUNC(harrison_full, harrison_polygon)
   
   # Run the function for all ponds using mapply 
       # (make for subset that you are using)
       # pond_polygon_list <- list(applegate_polygon, aquadro_polygon, harrison_polygon)
   
  # Run function over the two lists 
   pond_grid_list <- mapply(GridCreate_FUNC, pond_full_list, pond_polygon_list, USE.NAMES = TRUE, SIMPLIFY = FALSE)
   
   # Up to here is all set up, getting the grid made (place to put output from the model) and processing the input 
   #   data (make spatial, calc depth, add zero depth around boundary) getting ready to make a feed the model 

   # Now there are multiple models that you can build that use different methods (math) to interpolate the water
   #   and sediment depths at each point on the grid. You are going to build and run multiple models and look at the
   #   output, then make a decision about what model to use for your final estimates 
  
# 5.5 Pull out all of the grids and all of the fulls and save them individuallsy 
   
   # harrison_full <- as.data.frame(pond_full_list["Harrison"])
   # harrison_grid <- as.data.frame(pond_grid_list["Harrison"])
    
# 6. Triangular Irregular Network Surface (TIN) 
# _____________________________________________________________________________ 
 ######
     
   # 6.1) Write Function to fit TIN model 
   
     # 6.1.a) Write a function to fit a TIN model to sediment thickness  
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
            # Check that model works for sediment thickness 
                  # harrison_grid$TIN_sed_depth <- TIN_sed_FUNC(harrison_full, harrison_grid)
                  # aquadro_grid$TIN_sed_depth <- TIN_sed_FUNC(aquadro_full, aquadro_grid)
                  # applegate_grid$TIN_sed_depth <- TIN_sed_FUNC(applegate_full, applegate_grid)
          
        
      # 6.1.b) Write a function to fit a TIN model to water depths 
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

            # Check that model works for water depth 
                # harrison_grid$TIN_water_depth <- TIN_bathym_FUNC(harrison_full, harrison_grid)
                # aquadro_grid$TIN_water_depth <- TIN_bathym_FUNC(aquadro_full, aquadro_grid)
                # applegate_grid$TIN_water_depth <- TIN_bathym_FUNC(applegate_full, applegate_grid)
            
            
   # 6.2) Use TIN Model to predict water depth and Sed thickeness and add to grid
        
      # Sediment Thickness -- Apply that TIN function across the list of ponds 
        pond_grid_list_TIN_sed <- pond_grid_list
        TIN_sed_thickness <- mapply(TIN_sed_FUNC, pond_full_list, pond_grid_list, USE.NAMES = TRUE, SIMPLIFY = FALSE)
          # Note that in this configuration you will have one pond_grid_list that you will pass through each model and update it as you go 
  
      # Water Depth 
        pond_grid_list_TIN_water <- pond_grid_list
        TIN_water_depth <- mapply(TIN_bathym_FUNC, pond_full_list, pond_grid_list, USE.NAMES = TRUE, SIMPLIFY = FALSE)
    
       ###### 
    
# 7. Inverse Distance Weighting (IDW) 
# _____________________________________________________________________________  
   
   # 7.1) BATHYMETRY 
   
     # 7.1.1) Build a model for water depth using IDW
        
        # Write a function to build a model for water depth using IDW
         IDW_bathym_FUNC <- function(name_pond_full){
           output <- gstat::gstat(
             formula = Water_Depth_m ~ 1,
             data = as(name_pond_full, "Spatial"),
             nmax = 10, nmin = 3,
             set = list(idp = 0.5)
           )
         }
            # Check that the function works 
               # IDW_bathym_harrison_FIT <- IDW_bathym_FUNC(harrison_full) # Model based on inverse distance weighted (IDW) predicting the bathymetry (water depth) of harrison pond (farm and res pond named by last name of land owner)
               # IDW_bathym_applegate_FIT <- IDW_bathym_FUNC(applegate_full)
               # IDW_bathym_aquadro_FIT <- IDW_bathym_FUNC(aquadro_full)
               
          # Apply the IDW bathym function across all ponds in the list 
               IDW_bathym_FIT_list <- mapply(IDW_bathym_FUNC, pond_full_list, USE.NAMES = TRUE, SIMPLIFY = FALSE)
           
      # 7.1.2 Use the FIT for each pond to predict water depth & adds predicted values to the grid 
        
           #Write a function to predict bathymetry based on FIT -- Edited 10/18 cant't check with out programe 
           IDW_predict_bathym_FUNC <- function(model_FIT, name_grid){
             predicted_formal_class <- predict(model_FIT, newdata = as(name_grid, "Spatial"))
             predicted_sf <- st_as_sf(predicted_formal_class)
             name_grid$IDW_water_depth <- predicted_sf$var1.pred
           }
                # Check that the IDW bathym predict function works 
                  # harrison_grid$IDW_water_depth <- IDW_predict_FUNC(IDW_bathym_harrison_FIT, harrison_grid)
                  # aquadro_grid$IDW_water_depth <- IDW_predict_FUNC(IDW_bathym_aquadro_FIT, aquadro_grid)
                  # applegate_grid$IDW_water_depth <- IDW_predict_FUNC(IDW_bathym_applegate_FIT, applegate_grid)
            
        # Apply the IDW predict bathymetry function across all ponds in the list 
            pond_grid_list_IDW_water <- pond_grid_list
            IDW_water_depth <- mapply(IDW_predict_bathym_FUNC, IDW_bathym_FIT_list, pond_grid_list)
            
        
  # 7.2) SED THICKNESS 
            
      # 7.2.1) Build a model for water depth using IDW
            
          # Write a function to build a model for sed thickness using IDW
            IDW_sed_FUNC <- function(name_pond_full){
              output <- gstat::gstat(
                formula = Sed_Thickness_m ~ 1,
                data = as(name_pond_full, "Spatial"),
                nmax = 10, nmin = 3,
                set = list(idp = 0.5)
              )
            }
            
              # Check that the IDW sediment Function works 
              # IDW_sed_harrison_FIT <- IDW_sed_FUNC(harrison_pond_full) # Model based on inverse distance weighted (IDW) predicting the bathymetry (water depth) of harrison pond (farm and res pond named by last name of land owner)
              # IDW_sed_applegate_FIT <- IDW_sed_FUNC(applegate_pond_full)
              # IDW_sed_aquadro_FIT <- IDW_sed_FUNC(aquadro_pond_full)
        
        # Apply the IDW bathym function across all ponds in the list 
              IDW_seddepth_FIT_list <- mapply(IDW_sed_FUNC, pond_full_list, USE.NAMES = TRUE, SIMPLIFY = FALSE)
         
    # 7.2.2 Use the FIT for each pond to predict water depth & adds predicted values to the grid 
              
          # Write a function to predict sediment depth based on FIT 
              IDW_seddepth_predict_FUNC <- function(model_FIT, name_grid){
                predicted_formal_class <- predict(model_FIT, newdata = as(name_grid, "Spatial"))
                predicted_sf <- st_as_sf(predicted_formal_class)
                name_grid$IWD_sed_depth <- predicted_sf$var1.pred
              }
              
            # Apply the IDW predict sediment thickness function across all ponds in the list 
            pond_grid_list_IDW_sed <- pond_grid_list
            IDW_sed_thickness <- mapply(IDW_seddepth_predict_FUNC, IDW_seddepth_FIT_list, pond_grid_list)
            
# 8 Thin Plate Regression Spline (TPRS) 
# ______________________________________________________________________________
   
  # 8.1) BATHYMETRY 

    # 8.1.1) create a model for water depth using TPRS 
            # Create model one pond at a time (check)
                TPRS_bathym_harrison_FIT <- mgcv::gam(Water_Depth_m ~ s(X, Y, k = 60), data = harrison_full, method = "REML")
                TPRS_bathym_aquadro_FIT <- mgcv::gam(Water_Depth_m ~ s(X, Y, k = 60), data = aquadro_full, method = "REML")
                TPRS_bathym_applegate_FIT <- mgcv::gam(Water_Depth_m ~ s(X, Y, k = 60), data = applegate_full, method = "REML")
                
      # Write a function to create the model for bathymetry 
        CreateTPRSmodel_bathym_FUNC <- function(pond_full){
          output_FIT <- mgcv::gam(Water_Depth_m ~ s(X, Y, k = 60), data = pond_full, method = "REML")
        }
        
            # Apply that function to one pond at a time (check)
            TPRS_bathym_harrison_FIT <- CreateTPRSmodel_bathym_FUNC(harrison_full)
            TPRS_bathym_applegate_FIT <- CreateTPRSmodel_bathym_FUNC(applegate_full)
            TPRS_bathym_aquadro_FIT <- CreateTPRSmodel_bathym_FUNC(aquadro_full)
            
            # Run each one at a time 
            boyce_full <- pond_full_list[["Boyce"]]
            white_full <- pond_full_list[["White"]]
            shelterbelt_full <- pond_full_list[["Shelterbelt"]]
            edwards_full <- pond_full_list[["Edwards"]]
            TPRS_bathym_pumpkin_FIT <- CreateTPRSmodel_bathym_FUNC(edwards_full)
            TPRS_bathym_harrison_FIT <- CreateTPRSmodel_bathym_FUNC(harrison_full)
            
            str(boyce_full)
            str(applegate_full)
            str(harrison_full)
            
            summary(boyce_full$Sed_Thickness_m)
            summary(harrison_full$Sed_Thickness_m)
            summary(white_full$Sed_Thickness_m)
            
       
            
        # Apply that function to the list of ponds 
            # mapply structure: output_list <- mapply(Name_FUNC, first_list, second_list, USE.NAMES = TRUE, SIMPLIFY = FALSE)
        TPRS_bathym_FIT_list <- mapply(CreateTPRSmodel_bathym_FUNC, pond_full_list, USE.NAMES = TRUE, SIMPLIFY = FALSE)
      
    # 8.1.2) Use that TPRS model to get predictions and add them to the grid  
      # NOTE --> If this spits an error reinstall mgcv package and try again (no idea why that works but it does)
          # install.packages("mgcv")
          library(mgcv)
          
           # Predict One pond at a time (check)
           harrison_grid$TPRS_water_depth <- predict(TPRS_bathym_harrison_FIT, newdata = harrison_grid, type = "response")  
           aquadro_grid$TPRS_water_depth <- predict(TPRS_bathym_aquadro_FIT, newdata = aquadro_grid, type = "response")  
           applegate_grid$TPRS_water_depth <- predict(TPRS_bathym_applegate_FIT, newdata = applegate_grid, type = "response")  
         
      # Write a function to Predict Bathymetry -- to use the TPRS model for each pond to predict water depth for each pond in the list 
          PredictTPRS_bathym_FUNC <- function(TPRS_bathym_FIT, pond_grid){
            pond_grid$TPRS_water_depth <- predict(TPRS_bathym_FIT, newdata = pond_grid, type = "response")
            output <- pond_grid
          }
          
              # Apply that function one pond at a time (check)
              harrison_grid <- PredictTPRS_bathym_FUNC(TPRS_bathym_harrison_FIT, harrison_grid)
              applegate_grid <- PredictTPRS_bathym_FUNC(TPRS_bathym_applegate_FIT, applegate_grid)
              aquadro_grid <- PredictTPRS_bathym_FUNC(TPRS_bathym_aquadro_FIT, aquadro_grid)
              
      # Apply that TPRS bathrym prediction function across the list of ponds 
          pond_grid_list <- mapply(PredictTPRS_bathym_FUNC, TPRS_bathym_FIT_list, pond_grid_list, USE.NAMES = TRUE, SIMPLIFY = FALSE)
          # Note that in this configuration you will have one pond_grid_list that you will pass through each model and update it as you go 
          
  # 8.2) SEDIMENT THICKNESS 
       
    # 8.2.1) Create a model for Sediment thickness usign TPRS 
          
          # Trouble shooting 10/24
          harrison_full <- as.data.frame(pond_full_list["Harrison"])
          test_full <- as.data.frame(pond_full_list["Applegate"])
          names(test_full) <- c("source", "Pond_Name", "Water_Depth_m", "Sed_Thickness_m", "X", "Y", "geometry")
          head(test_full)
          TPRS_sedmap_boyce_FIT <- mgcv::gam(Sed_Thickness_m ~ s(X, Y, k = 60), data = test_full, method = "REML")
          
          
          # create a model for sediment thickness using TPRS one pond at a time (check)
           TPRS_sedmap_harrison_FIT <- mgcv::gam(Sed_Thickness_m ~ s(X, Y, k = 60), data = harrison_full, method = "REML")
           TPRS_sedmap_aquadro_FIT <- mgcv::gam(Sed_Thickness_m ~ s(X, Y, k = 60), data = aquadro_full, method = "REML")
           TPRS_sedmap_applegate_FIT <- mgcv::gam(Sed_Thickness_m ~ s(X, Y, k = 60), data = applegate_full, method = "REML")
           
           TPRS_sedmap_harrison_FIT <- mgcv::gam(Harrison.Sed_Thickness_m ~ s(Harrison.X, Harrison.Y, k = 60), data = harrison_full, method = "REML")
           TPRS_sedmap_boyce_FIT <- mgcv::gam(Sed_Thickness_m ~ s(X, Y, k = 60), data = boyce_full, method = "REML")
           
           
    # Write a function to create the model for sediment thickness for the full list of ponds  
           CreateTPRSmodel_seddepth_FUNC <- function(pond_full){
             output_FIT <- mgcv::gam(Sed_Thickness_m ~ s(X, Y, k = 60), data = pond_full, method = "REML")
           }
           
           # Apply that function to one pond at a time (check)
           TPRS_seddepth_harrison_FIT <- CreateTPRSmodel_seddepth_FUNC(harrison_full)
           TPRS_seddepth_applegate_FIT <- CreateTPRSmodel_seddepth_FUNC(applegate_full)
           TPRS_seddepth_aquadro_FIT <- CreateTPRSmodel_seddepth_FUNC(aquadro_full)
           
    # Apply that function to the list of ponds 
     # mapply structure: output_list <- mapply(Name_FUNC, first_list, second_list, USE.NAMES = TRUE, SIMPLIFY = FALSE)
     TPRS_seddepth_FIT_list <- mapply(CreateTPRSmodel_seddepth_FUNC, pond_full_list, USE.NAMES = TRUE, SIMPLIFY = FALSE)
     
           
  # 8.2.2) Use the model for each pond to predct sediment thickness and add them to the grid
           
           # Predict one pond at a time (check)  
             harrison_grid$TPRS_sed_depth <- predict(TPRS_seddepth_harrison_FIT, newdata = harrison_grid, type = "response")  
             aquadro_grid$TPRS_sed_depth <- predict(TPRS_seddepth_aquadro_FIT, newdata = aquadro_grid, type = "response")  
             applegate_grid$TPRS_sed_depth <- predict(TPRS_seddepth_applegate_FIT, newdata = applegate_grid, type = "response")  
         
      # Write a function to Predict Sediment Thickness-- to use the TPRS model for each pond to predict water depth for each pond in the list 
             PredictTPRS_seddepth_FUNC <- function(TPRS_seddepth_FIT, pond_grid){
               pond_grid$TPRS_sed_depth <- predict(TPRS_seddepth_FIT, newdata = pond_grid, type = "response")
               output <- pond_grid
             }
          
       # Apply that TPRS prediction function across the list of ponds 
       pond_grid_list <- mapply(PredictTPRS_seddepth_FUNC, TPRS_seddepth_FIT_list, pond_grid_list, USE.NAMES = TRUE, SIMPLIFY = FALSE)
          # Note that in this configuration you will have one pond_grid_list that you will pass through each model and update it as you go 
       
   
      
# 9. Soap Film Smooth (SOAP)   
# ______________________________________________________________________________ 
   
# Writing code to scale 9/29/23 
   
   # 9.1. Make Linestring for each pond (throws a warning but works) -- might have to do individually for each pond? 
            
            # Individually for each pond (check)
                # harrison_linestring <- harrison_polygon %>% st_cast("LINESTRING") %>% st_buffer(10)  
                # aquadro_linestring <- aquadro_polygon %>% st_cast("LINESTRING") %>% st_buffer(10)  
                # applegate_linestring <- applegate_polygon %>% st_cast("LINESTRING") %>% st_buffer(10)  
       
        # Write a function to create linestrings for each pond  
        Linestring_FUNC <- function(pond_polygon){
          pond_linestring <- pond_polygon %>% st_cast("LINESTRING") %>% st_buffer(10)
        }
              # Check function works 
                # harrison_linestring <- Linestring_FUNC(harrison_polygon)
                # aquadro_linestring <- Linestring_FUNC(aquadro_polygon)
                # harrison_linestring <- Linestring_FUNC(applegate_polygon)
                
        # Apply function across list of polygons 
          # mapply structure: output_list <- mapply(Name_FUNC, first_list, second_list, USE.NAMES = TRUE, SIMPLIFY = FALSE)
          pond_linestring_list <- mapply(Linestring_FUNC, pond_polygon_list, USE.NAMES = TRUE, SIMPLIFY = FALSE)
          
   
  # 9.2 make a Gam bound for each pond
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
   
      # 9.2.b apply function to create a gam bound for each pond individually (check)
        
        # Check that function works 
           #  harrison_gam_bound <- SOAP_gam_bound_FUNC(harrison_polygon)
           #  applegate_gam_bound <- SOAP_gam_bound_FUNC(applegate_polygon)
           #  aquadro_gam_bound <- SOAP_gam_bound_FUNC(aquadro_polygon)
          
      # Apply function to create gam boundary across the list of pond polygons 
        # mapply structure: output_list <- mapply(Name_FUNC, first_list, second_list, USE.NAMES = TRUE, SIMPLIFY = FALSE)
        gam_bound_list <- mapply(SOAP_gam_bound_FUNC, pond_polygon_list, USE.NAMES = TRUE, SIMPLIFY = FALSE)

   
  # 9.3. Create knot points for each pond 
      
      # 9.3.a Write a function to create knot points from line string and boundary 
   
         SOAP_knot_points_FUNC <- function(pond_boundary, pond_linestring){
           
               
               # FUNC Step 1. Make knot points 
               knot_points_1 <- st_make_grid(
                 pond_boundary,      
                 n = c(7, 7),
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
   
           #  Check Knot Points Function works
               # harrison_knot_points <- SOAP_knot_points_FUNC(harrison_polygon, harrison_linestring )
               # aquadro_knot_points <- SOAP_knot_points_FUNC(aquadro_polygon, aquadro_linestring )
               # applegate_knot_points <- SOAP_knot_points_FUNC(applegate_polygon, applegate_linestring )
           
       # 9.3.b Apply function to create knot points across the list of pond polygons 
       # mapply structure: output_list <- mapply(Name_FUNC, first_list, second_list, USE.NAMES = TRUE, SIMPLIFY = FALSE)
       knot_points_list <- mapply(SOAP_knot_points_FUNC, pond_polygon_list, pond_linestring_list, USE.NAMES = TRUE, SIMPLIFY = FALSE)
       
       
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
           
           # Apply function one at a time (check)
                 # SOAP_bathym_harrison_FIT <- SOAP_bathym_model_FUNC(harrison_full, harrison_polygon, harrison_knot_points, harrison_gam_bound)
                 # SOAP_bathym_aquadro_FIT <- SOAP_bathym_model_FUNC(aquadro_full, aquadro_polygon, aquadro_knot_points, aquadro_gam_bound)
                 # SOAP_bathym_applegate_FIT <- SOAP_bathym_model_FUNC(applegate_full, applegate_polygon, applegate_knot_points, applegate_gam_bound)
                 
           # Use mapply to apply bathymetry model over all ponds in a list 
           # mapply structure: output_list <- mapply(Name_FUNC, first_list, second_list, USE.NAMES = TRUE, SIMPLIFY = FALSE)
           SOAP_bathym_FIT_list <- mapply(SOAP_bathym_model_FUNC, pond_full_list, pond_polygon_list, knot_points_list, gam_bound_list,  USE.NAMES = TRUE, SIMPLIFY = FALSE)
           
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
        
            # Run one at a time to check that function works 
                # SOAP_seddepth_harrison_FIT <- SOAP_seddepth_model_FUNC(harrison_full, harrison_polygon, harrison_knot_points, harrison_gam_bound)
                # SOAP_seddepth_aquadro_FIT <- SOAP_seddepth_model_FUNC(aquadro_full, aquadro_polygon, aquadro_knot_points, aquadro_gam_bound)
                # SOAP_seddepth_applegate_FIT <- SOAP_seddepth_model_FUNC(applegate_full, applegate_polygon, applegate_knot_points, applegate_gam_bound)
            
        # Use mapply to apply bathymetry model over all ponds in a list 
        # mapply structure: output_list <- mapply(Name_FUNC, first_list, second_list, USE.NAMES = TRUE, SIMPLIFY = FALSE)
        SOAP_seddepth_FIT_list <- mapply(SOAP_seddepth_model_FUNC, pond_full_list, pond_polygon_list, knot_points_list, gam_bound_list,  USE.NAMES = TRUE, SIMPLIFY = FALSE)
        
        
            
   # 9.7 Use models to predict sediment and water depth 
        
        # 9.7.a  Use bathymetry GAM model to predict water depth for each pond 
        
              # Predict one pond at a time to check that it works 
                # harrison_grid$SOAP_water_depth <- predict.gam(SOAP_bathym_harrison_FIT, newdata = harrison_grid, type = "response")
                # aquadro_grid$SOAP_water_depth <- predict.gam(SOAP_bathym_aquadro_FIT, newdata = aquadro_grid, type = "response")
                # applegate_grid$SOAP_water_depth <- predict.gam(SOAP_bathym_applegate_FIT, newdata = applegate_grid, type = "response")
                
          # Write a function to use the GAM model to predict bathymetry and save to pond_grid 
                PredictSOAP_bathym_FUNC <- function(SOAP_bathym_FIT, pond_grid){
                  pond_grid$SOAP_water_depth <- predict(SOAP_bathym_FIT, newdata = pond_grid, type = "response")
                  output <- pond_grid
                }
            
              # Check that function works (run one at a time)
                    # harrison_grid <- PredictSOAP_bathym_FUNC(SOAP_bathym_harrison_FIT, harrison_grid)
                    # aquadro_grid <- PredictSOAP_bathym_FUNC(SOAP_bathym_aquadro_FIT, aquadro_grid)
                    # applegate_grid <- PredictSOAP_bathym_FUNC(SOAP_bathym_applegate_FIT, applegate_grid)
                    
          # Use mapply to apply prediction function across list of ponds
                # install.packages("mgcv")
                library(mgcv)
                pond_grid_list_SOAP_water <- pond_grid_list
                pond_grid_list_SOAP <- mapply(PredictSOAP_bathym_FUNC, SOAP_bathym_FIT_list, pond_grid_list, USE.NAMES = TRUE, SIMPLIFY = FALSE)
                # Note that in this configuration you will have one pond_grid_list that you will pass through each model and update it as you go 
              
        # 9.7.b USe sediment thickness GAM Model to predict sedimemt depth for each pond
              # Predict one pond at a time to check that it works 
                # harrison_grid$SOAP_sed_depth <- predict.gam(SOAP_seddepth_harrison_FIT, newdata = harrison_grid, type = "response")
                # aquadro_grid$SOAP_sed_depth <- predict.gam(SOAP_seddepth_aquadro_FIT, newdata = aquadro_grid, type = "response")
                # applegate_grid$SOAP_sed_depth <- predict.gam(SOAP_seddepth_applegate_FIT, newdata = applegate_grid, type = "response")
                
          
          # Write a function to use the GAM model to predict sediment depth and save to pond_grid 
            PredictSOAP_seddepth_FUNC <- function(SOAP_seddepth_FIT, pond_grid){
                  pond_grid$SOAP_sed_depth <- predict(SOAP_seddepth_FIT, newdata = pond_grid, type = "response")
                  output <- pond_grid
            }
            
            # Check that function works (run one at a time)
                # harrison_grid <- PredictSOAP_seddepth_FUNC(SOAP_seddepth_harrison_FIT, harrison_grid)
                # aquadro_grid <- PredictSOAP_seddepth_FUNC(SOAP_seddepth_aquadro_FIT, aquadro_grid)
                # applegate_grid <- PredictSOAP_seddepth_FUNC(SOAP_seddepth_applegate_FIT, applegate_grid)
          
          # Use mapply to apply prediction function across list of ponds 
            # pond_grid_list_SOAP_sed <- pond_grid_list
            pond_grid_list_SOAP <- mapply(PredictSOAP_seddepth_FUNC, SOAP_seddepth_FIT_list, pond_grid_list_SOAP, USE.NAMES = TRUE, SIMPLIFY = FALSE)
    

#_______________________________________________________________________________
# 10.Add the estimated sediment and water depths from the output lists to the grid for each pond                
    
    # Write functions 
            
        # Compile IDW and TIN
            Compile_estimates_FUNC <- function(pond_grid_list_SOAP, IDW_sed_thickness, IDW_water_depth, TIN_sed_thickness, TIN_water_depth){
              names(pond_grid_list_SOAP) <- c("X", "Y", "Geometry", "Pond_Name", "SOAP_water_depth", "SOAP_sed_thickness")
              output <- cbind(pond_grid_list_SOAP, IDW_sed_thickness, IDW_water_depth, TIN_sed_thickness, TIN_water_depth)
            }
            
        
        pond_grid_results_list <- mapply(Compile_estimates_FUNC, pond_grid_list_SOAP, IDW_sed_thickness, IDW_water_depth, TIN_sed_thickness, TIN_water_depth, USE.NAMES = TRUE, SIMPLIFY = FALSE)
            
        # write_xlsx(pond_grid_results_list , "Output_Files/Sediment_Volume/pond_grid_results_list_BiggerGrid_102723.xlsx")
        
        
#_______________________________________________________________________________
# 11. Compute Volume of water and volume of sediment 
   
   # 11.1 Compute Sediment Volume 
        
    # Calculate pond areas and save a separate list 
        Calc_Pond_Area_FUNC <- function(pond_boundary){
          pond_boundary_area <- as.numeric(st_area(pond_boundary)) 
          output <- pond_boundary_area
        }
        
        pond_polygon_area_list <- mapply(Calc_Pond_Area_FUNC, pond_polygon_list, USE.NAMES = TRUE, SIMPLIFY = FALSE)
        pond_polygon_area_list["Boyce"]
        
      
  # Write a function to calculate sediment volume for each pond 
        
        # FUNC 
       sed_vol_calc_FUNC <- function(pond_polygon_area, pond_results){
         
         pond_results <- as.data.frame(pond_results)
         names(pond_results) <- c("X", "Y", "Geometry", "Pond_Name", "SOAP_water_depth", "SOAP_sed_thickness", "IDW_sed_thickness", "IDW_water_depth", "TIN_sed_thickness", "TIN_water_depth")
         pond_results <- subset(pond_results, select = c("Pond_Name", "SOAP_water_depth", "SOAP_sed_thickness", "IDW_sed_thickness", "IDW_water_depth", "TIN_sed_thickness", "TIN_water_depth"))
         
         pond_polygon_area_df <- as.data.frame(pond_polygon_area)
         names(pond_polygon_area_df) <- "Surface_Area"
         surface_area <- pond_polygon_area_df$Surface_Area
         
         pond_name <- as.character(pond_results[1, "Pond_Name"])  # save pond name to use in the title of the plot 
         pond_name_form <- pond_name[1] # format pond name 
         
         output <- pond_results %>% 
           summarise(
             Pond_Name = pond_name_form,
             TIN_mean_sed_depth = mean(TIN_sed_thickness),
             TIN_sed_volume = mean(TIN_sed_thickness) * surface_area,
             IDW_mean_sed_depth = mean(IDW_sed_thickness),
             IDW_sed_volume = mean(IDW_sed_thickness) * surface_area,
             #TPRS_mean_sed_depth = mean(TPRS_sed_depth),
             # TPRS_sed_volume = mean(TPRS_sed_depth) * pond_boundary_area, 
             SOAP_mean_sed_depth = mean(SOAP_sed_thickness),
             SOAP_sed_volume = mean(SOAP_sed_thickness) * surface_area, 
           )
       }
    
       # Apply Function to calcualte sediment volume for each pond and put together into a df
        pond_summary_sed_vol <- mapply(sed_vol_calc_FUNC, pond_polygon_area_list, pond_grid_results_list, USE.NAMES = TRUE, SIMPLIFY = FALSE)
        pond_summary_sed_vol <- Reduce(full_join,pond_summary_sed_vol)
        
        # write_xlsx(pond_summary_sed_vol , "Output_Files/Sediment_Volume/pond_summary_sed_vol_102723.xlsx")
       
 
#_______________________________________________________________________________       
# 12. Summary statistics of model outputs 
  
  # 12.1) Boxplots comparing each model 
       
      # Format as long instead of wide format for plotting
         pond_sum <- pond_summary_sed_vol
         names(pond_sum)
         pond_sum <- subset(pond_sum, select = c("Pond_Name", "TIN_sed_volume","IDW_sed_volume" , "SOAP_sed_volume"))
         names(pond_sum)[names(pond_sum) == "TIN_sed_volume"] <- "TIN"
         names(pond_sum)[names(pond_sum) == "IDW_sed_volume"] <- "IDW"
         names(pond_sum)[names(pond_sum) == "SOAP_sed_volume"] <- "SOAP"
         head(pond_sum)
         pond_sum_long <- gather(pond_sum, model, sed_volume, TIN:SOAP, factor_key=TRUE)
         
      # side-by-side boxplots all ponds comparing models 
        plot_vol_est_by_model <- ggplot(pond_sum_long, aes(x=model, y=sed_volume, fill=model)) +
         geom_boxplot() +
         ylab("Estimated Sediment Volume (m3)") + 
         xlab("Model") + 
         ggtitle("Estimates Sediment Volume by Model")
        plot_vol_est_by_model
       
       
      # ggsave("Output_Figures/Sediment_Volume/Estimates_sed_vol_by_model.png", plot_vol_est_by_model, height = 6, width = 9)
       
       # Plot estimated volume for each pond for each model 
       plot_vol_est_by_model_facet <- ggplot(pond_sum_long, aes(x=model, y=sed_volume, fill=model)) +
         geom_boxplot() +
         ylab("Estimated Sediment Volume (m3)") + 
         xlab("Model") +
         facet_wrap(~Pond_Name, scales = "free") +
         ggtitle("Estimated Sediment Volume by Model by Pond")
       plot_vol_est_by_model_facet
       
       ggsave("Output_Figures/Sediment_Volume/Estimates_sed_vol_by_model_facet.png", plot_vol_est_by_model_facet, height = 6, width = 9)
       
   
#_______________________________________________________________________________ 
# 13. Plotting 
   
   # 13.1 Write a Function to plot Sediment depth -- TIN 
       Plot_sedmap_TIN_FUNC <- function(name_grid, name_boundary, pond_depths){
         pond_name <- as.character(name_grid[1, "Pond_Name"])  # save pond name to use in the title of the plot 
         pond_name_form <- pond_name[1] # format pond name 
         output_plot <- ggplot(name_grid) +
           geom_sf(data = name_boundary) +
           geom_raster(aes(X, Y, fill = TIN_sed_thickness)) +
           scale_fill_viridis_c() +
           geom_sf_text(aes(label = Sed_Thickness_m), data = pond_depths, size = 3) + 
           annotation_scale(location = "br") +
           labs(title= paste(pond_name, "Sediment Depth (m) -- TIN", sep = " "), x = NULL, y = NULL, fill = "Sediment Depth (m)")
       }
   
   # 13.2 Write a Function to plot Sediment depth -- IDW 
       Plot_sedmap_IDW_FUNC <- function(name_grid, name_boundary, pond_depths){
         pond_name <- as.character(name_grid[1, "Pond_Name"])  # save pond name to use in the title of the plot 
         pond_name_form <- pond_name[1] # format pond name 
         output_plot <- ggplot(name_grid) +
           geom_sf(data = name_boundary) +
           geom_raster(aes(X, Y, fill = IDW_sed_thickness)) +
           scale_fill_viridis_c() +
           geom_sf_text(aes(label = Sed_Thickness_m), data = pond_depths, size = 3) + 
           annotation_scale(location = "br") +
           labs(title= paste(pond_name, "Sediment Depth (m) -- IDW", sep = " "), x = NULL, y = NULL, fill = "Sediment Depth (m)")
       }
   
   # 13.3 Write a Function to plot Sediment depth -- TPRS 
       Plot_sedmap_TPRS_FUNC <- function(name_grid, name_boundary, pond_depths){
         pond_name <- as.character(name_grid[1, "Pond_Name"])  # save pond name to use in the title of the plot 
         pond_name_form <- pond_name[1] # format pond name 
         output_plot <- ggplot(name_grid) +
           geom_sf(data = name_boundary) +
           geom_raster(aes(X, Y, fill = TPRS_sed_thickness)) +
           scale_fill_viridis_c(option = "D") +
           geom_sf_text(aes(label = Sed_Thickness_m), data = pond_depths, size = 3) + 
           annotation_scale(location = "br") +
           labs(title= paste(pond_name, "Sediment Depth (m) -- TPRS", sep = " "), x = NULL, y = NULL, fill = "Sediment Depth (m)")
       }
   
   # 13.4 Write a Function to plot Sediment depth -- SOAP 
       Plot_sedmap_SOAP_FUNC <- function(name_grid, name_boundary, pond_depths){
         pond_name <- as.character(name_grid[1, "Pond_Name"])  # save pond name to use in the title of the plot 
         pond_name_form <- pond_name[1] # format pond name 
         output_plot <- ggplot(name_grid) +
           geom_sf(data = name_boundary) +
           geom_raster(aes(X, Y, fill = SOAP_sed_thickness)) +
           scale_fill_viridis_c(option = "D") +
           geom_sf_text(aes(label = Sed_Thickness_m), data = pond_depths, size = 3) + 
           annotation_scale(location = "br") +
           labs(title= paste(pond_name, "Sediment Depth (m) -- SOAP", sep = " "), x = NULL, y = NULL, fill = "Sediment Depth (m)")
         output <- output_plot
       }
       

    # 13.5  # Apply the plotting function over lists so that you don't have to write it out every time 
       #TIN 
       par(ask = TRUE)  #Setting par$ask equal to TRUE allows you to flip through all of the plots one at a a time 
       mapply(Plot_sedmap_TIN_FUNC, pond_grid_results_list, pond_polygon_list, meas_depths_latlong_list, USE.NAMES = TRUE, SIMPLIFY = FALSE)
       par(ask = FALSE)
       
        TIN_plot_list <-  mapply(Plot_sedmap_TIN_FUNC, pond_grid_results_list, pond_polygon_list, meas_depths_latlong_list, USE.NAMES = TRUE, SIMPLIFY = FALSE)
       
       # IDW 
         par(ask = TRUE)  #Setting par$ask equal to TRUE allows you to flip through all of the plots one at a a time 
         mapply(Plot_sedmap_IDW_FUNC, pond_grid_results_list, pond_polygon_list, meas_depths_latlong_list, USE.NAMES = TRUE, SIMPLIFY = FALSE)
         par(ask = FALSE)
         
         IDW_plot_list <- mapply(Plot_sedmap_IDW_FUNC, pond_grid_results_list, pond_polygon_list, meas_depths_latlong_list, USE.NAMES = TRUE, SIMPLIFY = FALSE)
         IDW_plot_list[1]
         ggsave("Output_Figures/Sediment_Volume/IDW_plot_list_102723.png", IDW_plot_list, height = 6, width = 9)
       
       # TPRS 
         par(ask = TRUE)  #Setting par$ask equal to TRUE allows you to flip through all of the plots one at a a time 
         mapply(Plot_sedmap_TPRS_FUNC, pond_grid_results_list, pond_polygon_list, meas_depths_latlong_list, USE.NAMES = TRUE, SIMPLIFY = FALSE)
         par(ask = FALSE)
         
         TPRS_plot_list <- mapply(Plot_sedmap_TPRS_FUNC, pond_grid_results_list, pond_polygon_list, meas_depths_latlong_list, USE.NAMES = TRUE, SIMPLIFY = FALSE)
         TPRS_plot_list[2]
       
       # SOAP
         par(ask = TRUE)  #Setting par$ask equal to TRUE allows you to flip through all of the plots one at a a time 
         mapply(Plot_sedmap_SOAP_FUNC, pond_grid_results_list, pond_polygon_list, meas_depths_latlong_list, USE.NAMES = TRUE, SIMPLIFY = FALSE)
         par(ask = FALSE)
         
         SOAP_plot_list <- mapply(Plot_sedmap_SOAP_FUNC, pond_grid_results_list, pond_polygon_list, meas_depths_latlong_list, USE.NAMES = TRUE, SIMPLIFY = FALSE)
         SOAP_plot_list[3]
         
    # 13.6 Plot each model for a subset of pond, 
       
       # Will need to pull out the maps for other intensive ponds (also export them to a set size when you save them)
       
       # Practice Ponds 
       # Applegate 
        plot_sedmap_TIN_applegate <- Plot_sedmap_TIN_FUNC(applegate_grid, applegate_polygon, applegate_depths_latlong)
        plot_sedmap_IDW_applegate <- Plot_sedmap_IDW_FUNC(applegate_grid, applegate_polygon, applegate_depths_latlong)
        plot_sedmap_TPRS_applegate <- Plot_sedmap_TPRS_FUNC(applegate_grid, applegate_polygon, applegate_depths_latlong)
        plot_sedmap_SOAP_applegate <- Plot_sedmap_SOAP_FUNC(applegate_grid, applegate_polygon, applegate_depths_latlong)
            plot_sedmap_TPRS_applegate
            plot_sedmap_SOAP_applegate
     
        # Aquadro 
        plot_sedmap_TIN_aquadro <- Plot_sedmap_TIN_FUNC(aquadro_grid, aquadro_polygon, aquadro_depths_latlong) 
        plot_sedmap_IDW_aquadro <- Plot_sedmap_IDW_FUNC(aquadro_grid, aquadro_polygon, aquadro_depths_latlong)
        plot_sedmap_TPRS_aquadro <- Plot_sedmap_TPRS_FUNC(aquadro_grid, aquadro_polygon, aquadro_depths_latlong)
        plot_sedmap_SOAP_aquadro <- Plot_sedmap_SOAP_FUNC(aquadro_grid, aquadro_polygon, aquadro_depths_latlong)
             plot_sedmap_TIN_aquadro
             plot_sedmap_IDW_aquadro
             plot_sedmap_TPRS_aquadro
             plot_sedmap_SOAP_aquadro
     
             
        # Harrison      
        plot_sedmap_TIN_harrison <- Plot_sedmap_TIN_FUNC(harrison_grid, harrison_polygon, harrison_depths_latlong)
        plot_sedmap_IDW_harrison <- Plot_sedmap_IDW_FUNC(harrison_grid, harrison_polygon, harrison_depths_latlong)
        plot_sedmap_TPRS_harrison <- Plot_sedmap_TPRS_FUNC(harrison_grid, harrison_polygon, harrison_depths_latlong)
        plot_sedmap_SOAP_harrison <- Plot_sedmap_SOAP_FUNC(harrison_grid, harrison_polygon, harrison_depths_latlong)
              plot_sedmap_TIN_harrison
              plot_sedmap_IDW_harrison
              plot_sedmap_TPRS_harrison
              plot_sedmap_SOAP_harrison
  
# _______________________________________________________________________________________________________________   
# 14. Save maps for each pond 
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
         
     
     
   
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
     
 