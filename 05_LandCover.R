############################################################
# LAND COVER AROUND PONDS
############################################################
# Holgerson Lab, Katie Gannon

# Check 
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
    library(gstat)
    library(mgcv)

  # Load data: 
    # Key for land cover grid ID 
    landcover_gridID <- read_xlsx("230321_LandCover_Values.xlsx")
  
    # Dissolved shape files for each pond and each buffer size 
    aquadro_dissolved_1km <- read_sf("Spatial_Data_LandCover/dissolved_1km_aquadro.shp")
    aquadro_dissolved_500m <- read_sf("Spatial_Data_LandCover/dissolved_500m_aquadro.shp")
    
    longhouse_dissolved_1km <- read_sf("Spatial_Data_LandCover/dissolved_1km_Longhouse.shp")
    longhouse_dissolved_500m <- read_sf("Spatial_Data_LandCover/dissolved_500m_Longhouse.shp")
    
    marks_dissolved_1km <- read_sf("Spatial_Data_LandCover/dissolved_1km_Marks.shp")
    marks_dissolved_500m <- read_sf("Spatial_Data_LandCover/dissolved_500m_Marks.shp")
    
    ecovillage_dissolved_1km <- read_sf("Spatial_Data_LandCover/dissolved_1km_Ecovillage.shp")
    ecovillage_dissolved_500m <- read_sf("Spatial_Data_LandCover/dissolved_500m_Ecovillage.shp")
    
    dybowski_dissolved_1km <- read_sf("Spatial_Data_LandCover/dissolved_1km_Dybowski.shp")
    dybowski_dissolved_500m <- read_sf("Spatial_Data_LandCover/dissolved_500m_Dybowski.shp")
    
# 1) Format 

    # Practice data sets 
      Input_Pond_Name <- "Dybowski"
      Input_Buffer_Size <- "1km"
      dissolved_data <- dybowski_dissolved_1km
      head(plc_data)
 
  # Write a function to format all dissolved dataframes long 
  Format_Long_FUNC <- function(dissolved_data, landcover_gridID, Input_Pond_Name, Input_Buffer_Size){
    plc_data <- left_join(dissolved_data, landcover_gridID)
    plc_data <- subset(plc_data, select = c("Class_Name", "Percent_Co"))  # Subset to only columns that you are interested in 
    plc_data$Percent_Co <- round(plc_data$Percent_Co * 100 , 3)   
    plc_data <- as.data.frame(plc_data)  # save as a data frame to be easier to work with 
    plc_data$geometry <- NULL  # Remove the geometry because we don't need it any more 
    plc_data$Pond_Name <- Input_Pond_Name
    plc_data$Buffer_Size <- Input_Buffer_Size
    output <- plc_data
  }
  
# 2) Use the function to format long all of the dissolved data 
  
  # 1 km 
    aqua_plc_1km <- Format_Long_FUNC(aquadro_dissolved_1km, landcover_gridID, "Aquadro", "1km")
    dybo_plc_1km <- Format_Long_FUNC(dybowski_dissolved_1km, landcover_gridID, "Dybowski", "1km")
    ecov_plc_1km <- Format_Long_FUNC(ecovillage_dissolved_1km, landcover_gridID, "Ecovillage", "1km")
    loho_plc_1km <- Format_Long_FUNC(longhouse_dissolved_1km, landcover_gridID, "Longhouse", "1km")
    mark_plc_1km <- Format_Long_FUNC(marks_dissolved_1km, landcover_gridID, "Marks", "1km")
  
  # 500 m 
    aqua_plc_500m <- Format_Long_FUNC(aquadro_dissolved_500m, landcover_gridID, "Aquadro", "500m")
    dybo_plc_500m <- Format_Long_FUNC(dybowski_dissolved_500m, landcover_gridID, "Dybowski", "500m")
    ecov_plc_500m <- Format_Long_FUNC(ecovillage_dissolved_500m, landcover_gridID, "Ecovillage", "500m")
    loho_plc_500m <- Format_Long_FUNC(longhouse_dissolved_500m, landcover_gridID, "Longhouse", "500m")
    mark_plc_500m <- Format_Long_FUNC(marks_dissolved_500m, landcover_gridID, "Marks", "500m")
  
# 3) Put dfs all together 
    landcov_long <- rbind(aqua_plc_1km, dybo_plc_1km, ecov_plc_1km, loho_plc_1km, mark_plc_1km,
                          aqua_plc_500m, dybo_plc_500m, ecov_plc_500m, loho_plc_500m, mark_plc_500m )
    landcov_long$Class_Name <- as.factor(landcov_long$Class_Name)
    levels(landcov_long$Class_Name) <- c("Baren_Land", "Cultivated_Crops", "Deciduous_Forest", 
                                         "Developed_High_Intensity", "Developed_Low_Intensity", "Developed_Medium_Intensity", 
                                         "Developed_Open_Space","Emergent_Herbaceous_Wetlands", "Evergreen_Forest", 
                                         "Grassland_Herbaceous","Mixed_Forest", "Open_Water", 
                                         "Pasture_Hay", "Shrub_Scrub", "Woody_Wetland")
  
# 4) Turn into wide format 
    landcov_wide <- spread(landcov_long, Class_Name, Percent_Co)  # Turn into wide formatt
    landcov_wide <- mutate_all(landcov_wide, ~replace_na(.,0))  # Change NAs to 0 (not included bc zero percent of the land classified as that category)
    
# 5) Lump Land Use Categories and get percent cover 
    landcov_wide$Urban <- landcov_wide$Developed_High_Intensity + landcov_wide$Developed_Low_Intensity + landcov_wide$Developed_Medium_Intensity + landcov_wide$Developed_Open_Space
    landcov_wide$Forested <- landcov_wide$Deciduous_Forest + landcov_wide$Evergreen_Forest + landcov_wide$Mixed_Forest
    landcov_wide$Agricultre <- landcov_wide$Pasture_Hay + landcov_wide$Cultivated_Crops
    landcov_wide$Wetland <- landcov_wide$Shrub_Scrub + landcov_wide$Woody_Wetland + landcov_wide$Emergent_Herbaceous_Wetlands
    landcov_wide$Other <- landcov_wide$Baren_Land + landcov_wide$Grassland_Herbaceous
    landcov_wide$Water <- landcov_wide$Open_Water
    
    landcov_simp <- subset(landcov_wide, select = c("Pond_Name", "Buffer_Size", "Urban", "Forested", "Agricultre", "Wetland", "Other", "Water"))
  
  
# NOPE ********************************************************************************************************
  # 1) Join dissolved shape files with the key of land cover grid --> You do not actully need to do this here
  aquadro_dissolved_1km_named <- left_join(aquadro_dissolved_1km, landcover_gridID)
  aquadro_plc_1km <- left_join(aquadro_dissolved_1km, landcover_gridID)   #Aquadro percent land cover for 1 km buffer
  aquadro_plc_500m <- left_join(aquadro_dissolved_500m, landcover_gridID)   #Aquadro percent land cover for 500 m buffer
  
  longhouse_dissolved_1km_named <- left_join(longhouse_dissolved_1km, landcover_gridID)
  longhouse_plc_1km <- left_join(longhouse_dissolved_1km, landcover_gridID)   #longhouse percent land cover for 1 km buffer
  longhouse_plc_500m <- left_join(longhouse_dissolved_500m, landcover_gridID)   #longhouse percent land cover for 500 m buffer
  
  marks_dissolved_1km_named <- left_join(marks_dissolved_1km, landcover_gridID)
  marks_plc_1km <- left_join(marks_dissolved_1km, landcover_gridID)   #marks percent land cover for 1 km buffer
  marks_plc_500m <- left_join(marks_dissolved_500m, landcover_gridID)   #marks percent land cover for 500 m buffer
  
  ecovillage_dissolved_1km_named <- left_join(ecovillage_dissolved_1km, landcover_gridID)
  ecovillage_plc_1km <- left_join(ecovillage_dissolved_1km, landcover_gridID)   #ecovillage percent land cover for 1 km buffer
  ecovillage_plc_500m <- left_join(ecovillage_dissolved_500m, landcover_gridID)   #ecovillage percent land cover for 500 m buffer
  
  dybowski_dissolved_1km_named <- left_join(dybowski_dissolved_1km, landcover_gridID)
  dybowski_plc_1km <- left_join(dybowski_dissolved_1km, landcover_gridID)   #dybowski percent land cover for 1 km buffer
  dybowski_plc_500m <- left_join(dybowski_dissolved_500m, landcover_gridID)   #dybowski percent land cover for 500 m buffer
  
  
  # Formatt 
  head(aquadro_plc_1km)
  pond_name <- "Aquadro"
  
  aquad_1km <- subset(aquadro_plc_1km, select = c("Class_Name", "Percent_Co"))  # Subset to only columns that you are interested in 
  names(aquad_1km)[names(aquad_1km) == "Percent_Co"] <- "Percent_Co_1km"  #Note in the column name that this is for the 1 km 
  aquad_1km <- as.data.frame(aquad_1km)  # save as a data frame to be easier to work with 
  aquad_1km$geometry <- NULL  # Remove the geometry because we don't need it any more 
  
  head(aquad_1km)
  aquad_1km$Pond_Name <- "Aquadro"
  aquad_1km$Buffer_Size <- "1km"
  wide <- spread(aquad_1km, Class_Name, Percent_Co_1km)
























