######################################
# miniDOT data -- DEC Ponds 2023  
######################################
# Holgerson Lab - Gannon 26 January 2024 

# ______________________________________________________________________________________________________________________________________________________
# 0. Set up R Environment 

    library(dplyr)
    library(sf)
    library(raster)
    library(ape) 
    suppressMessages(library(mosaic))
    suppressMessages(library(readxl))
    library(tidyverse)
    library(devtools)
    library(lme4)
    library(car)
    library(effects)
    library(mosaic)
    library(writexl)
    library(purrr)
    library(ggplot2)
    #install.packages("ggpmisc") <- try on desktop 
    # library(ggpmisc)

    setwd("~/DEC_Ponds")# Desktop 
    # setwd("~/OneDrive/Holgerson_Lab/HolgersonLab_Helpful_Code") # Mac
    
# 1.  Read in miniDOT data 
    
    # Read in miniDOT data in a batch 
    setwd("~/HolgersonLab_Helpful_Code/miniDOT_Data/021223_BubbleBath_Check_KG") # Desktop - #Reset the working directory to the folder containing just the files that need to be formatted
    # setwd("~/OneDrive/Holgerson_Lab/HolgersonLab_Helpful_Code") # Mac
    file_names <- list.files(pattern="*.TXT") #Get a list of all of the .txt files in the working directory 
    list_of_miniDOT_SN <- lapply(file_names, read.table, skip = , header = T, sep = ",", fill = TRUE)
    list_of_miniDOT_data <- lapply(file_names, read.table, skip = 6, header = T, sep = ",", fill = TRUE) #this pulls in the data but looses the serial numbers   
    setwd("~/HolgersonLab_Helpful_Code")    # Reset the working directory to the base 
    
    # Read in miniDOT data one at a time 
    boyce_cat_01dec2023 <- read.table("~/K Gannon/Data_Offload/MiniDOT/240126_DEC_Boyce/01dec2023_Boyce_miniDOT_Concat.TXT", read.table, skip = 6, header = T, sep = ",", fill = TRUE)

# 2.  Formatt each dataframe of miniDOT data 

# Write a function to clean up the raw miniDOT data and get it in a format that is easier to work with in R
miniDOT_cleaning_FUNC <- function(df){
  names(df) <- c("Unix.Timestamp_sec", "UTC_Date_Time", "EST", "Batter_volt", "Temp_C", "Dissolved_Oxygen_mg_l", "Dissolved_Oxygen_Sat_%", "Q")
  df2 <- df[-1, ]
  head(df2)
  df2 <- subset(df2, select = c("EST", "Temp_C", "Dissolved_Oxygen_mg_l", "Dissolved_Oxygen_Sat_%"))
  names(df2) <- c("Date_Time","Temp_C", "DO_mgl", "DOSat_per")
  head(df2)
  df2$Temp_C <- as.numeric(df2$Temp_C)
  df2$DO_mgl <- as.numeric(df2$DO_mgl)
  df2$DOSat_per <- as.numeric(df2$DOSat_per)
  df2$Date_Time <- as.character(df2$Date_Time)  # Needs to be as a character to pass through forloop bellow
  #df2$Date_Time  <- as.POSIXct(df2$Date_Time, tz = "EST", format = "%Y-%m-%d %H:%M")
  formatted_output <- as.data.frame(df2)
}

# Test that your function works 
test_df <- list_of_miniDOT_data[[8]]
test_output <- miniDOT_cleaning_FUNC(test_df)
head(test_output)

# Apply your cleaning function over the full list of miniDOT data 
list_cleaned_miniDOT_data <- lapply(list_of_miniDOT_data, miniDOT_cleaning_FUNC)
head(list_cleaned_miniDOT_data[[6]]) #Check that cleaning everything together worked 