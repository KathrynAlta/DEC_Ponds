#########################
# ANOXIC VOLUME AND ANOXIC FRACTION 
#########################

# 0. Set Up R Environment 
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

# 1. Input Data 
    mud_eureka <- read.csv("Input_Files/TXH&MUD/mudd_2022_eureka_compiled.csv")
    txh_eureka <- read.csv("Input_Files/TXH&MUD/txh_2022_eureka_compiled.csv")
    do_conversion_df <- read_xlsx("Input_Files/TXH&MUD/Kalff_DO_Conversion_Table.xlsx")
    
    # subset to only the columns that you need 
    mud_eureka <- subset(mud_eureka, select = c("pond", "date", "temp_c", "do_perc", "depth"))
    txh_eureka <- subset(txh_eureka, select = c("pond", "date", "temp_c", "do_perc", "depth"))
    
    # Remove Rows with missing data 
    mud_eureka <- mud_eureka[!is.na(mud_eureka$temp_c), ]
    mud_eureka <- mud_eureka[!is.na(mud_eureka$do_perc), ]
    
    txh_eureka <- txh_eureka[!is.na(txh_eureka$temp_c), ]
    txh_eureka <- txh_eureka[!is.na(txh_eureka$do_perc), ]

# 2. Convert compiled Eureka file from DO% to DO mg/L 
    
    # For every row in the eureka table add a colume with the mg/L of DO at 100% based on Kalff 
    eureka_temp <- 13.317
    selected_row <- do_conversion[do_conversion$Temp_C == round(eureka_temp) , ]
    DOmgl_at_100perc <- as.numeric(selected_row["DO_mgL"])
    
    eureka_data <- mud_eureka
    
    DO_at_temp_FUNC <- function(eureka_data){
      eureka_data <- as.data.frame(eureka_data)
      eureka_temp <- as.numeric(eureka_data$do_perc)
      selected_row <- do_conversion[do_conversion$Temp_C == round(eureka_temp) , ]
      DOmgl_at_100perc <- as.numeric(selected_row["DO_mgL"])
      # eureka_data$DO_at_temp <- DOmgl_at_100perc
      # output <- eureka_data
    }
    
    apply(mud_eureka, 1, DO_at_temp_FUNC)
    
    check <- DO_at_temp_FUNC(eureka_data = mud_eureka)
    # Check the code that you used for compiling/grabbing the right HOBO temp in the LGR flux code 
    
    # Then multiply the DO% by the mg/L 
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    

# 3. Get Anoxic depth for each date (Or leave R, do thi step manually, and then imput a spreadsheet of depths)