########################
# HEADSPACE CONCETRATIONS 
########################
# This code is written to process raw data files coming off of the GC. Here we calculate standard curves, check for
# drift, correct for drift, calculate concentrations in headspace, and calculate concentrations in water 

# 0. Set up R Environment 
    #Set working directory: 
    # setwd("~/DEC_Ponds") # Desktop 
    setwd("~/OneDrive/Holgerson_Lab/DEC_Ponds") # Mac 
    getwd()
    
    # Standard 
    library(dplyr)
    library(mosaic)
    library(readxl)
    library(tidyverse)
    library(devtools)
    library(lme4)
    library(car)
    library(effects)
    library(mosaic)
    library(writexl)

# 1. Load and format data from the GC 
    
    # Enter this data before running: 
    data_gc_run <- "04Oct2023"
    
    # Load and formatt N2O file 
      gc_raw_nitrous <- read.table("Input_Files/GC_Data/2023_10_04 - NSF Mixing082123rerun_PECD.txt", skip = 3, sep = "\t", dec = ".", fill = TRUE)  #bring in data from GC you need to name the columns because the column names on the raw file have a # in them and that throws off R reading the row
      colnames(gc_raw_nitrous) <- (1:ncol(gc_raw_nitrous))
      gc_nitrous <- subset(gc_raw_nitrous, select = c("1", "2", "6"))
      colnames(gc_nitrous) <- c( "Sample_ID", "Full_Sample_Name", "N2O_Area")
      gc_nitrous <- subset(gc_nitrous, select = c("Sample_ID", "Full_Sample_Name", "N2O_Area"))
      gc_nitrous <- gc_nitrous[2:(nrow(gc_nitrous) - 5),] # Remove the first row and the last 5 rows (these are stats that the GC generates that we don't need )
     
      # Remove the extra digits at the end of the sample names, these are also added by the GC and we don't need themt to do our caclulations 
        gc_nitrous$Short_Sample_Name <- sub('([^_]+\\_).*', '\\1', gc_nitrous$Full_Sample_Name) # Remove every character after "_" 
        gc_nitrous$Sample_Name <- gsub("_", "", gc_nitrous$Short_Sample_Name) # Remove all of the "_"
        gc_nitrous <- subset(gc_nitrous, select = c("Sample_ID", "Sample_Name", "N2O_Area")) # Remove the extra columns hat you made 
      
      # Remove the CO2$CH4$N2O at the end of the standards 
        gc_nitrous$Sample_Name_trimmed <- gsub("\\$", "_", gc_nitrous$Sample_Name) # replace all of the $ because it throws off the code 
        gc_nitrous$Sample_ID_trimmed_b <- str_replace_all(gc_nitrous$Sample_Name_trimmed, "CO2_CH4_N2O", "")
        gc_nitrous$Sample_Name <- gc_nitrous$Sample_ID_trimmed_b
        gc_nitrous <- subset(gc_nitrous, select = c("Sample_ID", "Sample_Name", "N2O_Area"))
      # Format column types 
        gc_nitrous$Sample_ID <- as.factor(gc_nitrous$Sample_ID)
        gc_nitrous$Sample_Name <- as.character(gc_nitrous$Sample_Name)
        
   # Load and formatt CH4 and CO2 file 
        gc_raw_co2_and_ch4 <- read.table("Input_files/GC_Data/2023_10_04 - NSF Mixing082123rerun_SFID.txt",sep = "\t", header = FALSE ,fill = TRUE)
        
        # seperate into two files based on the row numbers (this should be the same for every run if full)
        #     NOTE -- if we are going to be doing a bunch of runs that are not full I can re-code this to be cleaner 
        #     Or if you are running not a full run just go in and check your own row numbers and add them 
        
        # CH4 
        gc_methane <- gc_raw_co2_and_ch4[5:92,]  # seperate out just methane 
        colnames(gc_methane) <- (1:ncol(gc_methane)) # name columns with number to make them easier to call
        gc_methane <- subset(gc_methane, select = c("1", "2", "6")) # subset to only the columns that you need 
        colnames(gc_methane) <- c( "Sample_ID", "Full_Sample_Name", "CH4_Area") #give meaningful names to columns you will use 
        gc_methane <- subset(gc_methane, select = c("Sample_ID", "Full_Sample_Name", "CH4_Area")) # order column names in a way that makes sense 
        # Format Sample name 
          gc_methane$Short_Sample_Name <- sub('([^_]+\\_).*', '\\1', gc_methane$Full_Sample_Name) # Remove every character after "_" 
          gc_methane$Sample_Name <- gsub("_", "", gc_methane$Short_Sample_Name) # Remove all of the "_"
          gc_methane <- subset(gc_methane, select = c("Sample_ID", "Sample_Name", "CH4_Area")) # Remove the extra columns hat you made 
        # Remove the CO2$CH4$N2O at the end of the standards 
          gc_methane$Sample_Name_trimmed <- gsub("\\$", "_", gc_methane$Sample_Name) # replace all of the $ because it throws off the code 
          gc_methane$Sample_ID_trimmed_b <- str_replace_all(gc_methane$Sample_Name_trimmed, "CO2_CH4_N2O", "")
          gc_methane$Sample_Name <- gc_methane$Sample_ID_trimmed_b
        gc_methane <- subset(gc_methane, select = c("Sample_ID", "Sample_Name", "CH4_Area"))
          
        # CO2 
        gc_carbondiox <- gc_raw_co2_and_ch4[102:189,]  # seperate out just carbondiox 
        colnames(gc_carbondiox) <- (1:ncol(gc_carbondiox)) # name columns with number to make them easier to call
        gc_carbondiox <- subset(gc_carbondiox, select = c("1", "2", "6")) # subset to only the columns that you need 
        colnames(gc_carbondiox) <- c( "Sample_ID", "Full_Sample_Name", "CO2_Area") #give meaningful names to columns you will use 
        gc_carbondiox <- subset(gc_carbondiox, select = c("Sample_ID", "Full_Sample_Name", "CO2_Area")) # order column names in a way that makes sense 
        # Format Sample name 
        gc_carbondiox$Short_Sample_Name <- sub('([^_]+\\_).*', '\\1', gc_carbondiox$Full_Sample_Name) # Remove every character after "_" 
        gc_carbondiox$Sample_Name <- gsub("_", "", gc_carbondiox$Short_Sample_Name) # Remove all of the "_"
        gc_carbondiox <- subset(gc_carbondiox, select = c("Sample_ID", "Sample_Name", "CO2_Area")) # Remove the extra columns hat you made 
        # Remove the CO2$CH4$N2O at the end of the standards 
        gc_carbondiox$Sample_Name_trimmed <- gsub("\\$", "_", gc_carbondiox$Sample_Name) # replace all of the $ because it throws off the code 
        gc_carbondiox$Sample_ID_trimmed_b <- str_replace_all(gc_carbondiox$Sample_Name_trimmed, "CO2_CH4_N2O", "")
        gc_carbondiox$Sample_Name <- gc_carbondiox$Sample_ID_trimmed_b
        gc_carbondiox <- subset(gc_carbondiox, select = c("Sample_ID", "Sample_Name", "CO2_Area"))
    
    # Put data for all gases together 
        gc_data_co2_ch4 <- full_join(gc_carbondiox, gc_methane)
        str(gc_data_co2_ch4)
        gc_data <- full_join(gc_data_co2_ch4, gc_nitrous)
        
    # Add date of GC run as a column 
        gc_data$Date_Run <- data_gc_run  # Now on line 97 you finally have a clean organized df 
        
# 2. Standard curves 
        # NOTE: working just with N2O for now because it is the simplist, I will come back through and add CO2 and CH4
    
  # 2.1 Pull just the standards to make a seperate data frame and format 
        gc_nitrous$Type <- ifelse(grepl("A-", gc_nitrous$Sample_Name), "sample", "standard")  #make a column that differentiates standards from samples, I had to do this with the "A-" because for some reason the code was funky about using $ 
        gc_nitrous_standards <- gc_nitrous[gc_nitrous$Type == "standard" , ]
        gc_nitrous_standards <- subset(gc_nitrous_standards, Sample_Name != "zero" )
        
        # Seperate out to make columns for the concentrations of the standard gasses 
        
        # Replace the $ because they throw off the code 
        gc_nitrous_standards$St_Con_All <- gsub("\\$", "_", gc_nitrous_standards$Sample_Name)
        head(gc_nitrous_standards)

        # Make a column of CO2 Concentration 
        gc_nitrous_standards$St_Con_CO2 <- gsub("_.*", "\\1", gc_nitrous_standards$St_Con_All) # Get rid of everything after the first _
        head(gc_nitrous_standards)      
        
        # make a column of N2O Concentration 
        gc_nitrous_standards$St_Con_N2O <- gsub(".*_", "\\1", gc_nitrous_standards$St_Con_All)  # get rid of everything before the last _ 
        head(gc_nitrous_standards)
        
        # make a column of CH4 Concentrations 
          #  This might be the silliest work around I have ever coded substring name, start at the number of characters in first name plus one, stop at the total number of characters minus the number of characters in last name plus one 
        gc_nitrous_standards$St_Con_CH4 <- substring(gc_nitrous_standards$St_Con_All, (nchar(gc_nitrous_standards$St_Con_CO2)+2), (nchar(gc_nitrous_standards$St_Con_All) - (nchar(gc_nitrous_standards$St_Con_N2O) + 1)) ) 
        
        # Zero air is a concentration of O for all three gases 
        gc_nitrous_standards$St_Con_CH4 <- ifelse(gc_nitrous_standards$St_Con_All == "zero air", 0, gc_nitrous_standards$St_Con_CH4)
        gc_nitrous_standards$St_Con_CO2 <- ifelse(gc_nitrous_standards$St_Con_All == "zero air", 0, gc_nitrous_standards$St_Con_CO2)
        gc_nitrous_standards$St_Con_N2O <- ifelse(gc_nitrous_standards$St_Con_All == "zero air", 0, gc_nitrous_standards$St_Con_N2O)
        
    #2.2 Plot standard curve 
        plot(gc_nitrous_standards$St_Con_N2O, gc_nitrous_standards$Area, main="N2O Standard Curve",
             xlab="Standard Concentration", ylab="Area", pch=19)
  
        
        
        
# 3. Drift 
        
        
  
        
        
        
        
        
        
        
        
        
        
        
              
######## 
# Figuring out nameing 
        # Figuring out naming 
        # Replace the $ because they throw off the code 
        name <- as.character("K_A_G")
        new_name <- gsub("_", "+", name)
        money_name <- as.character("K$A$G")
        new_money_name <- gsub("\\$", "_", money_name)
        
        # Figuring out how to grab only part of a string 
        name <- as.character("Katie_Alta_Gannon")
        first_name <- gsub("_.*", "\\1", name) # Get rid of everything after the first _
        last_name <- gsub(".*_", "\\1", name)  # get rid of everything before the last _ 
        
        # grabbing only the concentration in the middle is the hardest 
        name <- as.character("Katie_Alta_Gannon")
        nchar(first_name)
        nchar(last_name)
        nchar(name)
        middle_name <- substring(name, (nchar(first_name)+2), (nchar(name) - (nchar(last_name) + 1)) ) # substring name, start at the number of characters in first name plus one, stop at the total number of characters minus the number of characters in last name plus one 
        
        middle_name <- substring(name, (nchar(first_name)+2), (nchar(name) - (nchar(last_name) + 1)) ) 
        
        