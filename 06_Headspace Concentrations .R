########################
# HEADSPACE CONCETRATIONS 
########################
# This code is written to process raw data files coming off of the GC. Here we calculate standard curves, check for
# drift, correct for drift, calculate concentrations in headspace, and calculate concentrations in water 

# 0. Set up R Environment 
    #Set working directory: 
    setwd("~/DEC_Ponds") # Desktop 
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
    
    #N2O
      gc_raw_nitrous <- read.table("Input_Files/GC_Data/2023_07_25 GC - NSF Mixing PECD.txt", skip = 3, sep = "", dec = ".", fill = TRUE, col.names = c("Sample_ID",	"Full_Sample_Name", "Level_Number",	"Ret. Time",	"Area",	"Height",	"Conc.",	"Std. Conc.",	"Area%",	"Height%",	"Accuracy[%]",	"Cal. Point", "Col13", "Col14"))  #bring in data from GC you need to name the columns because the column names on the raw file have a # in them and that throws off R reading the row
      gc_nitrous <- subset(gc_raw_nitrous, select = c("Sample_ID", "Full_Sample_Name", "Area")) # subset to only the columns that you need 
      gc_nitrous <- gc_nitrous[2:140,] # Remove the first row and the last 5 rows (these are stats that the GC generates that we don't need )
     
      # Remove the extra digits at the end of the sample names, these are also added by the GC and we don't need themt to do our caclulations 
        gc_nitrous$Short_Sample_Name <- sub('([^_]+\\_).*', '\\1', gc_nitrous$Full_Sample_Name) # Remove every character after "_" 
        gc_nitrous$Sample_Name <- gsub("_", "", gc_nitrous$Short_Sample_Name) # Remove all of the "_"
        gc_nitrous <- subset(gc_nitrous, select = c("Sample_ID", "Sample_Name", "Area")) # Remove the extra columns hat you made 
      
   # CH4 and CO2 
        gc_raw_co2_and_ch4 <- read.table("Input_files/GC_Data/2023_07_25 GC - NSF Mixing SFID.txt", header = FALSE ,fill = TRUE)
        
        # seperate into two files based on the row numbers (this should be the same for every run if full)
        #     NOTE -- if we are going to be doing a bunch of runs that are not full I can re-code this to be cleaner 
        
        gc_methane <- gc_raw_co2_and_ch4[5:143,]  # seperate out just methane 
        colnames(gc_methane) <- c("Sample_ID",	"Full_Sample_Name", "Level_Number",	"Ret. Time",	"Area",	"Height",	"Conc.",	"Std. Conc.",	"Area%",	"Height%",	"Accuracy[%]",	"Cal. Point", "Col13", "Col14")
        gc_methane$Short_Sample_Name <- sub('([^_]+\\_).*', '\\1', gc_methane$Full_Sample_Name) # Remove every character after "_" 
        gc_methane$Sample_Name <- gsub("_", "", gc_methane$Short_Sample_Name) # Remove all of the "_"
        gc_methane <- subset(gc_methane, select = c("Sample_ID", "Sample_Name", "Area")) # Remove the extra columns hat you made 
        
        
        gc_carbondiox <- gc_raw_co2_and_ch4[153:291,]  # seperate out just carbon dioxide
        colnames(gc_carbondiox) <- c("Sample_ID",	"Full_Sample_Name", "Level_Number",	"Ret. Time",	"Area",	"Height",	"Conc.",	"Std. Conc.",	"Area%",	"Height%",	"Accuracy[%]",	"Cal. Point", "Col13", "Col14")
        gc_carbondiox$Short_Sample_Name <- sub('([^_]+\\_).*', '\\1', gc_carbondiox$Full_Sample_Name) # Remove every character after "_" 
        gc_carbondiox$Sample_Name <- gsub("_", "", gc_carbondiox$Short_Sample_Name) # Remove all of the "_"
        gc_carbondiox <- subset(gc_carbondiox, select = c("Sample_ID", "Sample_Name", "Area")) # Remove the extra columns hat you made 
        
        
        #For the CH4 and CO2 file you will also have to split the two gases 

# 2. Standard curves 
        # NOTE: working just with N2O for now because it is the simplist, I will come back through and add CO2 and CH4
    
  # Pull just the standards 
        gc_nitrous$Type <- ifelse(grepl("A-", gc_nitrous$Sample_Name), "sample", "standard")  #make a column that differentiates standards from samples, I had to do this with the "A-" because for some reason the code was funky about using $ 
        head(gc_nitrous)
        str(gc_nitrous)
        gc_nitrous_standards <- gc_nitrous[gc_nitrous$Type == "standard"]
        
        
        
        
        
        
# 3. Drift 