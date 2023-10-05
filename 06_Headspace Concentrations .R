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
      gc_raw_nitrous <- read.table("Input_Files/GC_Data/2023_10_04 - NSF Mixing082123rerun_PECD.txt", skip = 3, sep = "", dec = ".", fill = TRUE, col.names = c("Sample_ID",	"Full_Sample_Name", "Level_Number",	"Ret. Time",	"Area",	"Height",	"Conc.",	"Std. Conc.",	"Area%",	"Height%",	"Accuracy[%]",	"Cal. Point", "Col13", "Col14"))  #bring in data from GC you need to name the columns because the column names on the raw file have a # in them and that throws off R reading the row
      gc_nitrous <- subset(gc_raw_nitrous, select = c("Sample_ID", "Full_Sample_Name", "Area")) # subset to only the columns that you need 
      nrow(gc_nitrous)
      gc_nitrous <- gc_nitrous[2:(nrow(gc_nitrous) - 5),] # Remove the first row and the last 5 rows (these are stats that the GC generates that we don't need )
     
      # Remove the extra digits at the end of the sample names, these are also added by the GC and we don't need themt to do our caclulations 
        gc_nitrous$Short_Sample_Name <- sub('([^_]+\\_).*', '\\1', gc_nitrous$Full_Sample_Name) # Remove every character after "_" 
        gc_nitrous$Sample_Name <- gsub("_", "", gc_nitrous$Short_Sample_Name) # Remove all of the "_"
        gc_nitrous <- subset(gc_nitrous, select = c("Sample_ID", "Sample_Name", "Area")) # Remove the extra columns hat you made 
      
   # CH4 and CO2 
        gc_raw_co2_and_ch4 <- read.table("Input_files/GC_Data/2023_10_04 - NSF Mixing082123rerun_SFID.txt", header = FALSE ,fill = TRUE)
        
        # seperate into two files based on the row numbers (this should be the same for every run if full)
        #     NOTE -- if we are going to be doing a bunch of runs that are not full I can re-code this to be cleaner 
        #     Or if you are running not a full run just go in and check your own row numbers and add them 
        
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
    
  # Pull just the standards to make a seperate data frame 
        gc_nitrous$Type <- ifelse(grepl("A-", gc_nitrous$Sample_Name), "sample", "standard")  #make a column that differentiates standards from samples, I had to do this with the "A-" because for some reason the code was funky about using $ 
        head(gc_nitrous)
        str(gc_nitrous)
        gc_nitrous_standards <- gc_nitrous[gc_nitrous$Type == "standard" , ]
        gc_nitrous_standards <- subset(gc_nitrous_standards, Sample_Name != "zero" )
        
  # Seperate out to make columns for the concentrations of the standard gasses 
        
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
        middle_name <- substring(name, (nchar(first_name)+2), (nchar(name) - (nchar(last_name) + 1)) ) 
        
        
    # Figuring out naming 10/5
        # Pulling everything before the first _ 
       
        middle_name <- gsub(".*_.*", "\\1", name)
        
        # Example 1 
            x<- "\nTYPE:    School\nCITY:   ATLANTA\n\n\nCITY:   LAS VEGAS\n\n" 
            unlist(regmatches(x, gregexpr("CITY:\\s*\\K.*", x, perl=TRUE)))
            unlist(regmatches(name, gregexpr("_*\\K.*", name, perl=TRUE)))
        
        
        # Example 2 
            a <- " anything goes here, STR1 GET_ME STR2, anything goes here"
            res <- str_match(a, "STR1\\s*(.*?)\\s*STR2")
            answer <- res[,2]
            
            b <- " anything goes here, psl1 COFFEE psl2, anything goes here"
            res <- str_match(b, "psl1\\s*(.*?)\\s*psl2")
            answer <- res[,2]
            
            c <- " anything goes here, _ COFFEE _, anything goes here"
            res <- str_match(c, "_\\s*(.*?)\\s*_")
            answer <- res[,2]
            
            res <- str_match(a, "_\\s*(.*?)\\s*_")
            
        # Example 3 
            test <- " anything goes here, STR1 GET_ME STR2, anything goes here STR1 GET_ME2 STR2"
            pattern <- "STR1\\s*(.*?)\\s*STR2"
            result <- regmatches(test, regexec(pattern, test))
            result[[1]][2]
            
            test <- " anything goes here, psl1 COFFEE psl2, anything goes here STR1 GET_ME2 STR2"
            pattern <- "psl1\\s*(.*?)\\s*psl2"
            result <- regmatches(test, regexec(pattern, test))
            result[[1]][2]
            
            test <- " anything goes here, _ COFFEE _, anything goes here STR1 GET_ME2 STR2"
            pattern <- "_\\s*(.*?)\\s*_"
            result <- regmatches(test, regexec(pattern, test))
            result[[1]][2]
            
            name <- as.character("Katie_Alta_Gannon")
            pattern <- "_\\s*(.*?)\\s*_"
            result <- regmatches(name, regexec(pattern, name))
            result[[1]][2] # This works to pull the name in the middle of the two _ 

        
        
        standard_name <- as.character(gc_nitrous_standards[11,2])

        sn <- "8_12_16"
        
        conc_co2 <- sub('([^$]+\\$).*', '\\1', standard_name)  #everything before the first $ including the $
        conc_co2 <- sub('([^_]+\\_).*', '\\1', sn)
        
        a <- "45216 Walnut Avenue Mary's Bake Shop"
        gsub("(Avenue).*", "\\1", a)
        
        b <- "45216 Walnut $ Mary's Bake Shop"
        gsub("$.*", "\\1", b)
        
        conc_co2 <- gsub("($).*", "\\1", a)
        gsub("(Avenue).*", "\\1", a)
        
        ?sub()
        
        # Understanding language 
        psl <- "I_Love_Halloween"
        conc_co2 <-  sub("I_*", "", psl) #anything after "I*" 
        
        
        
        pumpkin <- "J$A$C?K"
        holloween <- sub("$", "_", pumpkin) #everything before the $ including the $ sign
        halloween <- grep("_Love_", psl)
        holloween <- sub('([^$]+\\$).*', '\\1', pumpkin) #everything before the $ including the $ sign 
        
        
# 3. Drift 