#### Load libraries and set WD ####

library(readxl);library(FluxSeparator)

setwd("/Users/jonas/Library/CloudStorage/OneDrive-SyddanskUniversitet/DEC ponds/Data")

#### read sensor calibration file and read in data ####

read_excel("/Users/jonas/Library/CloudStorage/OneDrive-SyddanskUniversitet/DEC ponds/sensor_calibration_constants.xlsx") -> model_coef

list.files(pattern = ".csv") %>% 
  tibble(path = ., 
         sensor = case_when(str_detect(path,"J1") ~ "J1",
                            str_detect(path,"J2") ~ "J2",
                            str_detect(path,"J3") ~ "J3")) %>% 
  read_CH4_files() %>% 
  mutate(month = case_when(str_detect(files,"100923") ~ "Sep",
                           str_detect(files, "June") ~ "Jun",
                           T ~ "May"),
         site = case_when(str_detect(files,"BoyceANDWhite") ~ "BoyceANDWhite",
                          str_detect(files,"Levine") ~"HarrisonANDLevine",
                          T ~ "Howarth"),
         sep = round_date(datetime, "2 hours")) %>%         ## A little trick was needed here, as the data had the same PumpCycle numbers several times
  filter(pred_CH4 < 1000) -> data 

#### Looking at Howarth May data ####

data %>% 
  filter(site == "Howarth",
         PumpCycle > 1) %>%
  group_by(sensor,site, sep,PumpCycle) %>%
  mutate(PumpCycle = cur_group_id(),
         station = sensor) %>% 
  filter(row_number() > 5) -> data_How

data_How %>% 
  distinct(station)-> How_translate                       ## Need a translation to save site

data_How %>% 
  ebullitive_flux(runvar_cutoff = 0.05,
                  top_selection = "last",
                  IndexSpan = 40) -> ebul_How

data_How %>% 
  diffusive_flux(cutoff_start_value = 50,
                 runvar_cutoff = 0.05,
                 remove_observations_prior = 50,
                 show_plots = F)

#### Looking at HarrisonANDLevine June data ####

data %>% 
  filter(site == "HarrisonANDLevine") %>%
  group_by(sensor,site, sep,PumpCycle) %>%
  mutate(PumpCycle = cur_group_id(),
         station = sensor) %>% 
  filter(row_number() > 5) -> data_HaL
  
data_HaL %>% 
  distinct(station) -> HaL_translate

data_HaL %>% 
  ebullitive_flux(runvar_cutoff = 1,
                  top_selection = "last",
                  show_plots = F) %>% 
  left_join(HaL_translate)

data_HaL %>% 
  diffusive_flux(cutoff_start_value = 50,
                 runvar_cutoff = 1,
                 remove_observations_prior = 200,
                 show_plots = F)

write_csv(data_HaL, "data_test.csv")
