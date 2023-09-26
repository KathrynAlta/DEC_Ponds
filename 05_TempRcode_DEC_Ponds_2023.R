## DEC Ponds 2023 Temp Plots ####
## Code started by MAH May 2023 #

## Load packages ####
library(tidyverse)
library(scales)
library(cowplot)

## Load data ####
setwd("C:/Users/mah543/Dropbox/Cornell/Research/DEC_CarbonGrant/FarmResidPonds/HOBO_Data_2023")  #work
setwd("C:/Users/Meredith/Dropbox/Cornell/Research/DEC_CarbonGrant/FarmResidPonds/HOBO_Data_2023") #Home

boyce <- read.csv("Boyce_compiled.csv", strip.white=T, na.strings="na")
edwards <- read.csv("Edwards_compiled.csv", strip.white=T, na.strings="na")
harrison <- read.csv("Harrison_compiled.csv", strip.white=T, na.strings="na")
howarth <- read.csv("Howarth_compiled.csv", strip.white=T, na.strings="na")
levine <- read.csv("Levine_compiled.csv", strip.white=T, na.strings="NA")
mtpleasantse <- read.csv("MtPleasantSE_compiled.csv", strip.white=T, na.strings="na")
shelterbelt <- read.csv("Shelterbelt_compiled.csv", strip.white=T, na.strings="na")
white <- read.csv("White_compiled.csv", strip.white=T, na.strings="na")

#add column with pond name
boyce$pond <- "Boyce"
edwards$pond <- "Edwards"
howarth$pond <- "Boyce"
levine$pond <- "Levine"
mtpleasantse$pond <- "MtPleasantSE"
harrison$pond <- "Harrison"
shelterbelt$pond <- "Shelterbelt"
white$pond <- "White"

min(boyce$wtr_bottom_t18)        #9.472
min(edwards$wtr_bottom_t12)      #9.176
min(howarth$wtr_bottom_t10)      #8.978
min(levine$wtr_bottom_t6, na.rm=TRUE)      #10.846
min(mtpleasantse$wtr_bottom_t24) #8.779
min(harrison$wtr_bottom_t3)      #11.625
min(shelterbelt$wtr_bottom_t21)  #8.581
min(white$wtr_bottom_t15)        #12.883


max(boyce$wtr_top_t16)        #31.574
max(edwards$wtr_top_t7)       #32.086
max(howarth$wtr_top_t8)       #31.472
max(levine$wtr_top_t4 )       #33.743
max(mtpleasantse$wtr_top_t22) #34.163
max(harrison$wtr_top_t1)      #30.963
max(shelterbelt$wtr_top_t19)  #30.054
max(white$wtr_top_t13)        #32.394


#Make Plots ####


#*Boyce ####
head(boyce)
tail(boyce)
str(boyce)
boyce$datetime2 <- as.POSIXct(boyce$datetime, 
                              '%m/%d/%Y %H:%M', tz="US/Eastern")

boyce_long <- boyce %>% pivot_longer(
  cols = starts_with("wtr_"),
  names_to = "depth",
  names_prefix = "wtr_",
  values_to = "temp",
  values_drop_na = TRUE)
head(boyce_long)

boyce_temp <- boyce_long  %>% 
  ggplot(aes(x=datetime2, y=temp, color=depth)) + 
  #geom_hline(yintercept=100, linetype='dotted', col = 'blue')+
  geom_point(size=0.5) + #geom_path()+
  scale_x_datetime(breaks=date_breaks(width="7 days"), 
                   labels=date_format("%m/%d", tz="America/new_york"))+ theme_bw() +
  theme(text = element_text(size=12), axis.text.x = element_text(angle=-40))+ 
  xlab("Time") + ylab("Temperature") + ylim(c(9,35))+
  theme(legend.position = c(.1,.85))
boyce_temp 



#*Edwards ####
edwards$datetime2 <- as.POSIXct(edwards$datetime_proposed, 
                              '%m/%d/%Y %H:%M', tz="US/Eastern")

edwards_long <- edwards %>% pivot_longer(
  cols = starts_with("wtr_"),
  names_to = "depth",
  names_prefix = "wtr_",
  values_to = "temp",
  values_drop_na = TRUE)
head(edwards_long)

edwards_temp <- edwards_long  %>% subset(datetime2 >= "2023-04-25 17:30") %>%
  ggplot(aes(x=datetime2, y=temp, color=depth)) + 
  #geom_hline(yintercept=100, linetype='dotted', col = 'blue')+
  geom_point(size=0.5) + #geom_path()+
  scale_x_datetime(breaks=date_breaks(width="7 days"), 
                   labels=date_format("%m/%d", tz="America/new_york"))+ theme_bw() +
  theme(text = element_text(size=12), axis.text.x = element_text(angle=-40))+ 
  xlab("Time") + ylab("Temperature") + ylim(c(9,35))+
  theme(legend.position = c(.1,.85))
edwards_temp 




#*Harrison ####
harrison$datetime2 <- as.POSIXct(harrison$datetime_proposed, 
                              '%m/%d/%Y %H:%M', tz="US/Eastern")

harrison_long <- harrison %>% pivot_longer(
  cols = starts_with("wtr_"),
  names_to = "depth",
  names_prefix = "wtr_",
  values_to = "temp",
  values_drop_na = TRUE)
head(harrison_long)

harrison_temp <- harrison_long  %>% subset(datetime2 >= "2023-04-24 14:00") %>%
  ggplot(aes(x=datetime2, y=temp, color=depth)) + 
  geom_point(size=0.5) + #geom_path()+
  scale_x_datetime(breaks=date_breaks(width="7 days"), 
                   labels=date_format("%m/%d", tz="America/new_york"))+ theme_bw() +
  theme(text = element_text(size=12), axis.text.x = element_text(angle=-40))+ 
  xlab("Time") + ylab("Temperature") + ylim(c(9,35))+
  theme(legend.position = c(.1,.85))
harrison_temp 


#*Howarth ####
howarth$datetime2 <- as.POSIXct(howarth$datetime_proposed, 
                              '%m/%d/%Y %H:%M', tz="US/Eastern")

howarth_long <- howarth %>% pivot_longer(
  cols = starts_with("wtr_"),
  names_to = "depth",
  names_prefix = "wtr_",
  values_to = "temp",
  values_drop_na = TRUE)
head(howarth_long)

howarth_temp <- howarth_long  %>% subset(datetime2 >= "2023-04-25 12:40") %>%
  ggplot(aes(x=datetime2, y=temp, color=depth)) + 
  geom_point(size=0.5) + #geom_path()+
  scale_x_datetime(breaks=date_breaks(width="7 days"), 
                   labels=date_format("%m/%d", tz="America/new_york"))+ theme_bw() +
  theme(text = element_text(size=12), axis.text.x = element_text(angle=-40))+ 
  xlab("Time") + ylab("Temperature") + ylim(c(9,35))+
  theme(legend.position = c(.1,.85))
howarth_temp 



#*Levine ####
levine$datetime2 <- as.POSIXct(levine$datetime_proposed, 
                                '%m/%d/%Y %H:%M', tz="US/Eastern")
str(levine)

levine_long <- levine %>% pivot_longer(
  cols = starts_with("wtr_"),
  names_to = "depth",
  names_prefix = "wtr_",
  values_to = "temp",
  values_drop_na = TRUE)
head(levine_long)

levine_temp <- levine_long  %>% subset(datetime2 >= "2023-04-24 16:20") %>%
  ggplot(aes(x=datetime2, y=temp, color=depth)) + 
  #geom_hline(yintercept=100, linetype='dotted', col = 'blue')+
  geom_point(size=0.5) + #geom_path()+
  scale_x_datetime(breaks=date_breaks(width="7 days"), 
                   labels=date_format("%m/%d", tz="America/new_york"))+ theme_bw() +
  theme(text = element_text(size=12), axis.text.x = element_text(angle=-40))+ 
  xlab("Time") + ylab("Temperature") + ylim(c(9,35))+
  theme(legend.position = c(.1,.85))
levine_temp 


#*Mt Pleasant SE ####
head(mtpleasantse)
str(mtpleasantse)
mtpleasantse$datetime2 <- as.POSIXct(mtpleasantse$datetime_proposed, 
                               '%m/%d/%Y %H:%M', tz="US/Eastern")

mtpleasantse_long <- mtpleasantse %>% pivot_longer(
  cols = starts_with("wtr_"),
  names_to = "depth",
  names_prefix = "wtr_",
  values_to = "temp",
  values_drop_na = TRUE)
head(mtpleasantse_long)

mtpleasantse_temp <- mtpleasantse_long  %>% subset(datetime2 >= "2023-04-27 19:00") %>%
  ggplot(aes(x=datetime2, y=temp, color=depth)) + 
  geom_point(size=0.5) + #geom_path()+
  scale_x_datetime(breaks=date_breaks(width="7 days"), 
                   labels=date_format("%m/%d", tz="America/new_york"))+ theme_bw() +
  theme(text = element_text(size=12), axis.text.x = element_text(angle=-40))+ 
  xlab("Time") + ylab("Temperature") + ylim(c(9,35))+
  theme(legend.position = c(.1,.85))
mtpleasantse_temp 


#*Shelterbelt ####
shelterbelt$datetime2 <- as.POSIXct(shelterbelt$datetime_proposed, 
                              '%m/%d/%Y %H:%M', tz="US/Eastern")

shelterbelt_long <- shelterbelt %>% pivot_longer(
  cols = starts_with("wtr_"),
  names_to = "depth",
  names_prefix = "wtr_",
  values_to = "temp",
  values_drop_na = TRUE)
head(shelterbelt_long)

shelterbelt_temp <- shelterbelt_long  %>% subset(datetime2 >= "2023-04-27 14:00") %>%
  ggplot(aes(x=datetime2, y=temp, color=depth)) + 
  geom_point(size=0.5) + #geom_path()+
  scale_x_datetime(breaks=date_breaks(width="7 days"), 
                   labels=date_format("%m/%d", tz="America/new_york"))+ theme_bw() +
  theme(text = element_text(size=12), axis.text.x = element_text(angle=-40))+ 
  xlab("Time") + ylab("Temperature") + ylim(c(9,35))+
  theme(legend.position = c(.1,.85))
shelterbelt_temp


#*White ####
white$datetime2 <- as.POSIXct(white$datetime_proposed, 
                                    '%m/%d/%Y %H:%M', tz="US/Eastern")

white_long <- white %>% pivot_longer(
  cols = starts_with("wtr_"),
  names_to = "depth",
  names_prefix = "wtr_",
  values_to = "temp",
  values_drop_na = TRUE)
head(white_long)

white_temp <- white_long  %>% ggplot(aes(x=datetime2, y=temp, color=depth)) + 
  #geom_hline(yintercept=100, linetype='dotted', col = 'blue')+
  geom_point(size=0.5) + #geom_path()+
  scale_x_datetime(breaks=date_breaks(width="7 days"), 
                   labels=date_format("%m/%d", tz="America/new_york"))+ theme_bw() +
  theme(text = element_text(size=12), axis.text.x = element_text(angle=-40))+ 
  xlab("Time") + ylab("Temperature") + ylim(c(9,35))+
  theme(legend.position = c(.1,.85))
white_temp 

