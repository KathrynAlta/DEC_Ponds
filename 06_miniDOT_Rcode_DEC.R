# DO Data for 2023 DEC Ponds: Intensive ####
## Code started by MAH 20 Nov 2023 ####


#load packages ####
library(ggplot2)
library(tidyverse)
library(scales)


# Load data ####
do_edwards <- read.csv("miniDOT_data/RawData/Edwards_2023.csv", strip.white=T, na.strings="na")
do_harrison <- read.csv("miniDOT_data/RawData/Harrison_2023.csv", strip.white=T, na.strings="na")
do_howarth <- read.csv("miniDOT_data/RawData/Howarth_2023.csv", strip.white=T, na.strings="na")
do_levine <- read.csv("miniDOT_data/RawData/Levine_2023.csv", strip.white=T, na.strings="na")
do_mtpleasantse <- read.csv("miniDOT_data/RawData/MtPleasantSE_2023.csv", strip.white=T, na.strings="na")
do_shelterbelt <- read.csv("miniDOT_data/RawData/Shelterbelt_2023.csv", strip.white=T, na.strings="na")
do_white <- read.csv("miniDOT_data/RawData/White_2023.csv", strip.white=T, na.strings="na")
do_boyce <- read.csv("miniDOT_data/RawData/Boyce_2023.csv", strip.white=T, na.strings="na")



head(do_edwards)
head(do_harrison)
head(do_howarth)
head(do_levine)
head(do_mtpleasantse)
head(do_shelterbelt)
head(do_white)
head(do_boyce)

#set date-time format####
do_edwards$time2 <- as.POSIXct(do_edwards$datetime, "%Y-%m-%d %H:%M", tz="America/new_york")
do_harrison$time2 <- as.POSIXct(do_harrison$datetime, "%Y-%m-%d %H:%M", tz="America/new_york")
do_howarth$time2 <- as.POSIXct(do_howarth$datetime, "%Y-%m-%d %H:%M", tz="America/new_york")
do_levine$time2 <- as.POSIXct(do_levine$datetime, "%Y-%m-%d %H:%M", tz="America/new_york")
do_mtpleasantse$time2 <- as.POSIXct(do_mtpleasantse$datetime, "%Y-%m-%d %H:%M", tz="America/new_york")
do_shelterbelt$time2 <- as.POSIXct(do_shelterbelt$datetime, "%Y-%m-%d %H:%M", tz="America/new_york")
do_white$time2 <- as.POSIXct(do_white$datetime, "%Y-%m-%d %H:%M", tz="America/new_york")
do_boyce$time2 <- as.POSIXct(do_boyce$datetime, "%Y-%m-%d %H:%M", tz="America/new_york")



## PLOTS ####

##*Edwards ####
head(do_edwards)

plot_do_edwards <- do_edwards %>% 
  ggplot(aes(x=time2, y=do_perc)) + 
  geom_hline(yintercept=100, linetype='dotted', col = 'blue')+
  geom_point(size=0.5) + #geom_path()+
  scale_x_datetime(breaks=date_breaks(width="7 days"), 
                   labels=date_format("%m/%d", tz="America/new_york"))+ theme_bw() +
  theme(text = element_text(size=12), axis.text.x = element_text(angle=-40))+ 
  xlab("Time") + ylab("DO percent") #+ ylim(c(90,160))
plot_do_edwards 



##*Harrison ####
head(do_harrison)

plot_do_harrison <- do_harrison %>% 
  ggplot(aes(x=time2, y=do_perc)) + 
  geom_hline(yintercept=100, linetype='dotted', col = 'blue')+
  geom_point(size=0.5) + #geom_path()+
  scale_x_datetime(breaks=date_breaks(width="7 days"), 
                   labels=date_format("%m/%d", tz="America/new_york"))+ theme_bw() +
  theme(text = element_text(size=12), axis.text.x = element_text(angle=-40))+ 
  xlab("Time") + ylab("DO percent") #+ ylim(c(90,160))
plot_do_harrison


##*Howarth ####
head(do_howarth)

plot_do_howarth <- do_howarth %>% 
  ggplot(aes(x=time2, y=do_perc)) + 
  geom_hline(yintercept=100, linetype='dotted', col = 'blue')+
  geom_point(size=0.5) + #geom_path()+
  scale_x_datetime(breaks=date_breaks(width="7 days"), 
                   labels=date_format("%m/%d", tz="America/new_york"))+ theme_bw() +
  theme(text = element_text(size=12), axis.text.x = element_text(angle=-40))+ 
  xlab("Time") + ylab("DO percent") #+ ylim(c(90,160))
plot_do_howarth 



##*Levine ####
head(do_levine)

plot_do_levine <- do_levine %>% 
  ggplot(aes(x=time2, y=do_perc)) + 
  geom_hline(yintercept=100, linetype='dotted', col = 'blue')+
  geom_point(size=0.5) + #geom_path()+
  scale_x_datetime(breaks=date_breaks(width="7 days"), 
                   labels=date_format("%m/%d", tz="America/new_york"))+ theme_bw() +
  theme(text = element_text(size=12), axis.text.x = element_text(angle=-40))+ 
  xlab("Time") + ylab("DO percent") #+ ylim(c(90,160))
plot_do_levine



##*Mt Pleasant SE ####
head(do_mtpleasantse)

plot_do_mtpleasantse <- do_mtpleasantse %>% 
  ggplot(aes(x=time2, y=do_perc)) + 
  geom_hline(yintercept=100, linetype='dotted', col = 'blue')+
  geom_point(size=0.5) + #geom_path()+
  scale_x_datetime(breaks=date_breaks(width="7 days"), 
                   labels=date_format("%m/%d", tz="America/new_york"))+ theme_bw() +
  theme(text = element_text(size=12), axis.text.x = element_text(angle=-40))+ 
  xlab("Time") + ylab("DO percent") #+ ylim(c(90,160))
plot_do_mtpleasantse



##*Shelterbelt ####
head(do_shelterbelt)

plot_do_shelterbelt <- do_shelterbelt %>% 
  ggplot(aes(x=time2, y=do_perc)) + 
  geom_hline(yintercept=100, linetype='dotted', col = 'blue')+
  geom_point(size=0.5) + #geom_path()+
  scale_x_datetime(breaks=date_breaks(width="7 days"), 
                   labels=date_format("%m/%d", tz="America/new_york"))+ theme_bw() +
  theme(text = element_text(size=12), axis.text.x = element_text(angle=-40))+ 
  xlab("Time") + ylab("DO percent") #+ ylim(c(90,160))
plot_do_shelterbelt



##* White ####
head(do_white)

plot_do_white <- do_white %>% 
  ggplot(aes(x=time2, y=do_perc)) + 
  geom_hline(yintercept=100, linetype='dotted', col = 'blue')+
  geom_point(size=0.5) + #geom_path()+
  scale_x_datetime(breaks=date_breaks(width="7 days"), 
                   labels=date_format("%m/%d", tz="America/new_york"))+ theme_bw() +
  theme(text = element_text(size=12), axis.text.x = element_text(angle=-40))+ 
  xlab("Time") + ylab("DO percent") #+ ylim(c(90,160))
plot_do_white



##* boyce ####
head(do_boyce)

plot_do_boyce <- do_boyce %>% 
  ggplot(aes(x=time2, y=do_perc)) + 
  geom_hline(yintercept=100, linetype='dotted', col = 'blue')+
  geom_point(size=0.5) + #geom_path()+
  scale_x_datetime(breaks=date_breaks(width="7 days"), 
                   labels=date_format("%m/%d", tz="America/new_york"))+ theme_bw() +
  theme(text = element_text(size=12), axis.text.x = element_text(angle=-40))+ 
  xlab("Time") + ylab("DO percent") #+ ylim(c(90,160))
plot_do_boyce



## Merge data ####

do_edwards$pond <- as.factor("edwards")
do_harrison$pond <- as.factor("harrison")
do_howarth$pond <- as.factor("howarth")
do_levine$pond <- as.factor("levine")
do_mtpleasantse$pond <- as.factor("mtpleasantse")
do_shelterbelt$pond <- as.factor("shelterbelt")
do_white$pond <- as.factor("white")
do_boyce$pond <- as.factor("boyce")


data <- bind_rows(do_edwards, do_harrison, do_howarth, do_levine, 
                  do_mtpleasantse, do_shelterbelt, do_white, do_boyce) 
head(data)
tail(data)

plot_all <- data %>% 
  ggplot(aes(x=time2, y=do_perc, color=pond)) + 
  geom_hline(yintercept=100, linetype='dotted')+
  geom_point(size=0.5) + 
  geom_smooth()+
  scale_x_datetime(breaks=date_breaks(width="14 days"), 
                   labels=date_format("%m/%d", tz="America/new_york"))+ theme_bw() +
  theme(text = element_text(size=12), axis.text.x = element_text(angle=-40))+
  scale_colour_brewer(palette="Set1")+
  xlab("Time") + ylab("DO percent saturation") #+ ylim(c(90,160))
plot_all  + facet_wrap(~pond)


levels(data$pond)
plot_all <- data %>% 
  ggplot(aes(x=time2, y=do_perc, color=pond)) + 
  geom_hline(yintercept=100, linetype='dotted')+
  geom_point(size=0.5) + 
  geom_smooth()+
  scale_x_datetime(breaks=date_breaks(width="14 days"), 
                   labels=date_format("%m/%d", tz="America/new_york"))+ theme_bw() +
  theme(text = element_text(size=12), axis.text.x = element_text(angle=-40))+
  scale_colour_brewer(palette="Set1")+
  xlab("Time") + ylab("DO percent saturation") +
  scale_color_manual(values=c("#88A0A8", "#88A0A8", "#0A1045", "#88A0A8", 
                              "#0A1045", "#0A1045", "#0A1045", "#88A0A8")) 
plot_all  + facet_wrap(~pond)
