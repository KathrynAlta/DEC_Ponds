## Load packages ####
library(tidyverse)
library(scales)
library(cowplot)

## Load data ####
setwd("C:/Users/mah543/Dropbox/Cornell/Research/DEC_CarbonGrant/FarmResidPonds/EurekaData_2023")  #work
setwd("C:/Users/Meredith/Dropbox/Cornell/Research/DEC_CarbonGrant/FarmResidPonds/EurekaData_2023") #Home
setwd("C:/Users/pb577/OneDrive - Cornell University/Documents/DEC_Ponds/DEC_Ponds/Eureka_Data") #work PB

ext_round1 <- read.csv("Eureka_Extensive_Round1_compiled.csv", strip.white=T, na.strings="NA") 
ext_round2 <- read.csv("Eureka_Extensive_Round2_compiled.csv", strip.white=T, na.strings="NA") 

ext_round1$sampleround <- "Round 1"
ext_round2$sampleround <- "Round 2"

head(ext_round1)
head(ext_round2)


##* Bind datasets ####
eureka <- rbind(ext_round1,ext_round2)
head(eureka)
tail(eureka)

eureka$date2 <- as.Date(eureka$date, '%m/%d/%Y')
head(eureka)
str(eureka)

eureka$pond <- as.factor(as.character(eureka$pond))



## PLOTS ####
temp <- eureka %>% 
  ggplot(aes(x=temp_c, y=depth_m)) + 
  geom_point(aes(color=sampleround))+ geom_path(aes(color=sampleround))+
  scale_y_reverse()+  ylab("Depth (m)") + theme_bw()  +
  scale_color_manual(values=c("#88A0A8", "#0A1045"))
temp + facet_wrap(~pond, ncol=6) #scales="free_x") 


do_perc <- eureka %>% 
  ggplot(aes(x=do_perc, y=depth_m)) +
  geom_vline(xintercept=100, linetype="dashed") +
  geom_point(aes(color=sampleround))+ geom_path(aes(color=sampleround))+
  scale_y_reverse()+  ylab("Depth (m)") + theme_bw()  +
  scale_color_manual(values=c("#88A0A8", "#0A1045"))
do_perc + facet_wrap(~pond, ncol=6) #scales="free_x") 


ph <- eureka %>% 
  ggplot(aes(x=ph, y=depth_m)) +
  geom_point(aes(color=sampleround))+ geom_path(aes(color=sampleround))+
  scale_y_reverse()+  ylab("Depth (m)") + theme_bw()  +
  scale_color_manual(values=c("#88A0A8", "#0A1045"))
ph + facet_wrap(~pond, ncol=6) #scales="free_x") 


cond <- eureka %>% 
  ggplot(aes(x=cond, y=depth_m)) +
  geom_point(aes(color=sampleround))+ geom_path(aes(color=sampleround))+
  scale_y_reverse()+  ylab("Depth (m)") + theme_bw()  +
  scale_color_manual(values=c("#88A0A8", "#0A1045"))
cond + facet_wrap(~pond, ncol=6) #scales="free_x") 


turb <- eureka %>% 
  ggplot(aes(x=turb, y=depth_m)) +
  geom_point(aes(color=sampleround))+ geom_path(aes(color=sampleround))+
  scale_y_reverse()+  ylab("Depth (m)") + theme_bw()  +
  scale_color_manual(values=c("#88A0A8", "#0A1045"))
turb + facet_wrap(~pond, ncol=6) #scales="free_x") 


chla <- eureka %>% #filter(chla <100) %>%
  ggplot(aes(x=chla_ugl, y=depth_m)) +
  geom_point(aes(color=sampleround))+ geom_path(aes(color=sampleround))+
  scale_y_reverse()+  ylab("Depth (m)") + theme_bw()  +
  scale_color_manual(values=c("#88A0A8", "#0A1045"))
chla + facet_wrap(~pond, ncol=6) #scales="free_x") 


nh4 <- eureka %>% 
  ggplot(aes(x=nh4_mgl, y=depth_m)) +
  geom_point(aes(color=sampleround))+ geom_path(aes(color=sampleround))+
  scale_y_reverse()+  ylab("Depth (m)") + theme_bw()  +
  scale_color_manual(values=c("#88A0A8", "#0A1045"))
nh4 + facet_wrap(~pond, ncol=6) #scales="free_x")  


no3 <- eureka %>% 
  ggplot(aes(x=no3_mgl, y=depth_m)) +
  geom_point(aes(color=sampleround))+ geom_path(aes(color=sampleround))+
  scale_y_reverse()+  ylab("Depth (m)") + theme_bw()  +
  scale_color_manual(values=c("#88A0A8", "#0A1045"))
no3 + facet_wrap(~pond, ncol=6) #scales="free_x")  


orp <- eureka %>% 
  ggplot(aes(x=orp, y=depth_m)) +
  geom_point(aes(color=sampleround))+ geom_path(aes(color=sampleround))+
  scale_y_reverse()+  ylab("Depth (m)") + theme_bw()  +
  scale_color_manual(values=c("#88A0A8", "#0A1045"))
orp + facet_wrap(~pond, ncol=6) #scales="free_x")  



par <- eureka %>% filter(depth_m >= 0.5) %>%
  ggplot(aes(x=par_umol, y=depth_m)) +
  geom_point(aes(color=sampleround))+ geom_path(aes(color=sampleround))+
  scale_y_reverse()+  ylab("Depth (m)") + theme_bw()  +
  scale_color_manual(values=c("#88A0A8", "#0A1045"))
par + facet_wrap(~pond, ncol=6) #scales="free_x")  


## Modified figures for quarterly report #6
eureka_mod <- eureka
eureka_mod$pond <- as.character(eureka_mod$pond)

eureka_mod["pond"][eureka_mod["pond"] == "Applegate"] <- "1"
eureka_mod["pond"][eureka_mod["pond"] == "Aquadro"] <- "2"
eureka_mod["pond"][eureka_mod["pond"] == "Barber"] <- "3"
eureka_mod["pond"][eureka_mod["pond"] == "Carpenter"] <- "4"
eureka_mod["pond"][eureka_mod["pond"] == "Collmer"] <- "5"
eureka_mod["pond"][eureka_mod["pond"] == "Conley"] <- "6"
eureka_mod["pond"][eureka_mod["pond"] == "Dybowski"] <- "7"
eureka_mod["pond"][eureka_mod["pond"] == "Ecovillage"] <- "8"
eureka_mod["pond"][eureka_mod["pond"] == "English_Deep"] <- "9"
eureka_mod["pond"][eureka_mod["pond"] == "English_Shallow"] <- "10"
eureka_mod["pond"][eureka_mod["pond"] == "Engst"] <- "11"
eureka_mod["pond"][eureka_mod["pond"] == "Hahn"] <- "12"
eureka_mod["pond"][eureka_mod["pond"] == "Longhouse"] <- "13"
eureka_mod["pond"][eureka_mod["pond"] == "Lucas"] <- "14"
eureka_mod["pond"][eureka_mod["pond"] == "Marks"] <- "15"
eureka_mod["pond"][eureka_mod["pond"] == "Mt Pleasant NE"] <- "16"
eureka_mod["pond"][eureka_mod["pond"] == "Rogers"] <- "17"
eureka_mod["pond"][eureka_mod["pond"] == "StickAndStone"] <- "18"
eureka_mod["pond"][eureka_mod["pond"] == "Vesa"] <- "19"
eureka_mod["pond"][eureka_mod["pond"] == "Walnut_Ridge"] <- "20"


do_perc <- eureka_mod %>% 
  ggplot(aes(x=do_perc, y=depth_m)) +
  geom_vline(xintercept=100, linetype="dashed") +
  geom_point(aes(color=sampleround))+ geom_path(aes(color=sampleround))+
  scale_y_reverse()+  ylab("Depth (m)") + theme_bw()  +
  scale_color_manual(values=c("#88A0A8", "#0A1045"))
do_perc + facet_wrap(~pond, ncol=6)+
  theme(text = element_text(size=30), axis.text.x = element_text(angle=-40))#scales="free_x")+








