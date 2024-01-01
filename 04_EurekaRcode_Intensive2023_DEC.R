## Load packages ####
library(tidyverse)
library(scales)
library(cowplot)

## Load data ####

round1 <- read.csv("DEC_Eureka_Intensive_Round1_compiled.csv", strip.white=T, na.strings="NA") 
round2 <- read.csv("DEC_Eureka_Intensive_Round2_compiled.csv", strip.white=T, na.strings="NA") 
round3 <- read.csv("DEC_Eureka_Intensive_Round3_compiled.csv", strip.white=T, na.strings="NA") 
round4 <- read.csv("DEC_Eureka_Intensive_Round4_compiled.csv", strip.white=T, na.strings="NA") 
round5 <- read.csv("DEC_Eureka_Intensive_Round5_compiled.csv", strip.white=T, na.strings="NA") 
round6 <- read.csv("DEC_Eureka_Intensive_Round6_compiled.csv", strip.white=T, na.strings="NA") 
round7 <- read.csv("DEC_Eureka_Intensive_Round7_compiled.csv", strip.white=T, na.strings="NA") 

round1$sampleround <- "Round 1, April"
round2$sampleround <- "Round 2, May"
round3$sampleround <- "Round 3, June"
round4$sampleround <- "Round 4, July"
round5$sampleround <- "Round 5, August"
round6$sampleround <- "Round 6, September"
round7$sampleround <- "Round 7, October"

head(round1)
head(round2)
head(round3)
head(round4)
head(round5)
head(round6)
head(round7)

##* Bind datasets ####
eureka <- rbind(round1,round2,round3,round4,round5, round6, round7)
head(eureka)
tail(eureka)

eureka$date2 <- as.Date(eureka$date, '%m/%d/%Y')
head(eureka)
str(eureka)

eureka$pond <- as.factor(as.character(eureka$pond))


##* Add treatment ####
eureka$treatment <- ifelse(eureka$pond=="Boyce" | eureka$pond=="Edwards" | 
                             eureka$pond=="Harrison" | eureka$pond=="Levine", "Forest", "Farm")


## PLOTS ####
temp <- eureka %>% 
  ggplot(aes(x=temp_c, y=depth_m)) + 
  geom_point(aes(color=pond))+ geom_path(aes(color=pond))+
  scale_y_reverse()+  ylab("Depth (m)") + theme_bw()  
temp + facet_wrap(~sampleround, scales="free_x") 


do_perc <- eureka %>% 
  ggplot(aes(x=do_perc, y=depth_m, color=pond)) + 
  geom_point(aes(color=pond))+ geom_path(aes(color=pond))+
  scale_y_reverse()+  ylab("Depth (m)") + theme_bw() +
  geom_vline(xintercept=100, linetype="dashed")  
  #scale_color_manual(values=c("#88A0A8", "#88A0A8", "#88A0A8", "#0A1045", 
   #                           "#88A0A8", "#0A1045", "#0A1045", "#0A1045"))
do_perc + facet_wrap(~sampleround) 


ph <- eureka %>% 
  ggplot(aes(x=ph, y=depth_m, color=pond)) + 
  geom_point(aes(color=pond))+ geom_path(aes(color=pond))+
  scale_y_reverse()+  ylab("Depth (m)") + theme_bw() +
  scale_color_manual(values=c("#88A0A8", "#88A0A8", "#88A0A8", "#0A1045", 
                              "#88A0A8", "#0A1045", "#0A1045", "#0A1045"))
ph + facet_wrap(~sampleround) 


cond <- eureka %>% 
  ggplot(aes(x=cond, y=depth_m, color=pond)) + 
  geom_point(aes(color=pond))+ geom_path(aes(color=pond))+
  scale_y_reverse()+  ylab("Depth (m)") + theme_bw() +
  scale_color_manual(values=c("#88A0A8", "#88A0A8", "#88A0A8", "#0A1045", 
                              "#88A0A8", "#0A1045", "#0A1045", "#0A1045"))
cond + facet_wrap(~sampleround) 


turb <- eureka %>% 
  ggplot(aes(x=turb, y=depth_m, color=pond)) + 
  geom_point(aes(color=pond))+ geom_path(aes(color=pond))+
  scale_y_reverse()+  ylab("Depth (m)") + theme_bw() 
turb + facet_wrap(~sampleround) 


chla <- eureka %>% filter(chla <100) %>%
  ggplot(aes(x=chla, y=depth_m, color=pond)) + 
  geom_point(aes(color=pond))+ geom_path(aes(color=pond))+
  scale_y_reverse()+  ylab("Depth (m)") + theme_bw() +
  scale_color_manual(values=c("#88A0A8", "#88A0A8", "#88A0A8", "#0A1045", 
                              "#88A0A8", "#0A1045", "#0A1045", "#0A1045")) +
  theme(legend.position = "none")
chla + facet_wrap(~sampleround) 


nh4 <- eureka %>% 
  ggplot(aes(x=nh4_mgl, y=depth_m, color=pond)) + 
  geom_point(aes(color=pond))+ geom_path(aes(color=pond))+
  scale_y_reverse()+  ylab("Depth (m)") + theme_bw() + 
  scale_x_log10()+   
  scale_color_manual(values=c("#88A0A8", "#88A0A8", "#88A0A8", "#0A1045",
                              "#88A0A8", "#0A1045", "#0A1045", "#0A1045")) +
  theme(legend.position = "none")
nh4 + facet_wrap(~sampleround) 


no3 <- eureka %>% 
  ggplot(aes(x=no3_mgl, y=depth_m, color=pond)) + 
  geom_point(aes(color=pond))+ geom_path(aes(color=pond))+
  scale_y_reverse()+  ylab("Depth (m)") + theme_bw() +
  scale_color_manual(values=c("#88A0A8", "#88A0A8", "#88A0A8", "#0A1045", 
                              "#88A0A8", "#0A1045", "#0A1045", "#0A1045")) +
  theme(legend.position = "none")
no3 + facet_wrap(~sampleround) 



## PLOT MEANS ####

#* code ####
summarySE <- function(data=NULL, measurevar, groupvars=NULL, na.rm=FALSE,
                      conf.interval=.95, .drop=TRUE) {
  library(plyr)
  
  # New version of length which can handle NA's: if na.rm==T, don't count them
  length2 <- function (x, na.rm=FALSE) {
    if (na.rm) sum(!is.na(x))
    else       length(x)
  }
  
  # This does the summary. For each group's data frame, return a vector with
  # N, mean, and sd
  datac <- ddply(data, groupvars, .drop=.drop,
                 .fun = function(xx, col) {
                   c(N    = length2(xx[[col]], na.rm=na.rm),
                     mean = mean   (xx[[col]], na.rm=na.rm),
                     sd   = sd     (xx[[col]], na.rm=na.rm)
                   )
                 },
                 measurevar
  )
  
  # Rename the "mean" column    
  datac <- rename(datac, c("mean" = measurevar))
  
  datac$se <- datac$sd / sqrt(datac$N)  # Calculate standard error of the mean
  
  # Confidence interval multiplier for standard error
  # Calculate t-statistic for confidence interval: 
  # e.g., if conf.interval is .95, use .975 (above/below), and use df=N-1
  ciMult <- qt(conf.interval/2 + .5, datac$N-1)
  datac$ci <- datac$se * ciMult
  
  return(datac)
}


#get summary stats ####
eureka2 <- eureka %>% subset(notes!="bottom")

#* Temp ####
tgc <- summarySE(eureka2, measurevar="temp_c", groupvars=c("treatment", "sampleround", "depth_m"))
head(tgc)

sum_temp <- ggplot(tgc, aes(x=temp_c, y=depth_m, colour=treatment)) + 
  geom_errorbar(aes(xmin=temp_c-se, xmax=temp_c+se), width=.1) +
  geom_path() +  scale_y_reverse()+  
  geom_point() + scale_color_manual(values=c("#679436", "#324376")) + 
  theme_bw()
sum_temp + facet_wrap(~sampleround, scales="free_x")


#* DO ####
sum_do <- summarySE(eureka2, measurevar="do_perc", groupvars=c("treatment", "sampleround", "depth_m"))
head(sum_do)

plot_do_sum <- sum_do %>% subset(depth_m>0)%>% #subset(sampleround=="Round 3, June")%>%
  #subset(do_perc>25) %>%
  ggplot(aes(x=do_perc, y=depth_m, colour=treatment)) + 
  geom_vline(xintercept=100, linetype="dashed") +
  geom_errorbar(aes(xmin=do_perc-se, xmax=do_perc+se), width=.1) +
  geom_path() +  scale_y_reverse()+  
  geom_point() + scale_color_manual(values=c("#679436", "#324376")) +  
  theme_bw() + theme(legend.position = "none") +
  xlab("Dissolved oxygen (%)") + ylab("Depth (m)")
do_plot <- plot_do_sum + facet_wrap(~sampleround, scales="free_x")
do_plot

#* ph ####
sum_ph <- summarySE(eureka2, measurevar="ph", groupvars=c("treatment", "sampleround", "depth_m"))
head(sum_ph)

plot_ph_sum <- sum_ph %>%
  ggplot(aes(x=ph, y=depth_m, colour=treatment)) + 
  geom_errorbar(aes(xmin=ph-se, xmax=ph+se), width=.1) +
  geom_path() +  scale_y_reverse()+  
  geom_point() + scale_color_manual(values=c("#679436", "#324376")) + 
  theme_bw()
plot_ph_sum + facet_wrap(~sampleround, scales="free_x")

#* cond ####
sum_cond <- summarySE(eureka2, measurevar="cond", groupvars=c("treatment", "sampleround", "depth_m"))
head(sum_cond)

plot_sum_cond <- sum_cond %>% filter(depth_m > 0) %>%
  ggplot(aes(x=cond, y=depth_m, colour=treatment)) + 
  geom_errorbar(aes(xmin=cond-se, xmax=cond+se), width=.1) +
  geom_path() +  scale_y_reverse()+  
  geom_point() + scale_color_manual(values=c("#679436", "#324376")) + 
  theme_bw()
plot_sum_cond + facet_wrap(~sampleround, scales="free_x")


#* chla ####
eureka3 <- eureka2 %>% subset(chla<= 200) 
sum_chla <- summarySE(eureka3, measurevar="chla", groupvars=c("treatment", "sampleround", "depth_m"))
head(sum_chla)

plot_sum_chla <- sum_chla %>% 
  subset(depth_m>0)%>% #subset(sampleround=="Round 3, June")%>%
  ggplot(aes(x=chla, y=depth_m, colour=treatment)) + 
  geom_errorbar(aes(xmin=chla-se, xmax=chla+se), width=.1) +
  geom_path() +  scale_y_reverse()+  
  geom_point() + scale_color_manual(values=c("#679436", "#324376")) + 
  theme_bw() + xlab("Chlorophyll a (ug/L)") + ylab("Depth (m)")+
  labs(color='Land use type') #+ theme(legend.position = c(.8,.8))
chla_plot <- plot_sum_chla  + facet_wrap(~sampleround, scales="free_x")
chla_plot

#* nh4 ####
sum_nh4 <- summarySE(eureka2, measurevar="nh4_mgl", groupvars=c("treatment", "sampleround", "depth_m"))
head(sum_nh4)

plot_sum_nh4 <- sum_nh4 %>% 
  ggplot(aes(x=nh4_mgl, y=depth_m, colour=treatment)) + 
  geom_errorbar(aes(xmin=nh4_mgl-se, xmax=nh4_mgl+se), width=.1) +
  geom_path() +  scale_y_reverse()+  
  geom_point() + scale_color_manual(values=c("#679436", "#324376")) + 
  theme_bw()
plot_sum_nh4  + facet_wrap(~sampleround, scales="free_x")


#* no3 ####
sum_no3 <- summarySE(eureka2, measurevar="no3_mgl", groupvars=c("treatment", "sampleround", "depth_m"))
head(sum_no3)

plot_sum_no3 <- sum_no3 %>% 
  ggplot(aes(x=no3_mgl, y=depth_m, colour=treatment)) + 
  geom_errorbar(aes(xmin=no3_mgl-se, xmax=no3_mgl+se), width=.1) +
  geom_path() +  scale_y_reverse()+  
  geom_point() + scale_color_manual(values=c("#679436", "#324376")) + 
  theme_bw()
plot_sum_no3  + facet_wrap(~sampleround, scales="free_x")



plot_grid(do_plot, chla_plot, labels = c('A', 'B'), label_size = 12)
