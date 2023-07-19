hauptpfad <- "C:/Users/ThinkPad/Documents/FVA/P01677_WindWaldMethan/"
datapfad<- paste0(hauptpfad,"Daten/Urdaten/O2_grove/")
datapfad_Pico<- paste0(hauptpfad,"Daten/Urdaten/Picolog/")
plotpfad_O2_test <- paste0(hauptpfad,"Dokumentation/Berichte/plots/O2_test/")


detach("package:pkg.WWM", unload = TRUE)
library(pkg.WWM)
packages<-c("lubridate","stringr","ggplot2","units","dplyr","readODS")
check.packages(packages)


files <- c("230626.csv",#erster Versuch RH Variation aber Pumpe lange an (sieht man bei Grove)
           "230627.csv",#T variation
           "230704.csv",#RH Variation Pumpe nur kurz an
           "230704_2.csv",#T kalibration
           "230705_T.csv",
           "230707_T_P_offen.csv",
           "230707_RH_P_offen.csv")
data <- read_Pico(files)


data_P <- read_PP(range(data$date))
data_P1 <- subset(data_P, id == 1)
data_P1$P <- RcppRoll::roll_mean(data_P1$P,5,fill = NA)

data <- merge(data,data_P1,all.x = T)
# 

###############
#date ranges

CO2_input1 <- daterange_id(data,ymd_hm("23.06.26 10:00","23.06.26 11:15"))
CO2_input2 <- daterange_id(data,ymd_hm("23.06.27 09:00","23.06.27 11:20"))
data$rh_Versuch <- daterange_id(data,ymd_hm("23.06.26 11:10","23.06.26 13:47"))
data$CO2_input <- CO2_input1 | CO2_input2
data$T_Versuch <- !data$CO2_input & data$file == 2

data[daterange_id(data,ymd_hm("23.07.07 11:42","23.07.07 11:46")),"file"] <- NA
# 
###########
#data format
data_long <- tidyr::pivot_longer(data,matches("_perc"),names_to = "sensor",values_to = "O2")
data_long$sensor <- str_remove(data_long$sensor,"_perc")
###############
data_agg <- data_long %>% 
  mutate(date = round_date(date,"mins")) %>% 
  group_by(sensor,date) %>% 
  summarise(across(matches("rh|T_|O2|file|P"),mean)) %>% 
  group_by(sensor) %>% 
  mutate(T_diff = c(NA,diff(T_C)),
         rh_diff = c(NA,diff(rh)),
         O2_diff = c(NA,diff(O2)))


################################
#Kalibration plot
ggplot(subset(data_long,CO2_input & file == 2))+
  #geom_vline(xintercept = c(atm_date,CO2_date),linetype = 2,col = "grey")+
  #geom_hline(yintercept = c(20.5,0),linetype = 2,col = "grey")+
  geom_line(aes(date,O2,col=sensor))+
  #geom_point(data = data.frame(x= c(atm_date,CO2_date), y= c(20.5,0)),aes(x,y),pch= 3,size=3,stroke = 1)+
  labs(y = "O2 (%)",title = "calibration points")#+
#ggsave(paste0(plotpfad_O2_test,"calibration1.png"),width = 6,height = 5)


###############
#threshold plots
T_th <- 0.04
rh_th <- 0.3
rh_th_2 <- 0.5
########
#T_mV
ggplot(subset(data_agg,T_Versuch == 1))+
  geom_line(aes(date, T_diff))+
  geom_hline(yintercept = T_th)
###########
#rh
ggplot(subset(data_agg,rh_Versuch == 1))+
  geom_line(aes(date, rh_diff))+
  geom_hline(yintercept = rh_th)

##########
#Versuch 1 pump dates 
ggplot(subset(data_agg, rh_Versuch == 1))+
  geom_line(aes(date,O2_diff,col=sensor))+
  geom_hline(yintercept = 0.1)
Grove_agg <- subset(data_agg,sensor == "Grove" & rh_Versuch == 1)
pump_dates <- Grove_agg$date[abs(Grove_agg$O2_diff) > 0.1]

pump_df <- data.frame(start = pump_dates[c(1,4,7)] - 60,end = pump_dates[c(2,6,8)] + 60)

##########
#Versuch 2 pump dates 

Grove_agg_2 <- subset(data_agg,sensor == "Grove" & file == 3)
pump_dates_2 <- Grove_agg_2$date[abs(Grove_agg_2$O2_diff) > 0.35]
ggplot(subset(data_long,file == 3))+
  geom_line(aes(date,O2,col = sensor))+
  geom_vline(xintercept = pump_dates)

pump_df_2 <- data.frame(start = pump_dates_2[c(1,3,6,9)] - 60,end = pump_dates_2[c(2,5,8,11)] + 60)

##########
#Versuch 7 pump dates 

data_7 <- subset(data, file == 7)
ggplot(data_7)+
  geom_line(aes(date,P_diff))
pump_dates_7 <- unique(round_date(data_7$date[which(abs(data_7$P_diff) > 40)],"min"))

ggplot(subset(data_long,file == 7))+
  geom_line(aes(date,O2,col = sensor))+
  geom_vline(xintercept = pump_dates_7)

pump_df_7 <- data.frame(start = pump_dates_7[c(1,3)] - 60,end = pump_dates_7[c(2,4)] + 120)


###########
#Pumpd_dates_merge
data_agg$pump <- 0
pump_df_merge <- rbind(pump_df,pump_df_2,pump_df_7)
for(i in 1:nrow(pump_df_merge)){
  ids <- daterange_id(data_agg,c(pump_df_merge$start[i],pump_df_merge$end[i]))
  data_agg$pump[ids] <- 1
}

#####################################
#subsets for Scatter plots
sub1 <- subset(data_agg,(rh_Versuch == 1) & abs(T_diff) < T_th & abs(rh_diff) < rh_th& pump == 0 & T_C > 29.15 & T_C < 29.25)
sub2 <- subset(data_agg,T_Versuch == 1 & abs(T_diff) < T_th)
sub3 <- subset(data_agg,(file == 3) & abs(T_diff) < T_th & abs(rh_diff) < rh_th_2 & pump == 0& T_C > 23.8)
sub4 <- subset(data_agg,(file == 4) & abs(T_diff) < T_th)
sub5 <- subset(data_agg,(file == 5) & abs(T_diff) < T_th)

sub6 <- subset(data_agg,(file == 6) & abs(T_diff) < T_th)
sub7 <- subset(data_agg,(file == 7) & abs(T_diff) < 0.1 & abs(rh_diff) < rh_th_2 & pump == 0)

sub_ls <- list(sub1,sub2,sub3,sub4,sub5,sub6,sub7)

############
#time line without fast changes
###########
# Versuch 1 rh
ggplot()+
  geom_point(data = subset(data_agg,rh_Versuch == 1),aes(date,rh),alpha =0.2)+
  geom_point(data = subset(data_agg,rh_Versuch == 1 & abs(rh_diff) < rh_th),aes(date,rh),col=2)+
  geom_point(data = sub1,aes(date,rh),col=3)

ggplot()+
  geom_point(data = subset(data_agg,rh_Versuch == 1),aes(date,T_C),alpha =0.2)+
  geom_point(data = sub1,aes(date,T_C),col=3)
########
#Versuch 2 T_mV
ggplot()+
  geom_point(data = subset(data_agg,T_Versuch == 1),aes(date,T_C),alpha =0.2)+
  geom_point(data = sub2,aes(date,T_C),col=3)

###############
#Versuch 3 RH
ggplot()+
  geom_point(data = subset(data_agg,file == 3),aes(date,rh),alpha =0.2)+
  geom_point(data = subset(data_agg,file == 3 & abs(rh_diff) < rh_th_2),aes(date,rh),col=2)+
  geom_point(data = sub3,aes(date,rh),col=3)

###############
#Versuch 4 T
ggplot()+
  geom_point(data = subset(data_agg,file == 4),aes(date,T_C),alpha =0.2)+
  geom_point(data = sub4,aes(date,T_C),col=3)
###############
#Versuch 5 T
ggplot()+
  geom_point(data = subset(data_agg,file == 5),aes(date,T_C),alpha =0.2)+
  geom_point(data = sub5,aes(date,T_C),col=3)


###############
#Versuch 7 RH
ggplot()+
  geom_point(data = subset(data_agg,file == 7),aes(date,rh),alpha =0.2)+
  geom_point(data = sub7,aes(date,rh),col=3)




#####################
#Timeline 1


O2_plot_1 <- 
  ggplot(subset(data_long,rh_Versuch == 1))+
  geom_rect(data = pump_df,aes(ymin=-Inf, ymax = Inf, xmin = start, xmax = end,fill = ""),alpha = 0.3)+
  geom_line(aes(date,O2,col = sensor))+
  geom_point(data=sub1,aes(date,O2,col=sensor,pch = ""))+
  labs(y = "O2 (%)",col="sensor",title = "Versuch 1: RH variation",shape = "selection",fill = "pump")+
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank())
rh_plot_1 <- ggplot(subset(data_long,rh_Versuch))+
  geom_line(aes(date,rh))+
  labs(y = "rh (%)")+
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank())
T_plot_1 <- ggplot(subset(data_long,rh_Versuch))+
  geom_line(aes(date,T_C))+
  labs(y = "T (°C)")+
  theme(axis.title.x = element_blank())
ggpubr::ggarrange(O2_plot_1,rh_plot_1,T_plot_1,ncol=1,align = "v",common.legend = T,legend = "right",heights = c(3,1,1))+
  ggsave(paste0(plotpfad_O2_test,"Versuch1_RH.png"),width = 7,height = 6)


####################
#Timeline 2

O2_plot_2 <- ggplot(subset(data_long,T_Versuch))+
  geom_line(aes(date,O2,col = sensor))+
  geom_point(data=sub2,aes(date,O2,col=sensor,pch = ""))+
  labs(y = "O2 (%)",col="sensor",title = "Versuch 2: T variation",shape = "selection")+
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank())
rh_plot_2 <- 
  ggplot(subset(data_long,T_Versuch))+
  geom_line(aes(date,rh))+
  labs(y = "rh (%)")+
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank())
T_plot_2 <- 
  ggplot(subset(data_long,T_Versuch))+
  geom_line(aes(date,T_C))+
  labs(y = "T (°C)")+
  theme(axis.title.x = element_blank())
ggpubr::ggarrange(O2_plot_2,
                  rh_plot_2,
                  T_plot_2,ncol=1,align = "v",common.legend = T,legend = "right",heights = c(3,1,1))+
  ggsave(paste0(plotpfad_O2_test,"Versuch2_T.png"),width = 7,height = 6)

####################
#Timeline 3


O2_plot_3 <- ggplot(subset(data_long,file == 3))+
  geom_line(aes(date,O2,col = sensor))+
  geom_rect(data = pump_df_2,aes(ymin=-Inf, ymax = Inf, xmin = start, xmax = end,fill = ""),alpha = 0.3)+
  geom_point(data=sub3,aes(date,O2,col=sensor,pch = ""))+
  labs(y = "O2 (%)",col="sensor",title = "Versuch 3: RH variation",shape = "selection",fill = "pump")+
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank())
rh_plot_3 <- 
  ggplot(subset(data_long,file == 3))+
  geom_line(aes(date,rh))+
  labs(y = "rh (%)")+
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank())
T_plot_3 <- 
  ggplot(subset(data_long,file == 3))+
  geom_line(aes(date,T_C))+
  labs(y = "T (°C)")+
  theme(axis.title.x = element_blank())
ggpubr::ggarrange(O2_plot_3,
                  rh_plot_3,
                  T_plot_3,ncol=1,align = "v",common.legend = T,legend = "right",heights = c(3,1,1))+
  ggsave(paste0(plotpfad_O2_test,"Versuch3_RH.png"),width = 7,height = 6)
####################
#Timeline 4


O2_plot_4 <- ggplot(subset(data_long,file == 4))+
  geom_line(aes(date,O2,col = sensor))+
  geom_point(data=sub4,aes(date,O2,col=sensor,pch = ""))+
  labs(y = "O2 (%)",col="sensor",title = "Versuch 4: T variation",shape = "selection")+
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank())
rh_plot_4 <- 
  ggplot(subset(data_long,file == 4))+
  geom_line(aes(date,rh))+
  labs(y = "rh (%)")+
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank())
T_plot_4 <- 
  ggplot(subset(data_long,file == 4))+
  geom_line(aes(date,T_C))+
  labs(y = "T (°C)")+
  theme(axis.title.x = element_blank())
ggpubr::ggarrange(O2_plot_4,
                  rh_plot_4,
                  T_plot_4,ncol=1,align = "v",common.legend = T,legend = "right",heights = c(3,1,1))+
  ggsave(paste0(plotpfad_O2_test,"Versuch4_T.png"),width = 7,height = 6)
####################
#Timeline 5


O2_plot_5 <- ggplot(subset(data_long,file == 5))+
  geom_line(aes(date,O2,col = sensor))+
  geom_point(data=sub5,aes(date,O2,col=sensor,pch = ""))+
  labs(y = "O2 (%)",col="sensor",title = "Versuch 5: T variation",shape = "selection")+
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank())
rh_plot_5 <- 
  ggplot(subset(data_long,file == 5))+
  geom_line(aes(date,rh))+
  labs(y = "rh (%)")+
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank())
T_plot_5 <- 
  ggplot(subset(data_long,file == 5))+
  geom_line(aes(date,T_C))+
  labs(y = "T (°C)")+
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank())
P_plot_5 <- 
  ggplot(subset(data_long,file == 5))+
  geom_line(aes(date,P))+
  labs(y = "P (Pa)")+
  theme(axis.title.x = element_blank())


ggpubr::ggarrange(O2_plot_5,
                  rh_plot_5,
                  T_plot_5,
                  P_plot_5,ncol=1,align = "v",common.legend = T,legend = "right",heights = c(3,1,1,1.2))+
  ggsave(paste0(plotpfad_O2_test,"Versuch5_T_P.png"),width = 7,height = 7)

####################
#Timeline 6




O2_plot_6 <- ggplot(subset(data_long,file == 6))+
  geom_line(aes(date,O2,col = sensor))+
  geom_point(data=sub6,aes(date,O2,col=sensor,pch = ""))+
  labs(y = "O2 (%)",col="sensor",title = "Versuch 6: T variation",shape = "selection")+
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank())
rh_plot_6 <- 
  ggplot(subset(data_long,file == 6))+
  geom_line(aes(date,rh))+
  labs(y = "rh (%)")+
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank())
T_plot_6 <- 
  ggplot(subset(data_long,file == 6))+
  geom_line(aes(date,T_C))+
  labs(y = "T (°C)")+
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank())
P_plot_6 <- 
  ggplot(subset(data_long,file == 6))+
  geom_line(aes(date,P))+
  labs(y = "P (Pa)")+
  theme(axis.title.x = element_blank())


ggpubr::ggarrange(O2_plot_6,
                  rh_plot_6,
                  T_plot_6,
                  P_plot_6,ncol=1,align = "v",common.legend = T,legend = "right",heights = c(3,1,1,1.2))+
  ggsave(paste0(plotpfad_O2_test,"Versuch6_T_P.png"),width = 7,height = 7)
####################
#Timeline 7




O2_plot_7 <- ggplot(subset(data_long,file == 7))+
  geom_rect(data = pump_df_7,aes(ymin=-Inf, ymax = Inf, xmin = start, xmax = end,fill = ""),alpha = 0.3)+
  geom_line(aes(date,O2,col = sensor))+
  geom_point(data=sub7,aes(date,O2,col=sensor,pch = ""))+
  labs(y = "O2 (%)",col="sensor",title = "Versuch 7: RH variation",shape = "selection")+
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank())
rh_plot_7 <- 
  ggplot(subset(data_long,file == 7))+
  geom_line(aes(date,rh))+
  labs(y = "rh (%)")+
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank())
T_plot_7 <- 
  ggplot(subset(data_long,file == 7))+
  geom_line(aes(date,T_C))+
  labs(y = "T (°C)")+
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank())
P_plot_7 <- 
  ggplot(subset(data_long,file == 7))+
  geom_line(aes(date,P))+
  labs(y = "P (Pa)")+
  theme(axis.title.x = element_blank())


ggpubr::ggarrange(O2_plot_7,
                  rh_plot_7,
                  T_plot_7,
                  P_plot_7,ncol=1,align = "v",common.legend = T,legend = "right",heights = c(3,1,1,1.2))+
  ggsave(paste0(plotpfad_O2_test,"Versuch7_RH_P.png"),width = 7,height = 7)

##############
#Scatter plots


#############
#scatterplot
######
plot_formula <- y~ (x)

####################
#####
#scatterplot einzelne Versuche
for(i in seq_along(sub_ls)){
  rh_scatter_i <- 
    ggplot(sub_ls[[i]])+
    geom_smooth(aes(rh,O2),method = "glm",col=1,linetype = 2,lwd = 0.7)+
    geom_point(aes(rh,O2,col=T_C))+
    ggpubr::stat_regline_equation(aes(rh,O2,label =  paste(..eq.label..)),formula = plot_formula)+
    ggpubr::stat_regline_equation(label.y.npc = 0.85,aes(rh,O2,label =  ..rr.label..))+
    facet_wrap(~sensor,scales = "free_y")+
    scale_color_viridis_c()+
    labs(title = paste("Versuch",i),subtitle = "Luftfeuchte",x = "RH (%)",y= expression(O[2]~"(%)"),col="T (°C)")
  T_scatter_i <- 
    ggplot(sub_ls[[i]])+
    geom_smooth(aes(T_C,O2),method = "glm",col=1,linetype = 2,lwd = 0.7)+
    geom_point(aes(T_C,O2,col=rh))+
    ggpubr::stat_regline_equation(aes(T_C,O2,label =  paste(..eq.label..)),formula = plot_formula)+
    ggpubr::stat_regline_equation(label.y.npc = 0.85,aes(T_C,O2,label =  ..rr.label..))+
    facet_wrap(~sensor,scales = "free_y")+
    labs(subtitle = "Temperatur",x = "T (°C)",y= expression(O[2]~"(%)"),col="RH (%)")
  
  
  ggpubr::ggarrange(rh_scatter_i,T_scatter_i,ncol = 1)+
    ggsave(paste0(plotpfad_O2_test,"rh_T_scatter_",i,".png"),width = 7,height = 6)
}


ggplot(subset(sub1))+
  geom_smooth(aes(rh,O2),method = "glm",col=1,linetype = 2,lwd = 0.7)+
  geom_point(aes(rh,O2,col=T_C))+
  ggpubr::stat_regline_equation(aes(rh,O2,label =  paste(..eq.label..)),formula = plot_formula)+
  ggpubr::stat_regline_equation(label.y.npc = 0.85,aes(rh,O2,label =  ..rr.label..))+
  facet_wrap(.~sensor,scales = "free_y")+
  scale_color_viridis_c()+
  labs(title = paste("Versuch",i),x = "RH (%)",y= expression(O[2]~"(%)"),col="T (°C)")


sub3$T_cut <- str_replace_all(cut(sub3$T_C,2),c("\\(" = "", "," = "°C < T <= ","\\]" = "°C"))
ggplot(subset(sub3))+
  geom_smooth(aes(rh,O2),method = "glm",col=1,linetype = 2,lwd = 0.7)+
  geom_point(aes(rh,O2,col=T_C))+
  ggpubr::stat_regline_equation(aes(rh,O2,label =  paste(..eq.label..)),formula = plot_formula)+
  ggpubr::stat_regline_equation(label.y.npc = 0.85,aes(rh,O2,label =  ..rr.label..))+
  facet_grid(T_cut~sensor,scales = "free_y")+
  scale_color_viridis_c()+
  labs(title = "Versuch 3",x = "RH (%)",y= expression(O[2]~"(%)"),col="T (°C)")+
  ggsave(paste0(plotpfad_O2_test,"rh_scatter_T_facets.png"),width = 7,height = 6)
#################################################################
sub_all <- do.call(rbind,sub_ls)


ggplot(subset(sub_all,file %in% c(2,4,5,6)))+
  geom_smooth(aes(T_C,O2),method = "glm",col=1,linetype = 2,lwd = 0.7)+
  geom_point(aes(T_C,O2,col=rh))+
  ggpubr::stat_regline_equation(aes(T_C,O2,label =  ..eq.label..),formula = plot_formula)+
  ggpubr::stat_regline_equation(label.y.npc = 0.85,aes(T_C,O2,label =  ..rr.label..))+
  facet_wrap(~sensor)+
  labs(x = "T (°C)",y= expression(O[2]~"(%)"),col="Versuch")

T_scatter <- 
  ggplot(subset(sub_all,file %in% c(2,4,5,6)))+
  geom_smooth(aes(T_C,O2,col= factor(file)),method = "glm",linetype = 2,lwd = 0.7)+
  geom_point(aes(T_C,O2,col=factor(file)))+
  ggpubr::stat_regline_equation(aes(T_C,O2,col = factor(file),label =  paste(..eq.label..,..rr.label.., sep = "~~")),formula = plot_formula)+
  #ggpubr::stat_regline_equation(label.y.npc = 0.85,aes(T_C,O2,label =  ..rr.label..))+
  facet_wrap(~sensor)+
  #ylim(c(19.5,24))+
  labs(subtitle = "T-Versuche",x = "T (°C)",y= expression(O[2]~"(%)"),col="Versuch")

rh_scatter <- ggplot(subset(sub_all,file %in% c(1,3,7)))+
  geom_smooth(aes(rh,O2,col = factor(file)),method = "glm",linetype = 2,lwd = 0.7)+
  geom_point(aes(rh,O2,col=factor(file)))+
  ggpubr::stat_regline_equation(aes(rh,O2,col = factor(file),label =  paste(..eq.label..,..rr.label..,sep = "~~~")),formula = plot_formula)+
  #ggpubr::stat_regline_equation(label.y.npc = 0.85,aes(rh,O2,label =  ..rr.label..))+
  facet_wrap(~sensor)+
  #ylim(c(19.5,24))+
  labs(subtitle = "RH-Versuche",x = "RH (%)",y= expression(O[2]~"(%)"),col="Versuch")

ggpubr::ggarrange(rh_scatter,T_scatter,ncol = 1)+
  ggsave(paste0(plotpfad_O2_test,"rh_T_scatter_alle.png"),width = 8,height = 7)


ggplot(sub_all)+
  geom_boxplot(aes(sensor,O2,fill=factor(file)))
#############
#temp correction
names(sub_all)

O2_fm <- glm(T_C~O2,data = sub_all)
sub_all$O2_Tcorr <- predict(O2_fm,newdata = sub_all)


ggplot(sub_all)+
  geom_point(aes(O2_Tcorr,T_C,col=factor(file)))+
  facet_wrap(~sensor)

#######################
#experimetne
ranges_df <- sub_all %>% 
  group_by(sensor,file) %>% 
  summarise(across(c(O2,T_C,rh),list(min = ~min(.,na.rm = T), max = ~max(.,na.rm = T))))

ggplot(subset(ranges_df,file %in% c(2,4,5)))+
  geom_rect(aes(ymin = O2_min,ymax = O2_max,xmin = T_C_min,xmax = T_C_max,fill=sensor),alpha = 0.3)+
  facet_wrap(~file)
T_fms <- sub_all %>% 
  ungroup() %>% 
  nest_by(file,sensor) %>% 
  mutate(fm = list(glm(O2 ~ T_C + rh,data = data)),
         slope_T = fm$coefficients["T_C"],
         slope_rh = fm$coefficients["rh"])
T_fms$fm[[1]]$coefficients
ggplot(subset(T_fms,file %in% c(2,4,5)))+
  geom_point(aes(factor(file),slope_T*100,col = sensor))+
  labs(y = "O2 T-Abhängigkeit  (% O2 pro °C)",x = "Versuch")

T_fm_all <- sub_all %>% 
  ungroup() %>% 
  nest_by(sensor) %>% 
  mutate(fm = list(glm(O2 ~ T_C + rh,data = data)),
         slope_T = fm$coefficients["T_C"],
         slope_rh = fm$coefficients["rh"])
summary(T_fm_all$fm[[1]])
subset(T_fms,file == 3)
ggplot(subset(T_fms,file %in% c(1,3)))+
  geom_point(aes(factor(file),slope_rh*100,col = sensor))+
  labs(y = "O2 RH-Abhängigkeit  (%O2 pro %RH)",x = "Versuch")
