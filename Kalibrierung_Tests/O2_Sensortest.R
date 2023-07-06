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
           "230705_T.csv")
data <- read_Pico(files)

# 

###############
#date ranges

CO2_input1 <- daterange_id(data,ymd_hm("23.06.26 10:00","23.06.26 11:15"))
CO2_input2 <- daterange_id(data,ymd_hm("23.06.27 09:00","23.06.27 11:20"))
data$rh_Versuch <- daterange_id(data,ymd_hm("23.06.26 11:10","23.06.26 13:47"))
data$CO2_input <- CO2_input1 | CO2_input2
data$T_Versuch <- !data$CO2_input & data$file == 2

# 
###########
#data format
data_long <- tidyr::pivot_longer(data,matches("_perc"),names_to = "sensor",values_to = "O2")
data_long$sensor <- str_remove(data_long$sensor,"_perc")
###############
data_agg <- data_long %>% 
  mutate(date = round_date(date,"mins")) %>% 
  group_by(sensor,date) %>% 
  summarise(across(matches("rh|T_|O2|file"),mean)) %>% 
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


###########
#Pumpd_dates_merge
data_agg$pump <- 0
pump_df_merge <- rbind(pump_df,pump_df_2)
for(i in 1:nrow(pump_df_merge)){
  ids <- daterange_id(data_agg,c(pump_df_merge$start[i],pump_df_merge$end[i]))
  data_agg$pump[ids] <- 1
}
############
#time line without fast changes

sub1 <- subset(data_agg,(rh_Versuch == 1) & abs(T_diff) < T_th & abs(rh_diff) < rh_th& pump == 0)
sub2 <- subset(data_agg,T_Versuch == 1 & abs(T_diff) < T_th)
sub3 <- subset(data_agg,(file == 3) & abs(T_diff) < T_th & abs(rh_diff) < rh_th_2 & pump == 0)
sub4 <- subset(data_agg,(file == 4) & abs(T_diff) < T_th)
sub5 <- subset(data_agg,(file == 5) & abs(T_diff) < T_th)

sub_ls <- list(sub1,sub2,sub3,sub4,sub5)
###########
# Versuch 1 rh
ggplot()+
  geom_point(data = subset(data_agg,rh_Versuch == 1),aes(date,rh),alpha =0.2)+
  geom_point(data = subset(data_agg,rh_Versuch == 1 & abs(rh_diff) < rh_th),aes(date,rh),col=2)+
  geom_point(data = sub1,aes(date,rh),col=3)
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




#####################
#Timeline 1


O2_plot_1 <- 
  ggplot(subset(data_long,rh_Versuch == 1))+
  geom_rect(data = pump_df,aes(ymin=-Inf, ymax = Inf, xmin = start, xmax = end,fill = "pump"),alpha = 0.3)+
  geom_line(aes(date,O2,col = sensor))+
  geom_point(data=sub1,aes(date,O2,col=sensor,pch = "selection"))+
  labs(y = "O2 (%)",col="sensor",title = "Versuch 1: RH variation")+
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
ggpubr::ggarrange(O2_plot_1,rh_plot_1,T_plot_1,ncol=1,common.legend = T,legend = "right",heights = c(3,1,1))+
  ggsave(paste0(plotpfad_O2_test,"Versuch1_RH.png"),width = 7,height = 6)


####################
#Timeline 2

O2_plot_2 <- ggplot(subset(data_long,T_Versuch))+
  geom_line(aes(date,O2,col = sensor))+
  geom_point(data=sub2,aes(date,O2,col=sensor,pch = "selection"))+
  labs(y = "O2 (%)",col="sensor",title = "Versuch 2: T variation")+
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
                  T_plot_2,ncol=1,common.legend = T,legend = "right",heights = c(3,1,1))+
  ggsave(paste0(plotpfad_O2_test,"Versuch2_T.png"),width = 7,height = 6)

####################
#Timeline 3


O2_plot_3 <- ggplot(subset(data_long,file == 3))+
  geom_line(aes(date,O2,col = sensor))+
  geom_rect(data = pump_df_2,aes(ymin=-Inf, ymax = Inf, xmin = start, xmax = end,fill = "pump"),alpha = 0.3)+
  geom_point(data=sub3,aes(date,O2,col=sensor,pch = "selection"))+
  labs(y = "O2 (%)",col="sensor",title = "Versuch 3: RH variation")+
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
                  T_plot_3,ncol=1,common.legend = T,legend = "right",heights = c(3,1,1))+
  ggsave(paste0(plotpfad_O2_test,"Versuch3_RH.png"),width = 7,height = 6)
####################
#Timeline 4


O2_plot_4 <- ggplot(subset(data_long,file == 4))+
  geom_line(aes(date,O2,col = sensor))+
  geom_point(data=sub4,aes(date,O2,col=sensor,pch = "selection"))+
  labs(y = "O2 (%)",col="sensor",title = "Versuch 4: T variation")+
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
                  T_plot_4,ncol=1,common.legend = T,legend = "right",heights = c(3,1,1))+
  ggsave(paste0(plotpfad_O2_test,"Versuch4_T.png"),width = 7,height = 6)
####################
#Timeline 5


O2_plot_5 <- ggplot(subset(data_long,file == 5))+
  geom_line(aes(date,O2,col = sensor))+
  geom_point(data=sub5,aes(date,O2,col=sensor,pch = "selection"))+
  labs(y = "O2 (%)",col="sensor",title = "Versuch 5: T variation")+
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
  theme(axis.title.x = element_blank())
ggpubr::ggarrange(O2_plot_5,
                  rh_plot_5,
                  T_plot_5,ncol=1,common.legend = T,legend = "right",heights = c(3,1,1))+
  ggsave(paste0(plotpfad_O2_test,"Versuch5_T.png"),width = 7,height = 6)


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
  labs(title = paste("Versuch",i),x = "RH (%)",y= expression(O[2]~"(%)"),col="T (°C)")
T_scatter_i <- 
  ggplot(sub_ls[[i]])+
  geom_smooth(aes(T_C,O2),method = "glm",col=1,linetype = 2,lwd = 0.7)+
  geom_point(aes(T_C,O2,col=rh))+
  ggpubr::stat_regline_equation(aes(T_C,O2,label =  paste(..eq.label..)),formula = plot_formula)+
  ggpubr::stat_regline_equation(label.y.npc = 0.85,aes(T_C,O2,label =  ..rr.label..))+
  facet_wrap(~sensor)+
  labs(x = "T (°C)",y= expression(O[2]~"(%)"),col="RH (%)")


ggpubr::ggarrange(rh_scatter_i,T_scatter_i,ncol = 1)+
  ggsave(paste0(plotpfad_O2_test,"rh_T_scatter_",i,".png"),width = 7,height = 6)
}

#################################################################
sub_all <- rbind(sub1,sub2,sub3,sub4,sub5)


#T_scatter <- 
  ggplot(subset(sub_all,file %in% c(2,4,5)))+
  geom_point(aes(T_C,O2,col=factor(file)))+
  facet_wrap(~sensor)+
  labs(x = "T (°C)",y= expression(O[2]~"(%)"),col="Versuch")

T_scatter <- 
  ggplot(sub_all)+
  geom_smooth(aes(T_C,O2),method = "glm",col=1,linetype = 2,lwd = 0.7)+
  geom_point(aes(T_C,O2,col=factor(file)))+
  ggpubr::stat_regline_equation(aes(T_C,O2,label =  ..eq.label..),formula = plot_formula)+
  ggpubr::stat_regline_equation(label.y.npc = 0.85,aes(T_C,O2,label =  ..rr.label..))+
  facet_wrap(~sensor)+
  labs(x = "T (°C)",y= expression(O[2]~"(%)"),col="Versuch")

rh_scatter <- ggplot(sub_all)+
  geom_smooth(aes(rh,O2),method = "glm",col=1,linetype = 2,lwd = 0.7)+
  geom_point(aes(rh,O2,col=factor(file)))+
  ggpubr::stat_regline_equation(aes(rh,O2,label =  paste(..eq.label..)),formula = plot_formula)+
  ggpubr::stat_regline_equation(label.y.npc = 0.85,aes(rh,O2,label =  ..rr.label..))+
  facet_wrap(~sensor)+
  labs(x = "RH (%)",y= expression(O[2]~"(%)"),col="Versuch")

ggpubr::ggarrange(rh_scatter,T_scatter,ncol = 1)+
  ggsave(paste0(plotpfad_O2_test,"rh_T_scatter_alle.png"),width = 7,height = 6)

#############
#temp correction
O2_fm <- glm(T_C~O2,data = sub_all)
sub_all$O2_Tcorr <- predict(O2_fm,newdata = sub_all)


ggplot(sub_all)+
  geom_point(aes(O2_Tcorr,T_C,col=factor(file)))+
  facet_wrap(~sensor)