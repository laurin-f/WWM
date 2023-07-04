hauptpfad <- "C:/Users/ThinkPad/Documents/FVA/P01677_WindWaldMethan/"
datapfad<- paste0(hauptpfad,"Daten/Urdaten/O2_grove/")
datapfad_Pico<- paste0(hauptpfad,"Daten/Urdaten/Picolog/")
plotpfad_O2_test <- paste0(hauptpfad,"Dokumentation/Berichte/plots/O2_test/")


detach("package:pkg.WWM", unload = TRUE)
library(pkg.WWM)
packages<-c("lubridate","stringr","ggplot2","units","dplyr","readODS")
check.packages(packages)

#files <- list.files(datapfad,"\\d{6}.*.TXT",full.names = T)
##RH_test1
data1 <- read.table(paste0(datapfad_Pico,"230626.csv"),sep = ",",header = T,dec = ",")
#T_test1
data2 <- read.table(paste0(datapfad_Pico,"230627.csv"),sep = ",",header = T,dec = ",")
#RH_test2
data3 <- read.table(paste0(datapfad_Pico,"230704.csv"),sep = ",",header = T,dec = ",")
#T_test2
data3 <- read.table(paste0(datapfad_Pico,"230704_2.csv"),sep = ",",header = T,dec = ",")

data1$Versuch <- 1
data2$Versuch <- 2
data <- rbind(data1,data2)
names(data) <- 
  str_replace_all(names(data),c("Mittel" = "_","\\." = "","X" = "date_int"))


data$date <- as_datetime(data$date_int)
data$Grove_V <- data$Grove_mV / 1000
data$Grove_V <- RcppRoll::roll_mean(data$Grove_V,60,fill = NA)

#####################
#calibration 

load(file = paste0(datapfad_Pico,"O2_fm.Rdata"))
load(file = paste0(datapfad_Pico,"rh_T_fm.Rdata"))

sensors <- c("Grove","SK25","KE50")
for(sensor in sensors){
  sensor_V <- grep(paste0(sensor,".+V"),names(O2_fm),value = T)
  data[,paste0(sensor,"_perc")] <- data[,sensor_V] * O2_fm[2,sensor_V] + O2_fm[1,sensor_V]
  data[,paste0(sensor,"_diff")] <- c(NA,diff(data[,sensor_V]))
}

data$rh <- predict(rh_fm,newdata = data)
data$T_C <- predict(T_fm,newdata = data)
###############
#date ranges
CO2_input1 <- daterange_id(data,ymd_hm("23.06.26 08:00","23.06.26 09:15"))
CO2_input2 <- daterange_id(data,ymd_hm("23.06.27 07:00","23.06.27 09:20"))
data$rh_Versuch <- daterange_id(data,ymd_hm("23.06.26 09:15","23.06.26 11:47"))
data$CO2_input <- CO2_input1 | CO2_input2
data$T_Versuch <- !data$CO2_input & data$Versuch == 2


###########
#data format

data_long <- tidyr::pivot_longer(data,matches(paste0(sensors,"_perc")),names_to = "sensor",values_to = "O2")
data_long$sensor <- str_remove(data_long$sensor,"_perc")
###############
names(data_long)
data_agg <- data_long %>% 
  mutate(date = round_date(date,"mins")) %>% 
  group_by(sensor,date) %>% 
  summarise(across(matches("rh|T_|O2|Versuch"),mean)) %>% 
  group_by(sensor) %>% 
  mutate(T_diff = c(NA,diff(T_C)),
         rh_diff = c(NA,diff(rh)),
         O2_diff = c(NA,diff(O2)))


################################
#Kalibration plot
ggplot(subset(data_long,CO2_input & Versuch == 2))+
  #geom_vline(xintercept = c(atm_date,CO2_date),linetype = 2,col = "grey")+
  #geom_hline(yintercept = c(20.5,0),linetype = 2,col = "grey")+
  geom_line(aes(date,O2,col=sensor))+
  #geom_point(data = data.frame(x= c(atm_date,CO2_date), y= c(20.5,0)),aes(x,y),pch= 3,size=3,stroke = 1)+
  labs(y = "O2 (%)",title = "calibration points")+
  ggsave(paste0(plotpfad_O2_test,"calibration1.png"),width = 6,height = 5)

#####################
#Timeline 1

ggplot(subset(data_agg, rh_Versuch == 1))+
  geom_line(aes(date,O2_diff,col=sensor))
Grove_agg <- subset(data_agg,sensor == "Grove" & rh_Versuch == 1)
pump_dates <- Grove_agg$date[abs(Grove_agg$O2_diff) > 0.15]

#O2_plot_1 <- 
  ggplot(subset(data_long,rh_Versuch == 1))+
  geom_vline(xintercept = pump_dates)+
  geom_line(aes(date,O2,col = sensor))+
  labs(y = "O2 (%)",col="sensor",title = "RH variation")#+
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
  ggsave(paste0(plotpfad_O2_test,"rh_test.png"),width = 7,height = 6)


####################
#Timeline 2

O2_plot_2 <- ggplot(subset(data_long,T_Versuch))+
  geom_line(aes(date,O2,col = sensor))+
  labs(y = "O2 (%)",col="sensor",title = "T variation")+
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
  ggsave(paste0(plotpfad_O2_test,"T_test.png"),width = 7,height = 6)


##############
#Scatter plots

###############
#threshold plots
T_th <- 0.04
rh_th <- 0.3
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



############
#time line without fast changes
########
#T_mV
ggplot()+
  geom_point(data = subset(data_agg,T_Versuch == 1),aes(date,T_C),alpha =0.2)+
  geom_point(data = subset(data_agg,T_Versuch == 1 & abs(T_diff) < T_th),aes(date,T_C),col=2)
###########
#rh
ggplot()+
  geom_point(data = subset(data_agg,rh_Versuch == 1),aes(date,rh),alpha =0.2)+
  geom_point(data = subset(data_agg,rh_Versuch == 1 & abs(rh_diff) < rh_th),aes(date,rh),col=2)

#############
#scatterplot
######
plot_formula <- y~ (x)
#rh
rh_scatter <- ggplot(subset(data_agg,(rh_Versuch == 1) & abs(T_diff) < T_th & abs(rh_diff) < rh_th))+
  ggpubr::stat_regline_equation(aes(rh,O2,label =  paste(..eq.label..)),formula = plot_formula)+
  geom_smooth(aes(rh,O2),method = "glm",col=1,linetype = 2,lwd = 0.7)+
  geom_point(data = subset(data_agg,rh_Versuch == 1),aes(rh,O2,col=sensor),alpha = 0.1)+
  geom_point(aes(rh,O2,col=sensor))+
  facet_wrap(~sensor,scales = "free_y")+
  guides(col = F)

#####
#T
T_scatter <- 
  ggplot(subset(data_agg,T_Versuch == 1 & abs(T_diff) < T_th))+
  ggpubr::stat_regline_equation(aes(T_C,O2,label =  paste(..eq.label..)),formula = plot_formula)+
  geom_smooth(aes(T_C,O2),method = "glm",col=1,linetype = 2,lwd = 0.7)+
  geom_point(data = subset(data_agg,T_Versuch == 1),aes(T_C,O2,col=sensor),alpha = 0.1)+
  geom_point(aes(T_C,O2,col=sensor))+
  facet_wrap(~sensor)+
  guides(col = F)

ggpubr::ggarrange(rh_scatter,T_scatter,ncol = 1)+
  ggsave(paste0(plotpfad_O2_test,"rh_T_scatter.png"),width = 7,height = 6)
  
ggplot(subset(data_agg,T_Versuch == 1 & abs(T_diff) < T_th))+
  geom_point(aes(rh,O2,col=T_mV))+
  facet_wrap(~sensor,scales = "free_y")
ggplot(data_long[!CO2_input,])+
  geom_point(aes(value,T_mV,col=rh))+
  facet_wrap(~sensor)
#  ggsave(paste0(plotpfad_O2_test,"O2_Sensortest_2.png"),width = 7,height = 5)

