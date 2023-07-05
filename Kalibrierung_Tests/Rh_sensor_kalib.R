hauptpfad <- "C:/Users/ThinkPad/Documents/FVA/P01677_WindWaldMethan/"
datapfad<- paste0(hauptpfad,"Daten/Urdaten/O2_grove/")
datapfad_Pico<- paste0(hauptpfad,"Daten/Urdaten/Picolog/")
datapfad_testo<- paste0(hauptpfad,"Daten/Urdaten/Testo_174H/")
plotpfad_O2_test <- paste0(hauptpfad,"Dokumentation/Berichte/plots/O2_test/")


detach("package:pkg.WWM", unload = TRUE)
library(pkg.WWM)
packages<-c("lubridate","stringr","ggplot2","units","dplyr","readODS")
check.packages(packages)

#files <- list.files(datapfad,"\\d{6}.*.TXT",full.names = T)

data_Pico_1 <- read.table(paste0(datapfad_Pico,"230703.csv"),sep = ",",header = T,dec = ",")
data_Pico_1$File <- 1
data_Pico_2 <- read.table(paste0(datapfad_Pico,"230703_2.csv"),sep = ",",header = T,dec = ",")
data_Pico_2$File <- 2
data_Pico_3 <- read.table(paste0(datapfad_Pico,"230704_2.csv"),sep = ",",header = T,dec = ",")
data_Pico_3$File <- 3

data_Pico <- rbind(data_Pico_1,data_Pico_2,data_Pico_3)



names(data_Pico) <- 
  str_replace_all(names(data_Pico),c("Mittel" = "_","\\." = "","X" = "date_int"))
data_Pico$date <- as_datetime(data_Pico$date_int,tz = "CET")
data_testo_1 <- read.table(paste0(datapfad_testo,"230703.csv"),sep = ";",header = T,dec = ".")
data_testo_2 <- read.table(paste0(datapfad_testo,"230704.csv"),sep = ";",header = T,dec = ".")
data_testo <- rbind(data_testo_1,data_testo_2)

names(data_testo) <- c("ID","date","T_C","rh","x")
data_testo$date <- dmy_hms(data_testo$date,tz = "CET")

data_Pico$date <- data_Pico$date - 60
data_Pico_agg <- data_Pico %>% 
  mutate(date = round_date(date,"mins")) %>% 
  group_by(date) %>% 
  summarise(across(everything(),mean))

data <- merge(data_Pico_agg,data_testo)
data$T_diff <- c(NA,diff(data$T_C))
data$T_V_diff <- c(NA,diff(data$T_V))
data$RH_diff <- c(NA,diff(data$RH_V))


######################
#RH calibration
data_sub <- sub_daterange(data,ymd_hm("23-07-03 12:20","23-07-03 12:50",tz = "CET"))

ggplot(data_sub)+
  geom_line(aes(date,RH_diff))
################
#RH-timeline plot
ggplot(data_sub)+
  geom_line(aes(date,rh,col="testo"))+
  geom_line(aes(date,RH_V*100,col="Pico"))
################
#RH Scatter plot
fm_data <- subset(data_sub,abs(RH_diff) < 0.01)

ggplot(fm_data)+
  geom_smooth(aes(RH_V,rh),method ="glm")+
  geom_point(aes(RH_V,rh))

##############
#rh-fm
rh_fm <- glm(rh~RH_V,data = fm_data)
summary(rh_fm)


#####################################
#T calibration
# 
# data_T <- sub_daterange(data,ymd_hm("23-07-03 14:00","23-07-03 14:20",tz = "CET"))
# 
# ggplot(data_T)+
#   geom_line(aes(date, T_V_diff))+
#   geom_point(aes(date, T_V_diff))+
#   geom_hline(yintercept = c(0.01,-0.01))
# 
# ################
# #T-timeline plot
# fm_data_T <- subset(data_T,File ==2 &abs(T_V_diff) < 0.01)
# 
# cal_points <-  ymd_hm("23.07.03 14:07","23.07.03 14:17",tz = "CET")
# ggplot(data_T)+
#   geom_line(aes(date,T_C,col="T_C"))+
#   geom_line(aes(date,T_V*38,col="T_V"))+
#   geom_point(data = fm_data_T,aes(date,T_V*38,col="T_V"))+
#   geom_vline(xintercept =cal_points)
# 
# ################
# #T-scatter plot
# fm_data_T <- subset(data_T, date %in% cal_points)
# ggplot(fm_data_T)+
#   geom_smooth(aes(T_V,T_C),method = "glm")+
#   geom_point(aes(T_V,T_C))
# 
# T_fm <- glm(T_C~T_V,data = fm_data_T)
# 

#####################################
#T calibration 2

data_T <- sub_daterange(data,ymd_hm("23-07-04 14:00","23-07-04 14:36",tz = "CET"))
#data_T <- subset(data,File == 3)

ggplot(data_T)+
  geom_line(aes(date, T_V_diff))+
  geom_point(aes(date, T_V_diff))+
  geom_hline(yintercept = c(0.001,-0.001))
ggplot(data_T)+
  geom_line(aes(date, T_diff))+
  geom_point(aes(date, T_diff))+
  geom_hline(yintercept = c(0.001,-0.001))
################
#T-timeline plot
#cal_points <-  ymd_hm("23.07.03 14:07","23.07.03 14:17",tz = "CET")
fm_data_T <- subset(data_T,File ==3 &abs(T_V_diff) < 0.01 & abs(T_diff) < 0.01)

ggplot(data_T)+
  geom_line(aes(date,T_C,col="T_C"))+
  geom_line(aes(date,T_V*38,col="T_V"))+
  geom_point(data = fm_data_T,aes(date,T_C,col="T_C"))+
  geom_point(data = fm_data_T,aes(date,T_V*38,col="T_V"))
#  geom_vline(xintercept =cal_points)

################
#T-scatter plot
#fm_data_T <- subset(data_T, date %in% cal_points)
ggplot(fm_data_T)+
  geom_smooth(aes(T_V,T_C),method = "glm")+
  geom_point(aes(T_V,T_C))

T_fm <- glm(T_C~T_V,data = fm_data_T)


###################
#save
save(T_fm,rh_fm,file = paste0(datapfad_Pico,"rh_T_fm.Rdata"))
#ggplot(data)+
