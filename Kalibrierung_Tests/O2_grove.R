hauptpfad <- "C:/Users/ThinkPad/Documents/FVA/P01677_WindWaldMethan/"
datapfad<- paste0(hauptpfad,"Daten/Urdaten/O2_grove/")
datapfad_Pico<- paste0(hauptpfad,"Daten/Urdaten/Picolog/")
plotpfad_O2_test <- paste0(hauptpfad,"Dokumentation/Berichte/plots/O2_test/")


detach("package:pkg.WWM", unload = TRUE)
library(pkg.WWM)
packages<-c("lubridate","stringr","ggplot2","units","dplyr","readODS")
check.packages(packages)

files <- list.files(datapfad,"\\d{6}.*.TXT",full.names = T)
data_ls <- lapply(files,read.table,sep = ";",header = T)
data <- do.call(rbind,data_ls)
class(data$O2_perc)
data$date <- ymd_hms(data$date)
ggplot(data)+
#  geom_line(aes(date, O2_perc))+
  geom_line(aes(date, RcppRoll::roll_mean(O2_perc,3600,fill = NA)))





###################
#Vergleich Arduino und Picolog
#1 Atem in geschlossenen Eimer 
Arduino <- read.table(paste0(datapfad,"230414 (2).TXT"),sep = ";",header = T)
Arduino$date_wrong <- ymd_hms(Arduino$date)

Picolog <- read.table(paste0(datapfad_Pico,"230517.csv"),sep = ",",header = T,dec = ",")
names(Picolog) <- c("date_int","V")
sapply(Arduino,class)
lubridate::as_date()
Picolog$date <- as_datetime(Picolog$date_int)


Arduino$date <- Arduino$date_wrong - (max(Arduino$date_wrong)-max(Picolog$date)) + 60

data <- merge(Arduino,Picolog)
fm <- glm(O2_perc~V,data=data)
data$O2_pico <- predict(fm,newdata = data)
ggplot(data)+
  geom_line(aes(date,O2_perc,col="Arduino"))+
  geom_line(aes(date,O2_pico,col="Picolog"))+
  ggsave(paste0(plotpfad_O2_test,"Arduino_vs_Picolog.png"),width = 6,height = 4)

##############
#Atem test offen
Arduino <- read.table(paste0(datapfad,"230517.TXT"),sep = ";",header = T)
Arduino$date_raw <- ymd_hms(Arduino$date,tz = "CET")

Picolog <- read.table(paste0(datapfad_Pico,"230517_2.csv"),sep = ",",header = T,dec = ",")
names(Picolog) <- c("date_int","V")
Picolog$date <- as_datetime(Picolog$date_int)


Arduino$date <- Arduino$date_raw - (max(Arduino$date_raw)-max(Picolog$date)) + 4

data <- merge(Arduino,Picolog)
fm2 <- glm(O2_perc~V,data=data)
data$O2_pico <- predict(fm2,newdata = data)
ggplot(data)+
  geom_line(aes(date,O2_perc,col="Arduino"))+
  geom_line(aes(date,O2_pico,col="Picolog"))+
  ggsave(paste0(plotpfad_O2_test,"Arduino_vs_Picolog_atemtest.png"),width = 6,height = 4)

###########################
#Vergleich Grove SK-25 und KE-50
Picolog <- read.table(paste0(datapfad_Pico,"230619.csv"),sep = ",",header = T,dec = ",")
names(Picolog) <- c("date_int","Grove","SK25","KE50")

Picolog$date <- as_datetime(Picolog$date_int)
Picolog$SK25 <- abs(Picolog$SK25)
Picolog$KE50 <- abs(Picolog$KE50)
Pico_long <- tidyr::pivot_longer(Picolog,2:4,names_to = "sensor")
ggplot(Pico_long)+
  geom_line(aes(date,value,col=sensor))+
  facet_wrap(~sensor,scales = "free_y",ncol=1)+
  labs(y="output (mV)",x="")+
  ggsave(paste0(plotpfad_O2_test,"O2_Sensortest_1.png"),width = 7,height = 5)
  
