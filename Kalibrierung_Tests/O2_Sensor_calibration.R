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
data <- read.table(paste0(datapfad_Pico,"230703.csv"),sep = ",",header = T,dec = ",")
names(data) <- 
  str_replace_all(names(data),c("Mittel" = "_","\\." = "","X" = "date_int"))


data$Grove_V <- RcppRoll::roll_mean(data$Grove_V,30,fill = NA)

data$date <- as_datetime(data$date_int, tz = "CET")
CO2_date <- ymd_hm("23.07.03 13:20", tz = "CET")
atm_date <- ymd_hm("23.07.03 11:30", tz = "CET")
ggplot(data)+
  geom_vline(xintercept = c(CO2_date,atm_date))+
  geom_line(aes(date,SK25_mV))

sensors <- c("Grove","SK25","KE50")
cal_df <- data %>% 
  summarise(across(matches(sensors),function(x) c(x[date == atm_date],x[date == CO2_date]))) %>% 
  mutate(O2_perc = rep(c(20.95,0)))
fm_df <- cal_df %>% 
  summarise(across(matches(sensors),function(x)(glm(O2_perc ~ x))$coefficients))
names(data)
for(sensor in sensors){
  sensor_V <- grep(sensor,names(data),value = T)
  data[,paste0(sensor,"_perc")] <- data[,sensor_V] * fm_df[2,sensor_V] + fm_df[1,sensor_V]
  data[,paste0(sensor,"_diff")] <- c(NA,diff(data[,sensor_V]))
}

quantile(data$Grove_diff,na.rm = T)
ggplot(data)+
  geom_point(aes(date,Grove_diff,col="Grove"))+
  geom_point(aes(date,SK25_diff,col="SK25"))+
  ylim(c(-0.01,0.01))
Grove_fm_data <- subset(data,abs(Grove_diff) < 0.005 & abs(SK25_diff) < 0.005 )


ggplot(data)+
  geom_line(aes(date,Grove_V,col="Grove"))+
  geom_line(aes(date,SK25_mV,col="SK25"))+
  geom_point(data = Grove_fm_data,aes(date,Grove_V,col="Grove"))+
  geom_point(data = Grove_fm_data,aes(date,SK25_mV,col="SK25"))



ggplot(Grove_fm_data)+
  geom_line(aes(SK25_perc,Grove_V))

Grove_fm <- glm(SK25_perc ~ Grove_V,data = Grove_fm_data)

fm_df$Grove_V <- Grove_fm$coefficients
summary(Grove_fm)

data$Grove_perc <- predict(Grove_fm,newdata = data)

data_long <- tidyr::pivot_longer(data,matches("_perc"),values_to = "O2",names_to = "sensor")

ggplot(subset(data_long))+
  geom_vline(xintercept = c(atm_date,CO2_date),linetype = 2,col = "grey")+
  geom_hline(yintercept = c(20.95,0),linetype = 2,col = "grey")+
  geom_line(aes(date,O2,col=sensor))+
  geom_point(data = data.frame(x= c(atm_date,CO2_date), y= c(20.95,0)),aes(x,y),pch= 3,size=3,stroke = 1)+
  labs(y = "O2 (%)",title = "calibration points")+
  ggsave(paste0(plotpfad_O2_test,"O2_calibration.png"),width = 6,height = 5)

O2_fm <- fm_df
save(O2_fm,file = paste0(datapfad_Pico,"O2_fm.Rdata"))
