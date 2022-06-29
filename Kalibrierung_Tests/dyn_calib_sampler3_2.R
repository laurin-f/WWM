hauptpfad <- "C:/Users/ThinkPad/Documents/FVA/P01677_WindWaldMethan/"
metapfad<- paste0(hauptpfad,"Daten/Metadaten/")
metapfad_PP <- paste0(metapfad,"PP_Kammer/")
metapfad_comsol<- paste0(metapfad,"COMSOL/")
datapfad<- paste0(hauptpfad,"Daten/Urdaten/Dynament/")
datapfad_probe3<- paste0(hauptpfad,"Daten/Urdaten/Profileprobe3_Arduino/")
plotpfad_PPchamber <- paste0(hauptpfad,"Dokumentation/Berichte/plots/PP_Kammer/")
samplerpfad <- paste0(hauptpfad,"Daten/aufbereiteteDaten/sampler_data/") 

datapfad_PP_Kammer <- paste0(hauptpfad,"Daten/aufbereiteteDaten/PP_Kammer/") 

klimapfad<- paste0(hauptpfad,"Daten/Urdaten/Klimadaten_Hartheim/")
soilpfad<-paste0(hauptpfad,"Daten/Urdaten/Boden_Hartheim/")
kammer_datapfad <- paste0(hauptpfad,"Daten/aufbereiteteDaten/Kammermessungen/")
inj_pfad <- "C:/Users/ThinkPad/Documents/FVA/P01677_WindWaldMethan/Daten/Urdaten/Kammermessungen_Arduino"
detach("package:pkg.WWM", unload = TRUE)
library(pkg.WWM)
packages<-c("lubridate","stringr","ggplot2","units","dplyr","readODS")
check.packages(packages)

test <- read.csv(paste0(datapfad_probe3,"000101.TXT"),sep=";")
datelim <- ymd_hm("2022.06.28 12:00","2022.06.28 18:00")
gga <- read_GGA(datelim=datelim)
start <- range(gga$date)[1]-60*15
test$date <- seq.POSIXt(start,by=4,length.out = nrow(test))

ggplot(test)+
  geom_line(aes(date,CO2_tiefe1))+
  geom_line(aes(date,CO2_tiefe2))+
  geom_line(aes(date,CO2_tiefe3))+
  geom_line(aes(date,CO2_tiefe4))+
  geom_line(aes(date,CO2_tiefe5))+
  geom_line(aes(date,CO2_tiefe6))+
  geom_line(aes(date,CO2_tiefe7))+
  geom_line(data=gga,aes(date,CO2,col="gga"))


