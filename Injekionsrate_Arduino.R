#pfade definieren
detach("package:pkg.WWM", unload = TRUE)
hauptpfad <- "C:/Users/ThinkPad/Documents/FVA/P01677_WindWaldMethan/"
metapfad<- paste0(hauptpfad,"Daten/Metadaten/")
metapfad_harth<- paste0(metapfad,"Hartheim/")
metapfad_comsol<- paste0(metapfad,"COMSOL/")

datapfad_inj<- paste0(hauptpfad,"Daten/Urdaten/Injektionsrate_Arduino/")
plotpfad_harth <- paste0(hauptpfad,"Dokumentation/Berichte/plots/hartheim/")
samplerpfad <- paste0(hauptpfad,"Daten/aufbereiteteDaten/sampler_data/") 
datapfad_harth <- paste0(hauptpfad,"Daten/aufbereiteteDaten/Hartheim/") 
klimapfad<- paste0(hauptpfad,"Daten/Urdaten/Klimadaten_Hartheim/")
soilpfad<-paste0(hauptpfad,"Daten/Urdaten/Boden_Hartheim/")
kammer_datapfad <- paste0(hauptpfad,"Daten/aufbereiteteDaten/Kammermessungen/")
#Packages laden
library(pkg.WWM)
packages<-c("lubridate","stringr","ggplot2","units","dplyr")
check.packages(packages)


files <- list.files(datapfad_inj,".TXT$",full.names = T)
data_ls <- lapply(files,read.table,sep=";",header=T)
data <- do.call(rbind,data_ls)
colnames(data)
data$date <- ymd_hms(data$date)
range(data$date)
range(data$CO2_ppm)
data <- data[order(data$date),]
data$CO2_ppm[data$CO2_ppm < 0] <- NA
ggplot(data)+
  geom_line(aes(date,CO2_ppm))+xlim(ymd_h(c("2021_04-06 14","2021_04-07 16")))
