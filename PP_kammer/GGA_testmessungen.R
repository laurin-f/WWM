hauptpfad <- "C:/Users/ThinkPad/Documents/FVA/P01677_WindWaldMethan/"
metapfad<- paste0(hauptpfad,"Daten/Metadaten/")
metapfad_harth<- paste0(metapfad,"Hartheim/")
metapfad_comsol<- paste0(metapfad,"COMSOL/")
datapfad<- paste0(hauptpfad,"Daten/Urdaten/Dynament/")
plotpfad_PPchamber <- paste0(hauptpfad,"Dokumentation/Berichte/plots/PP_Kammer/")
samplerpfad <- paste0(hauptpfad,"Daten/aufbereiteteDaten/sampler_data/") 
datapfad_PP_Kammer <- paste0(hauptpfad,"Daten/aufbereiteteDaten/PP_Kammer/") 
klimapfad<- paste0(hauptpfad,"Daten/Urdaten/Klimadaten_Hartheim/")
soilpfad<-paste0(hauptpfad,"Daten/Urdaten/Boden_Hartheim/")
kammer_datapfad <- paste0(hauptpfad,"Daten/aufbereiteteDaten/Kammermessungen/")

chamber_arduino_pfad <- paste0(hauptpfad,"/Daten/Urdaten/Kammermessungen_Arduino/")
metapfad_PP <- paste0(metapfad,"PP_Kammer/")
detach("package:pkg.WWM", unload = TRUE)
library(pkg.WWM)
packages<-c("lubridate","stringr","ggplot2","units","dplyr","readODS")
check.packages(packages)


pp_bemerkungen <- read_ods(paste0(metapfad_PP,"PP_Kammer_Bemerkungen.ods"))
pp_bemerkungen$Start <- dmy_hm(pp_bemerkungen$Start)

pp_chamber <- read_ods(paste0(metapfad_PP,"PP_Kammer_Messungen.ods"))
pp_chamber$Start <- dmy_hm(pp_chamber$Start)
pp_chamber$Ende <- dmy_hm(pp_chamber$Ende)

micro <- read_GGA(table.name = "micro",datelim=t(pp_chamber[8,c("Start","Ende")]))
gga <- read_GGA(table.name = "gga",datelim=t(pp_chamber[8,c("Start","Ende")]))

ggplot(gga)+
  geom_line(aes(date,CO2))+
  geom_point(aes(date,CO2))
ggplot(sub_daterange(micro,ymd_hm("2022.04.04 16:00","2022.04.04 18:00")))+
  geom_line(aes(date,CO2))+
  geom_vline(data=pp_bemerkungen[4:6,],aes(xintercept=Start))
range(micro$date)
