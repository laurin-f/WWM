hauptpfad <- "C:/Users/ThinkPad/Documents/FVA/P01677_WindWaldMethan/"
metapfad<- paste0(hauptpfad,"Daten/Metadaten/")
metapfad_harth<- paste0(metapfad,"Hartheim/")
metapfad_comsol<- paste0(metapfad,"COMSOL/")
datapfad<- paste0(hauptpfad,"Daten/Urdaten/Dynament/")
plotpfad_PPchamber <- paste0(hauptpfad,"Dokumentation/Berichte/plots/PP_Kammer/")
samplerpfad <- paste0(hauptpfad,"Daten/aufbereiteteDaten/sampler_data/") 


klimapfad<- paste0(hauptpfad,"Daten/Urdaten/Klimadaten_Hartheim/")
soilpfad<-paste0(hauptpfad,"Daten/Urdaten/Boden_Hartheim/")
kammer_datapfad <- paste0(hauptpfad,"Daten/aufbereiteteDaten/Kammermessungen/")
datapfad_PP_Kammer <- paste0(hauptpfad,"Daten/aufbereiteteDaten/PP_Kammer/") 

chamber_arduino_pfad <- paste0(hauptpfad,"/Daten/Urdaten/Kammermessungen_Arduino/")
metapfad_PP <- paste0(metapfad,"PP_Kammer/")
detach("package:pkg.WWM", unload = TRUE)
library(pkg.WWM)
packages<-c("lubridate","stringr","ggplot2","units","dplyr","readODS")
check.packages(packages)
theme_set(theme_classic())


datelim <- ymd_h("22.12.05 10","22.12.09 10")
datelim2 <- ymd_hm("22.12.08 10:55","22.12.08 11:00")
datelim2 <- ymd_hm("22.12.05 10:55","22.12.05 11:00")
datelim2 <- ymd_hm("22.12.03 13:00","22.12.03 14:00")
data <- read_PP(datelim2)

data <- data %>% 
  group_by(id) %>%
  mutate(P_roll = RcppRoll::roll_mean(P,3*60,fill=NA))

ggplot(subset(data,id < 6))+
  geom_line(aes(date,P,col=factor(id)))

data_1min <- read_PP(datelim,table="PP_1min")
data_1min <- data_1min %>% 
  group_by(id) %>%
  mutate(P_roll = RcppRoll::roll_mean(P,3*60,fill=NA))

ggplot(subset(data_1min,id < 6))+
  geom_line(aes(date,P_roll,col=factor(id)))
ggplot(subset(data_1min,id < 6))+
  geom_line(aes(date,PPC,col=factor(id)))
  
