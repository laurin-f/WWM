hauptpfad <- "C:/Users/ThinkPad/Documents/FVA/P01677_WindWaldMethan/"
metapfad<- paste0(hauptpfad,"Daten/Metadaten/")
metapfad_harth<- paste0(metapfad,"Hartheim/")
metapfad_comsol<- paste0(metapfad,"COMSOL/")
datapfad<- paste0(hauptpfad,"Daten/Urdaten/Dynament/")
plotpfad_PPchamber <- paste0(hauptpfad,"Dokumentation/Berichte/plots/PP_Kammer/")
samplerpfad <- paste0(hauptpfad,"Daten/aufbereiteteDaten/sampler_data/") 


klimapfad_CR1000<- paste0(hauptpfad,"Daten/Urdaten/Klimadaten_Hartheim/Hartheim CR1000/")

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

datelim <- ymd_h("23.04.10 05","23.04.18 05")

micro <- read_GGA(datelim = datelim,table.name = "micro")
names(micro)
quantile(micro$H2O)
CO2 <- 
ggplot(micro)+
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank())+
#  geom_vline(xintercept = ymd_h("23.04.13 04"))+
  geom_point(aes(date,CO2),pch=20)+
  ylim(300,1600)
CH4 <- ggplot(micro)+
  geom_point(aes(date,CH4),pch=20)+
  ylim(-1,5)+
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank())
H2O<- ggplot(micro)+
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank())+
  ylim(-10,20000)+
  geom_point(aes(date,H2O),pch=20)
Temp <- ggplot(micro)+
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank())+
  geom_point(aes(date,GasT_C,col="Gas"),pch=20)+
  geom_point(aes(date,AmbT_C,col="Amb"),pch=20)+
  labs(y = "T_C")
P_torr <- ggplot(micro)+
  geom_point(aes(date,GasP_torr),pch=20)
png(paste0(plotpfad_PPchamber,"MGGA_defect.png"),width = 9,height = 8,units = "in",res=300)

egg::ggarrange(CO2,CH4,H2O,Temp,P_torr,ncol=1)
dev.off()
