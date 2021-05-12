#pfade definieren
detach("package:pkg.WWM", unload = TRUE)
hauptpfad <- "C:/Users/ThinkPad/Documents/FVA/P01677_WindWaldMethan/"
metapfad<- paste0(hauptpfad,"Daten/Metadaten/")
metapfad_harth<- paste0(metapfad,"Hartheim/")
metapfad_comsol<- paste0(metapfad,"COMSOL/")
datapfad<- paste0(hauptpfad,"Daten/Urdaten/Dynament/")
plotpfad_harth <- paste0(hauptpfad,"Dokumentation/Berichte/plots/Hartheim/")
samplerpfad <- paste0(hauptpfad,"Daten/aufbereiteteDaten/sampler_data/") 
datapfad_harth <- paste0(hauptpfad,"Daten/aufbereiteteDaten/Hartheim/") 
klimapfad<- paste0(hauptpfad,"Daten/Urdaten/Klimadaten_Hartheim/")
soilpfad<-paste0(hauptpfad,"Daten/Urdaten/Boden_Hartheim/")
comsolpfad<- paste0(hauptpfad,"Daten/aufbereiteteDaten/COMSOL/")
kammer_datapfad <- paste0(hauptpfad,"Daten/aufbereiteteDaten/Kammermessungen/")
aufbereitete_ds<-paste0(hauptpfad,"Daten/aufbereiteteDaten/DS_Labor/")
#Packages laden
library(pkg.WWM)
packages<-c("lubridate","stringr","ggplot2","units","dplyr","ggpubr","png","cowplot","magick","ggforce","latex2exp","imputeTS")
check.packages(packages)
theme_set(theme_classic())

#daten laden
load(file=paste0(klimapfad,"klima_data.RData"))
load(paste0(samplerpfad,"Hartheim_CO2.RData"))
load(paste0(kammer_datapfad,"Kammer_flux.RData"))
load(paste0(aufbereitete_ds,"Labor_Vergleich.RData"))
#aus "plots_COMSOL_out_Hartheim_sweep.R"
#load(paste0(comsolpfad,"plotdata_Methodenpaper.RData"))
#load(paste0(comsolpfad,"plotdata_Methodenpaper_drift.RData"))
load(paste0(comsolpfad,"plotdata_Methodenpaper_roll.RData"))
load(paste0(comsolpfad,"sandkiste_sweep_data_sub.RData"))
load(paste0(samplerpfad,"tracereinspeisung_sandkiste_agg.RData"))
load(paste0(datapfad_harth,"PPC_DS.RData"))

range3 <- range(data$date[data$Position ==8],na.rm = T)
data$date[data$date>ymd("2020.07.24")][1]
data$CO2_roll_ref_NA <- data$CO2_roll_ref
data$CO2_roll_ref_NA[is.na(data$preds_drift)] <- NA
ggplot(subset(data,date > range3[1] & date < range3[2]))+
  geom_ribbon(aes(date, ymin=preds_drift,ymax=CO2_roll_inj,fill=as.factor(-tiefe)),alpha=0.3)+
  geom_line(aes(date, CO2_roll_inj,col=as.factor(-tiefe)))+
  geom_line(aes(date, preds_drift,col=as.factor(-tiefe)))+
  guides(fill=F)+
  labs(x="",y=expression(CO[2]~"[ppm]"),col="tiefe [cm]")+
  ggsave(paste0(plotpfad_harth,"CO2_Werstattgespr.jpg"),width=5,height=2.6)

ggplot(subset(data,date > range3[1] & date < range3[2] & tiefe==-17.5 & Pumpstufe == 0))+
  geom_line(aes(date,preds_drift,col="ref adj"))+
  geom_line(aes(date,CO2_roll_ref_NA,col="ref"))+
  geom_line(aes(date,CO2_roll_inj,col="inj"))+
  facet_wrap(~paste("tiefe=",-tiefe,"cm"))+
  labs(x="",y=expression(CO[2]~"[ppm]"),col="")+
  ggsave(paste0(plotpfad_harth,"CO2_adj_Werstattgespr.jpg"),width=6.5,height=2.5)
