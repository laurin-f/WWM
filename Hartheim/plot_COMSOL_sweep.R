rm(list=ls())
#pfade definieren
detach("package:pkg.WWM", unload = TRUE)
hauptpfad <- "C:/Users/ThinkPad/Documents/FVA/P01677_WindWaldMethan/"
metapfad<- paste0(hauptpfad,"Daten/Metadaten/")
metapfad_tracer<- paste0(metapfad,"Tracereinspeisung/")
metapfad_comsol<- paste0(metapfad,"COMSOL/")
comsolpfad<- paste0(hauptpfad,"Daten/aufbereiteteDaten/COMSOL/")
plotpfad <- paste0(hauptpfad,"Dokumentation/Berichte/plots/Hartheim/")
samplerpfad <- paste0(hauptpfad,"Daten/aufbereiteteDaten/sampler_data/") 
kammer_datapfad <- paste0(hauptpfad,"Daten/aufbereiteteDaten/Kammermessungen/")
datapfad_harth <- paste0(hauptpfad,"Daten/aufbereiteteDaten/Hartheim/") 
#Packages laden
library(pkg.WWM)
packages<-c("lubridate","stringr","ggplot2","ggforce","units","egg","dplyr")
sweep_long
check.packages(packages)

######################################
#data_agg
load(paste0(kammer_datapfad,"Kammer_flux.RData"))
load(paste0(samplerpfad,"Hartheim_CO2.RData"))
load(paste0(datapfad_harth,"PPC_DS.RData"))
load(paste0(comsolpfad,"plotdata_Methodenpaper_roll.RData"))


load(paste0(datapfad_harth,"DS_long_list_withPos1.RData"))
DS_long_listPos1 <- DS_long_list
#load(paste0(datapfad_harth,"DS_long_list_poly1.RData"))
load(paste0(datapfad_harth,"DS_long_list.RData"))
#data <- data %>% 
#  mutate(across(matches("CO2_tracer"),~case_when(. < 0 ~ 0,
#                                                T ~ .)))

ggplot(DS_long_listPos1$SWC)+
  geom_line(aes(date,DSD0,group=id,col="SWC"))+
  geom_line(data=DS_long_list$drift,aes(date,DSD0,group=id,col="drift"))+
  geom_line(data=DS_long_listPos1$drift,aes(date,DSD0,group=id,col="driftPo2"),linetype=2)

data_sub <- subset(data,Position == 7 & tiefe > -27 & tiefe < 0)
data_sub <- subset(data,date >ymd_h("2020-06-30 02")& date <ymd_h("2020-07-29 12")  )

ggplot(data_sub)+
#  geom_hline(yintercept = 0,linetype=2)+
  geom_line(aes(date,CO2_roll_inj,group=tiefe))+
  geom_line(data=subset(data_sub,Pumpstufe != 0),aes(date,CO2_roll_inj,group=tiefe),col="grey")+
  #geom_line(aes(date,preds_SWC_T,group=tiefe,col="SWC_T"))+
  geom_line(aes(date,preds_SWC,group=tiefe,col="SWC"))+
  #geom_line(aes(date,preds_SWC_WS,group=tiefe,col="SWC_WS"))+
  geom_line(aes(date,preds_drift,group=tiefe,col="drift"))+
  #facet_grid(tiefe~.,scales="free")+
  labs(x="",y=expression(CO[2]~"[ppm]"))

    

ggplot(subset(data,Position == 7 & tiefe > -17))+
 geom_hline(yintercept = 0,linetype=2)+

  geom_line(aes(date,CO2_tracer_drift,group=tiefe,col="drift"))+
  geom_line(aes(date,CO2_mol_per_m3,group=tiefe,col="SWC"))#+
  

ggplot(subset(data,Position == 7:8 & tiefe == 0))+
#  geom_hline(yintercept = 0,linetype=2)+
  
  geom_line(aes(date,Wind,group=tiefe))

